# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project overview

This is an R project that maps wood fuel (WF) and solid fuel appliance (SFA) heat sources across England and Wales using Energy Performance Certificate (EPC) data. The analysis estimates prevalence and geographic concentration of wood-burning heating at LSOA, ward, and local authority level, and validates EPC-derived estimates against air quality monitoring data (PM₂.₅) from the `openair` package.

## Running the pipeline

The entry point is `run.R`. Before running, update `path_data_epc_folders` in `run.R` to point to the unzipped EPC data directory on your local machine.

```r
# Restore the renv library (first time only)
renv::restore()

# Run the full pipeline (merges raw EPC data, then runs targets)
source("run.R")

# Run targets pipeline only (if raw parquet already exists)
targets::tar_make()

# Inspect pipeline status and dependency graph
targets::tar_visnetwork()
targets::tar_outdated()

# Load a specific target into the environment for inspection
targets::tar_load(data_epc_cleaned_covars)
targets::tar_read(data_epc_lsoa_cross_section)
```

The pipeline caches all targets in `_targets/` using the `qs` format (faster than default RDS). If the `qs` package is unavailable, comment out `format = "qs"` in `_targets.R` line 54.

## Architecture

### Pipeline orchestration (`_targets.R`)

The pipeline is defined as a single `list()` of `tar_target()` calls, grouped into three logical sections:

1. **Data generation** — reads, cleans, merges, and spatially enriches EPC data; produces LSOA/ward/LA cross-sections with predicted WF concentration and prevalence
2. **Figures** — creates choropleth maps, scatter plots, and facet wraps; figures are saved directly to `Output/Figures/` and `Output/Maps/` as PNG files
3. **Tables and models** — fits beta regression models and exports LaTeX tables to `Output/Tables/`

### Function loading

`_targets.R` calls `tar_source()` on two files:
- `Scripts/functions.R` — sources all analysis scripts in order
- `Scripts/LoadEnv.R` — defines global ggplot2 theme options (`scatter_plot_opts`) and the colourblind-friendly palette (`cbbPalette`)

### Data flow

```
Raw EPC CSVs (by LA)
  └─> get_epc_data()             [GetEPCData.R] — parallel merge to parquet
        └─> clean_data_epc()     [CleanDataEPC.R] — identifies WF/SFA, standardises property types
              └─> merge_data_epc_cleaned_covars()  [MergeDataEPCCleanedCovars.R]
                    ├─ make_uprn_sca_lookup()      [MakeUPRNSCALookup.R + MergeGeoDataSCA.R]
                    └─ make_lsoa_lookup_data()     [MakeLSOALookupData.R]
                          (IMD, ethnicity, age, urban/rural, ward/region geography)
                    └─> data_epc_cleaned_covars    — property-level enriched dataset
                          └─> make_summary_data_by_group()  [MakeSummaryDataByGroup.R]
                                (LSOA / ward / LA / region cross-sections)
                                └─> prepare_data_to_map()   [PrepareDataToMap.R]
                                      └─> make_choropleth_map()  [MakeChoroplethMap.R]

get_openair_data()               [GetOpenairData.R] — downloads PM₂.₅ from AURN/AQE/WAQN
  └─> merge_openair_epc_data()   [MergeOpenairEPCData.R] — spatial buffer join (500/1000/2000m)
        └─> make_openair_epc_laei_data()  [MakeOpenairEPCLAEIData.R] — overlays LAEI/NAEI grid
              └─> make_patchwork_plot_openair()  [MakePatchworkPlotOpenair.R]
                    └─> make_scatter_plot_openair()  [MakeScatterPlotOpenair.R]
                          └─> get_bootstrap_ci()     [GetBootstrapCI.R]
```

### Key design choices

**WF/SFA classification** (`CleanDataEPC.R`): Properties are classified as having a wood fuel or solid fuel appliance by string-matching `MAINHEAT_DESCRIPTION` and `SECONDHEAT_DESCRIPTION` against lookup vectors. The binary indicators `any_wood_h` / `any_sfa_h` are restricted to houses (detached, semi-detached, terrace) for prevalence calculations.

**Prevalence estimation** (`MakeSummaryDataByGroup.R`): EPC-derived WF prevalence by property type within each LSOA is applied to Census 2021 housing stock counts (`data_housing_type_census`) to produce population-adjusted estimates (`wood_perc_h_predicted`, `wood_conc_pred`). This corrects for EPC sampling bias by housing type.

**UPRN vs. postcode linkage** (`MergeDataEPCCleanedCovars.R`): EPC records with a UPRN are joined to statistical geographies via the ONS UPRN lookup. Records with missing UPRNs fall back to postcode-level joining. Postcodes spanning multiple LSOAs are excluded.

**Spatial coordinate systems**: UPRN coordinates are stored as British National Grid (BNG, EPSG:27700) for distance-based operations. The `MergeGeoDataSCA.R` script converts to Web Mercator (EPSG:3857) for longitude/latitude storage used in mapping.

**SCA overlay** (`MergeGeoDataSCA.R`): England and Wales Smoke Control Area shapefiles are spatial-joined to each UPRN to create the `sca_area` binary indicator.

### Data inputs

All data lives in `Data/` (not committed; downloaded separately per README):
- `Data/raw/epc_data/` — raw EPC parquet and unzipped CSV folders
- `Data/raw/geo_files/nsul_lookup.parquet` — ONS UPRN statistical geography lookup
- `Data/raw/sca_data/` — Smoke Control Area shapefiles (England and Wales)
- `Data/raw/lsoa_data/` — IMD scores, ethnicity, urban/rural, ward/region lookups
- `Data/raw/map_boundary_data/` — LSOA, ward, LA shapefiles
- `Data/raw/laei_data/` / `Data/raw/naei_data/` — London/National Air Emissions Inventory grids
- `Data/raw/census_data/` — Census 2021 housing type counts (TS044)
- `Data/raw/os_data/` — OS AddressBase housing type summary (pre-processed parquet)

### Output

- `Output/Figures/` — PNG plots (scatter, facet, patchwork)
- `Output/Maps/` — PNG choropleth maps
- `Output/Tables/` — LaTeX `.tex` table files (produced with `gtsave`)
