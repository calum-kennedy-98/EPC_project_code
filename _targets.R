# =============================================================================
# _targets.R — Analysis pipeline for "High resolution mapping of wood burning
# hotspots using Energy Performance Certificates: A case study in England and Wales"
#
# OVERVIEW
# This file defines the full reproducible analysis pipeline using the {targets}
# package. Running targets::tar_make() (or sourcing run.R) executes all targets
# in dependency order, caching results in _targets/ as .qs files.
#
# DATA INPUTS (all under Data/, downloaded separately — see README.md)
#   - Data/raw/epc_data/data_epc_raw.parquet   Raw EPC records (produced by run.R)
#   - Data/raw/geo_files/nsul_lookup.parquet    ONS UPRN-to-LSOA lookup
#   - Data/raw/sca_data/                        Smoke Control Area shapefiles (England, Wales)
#   - Data/raw/lsoa_data/                       IMD 2019, ethnicity (TS021), urban/rural,
#                                               ward/region lookups, LSOA area, median age
#   - Data/raw/census_data/                     Census 2021 accommodation types (TS044)
#   - Data/raw/map_boundary_data/               LSOA, ward, and LAD boundary shapefiles
#   - Data/raw/laei_data/                       London Atmospheric Emissions Inventory shapefile
#   - Data/raw/naei_data/                       National Atmospheric Emissions Inventory shapefile
#   - Data/raw/os_data/                         OS AddressBase housing type summary (parquet)
#
# PIPELINE SECTIONS
#   1. DATA GENERATION (lines ~64-338)
#      Cleans raw EPC data; builds the UPRN-to-SCA lookup and LSOA covariate lookup;
#      merges all covariates onto EPC records; produces cross-sectional summary datasets
#      at LSOA, ward, LA, and region level with Census-adjusted WF prevalence estimates;
#      loads boundary shapefiles and merges to summary data for mapping; loads LAEI and
#      NAEI inventory shapefiles; derives inline summary datasets for regional and
#      temporal trend analysis; downloads PM2.5 monitoring data from AURN, AQE, and WAQN
#      and merges to EPC data via spatial buffers of 500 m, 1000 m, and 2000 m.
#
#   2. FIGURES (lines ~339-1013)
#      Produces all manuscript and supplementary figures, including:
#      - Choropleth maps of WF/SFA prevalence and concentration at LSOA level for
#        England/Wales and London, with separate panels for national and London insets
#      - LAEI/NAEI emission and predicted concentration maps for London
#      - Facet wrap scatter plots of WF prevalence vs. IMD score, white ethnicity,
#        and median age by region
#      - Facet wrap line plots of WF prevalence by property type, IMD decile, and year
#        (urban and rural separately)
#      - Bubble scatter plots of WF prevalence vs. IMD and ethnicity by region
#      - Three-panel openair correlation plots for AURN/AQE/WAQN monitoring data,
#        comparing EPC-derived WF density and LAEI/NAEI emissions to PM2.5 levels
#      All figures saved to Output/Figures/ and Output/Maps/ as PNG files.
#
#   3. TABLES AND MODELS (lines ~1014-1329)
#      - Beta regression models of Census-adjusted WF prevalence at LSOA level,
#        stratified by urban/rural classification, with region fixed effects
#      - Summary tables: housing characteristics by WF status, LSOA decile
#        characteristics by WF prevalence, housing type composition (OS vs EPC),
#        WF prevalence by EPC sequence number and property type
#      All tables exported to Output/Tables/ as LaTeX .tex files via gtsave().
#
# OUTPUTS
#   Output/Figures/   PNG figures (700 dpi unless noted)
#   Output/Maps/      PNG choropleth maps (700 dpi)
#   Output/Tables/    LaTeX table files (.tex)
#
# NOTES
#   - format = "qs" on line 54 requires the {qs2} package. If unavailable, comment
#     out this line to fall back to the default RDS format (slower).
#   - The crew_controller block (lines ~14-18) is commented out. Uncomment and
#     adjust workers to enable parallel execution via the {crew} package.
#   - Targets with format = "file" are saved by the function itself (e.g. ggsave);
#     the target value is the file path, used by targets for change detection.
# =============================================================================

### Targets file to produce EPC manuscript

# Load packages required to define the pipeline
library(targets)
library(tarchetypes)
library(tidylog)
library(here)
library(ggplot2)
library(viridis)
library(crew)

# Set options to prefer tidylog if conflicts

# controller <- crew_controller_local(
#   name = "controller",
#   workers = 8,
#   seconds_idle = 20
# )

# Set target options:
tar_option_set(
  
  packages = c("here",
               "arrow",
               "dplyr",
               "stringr",
               "httr2",
               "data.table",
               "janitor",
               "openair",
               "lubridate",
               "ggpubr",
               "papeR",
               "gtsummary",
               "gt",
               "vroom",
               "readxl",
               "readODS",
               "sf",
               "ggplot2",
               "tidylog",
               "conflicted",
               "quarto",
               "qs2",
               "extrafont",
               "viridis",
               "patchwork",
               "tidyr",
               "scales",
               "boot",
               "rmapshaper",
               "betareg",
               "statmod"),
  format = "qs",
  memory = "transient",
  garbage_collection = TRUE
)

# Run the R scripts with custom functions:
tar_source(here::here("Scripts/functions.R"))
tar_source(here::here("Scripts/LoadEnv.R"))

# Set list of targets
list(
  
  # Generate datasets ----------------------------------------------------------
  
  tar_target(path_data_epc_raw, here("Data/raw/epc_data/data_epc_raw.parquet"),
             format = "file"),
  
  tar_target(data_epc_cleaned,
             clean_data_epc(path_data_epc_raw = path_data_epc_raw),
             format = "parquet"),

  tar_target(data_uprn_sca_lookup, make_uprn_sca_lookup(path_stat_geo_files = here("Data/raw/geo_files/nsul_lookup.parquet"),
                                                        sca_path_eng = here("Data/raw/sca_data/Smoke_Control_Area_Boundaries_and_Exemptions.shp"),
                                                        sca_path_wal = here("Data/raw/sca_data/final_wales_sca.shp"),
                                                        long_var = "gridgb1e",
                                                        lat_var = "gridgb1n"),
             format = "parquet"),
  
  # Load summary OS/EPC data (does not update with pipeline)
  tar_target(data_housing_type_os_epc, read_parquet(here("Data/raw/os_data/data_housing_type_os_epc.parquet"))),
  
  tar_target(data_housing_type_census, make_data_housing_type_census(path_data_housing_type_census = here("Data/raw/census_data/TS044-2021-4-filtered-2024-10-15T15_26_58Z.csv"),
                                                                     path_region = "Data/raw/lsoa_data/Ward_to_Local_Authority_District_to_County_to_Region_to_Country_dec22.csv",
                                                                     path_ward = "Data/raw/lsoa_data/LSOA_(2021)_to_Ward_to_Lower_Tier_Local_Authority_(May_2022)_Lookup_for_England_and_Wales.csv")),

  tar_target(data_epc_cleaned_covars, merge_data_epc_cleaned_covars(data = data_epc_cleaned,
                                                                    data_uprn_sca_lookup = data_uprn_sca_lookup,
                                                                    path_lsoa_size = "Data/raw/lsoa_data/SAM_LSOA_DEC_2021_EW_in_KM.csv",
                                                                    path_imd_eng = "Data/raw/lsoa_data/File_5_-_IoD2019_Scores.xlsx",
                                                                    path_imd_wales = "Data/raw/lsoa_data/wimd-2019-index-and-domain-scores-by-small-area.ods",
                                                                    path_lsoa11_lsoa21_lookup = "Data/raw/lsoa_data/LSOA_(2011)_to_LSOA_(2021)_to_Local_Authority_District_(2022)_Best_Fit_Lookup_for_EW_(V2).csv",
                                                                    path_ethnicity = "Data/raw/lsoa_data/TS021-2021-3-filtered-2023-10-02T10_09_04Z.csv",
                                                                    path_region = "Data/raw/lsoa_data/Ward_to_Local_Authority_District_to_County_to_Region_to_Country_dec22.csv",
                                                                    path_ward = "Data/raw/lsoa_data/LSOA_(2021)_to_Ward_to_Lower_Tier_Local_Authority_(May_2022)_Lookup_for_England_and_Wales.csv",
                                                                    path_urban_rural = "Data/raw/lsoa_data/Rural_Urban_Classification_(2011)_of_Lower_Layer_Super_Output_Areas_in_England_and_Wales.csv",
                                                                    path_age = "Data/raw/lsoa_data/sapelsoabroadage20112022.xlsx"),
             format = "parquet"),

  tar_target(data_epc_lsoa_cross_section, make_summary_data_by_group(data = data_epc_cleaned_covars,
                                                                   data_housing_type_census = data_housing_type_census,
                                                                   lsoa_var = "lsoa21cd",
                                                                   geo_level_var = "lsoa21cd",
                                                                   housing_type_var = "property_type_census",
                                                                   n_cutoff_conc_pred = 20,
                                                                   group_vars = c("lsoa21cd",
                                                                                  "rgn22nm"),
                                                                   most_recent_only = TRUE),
             format = "parquet"),
  
  tar_target(data_epc_ward_cross_section, make_summary_data_by_group(data = data_epc_cleaned_covars,
                                                                   data_housing_type_census = data_housing_type_census,
                                                                   lsoa_var = "lsoa21cd",
                                                                   geo_level_var = "wd22cd",
                                                                   housing_type_var = "property_type_census",
                                                                   n_cutoff_conc_pred = 20,
                                                                   group_vars = c("wd22cd",
                                                                                  "rgn22nm"),
                                                                   most_recent_only = TRUE),
             format = "parquet"),
  
  tar_target(data_epc_la_cross_section, make_summary_data_by_group(data = data_epc_cleaned_covars,
                                                                     data_housing_type_census = data_housing_type_census,
                                                                     lsoa_var = "lsoa21cd",
                                                                     geo_level_var = "lad22cd",
                                                                     housing_type_var = "property_type_census",
                                                                     n_cutoff_conc_pred = 20,
                                                                     group_vars = c("lad22cd",
                                                                                    "rgn22nm"),
                                                                     most_recent_only = TRUE),
             format = "parquet"),
  
  tar_target(data_epc_region_cross_section, make_summary_data_by_group(data = data_epc_cleaned_covars,
                                                               data_housing_type_census = data_housing_type_census,
                                                               lsoa_var = "lsoa21cd",
                                                               geo_level_var = "rgn22nm",
                                                               housing_type_var = "property_type_census",
                                                               n_cutoff_conc_pred = 20,
                                                               group_vars = c("rgn22nm"),
                                                               most_recent_only = TRUE),
             format = "parquet"),
  
  tar_target(lsoa_boundaries, get_shapefile(shapefile_path = here("Data/raw/map_boundary_data/LSOA_2021_EW_BGC_V5.shp"),
                                            geography_var = lsoa21cd) %>%
               
               ms_simplify(keep = 0.25)),
  
  tar_target(la_boundaries, get_shapefile(shapefile_path = here("Data/raw/map_boundary_data/LAD_DEC_2022_UK_BFC_V2.shp"),
                                          geography_var = lad22cd)),
  
  tar_target(ward_boundaries, get_shapefile(shapefile_path = here("Data/raw/map_boundary_data/WD_DEC_22_GB_BFC.shp"),
                                          geography_var = wd22cd)),

  tar_target(data_epc_lsoa_cross_section_to_map, prepare_data_to_map(fill_data = data_epc_lsoa_cross_section,
                                                                     shapefile_data = lsoa_boundaries,
                                                                     join_var = "lsoa21cd")),
  
  tar_target(data_epc_ward_cross_section_to_map, prepare_data_to_map(fill_data = data_epc_ward_cross_section,
                                                                     shapefile_data = ward_boundaries,
                                                                     join_var = "wd22cd")),
  
  tar_target(data_epc_la_cross_section_to_map, prepare_data_to_map(fill_data = data_epc_la_cross_section,
                                                                     shapefile_data = la_boundaries,
                                                                     join_var = "lad22cd")),
  
  # Load LAEI and NAEI shapefiles with predicted concentration of WF heat sources (does not update with pipeline)
  tar_target(data_laei, st_read(here("Data/raw/laei_data/data_laei.shp")) %>%
               
               rename(n_wood_pred = n_wd_pr,
                      pm_25_emissions = pm_25_m) %>%
               
               mutate(log_n_wood_pred = ifelse(n_wood_pred > 0, log(n_wood_pred), 0))),
  
  tar_target(data_naei, st_read(here("Data/raw/naei_data/data_naei_merged.shp")) %>%
               
               rename(n_wood_pred = n_wd_pr,
                      pm_25_emissions = pm_25_m) %>%
               
               mutate(log_n_wood_pred = ifelse(n_wood_pred > 0, log(n_wood_pred), 0))),
  
  tar_target(data_indicators_region, data_epc_cleaned_covars %>% 
               
               # Change region vars for formatting plot
               mutate(rgn22nm = case_when(rgn22nm == "Yorkshire and The Humber" ~ "Yorkshire and\nThe Humber",
                                          .default = rgn22nm)) %>%
               
               # Generate percentage of WF heat sources and mean IMD score by LSOA
               # We are measuring prevalence so restrict to houses only
               summarise(wood_perc = mean(any_wood_h, na.rm = TRUE) * 100, 
                         imd_score = mean(imd_score, na.rm = TRUE),
                         median_age_mid_2022 = mean(median_age_mid_2022, na.rm = TRUE),
                         white_pct = mean(white_pct, na.rm = TRUE),
                         urban = mean(urban, na.rm = TRUE),
                         .by = c(lsoa21cd, rgn22nm)) %>% 
               
               # Remove NA urban areas
               filter(!is.na(urban))),
  
  tar_target(data_wood_pc_property_type_region, data_epc_cleaned_covars %>% 
               
               # Summarise WF prevalence by region, year, and housing type
               summarise(wood_perc = mean(any_wood_h, na.rm = TRUE) * 100, .by = c(year, property_type_census, rgn22nm)) %>% 
               
               # Filter Nan values (for flats, house type missing)
               filter(!is.nan(wood_perc)) %>%
               
               # Change region name for plotting
               mutate(rgn22nm = case_when(rgn22nm == "Yorkshire and The Humber" ~ "Yorkshire and\nThe Humber", .default = rgn22nm))),
  
  tar_target(data_wood_pc_property_type_imd_decile_urban, data_epc_cleaned_covars %>% 
               
               # Generate new IMD decile variable by urban/rural
               mutate(imd_decile_ruc = factor(ntile(desc(imd_score), n = 10), labels = c("1 - Most deprived",
                                                                                         "2",
                                                                                         "3",
                                                                                         "4",
                                                                                         "5",
                                                                                         "6",
                                                                                         "7",
                                                                                         "8",
                                                                                         "9",
                                                                                         "10 - Least deprived")),
                      .by = urban) %>%
               
               # Filter urban LSOAs
               filter(urban == 1) %>%
               
               # Summarise WF prevalence by region, year, and housing type
               summarise(wood_perc = mean(any_wood_h, na.rm = TRUE) * 100, .by = c(year, property_type_census, imd_decile_ruc)) %>% 
               
               # Filter Nan values (for flats, house type missing)
               filter(!is.nan(wood_perc) & !is.na(imd_decile_ruc))),
  
  tar_target(data_wood_pc_property_type_imd_decile_rural, data_epc_cleaned_covars %>% 
               
               # Generate new IMD decile variable by urban/rural
               mutate(imd_decile_ruc = factor(ntile(desc(imd_score), n = 10), labels = c("1 - Most deprived",
                                                                                         "2",
                                                                                         "3",
                                                                                         "4",
                                                                                         "5",
                                                                                         "6",
                                                                                         "7",
                                                                                         "8",
                                                                                         "9",
                                                                                         "10 - Least deprived")),
                      .by = urban) %>%
               
               # Filter rural LSOAs
               filter(urban == 0) %>%
               
               # Summarise WF prevalence by region, year, and housing type
               summarise(wood_perc = mean(any_wood_h, na.rm = TRUE) * 100, .by = c(year, property_type_census, imd_decile_ruc)) %>% 
               
               # Filter Nan values (for flats, house type missing)
               filter(!is.nan(wood_perc) & !is.na(imd_decile_ruc))),
  
  tar_target(data_prevalence_la_by_year, data_epc_cleaned_covars %>%
               
               # SUmmarise prevalence of WF in houses and n by Local Authority and year
               summarise(wood_perc_h = mean(any_wood_h, na.rm = TRUE) * 100,
                         n = sum(!is.na(any_wood_h)),
                         .by = c(lad22cd, year)) %>%
               
               # Filter LAs where < 20 observations in any year
               filter(all(n >= 20), .by = lad22cd) %>%
               
               # Normalise values to percentage point change from 2009
               mutate(wood_perc_h = wood_perc_h - last(wood_perc_h), .by = lad22cd)),
  
  tar_target(data_prevalence_summary_by_year, data_epc_cleaned_covars %>%
               
               # SUmmarise prevalence of WF in houses and n by Local Authority and year
               summarise(wood_perc_h = mean(any_wood_h, na.rm = TRUE) * 100,
                         .by = c(year)) %>%
               
               # Normalise values to percentage point change from 2009
               mutate(wood_perc_h = wood_perc_h - last(wood_perc_h))),
  
  tar_target(data_openair, get_openair_data(source_list = c("aurn",
                                                            "aqe",
                                                            "waqn"),
                                            year_list = 2022:2024,
                                            frequency = "hourly",
                                            pollutant_list = c("pm2.5"))),
  
  tar_target(data_openair_epc, merge_openair_epc_data(data_openair,
                                                      data_epc_cleaned_covars,
                                                      long_var_epc = "long",
                                                      lat_var_epc = "lat",
                                                      buffer_radius = 1000)),
  
  tar_target(data_openair_epc_buffer_500, merge_openair_epc_data(data_openair,
                                                      data_epc_cleaned_covars,
                                                      long_var_epc = "long",
                                                      lat_var_epc = "lat",
                                                      buffer_radius = 500)),
  
  tar_target(data_openair_epc_buffer_2000, merge_openair_epc_data(data_openair,
                                                      data_epc_cleaned_covars,
                                                      long_var_epc = "long",
                                                      lat_var_epc = "lat",
                                                      buffer_radius = 2000)),
  
  tar_target(data_openair_epc_laei, make_openair_epc_laei_data(data_openair_epc,
                                                               data_laei)),
  
  tar_target(data_openair_epc_naei, make_openair_epc_laei_data(data_openair_epc,
                                                               data_naei)),
  
  tar_target(data_summary_imd_decile_region, data_epc_cleaned_covars %>% 
               
               summarise(any_wood_h = mean(any_wood_h, na.rm = TRUE) * 100,
                         white_pct = mean(white_pct, na.rm = TRUE),
                         num_people = sum(num_people, na.rm = TRUE),
                         imd_score = mean(imd_score, na.rm = TRUE),
                         .by = c(imd_decile,
                                 rgn22nm)) %>%
               
               # Arrange by population to ensure smaller bubbles appear in front
               arrange(desc(num_people))),
  
  tar_target(data_summary_imd_decile_region_urban, data_epc_cleaned_covars %>% 
               
               filter(urban == 1) %>%
               
               summarise(any_wood_h = mean(any_wood_h, na.rm = TRUE) * 100,
                         white_pct = mean(white_pct, na.rm = TRUE),
                         num_people = sum(num_people, na.rm = TRUE),
                         imd_score = mean(imd_score, na.rm = TRUE),
                         .by = c(imd_decile,
                                 rgn22nm)) %>%
               
               # Arrange by population to ensure smaller bubbles appear in front
               arrange(desc(num_people))),
  
  # Make figures ---------------------------------------------------------------
  
  tar_target(scatter_plot_pc_wood_imd, make_grouped_scatter_plot(data = data_summary_imd_decile_region,
                                                                 x_var = imd_score,
                                                                 y_var = any_wood_h,
                                                                 group_var = imd_decile,
                                                                 colour_var = rgn22nm,
                                                                 size_var = num_people,
                                                                 legend_position = "bottom") + 
               labs(colour = NULL,
                    x = "Mean IMD score",
                    y = "Wood fuel (%)") +
    
    guides(size = guide_legend(title = "Population")) +
      
      ggtitle("A")),

  tar_target(scatter_plot_pc_wood_white_eth, make_grouped_scatter_plot(data = data_summary_imd_decile_region,
                                                                 x_var = white_pct,
                                                                 y_var = any_wood_h,
                                                                 group_var = imd_decile,
                                                                 colour_var = rgn22nm,
                                                                 size_var = num_people,
                                                                 legend_position = "bottom") + 
               labs(colour = NULL,
                    x = "White ethnicity (%)",
                    y = NULL) +
               
               guides(size = guide_legend(title = "Population")) +
               
               ggtitle("B")),
  
  tar_target(scatter_plot_pc_wood_imd_urban, make_grouped_scatter_plot(data = data_summary_imd_decile_region_urban,
                                                                 x_var = imd_score,
                                                                 y_var = any_wood_h,
                                                                 group_var = imd_decile,
                                                                 colour_var = rgn22nm,
                                                                 size_var = num_people,
                                                                 legend_position = "bottom") + 
               labs(colour = NULL,
                    x = "Mean IMD score",
                    y = "Wood fuel (%)") +
               
               guides(size = guide_legend(title = "Population")) +
               
               ggtitle("A")),
  
  tar_target(scatter_plot_pc_wood_white_eth_urban, make_grouped_scatter_plot(data = data_summary_imd_decile_region_urban,
                                                                       x_var = white_pct,
                                                                       y_var = any_wood_h,
                                                                       group_var = imd_decile,
                                                                       colour_var = rgn22nm,
                                                                       size_var = num_people,
                                                                       legend_position = "bottom") + 
               labs(colour = NULL,
                    x = "White ethnicity (%)",
                    y = NULL) +
               
               guides(size = guide_legend(title = "Population")) +
               
               ggtitle("B")),
  
  tar_target(scatter_wood_conc_vs_predicted_lsoa, (data_epc_lsoa_cross_section %>%
               
               ggplot(aes(x = wood_conc,
                          y = wood_conc_pred)) +
                 
                 ggtitle("A") +
               
               geom_point(alpha = 0.1,
                          size = 0.5) +
               
               scatter_plot_opts +
               
               geom_abline(alpha = 0.2) +
               
               labs(x = "Estimated concentration (using EPCs)",
                    y = "Estimated\n(Census)"))),
  
  tar_target(scatter_wood_perc_h_vs_predicted_lsoa, (data_epc_lsoa_cross_section %>%
                                                     
                                                     ggplot(aes(x = wood_perc_h,
                                                                y = wood_perc_h_predicted)) +
                                                       
                                                       ggtitle("B") +
                                                     
                                                     geom_point(alpha = 0.1,
                                                                size = 0.5) +
                                                     
                                                     scatter_plot_opts +
                                                     
                                                     geom_abline(alpha = 0.2) +
                                                     
                                                     labs(x = "Estimated prevalence (using EPCs)",
                                                          y = ""))),
  
  # Prevalence maps

  tar_target(choropleth_map_wood_pc_lsoa, make_choropleth_map(fill_data = data_epc_lsoa_cross_section_to_map,
                                                 fill_var = wood_perc_h_predicted,
                                                 filter_low_n = TRUE,
                                                 n_var = epc,
                                                 n_threshold = 10,
                                                 boundary_data = NULL,
                                                 fill_palette = "inferno",
                                                 scale_lower_lim = 0,
                                                 scale_upper_lim = 100,
                                                 winsorise = FALSE,
                                                 lower_perc = NULL,
                                                 upper_perc = NULL,
                                                 legend_title = "Percentage",
                                                 legend_position = "inside") +
               ggtitle("A")),
  
  tar_target(choropleth_map_sfa_pc_lsoa, make_choropleth_map(fill_data = data_epc_lsoa_cross_section_to_map,
                                                              fill_var = sfa_perc_predicted,
                                                              filter_low_n = TRUE,
                                                              n_var = epc,
                                                              n_threshold = 10,
                                                              boundary_data = la_boundaries,
                                                              fill_palette = "inferno",
                                                              scale_lower_lim = 0,
                                                              scale_upper_lim = 100,
                                                              winsorise = FALSE,
                                                              lower_perc = NULL,
                                                              upper_perc = NULL,
                                                              legend_title = "Percentage",
                                                              legend_position = "inside") +
               ggtitle("A")),

  tar_target(choropleth_map_wood_pc_lsoa_london, make_choropleth_map(fill_data = data_epc_lsoa_cross_section_to_map[data_epc_lsoa_cross_section_to_map$rgn22nm == "London",],
                                                              fill_var = wood_perc_h_predicted,
                                                              filter_low_n = TRUE,
                                                              n_var = epc,
                                                              n_threshold = 10,
                                                              boundary_data = NULL,
                                                              fill_palette = "inferno",
                                                              scale_lower_lim = NULL,
                                                              scale_upper_lim = NULL,
                                                              winsorise = FALSE,
                                                              lower_perc = NULL,
                                                              upper_perc = NULL,
                                                              legend_title = "Percentage",
                                                              legend_position = "bottom") +
               
               # Add annotation around highlighted boundaries
               geom_sf(data = la_boundaries[grepl("E09000027|E09000021|E090000029", la_boundaries$lad22cd),],
                       fill = NA,
                       lwd = 0.5,
                       colour = "blue")),
  
  tar_target(choropleth_map_sfa_pc_lsoa_london, make_choropleth_map(fill_data = data_epc_lsoa_cross_section_to_map[data_epc_lsoa_cross_section_to_map$rgn22nm == "London",],
                                                                     fill_var = sfa_perc_predicted,
                                                                     filter_low_n = TRUE,
                                                                     n_var = epc,
                                                                     n_threshold = 10,
                                                                     boundary_data = la_boundaries[str_sub(la_boundaries$lad22cd, 1, 3) == "E09",],
                                                                     fill_palette = "inferno",
                                                                     scale_lower_lim = NULL,
                                                                     scale_upper_lim = NULL,
                                                                     winsorise = FALSE,
                                                                     lower_perc = NULL,
                                                                     upper_perc = NULL,
                                                                     legend_title = "Percentage",
                                                                     legend_position = "bottom")),
  
  # Predicted concentration maps
  
  tar_target(choropleth_map_wood_conc_pred_lsoa, make_choropleth_map(fill_data = data_epc_lsoa_cross_section_to_map,
                                                                            fill_var = wood_conc_pred,
                                                                            filter_low_n = TRUE,
                                                                            n_var = epc,
                                                                            n_threshold = 10,
                                                                            boundary_data = NULL,
                                                                            fill_palette = "inferno",
                                                                            scale_lower_lim = NULL,
                                                                            scale_upper_lim = NULL,
                                                                            winsorise = TRUE,
                                                                            lower_perc = 0.05,
                                                                            upper_perc = 0.95,
                                                                            legend_title = expression(atop("Concentration", paste("per ", km^{2}))),
                                                                     legend_position = "inside") +
               ggtitle("B")),

  tar_target(choropleth_map_wood_conc_pred_lsoa_london, make_choropleth_map(fill_data = data_epc_lsoa_cross_section_to_map[data_epc_lsoa_cross_section_to_map$rgn22nm == "London",],
                                                                     fill_var = wood_conc_pred,
                                                                     filter_low_n = TRUE,
                                                                     n_var = epc,
                                                                     n_threshold = 10,
                                                                     boundary_data = NULL,
                                                                     fill_palette = "inferno",
                                                                     scale_lower_lim = NULL,
                                                                     scale_upper_lim = NULL,
                                                                     winsorise = TRUE,
                                                                     lower_perc = 0.05,
                                                                     upper_perc = 0.95,
                                                                     legend_title = expression(atop("Concentration", paste("per ", km^{2}))),
                                                                     legend_position = "bottom")),
  
  tar_target(choropleth_map_sfa_conc_pred_lsoa, make_choropleth_map(fill_data = data_epc_lsoa_cross_section_to_map,
                                                                     fill_var = sfa_conc_pred,
                                                                     filter_low_n = TRUE,
                                                                     n_var = epc,
                                                                     n_threshold = 10,
                                                                     boundary_data = la_boundaries,
                                                                     fill_palette = "inferno",
                                                                     scale_lower_lim = NULL,
                                                                     scale_upper_lim = NULL,
                                                                     winsorise = TRUE,
                                                                     lower_perc = 0.05,
                                                                     upper_perc = 0.95,
                                                                     legend_title = expression(atop("Concentration", paste("per ", km^{2}))),
                                                                     legend_position = "inside") +
               ggtitle("B")),
  
  tar_target(choropleth_map_sfa_conc_pred_lsoa_london, make_choropleth_map(fill_data = data_epc_lsoa_cross_section_to_map[data_epc_lsoa_cross_section_to_map$rgn22nm == "London",],
                                                                            fill_var = sfa_conc_pred,
                                                                            filter_low_n = TRUE,
                                                                            n_var = epc,
                                                                            n_threshold = 10,
                                                                            boundary_data = la_boundaries[str_sub(la_boundaries$lad22cd, 1, 3) == "E09",],
                                                                            fill_palette = "inferno",
                                                                            scale_lower_lim = NULL,
                                                                            scale_upper_lim = NULL,
                                                                            winsorise = TRUE,
                                                                            lower_perc = 0.05,
                                                                            upper_perc = 0.95,
                                                                            legend_title = expression(atop("Concentration", paste("per ", km^{2}))),
                                                                            legend_position = "bottom")),
  
  tar_target(choropleth_map_wood_emissions_laei, data_laei %>%
               
               ggplot() +
               
               geom_sf(aes(fill = pm_25_emissions),
                       colour = NA) +
               
               geom_sf(data = la_boundaries[str_sub(la_boundaries$lad22cd, 1, 3) == "E09",],
                       fill = NA,
                       lwd = 0.01,
                       colour = "gray50") +
               
               scale_fill_viridis(option = "inferno",
                                  direction = -1) +
               
               theme_void() +
               
               theme(legend.title = element_text(size = 10),
                     legend.text = element_text(size = 8),
                     legend.justification.inside = c(1, 1),
                     plot.title = element_text(face = "bold")) +
               
               guides(fill = guide_colourbar(position = "bottom",
                                             title = expression("Estimated wood fuel PM"[2.5]~"(kilotonnes/year)"))) +
               
               ggtitle("A")),
  
  tar_target(choropleth_map_n_wf_laei, data_laei %>%
               
               ggplot() +
               
               geom_sf(aes(fill = n_wood_pred),
                       colour = NA) +
               
               geom_sf(data = la_boundaries[str_sub(la_boundaries$lad22cd, 1, 3) == "E09",],
                       fill = NA,
                       lwd = 0.01,
                       colour = "gray50") +
               
               scale_fill_viridis(option = "inferno",
                                  direction = -1) +
               
               theme_void() +
               
               theme(legend.title = element_text(size = 10),
                     legend.text = element_text(size = 8),
                     legend.justification.inside = c(1, 1),
                     plot.title = element_text(face = "bold")) +
               
               guides(fill = guide_colourbar(position = "bottom",
                                             title = "Number of wood fuel\nheat sources")) +
               
               ggtitle("B")),
  
  # Facet wraps
  
  tar_target(facet_wood_pc_imd_score_region, (data_indicators_region %>% 
               
               # Make plot
               ggplot(aes(x = imd_score, 
                          y = wood_perc, 
                          colour = factor(urban, labels = c("Rural",
                                                            "Urban")))) + 
               
               geom_point(alpha = 0.4,
                          size = 0.1) + 
               
               facet_wrap(~rgn22nm) + 
               
               # Set plot options
               scatter_plot_opts +
                 
               scale_colour_manual(values = cbbPalette) +
               
               labs(x = "IMD Score",
                    y = "WF heat\nsource (%)",
                    colour = "") +
                 
               guides(colour = guide_legend(override.aes = list(size = 5))) +
               
               theme(strip.text = element_text(size = 8),
                     legend.position = "inside",
                     legend.position.inside = c(0.7, 0.1))) %>%
               
               ggsave("Output/Figures/facet_wood_pc_imd_score_region.png", ., height = 5, width = 8, dpi = 700),
             format = "file"),
  
  tar_target(facet_wood_pc_white_pct_region, (data_indicators_region %>%
               
               # Make scatter plot
               ggplot(aes(x = white_pct, 
                          y = wood_perc,
                          colour = factor(urban, labels = c("Rural",
                                                            "Urban")))) + 
               
                geom_point(alpha = 0.4,
                            size = 0.1) + 
               
                 # Facet by region var
                facet_wrap(~rgn22nm) + 
               
                # Set plot options
                scatter_plot_opts +
                 
                scale_colour_manual(values = cbbPalette) +
               
                labs(x = "White ethnicity (%)",
                      y = "WF heat\nsource (%)",
                     colour = "") +
                 
                guides(colour = guide_legend(override.aes = list(size = 5))) +
               
                theme(strip.text = element_text(size = 8),
                      legend.position = "inside",
                      legend.position.inside = c(0.7, 0.1))) %>%
               
               ggsave("Output/Figures/facet_wood_pc_white_pct_region.png", ., height = 5, width = 8, dpi = 700),
             format = "file"),
  
  tar_target(facet_wood_pc_median_age_region, (data_indicators_region %>%
                                                
                                                # Make plot
                                                ggplot(aes(x = median_age_mid_2022, 
                                                           y = wood_perc,
                                                           colour = factor(urban, labels = c("Rural",
                                                                                             "Urban")))) + 
                                                
                                                geom_point(alpha = 0.4,
                                                           size = 0.1) + 
                                                
                                                facet_wrap(~rgn22nm) + 
                                                
                                                # Set plot options
                                                scatter_plot_opts +
                                                 
                                                scale_colour_manual(values = cbbPalette) +
                                                
                                                labs(x = "Median age",
                                                     y = "WF heat\nsource (%)",
                                                     colour = "") +
                                                 
                                                guides(colour = guide_legend(override.aes = list(size = 5))) +
                                                
                                                theme(strip.text = element_text(size = 8),
                                                      legend.position = "inside",
                                                      legend.position.inside = c(0.7, 0.1))) %>%
               
               ggsave("Output/Figures/facet_wood_pc_median_age_region.png", ., height = 5, width = 8, dpi = 700),
             format = "file"),
  
  tar_target(facet_wood_pc_property_type_region, (data_wood_pc_property_type_region %>% 
               
               # Make facet wrap
               ggplot() + 
               
               geom_line(aes(x = year, y = wood_perc, colour = property_type_census),
                         lwd = 1) + 
               
               facet_wrap(~rgn22nm) + 
               
               scatter_plot_opts +
                 
                 scale_colour_manual(values = cbbPalette) +
                 
                 labs(x = "",
                      y = "Wood fuel\nprevalence (%)",
                      colour = "Property type") +
                 
                 theme(legend.position = "inside",
                       legend.position.inside = c(0.7, 0.1))) %>%
               
               ggsave("Output/Figures/facet_wood_pc_property_type_region.png", ., height = 5, width = 8, dpi = 700),
             format = "file"),
  
  tar_target(facet_wood_pc_property_type_imd_decile_urban, data_wood_pc_property_type_imd_decile_urban %>% 
                                                        
                                                        # Make facet wrap
                                                        ggplot() + 
                                                        
                                                        geom_line(aes(x = year, y = wood_perc, colour = property_type_census),
                                                                  lwd = 1) + 
                                                        
                                                        facet_wrap(~imd_decile_ruc) + 
                                                        
                                                        scatter_plot_opts +
               
               scale_colour_manual(values = cbbPalette) +
                                                        
                                                        labs(x = "",
                                                             y = "Wood fuel\nprevalence (%)",
                                                             colour = "Property type") +
                                                          
                                                          ggtitle("A")),
  
  tar_target(facet_wood_pc_property_type_imd_decile_rural, data_wood_pc_property_type_imd_decile_rural %>% 
                                                              
                                                              # Make facet wrap
                                                              ggplot() + 
                                                              
                                                              geom_line(aes(x = year, y = wood_perc, colour = property_type_census),
                                                                        lwd = 1) + 
                                                              
                                                              facet_wrap(~imd_decile_ruc) + 
                                                              
                                                              scatter_plot_opts +
               
               scale_colour_manual(values = cbbPalette) +
                                                              
                                                              labs(x = "",
                                                                   y = "Wood fuel\nprevalence (%)",
                                                                   colour = "Property type") +
                                                              
                                                              ggtitle("B")),
  
  # Patchwork plots
  tar_target(patchwork_choropleth_map_wood_pc_conc_pred_lsoa, (make_patchwork_plot(list = list(choropleth_map_wood_pc_lsoa,
                                                                                              choropleth_map_wood_conc_pred_lsoa),
                                                                                  guides = "keep",
                                                                                  ncol = 2)) %>%
               
               ggsave("Output/Maps/patchwork_choropleth_map_wood_pc_conc_pred_lsoa.png", ., dpi = 700, width = 8, height = 5),
             format = "file"),
  
  tar_target(patchwork_choropleth_map_wood_pc_conc_pred_lsoa_combined, (make_patchwork_plot(list = list(choropleth_map_wood_pc_lsoa,
                                                                                               choropleth_map_wood_conc_pred_lsoa,
                                                                                               choropleth_map_wood_pc_lsoa_london,
                                                                                               choropleth_map_wood_conc_pred_lsoa_london),
                                                                                   guides = "keep",
                                                                                   ncol = 2)) %>%
               
               ggsave("Output/Maps/patchwork_choropleth_map_wood_pc_conc_pred_lsoa_combined.png", ., dpi = 700, width = 8, height = 8),
             format = "file"),
  
  tar_target(patchwork_choropleth_map_sfa_pc_conc_pred_lsoa_combined, (make_patchwork_plot(list = list(choropleth_map_sfa_pc_lsoa,
                                                                                               choropleth_map_sfa_conc_pred_lsoa,
                                                                                              choropleth_map_sfa_pc_lsoa_london,
                                                                                              choropleth_map_sfa_conc_pred_lsoa_london),
                                                                                   guides = "keep",
                                                                                   ncol = 2)) %>%
               
               ggsave("Output/Maps/patchwork_choropleth_map_sfa_pc_conc_pred_lsoa_combined.png", ., dpi = 700, width = 8, height = 8),
             format = "file"),
  
  tar_target(patchwork_choropleth_map_wood_pc_conc_pred_lsoa_london, (make_patchwork_plot(list = list(choropleth_map_wood_pc_lsoa_london,
                                                                                               choropleth_map_wood_conc_pred_lsoa_london),
                                                                                   guides = "keep",
                                                                                   ncol = 2)) %>%
               
               ggsave("Output/Maps/patchwork_choropleth_map_wood_pc_conc_pred_lsoa_london.png", ., dpi = 700, width = 8, height = 5),
             format = "file"),
  
  tar_target(patchwork_scatter_wood_pc_imd_white_eth, (make_patchwork_plot(list = list(scatter_plot_pc_wood_imd,
                                                                                      scatter_plot_pc_wood_white_eth),
                                                                           legend_position = "right",
                                                                          guides = "collect",
                                                                          ncol = 1)) %>%
               
               ggsave("Output/Figures/patchwork_scatter_wood_pc_imd_white_eth.png", ., dpi = 700, width = 8, height = 5),
             format = "file"),
  
  tar_target(patchwork_scatter_wood_pc_imd_white_eth_urban, (make_patchwork_plot(list = list(scatter_plot_pc_wood_imd_urban,
                                                                                       scatter_plot_pc_wood_white_eth_urban),
                                                                           legend_position = "right",
                                                                           guides = "collect",
                                                                           ncol = 1)) %>%
               
               ggsave("Output/Figures/patchwork_scatter_wood_pc_imd_white_eth_urban.png", ., dpi = 700, width = 8, height = 5),
             format = "file"),
  
  tar_target(patchwork_scatter_wood_pc_imd_white_eth_urban_storymap, (make_patchwork_plot(list = list(scatter_plot_pc_wood_imd_urban,
                                                                                             scatter_plot_pc_wood_white_eth_urban),
                                                                                 legend_position = "bottom",
                                                                                 guides = "collect",
                                                                                 ncol = 2) & 
                                                                        
                                                                        plot_annotation("Prevalence of wood fuel heat sources vs. IMD score and percentage white ethicity\nby region and LSOA IMD decile - urban areas",
                                                                                        theme = theme(plot.title = element_text(face = "bold")))) %>%
               
               ggsave("Output/Figures/patchwork_scatter_wood_pc_imd_white_eth_urban_storymap.png", ., width = 8, height = 5),
             format = "file"),
  
  tar_target(patchwork_facet_wood_pc_imd_decile, (make_patchwork_plot(list = list(facet_wood_pc_property_type_imd_decile_urban,
                                                                                  facet_wood_pc_property_type_imd_decile_rural),
                                                                      guides = "collect",
                                                                      legend_position = "bottom",
                                                                      ncol = 1)) %>%
               
               ggsave("Output/Figures/patchwork_facet_wood_pc_imd_decile.png", ., dpi = 700, width = 8, height = 8),
             format = "file"),
  
  tar_target(patchwork_wood_conc_perc_h_pred_actual, (make_patchwork_plot(list = list(scatter_wood_conc_vs_predicted_lsoa,
                                                                                     scatter_wood_perc_h_vs_predicted_lsoa),
                                                                         legend_position = "right")) %>%
               
               ggsave("Output/Figures/patchwork_wood_conc_perc_h_pred_actual.png", ., dpi = 700, width = 8, height = 5),
             format = "file"),
  
  tar_target(patchwork_laei_wood_emissions_n_wf, (make_patchwork_plot(list = list(choropleth_map_wood_emissions_laei,
                                                                                  choropleth_map_n_wf_laei),
                                                                      guides = "keep")) %>%
               
               ggsave("Output/Maps/patchwork_laei_wood_emissions_n_wf.png", ., dpi = 700, width = 8, height = 5),
             format = "file"),
  
  tar_target(patchwork_openair_plots_urban_laei, (make_patchwork_plot_openair(data_openair = data_openair_epc_laei,
                                                                              x_var = log_n_wf,
                                                                              x_lab = "ln(Number of wood fuel heat sources)",
                                                                              site_type = "Urban Background",
                                                                              source_list = c("aurn",
                                                                                              "aqe"),
                                                                              pm2.5_var = pm2.5,
                                                                              pm2.5_diff_peak_var = pm2.5_diff_peak,
                                                                              correlation_method = "spearman",
                                                                              bootstrap_method = "bca",
                                                                              n_rep = 10000,
                                                                              conf_int = 0.95)) %>%
               
               ggsave("Output/Figures/patchwork_openair_plots_urban_laei.png", ., dpi = 700, width = 8, height = 5),
             format = "file"),
  
  tar_target(patchwork_openair_plots_urban_laei_alt, (make_patchwork_plot_openair(data_openair = data_openair_epc_laei,
                                                                                  x_var = pm_25_emissions,
                                                                                  x_lab = bquote(paste("Estimated annual ", PM[2.5], " emissions (kt) - LAEI")),
                                                                                  site_type = "Urban Background",
                                                                                  source_list = c("aurn",
                                                                                                  "aqe"),
                                                                                  pm2.5_var = pm2.5,
                                                                                  pm2.5_diff_peak_var = pm2.5_diff_peak,
                                                                                  correlation_method = "spearman",
                                                                                  bootstrap_method = "bca",
                                                                                  n_rep = 10000,
                                                                                  conf_int = 0.95)) %>%
               
               ggsave("Output/Figures/patchwork_openair_plots_urban_laei_alt.png", ., dpi = 700, width = 8, height = 5),
             format = "file"),
  
  tar_target(patchwork_openair_plots_urban_naei, (make_patchwork_plot_openair(data_openair = data_openair_epc_naei,
                                                                              x_var = log_n_wf,
                                                                              x_lab = "ln(Number of wood fuel heat sources)",
                                                                              site_type = "Urban Background",
                                                                              source_list = c("aurn",
                                                                                              "aqe"),
                                                                              pm2.5_var = pm2.5,
                                                                              pm2.5_diff_peak_var = pm2.5_diff_peak,
                                                                              correlation_method = "spearman",
                                                                              bootstrap_method = "bca",
                                                                              n_rep = 10000,
                                                                              conf_int = 0.95)) %>%
               
               ggsave("Output/Figures/patchwork_openair_plots_urban_naei.png", ., dpi = 700, width = 8, height = 5),
             format = "file"),
  
  tar_target(patchwork_openair_plots_urban_naei_alt, (make_patchwork_plot_openair(data_openair = data_openair_epc_naei,
                                                                                  x_var = pm_25_emissions,
                                                                                  x_lab = bquote(paste("Estimated annual ", PM[2.5], " emissions (kt) - NAEI")),
                                                                                  site_type = "Urban Background",
                                                                                  source_list = c("aurn",
                                                                                                  "aqe"),
                                                                                  pm2.5_var = pm2.5,
                                                                                  pm2.5_diff_peak_var = pm2.5_diff_peak,
                                                                                  correlation_method = "spearman",
                                                                                  bootstrap_method = "bca",
                                                                                  n_rep = 10000,
                                                                                  conf_int = 0.95)) %>%
               
               ggsave("Output/Figures/patchwork_openair_plots_urban_naei_alt.png", ., dpi = 700, width = 8, height = 5),
             format = "file"),
  
  tar_target(patchwork_openair_plots_urban_background, (make_patchwork_plot_openair(data_openair = data_openair_epc,
                                                                                    x_var = log_n_wf,
                                                                                    x_lab = bquote(paste("ln(Number of wood fuel heat sources)")),
                                                                                    site_type = "Urban Background",
                                                                                    source_list = c("aurn"),
                                                                                    pm2.5_var = pm2.5,
                                                                                    pm2.5_diff_peak_var = pm2.5_diff_peak,
                                                                                    correlation_method = "spearman",
                                                                                    bootstrap_method = "bca",
                                                                                    n_rep = 10000,
                                                                                    conf_int = 0.95)) %>%
               
               ggsave("Output/Figures/patchwork_openair_plots_urban_background.png", ., dpi = 700, width = 8, height = 5),
             format = "file"),
  
  tar_target(patchwork_openair_plots_urban_background_buffer_500, (make_patchwork_plot_openair(data_openair = data_openair_epc_buffer_500,
                                                                                    x_var = log_n_wf,
                                                                                    x_lab = bquote(paste("ln(Number of wood fuel heat sources)")),
                                                                                    site_type = "Urban Background",
                                                                                    source_list = c("aurn"),
                                                                                    pm2.5_var = pm2.5,
                                                                                    pm2.5_diff_peak_var = pm2.5_diff_peak,
                                                                                    correlation_method = "spearman",
                                                                                    bootstrap_method = "bca",
                                                                                    n_rep = 10000,
                                                                                    conf_int = 0.95)) %>%
               
               ggsave("Output/Figures/patchwork_openair_plots_urban_background_buffer_500.png", ., dpi = 700, width = 8, height = 5),
             format = "file"),
  
  tar_target(patchwork_openair_plots_urban_background_buffer_2000, (make_patchwork_plot_openair(data_openair = data_openair_epc_buffer_2000,
                                                                                               x_var = log_n_wf,
                                                                                               x_lab = bquote(paste("ln(Number of wood fuel heat sources)")),
                                                                                               site_type = "Urban Background",
                                                                                               source_list = c("aurn"),
                                                                                               pm2.5_var = pm2.5,
                                                                                               pm2.5_diff_peak_var = pm2.5_diff_peak,
                                                                                               correlation_method = "spearman",
                                                                                               bootstrap_method = "bca",
                                                                                               n_rep = 10000,
                                                                                               conf_int = 0.95)) %>%
               
               ggsave("Output/Figures/patchwork_openair_plots_urban_background_buffer_2000.png", ., dpi = 700, width = 8, height = 5),
             format = "file"),
  
  tar_target(patchwork_openair_plots_urban_density, (make_patchwork_plot_openair(data_openair = data_openair_epc,
                                                                                 x_var = log_n,
                                                                                 x_lab = bquote(paste("ln(Number of properties)")),
                                                                                 site_type = "Urban Background",
                                                                                 source_list = c("aurn"),
                                                                                 pm2.5_var = pm2.5,
                                                                                 pm2.5_diff_peak_var = pm2.5_diff_peak,
                                                                                 correlation_method = "spearman",
                                                                                 bootstrap_method = "bca",
                                                                                 n_rep = 10000,
                                                                                 conf_int = 0.95)) %>%
               
               ggsave("Output/Figures/patchwork_openair_plots_urban_density.png", ., dpi = 700, width = 8, height = 5),
             format = "file"),
  
  tar_target(patchwork_openair_plots_urban_all_networks, (make_patchwork_plot_openair(data_openair = data_openair_epc,
                                                                                      x_var = log_n_wf,
                                                                                      x_lab = bquote(paste("ln(Number of wood fuel heat sources)")),
                                                                                      site_type = "Urban Background",
                                                                                      source_list = c("aurn",
                                                                                                      "aqe",
                                                                                                      "waqn"),
                                                                                      pm2.5_var = pm2.5,
                                                                                      pm2.5_diff_peak_var = pm2.5_diff_peak,
                                                                                      correlation_method = "spearman",
                                                                                      bootstrap_method = "bca",
                                                                                      n_rep = 10000,
                                                                                      conf_int = 0.95)) %>%
               
               ggsave("Output/Figures/patchwork_openair_plots_urban_all_networks.png", ., dpi = 700, width = 8, height = 5),
             format = "file"),
  
  # Run LSOA logit models
  tar_target(results_betareg_model, betareg(wood_perc_h_predicted_normalised ~ urban + 
                                       sca_area + 
                                       median_age_mid_2022 + 
                                       white_pct + 
                                       imd_score +
                                       factor(rgn22nm), 
                                     data = data_epc_lsoa_cross_section, 
                                     link = "logit")),
  
  tar_target(results_betareg_model_urban, betareg(wood_perc_h_predicted_normalised ~ 
                                        sca_area + 
                                        median_age_mid_2022 + 
                                        white_pct + 
                                        imd_score +
                                        factor(rgn22nm), 
                                      data = data_epc_lsoa_cross_section[data_epc_lsoa_cross_section$urban == 1,], 
                                      link = "logit")),
  
  tar_target(results_betareg_model_rural, betareg(wood_perc_h_predicted_normalised ~ 
                                                    sca_area + 
                                                    median_age_mid_2022 + 
                                                    white_pct + 
                                                    imd_score +
                                                    factor(rgn22nm), 
                                                  data = data_epc_lsoa_cross_section[data_epc_lsoa_cross_section$urban == 0,], 
                                                  link = "logit")),
  
  tar_target(output_betareg_model, tbl_regression(results_betareg_model, 
                                                estimate_fun = label_style_sigfig(digits = 4),
                                                label = list(urban = "Urban", 
                                                             sca_area = "Smoke Control Area", 
                                                             median_age_mid_2022 = "Median Age", 
                                                             white_pct = "Percentage white ethnicity", 
                                                             imd_score = "IMD score",
                                                             `factor(rgn22nm)` = "Region")) %>% 
               as_gt() %>% 
               cols_hide(p.value) %>% 
               gtsave("Output/Tables/output_betareg_model.tex")),
  
  tar_target(output_betareg_model_urban, tbl_regression(results_betareg_model_urban, 
                                                estimate_fun = label_style_sigfig(digits = 4),
                                                label = list(sca_area = "Smoke Control Area", 
                                                             median_age_mid_2022 = "Median Age", 
                                                             white_pct = "Percentage white ethnicity", 
                                                             imd_score = "IMD score",
                                                             `factor(rgn22nm)` = "Region")) %>% 
               as_gt() %>% 
               cols_hide(p.value) %>% 
               gtsave("Output/Tables/output_betareg_model_urban.tex")),
  
  tar_target(output_betareg_model_rural, tbl_regression(results_betareg_model_rural, 
                                                        estimate_fun = label_style_sigfig(digits = 4),
                                                        label = list(sca_area = "Smoke Control Area", 
                                                                     median_age_mid_2022 = "Median Age", 
                                                                     white_pct = "Percentage white ethnicity", 
                                                                     imd_score = "IMD score",
                                                                     `factor(rgn22nm)` = "Region")) %>% 
               as_gt() %>% 
               cols_hide(p.value) %>% 
               gtsave("Output/Tables/output_betareg_model_rural.tex")),
  
  # Summary tables -------------------------------------------------------------
  tar_target(tab_housing_chars_by_any_wood, (data_epc_cleaned_covars %>% 
               
               # Filter only most recent EPCs
               filter(most_recent == TRUE) %>%
               
               mutate(any_wood = factor(any_wood, labels = c("No", "Yes")),
                      sca_area = factor(sca_area, labels = c("No", "Yes")),
                      urban = factor(urban, labels = c("No", "Yes"))) %>%
               
               select(property_type_census,
                      tenure,
                      any_wood,
                      sca_area,
                      urban) %>% 
               
               tbl_summary(by = "any_wood",
                           missing = "ifany",
                           type = all_dichotomous() ~ "categorical",
                           digits = all_continuous() ~ 0,
                           missing_text = "Missing",
                           label = list(property_type_census ~ "Property type",
                                        tenure ~ "Tenure",
                                        sca_area ~ "Smoke Control Area",
                                        urban = "Urban LSOA")) %>%
               
               # Modify header
               modify_header(label ~ "",
                             all_stat_cols() ~ "**{level}** N = {n}") %>%
               
               # Modify spanning header
               modify_spanning_header(all_stat_cols() ~ "**Wood fuel heat source**") %>%
               
               as_gt() %>%
               
               # Add note to table
               tab_source_note(md("Note: We used the subset of unique properties in the EPC dataset. 
                                  We excluded properties which had missing information on WF presence. Each column shows the count of 
                                  properties in the EPC database with the specified characteristic and WF status, and the percentage of all 
                                  properties of that WF status with the specified characteristic. Missing values within characteristics are reported."))) %>%
               
               gtsave("Output/Tables/tab_housing_chars_by_any_wood.tex"),
             format = "file"),
  
  tar_target(tab_summary_wf_decile, (data_epc_lsoa_cross_section %>% 
                                       
                                       # Create variable to indicate decile by WF prevalence
                                       mutate(wf_decile = ntile(wood_perc_h_predicted, n = 10)) %>% 
                                       
                                       # Generate summary stats of relevant variables
                                       summarise(wood_perc_h_predicted = mean(wood_perc_h_predicted, na.rm = TRUE),
                                                 imd_score = mean(imd_score, na.rm = TRUE), 
                                                 white_pct = mean(white_pct, na.rm = TRUE), 
                                                 median_age_mid_2022 = mean(median_age_mid_2022, na.rm = TRUE), 
                                                 urban = mean(urban, na.rm = TRUE) * 100,
                                                 .by = wf_decile) %>% 
                                       
                                       # Arrange
                                       arrange(wf_decile) %>%
                                       
                                       # Make gt table
                                       gt() %>%
                                       
                                       # Format column labels
                                       cols_label(wf_decile = "Decile",
                                                  wood_perc_h_predicted = "WF prevalence (%)",
                                                  imd_score = "IMD score",
                                                  white_pct = "White ethnic\nbackground (%)",
                                                  median_age_mid_2022 = "Median age",
                                                  urban = "Urban (%)") %>%
                                       
                                       # Format numbers
                                       fmt_number(columns = c(wood_perc_h_predicted,
                                                              white_pct,
                                                              urban,
                                                              imd_score,
                                                              median_age_mid_2022),
                                                  decimals = 1) %>%
                                       
                                       # Align columns to centre
                                       cols_align("center") %>%
                                       
                                       # Add note to table
                                       tab_source_note(md("Note: The table presents summary statistics on LSOA-level 
                                                          socio-economic indicators grouped by deciles calculated using 
                                                          the estimated prevalence of wood fuel heat sources by LSOA. The first 
                                                          decile represents LSOAs with the lowest prevalence of WF heat sources, 
                                                          and the tenth decile represents LSOAs with the highest prevalence of
                                                          wood fuel heat sources."))) %>%
               
               gtsave("Output/Tables/tab_summary_wf_decile.tex"),
             format = "file"),
  
  tar_target(tab_housing_type_os_epc, (gt(data_housing_type_os_epc) %>% 
                                         
                                         # Add spanning columns
                                         tab_spanner("OS AddressBase", columns = c(n_os,
                                                                                   perc_os)) %>%
                                         
                                         tab_spanner("EPC data", columns = c(n_epc,
                                                                             perc_epc)) %>%
                                         
                                         # Format percentage/numeric columns
                                         fmt_percent(columns = c(perc_os,
                                                                 perc_epc), decimals = 1) %>%
                                         
                                         fmt_number(columns = c(n_os,
                                                                n_epc), decimals = 0) %>%
                                         
                                         # Label columns
                                         cols_label(property_type_census = "Property type",
                                                    n_os = "N",
                                                    perc_os = "Percentage",
                                                    n_epc = "N",
                                                    perc_epc = "Percentage") %>%
                                         
                                         # Align columns to centre
                                         cols_align("center") %>%
                                         
                                         # Add note
                                         tab_source_note(md("Note: Property types in the OS AddressBase and 
                                                            EPC data were re-categorised to match Census 2021 categories. 
                                                            In total, the OS AddressBase contained 28,638,819 properties.
                                                            In this table, we used EPC data up to to October 2024"))) %>%
               
               gtsave("Output/Tables/tab_housing_type_os_epc.tex"),
             format = "file"),
  
  tar_target(tab_wood_pc_epc_number, (
    
    # Make list of summary tables by EPC number
    make_summary_tabs_by_epc_number(data = data_epc_cleaned_covars,
                                    max_n_epc = 4) %>%
      
      # Make gt table
      gt() %>%
      
      # Hide n_epc col
      cols_hide(n_epc) %>%
      
      # Format column labels
      cols_label(property_type_census = "Property type",
                 n_1 = "First EPC",
                 n_2 = "Second EPC",
                 n_3 = "Third EPC",
                 n_4 = "Fourth EPC") %>%
      
      # Change column order
      cols_move_to_start(columns = c(property_type_census,
                                     n_1,
                                     n_2,
                                     n_3,
                                     n_4)) %>%
      
      # Format percentage
      fmt_percent(columns = c(wood_perc_h_1,
                              wood_perc_h_2,
                              wood_perc_h_3,
                              wood_perc_h_4), decimals = 1) %>%
      
      # Format numeric cols
      fmt_number(columns = c(n_1,
                             n_2,
                             n_3,
                             n_4), decimals = 0) %>%
      
      # Merge columns into desired pattern
      cols_merge(columns = c(n_1,
                             wood_perc_h_1),
                 pattern = "<<{1}>> <<({2})>>") %>%
      
      cols_merge(columns = c(n_2,
                             wood_perc_h_2),
                 pattern = "<<{1}>> <<({2})>>") %>%
      
      cols_merge(columns = c(n_3,
                             wood_perc_h_3),
                 pattern = "<<{1}>> <<({2})>>") %>%
      
      cols_merge(columns = c(n_4,
                             wood_perc_h_4),
                 pattern = "<<{1}>> <<({2})>>") %>%
      
      # Add row groups
      tab_row_group(label = "Number of EPCs: 4",
                    rows = n_epc == 4) %>%
      
      tab_row_group(label = "Number of EPCs: 3",
                    rows = n_epc == 3) %>%
      
      tab_row_group(label = "Number of EPCs: 2",
                    rows = n_epc == 2) %>%
      
      # Align columns to centre
      cols_align("center") %>%
      
      # Add note
      tab_source_note(md("Note: This table shows the percentage of EPCs with a wood fuel
                         heat source by property type and EPC number, among the properties in 
                         the EPC data which had more than one EPC during the period January 2009 February 2025. 
                         We restrict our analysis to houses (excluding flats, other accommodation, 
                         and properties with missing house form) and restrict our analysis to properties with fewer than five EPCs, 
                         due to small sample size for higher numbers of EPCs. The total number of properties within rows 
                         are not equal due to changes in property classifications in the EPC dataset."))) %>%
      
      gtsave("Output/Tables/tab_wood_pc_epc_number.tex"),
    format = "file"),
  
  tar_target(tab_housing_chars_by_any_sfa, (data_epc_cleaned_covars %>% 
                                              
                                              # Filter only most recent EPCs
                                              filter(most_recent == TRUE) %>%
                                              
                                              mutate(any_sfa = factor(any_sfa, labels = c("No", "Yes")),
                                                     sca_area = factor(sca_area, labels = c("No", "Yes")),
                                                     urban = factor(urban, labels = c("No", "Yes"))) %>%
                                              
                                              select(property_type_census,
                                                     tenure,
                                                     any_sfa,
                                                     sca_area,
                                                     urban) %>% 
                                              
                                              tbl_summary(by = "any_sfa",
                                                          missing = "ifany",
                                                          type = all_dichotomous() ~ "categorical",
                                                          digits = all_continuous() ~ 0,
                                                          missing_text = "Missing",
                                                          label = list(property_type_census ~ "Property type",
                                                                       tenure ~ "Tenure",
                                                                       sca_area = "Smoke Control Area",
                                                                       urban = "Urban")) %>%
                                              
                                              # Modify header
                                              modify_header(label ~ "",
                                                            all_stat_cols() ~ "**{level}** N = {n}") %>%
                                              
                                              # Modify spanning header
                                              modify_spanning_header(all_stat_cols() ~ "**Solid fuel heat source**") %>%
                                              
                                              # Set to gt object to use gt functions 
                                              as_gt() %>%
                                              
                                              # Add note to table
                                              tab_source_note(md("Note: We used the subset of unique properties in the EPC dataset. 
                                                                 We excluded properties which had missing information on SF presence. 
                                                                 Each column shows the count of properties in the EPC database with the 
                                                                 specified characteristic and SF status, and the percentage of all properties 
                                                                 of that SF status with the specified characteristic. Missing values within characteristics 
                                                                 are reported."))) %>% 
               
               gtsave("Output/Tables/tab_housing_chars_by_any_sfa.tex"),
             format = "file")
)
