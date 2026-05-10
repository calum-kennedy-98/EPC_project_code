# Name of script: MakeLSOALookupData
# Description:  Loads data sources (IMD, ethnicity, etc) to make LSOA-level lookup data 
# data to merge with main EPC data
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 06-09-2024
# Latest update by: Calum Kennedy
# Latest update on: 06-09-2024
# Update notes: 

# Comments ---------------------------------------------------------------------



#' Construct an LSOA-level covariate lookup table
#'
#' @description
#' Assembles a single LSOA-level data frame by loading and joining multiple secondary
#' data sources: area size, Indices of Multiple Deprivation (IMD 2019), urban/rural
#' classification, median age, ethnicity, and ward/region geography. The resulting
#' lookup is joined to the cleaned EPC data in \code{merge_data_epc_cleaned_covars}
#' to enable area-level analysis.
#'
#' @param path_lsoa_size A character string giving the file path to the ONS LSOA
#'   area measurement CSV (SAM_LSOA_DEC_2021_EW). Must contain columns \code{LSOA21CD}
#'   and \code{"Extent of the Realm (Area in KM2)"}.
#' @param path_imd_eng A character string giving the file path to the English IMD 2019
#'   Excel workbook (File 5, IoD2019 Scores). Data are read from the sheet
#'   \code{"IoD2019 Scores"} using LSOA 2011 codes.
#' @param path_imd_wales A character string giving the file path to the Welsh IMD 2019
#'   ODS file (WIMD 2019). Data are read from the sheet \code{"Data"} (range A4:D1913)
#'   using LSOA 2011 codes.
#' @param path_lsoa11_lsoa21_lookup A character string giving the file path to the ONS
#'   best-fit lookup CSV mapping 2011 LSOA codes to 2021 LSOA codes. Used to
#'   crosswalk IMD and urban/rural data from 2011 to 2021 boundaries.
#' @param path_ethnicity A character string giving the file path to the Census 2021
#'   ethnicity table CSV (TS021), containing 20-category ethnic group counts by LSOA
#'   2021 code.
#' @param path_region A character string giving the file path to the ONS ward-to-region
#'   lookup CSV (Ward to LAD to County to Region to Country, December 2022), containing
#'   columns \code{wd22cd}, \code{lad22cd}, \code{lad22nm}, \code{rgn22nm}, and
#'   \code{ctry22nm}.
#' @param path_ward A character string giving the file path to the ONS LSOA-to-ward
#'   lookup CSV (LSOA 2021 to Ward to LTLA, May 2022), containing columns
#'   \code{lsoa21cd}, \code{lsoa21nm}, \code{wd22cd}, and \code{wd22nm}.
#' @param path_urban_rural A character string giving the file path to the ONS
#'   Rural/Urban Classification 2011 CSV for LSOAs in England and Wales, containing
#'   columns \code{LSOA11CD} and \code{RUC11}.
#' @param path_age A character string giving the file path to the ONS LSOA median age
#'   Excel workbook. Data are read from the sheet \code{"Median age LSOA 2021"}
#'   (skipping 3 header rows), containing columns \code{lsoa_2021_code} and
#'   \code{median_age_mid_2022}.
#'
#' @return A tibble with one row per 2021 LSOA in England and Wales, containing:
#'   \describe{
#'     \item{\code{lsoa21cd}}{Character. 2021 LSOA code.}
#'     \item{\code{area_in_km2}}{Numeric. Area of the LSOA in km\eqn{^2}.}
#'     \item{\code{imd_score}}{Numeric. IMD 2019 score (English scale for English LSOAs;
#'       WIMD 2019 score for Welsh LSOAs). Where a 2021 LSOA corresponds to multiple
#'       2011 LSOAs, the mean IMD score across those 2011 LSOAs is used.}
#'     \item{\code{ruc11}}{Factor. Rural/Urban Classification 2011 string (e.g.
#'       \code{"Urban major conurbation"}). \code{NA} for LSOAs that could not be
#'       uniquely matched via the 2011-to-2021 crosswalk.}
#'     \item{\code{median_age_mid_2022}}{Numeric. Median age of residents at mid-2022.}
#'     \item{\code{num_people}, \code{num_white}, \code{white_pct}}{Numeric. Census 2021
#'       total resident count, white ethnic group count, and percentage white, respectively.}
#'     \item{\code{wd22cd}, \code{wd22nm}, \code{lad22cd}, \code{lad22nm},
#'       \code{rgn22nm}}{Factor. Ward, LAD, and region codes and names (December 2022
#'       boundaries). Wales is treated as a single region (\code{rgn22nm = "Wales"}).}
#'     \item{\code{imd_decile}, \code{white_dec}}{Integer. IMD and white ethnicity
#'       deciles (1 = most deprived / highest white percentage), calculated within
#'       region (\code{rgn22nm}).}
#'   }
#'
#' @details
#' \strong{IMD crosswalk}: IMD 2019 scores are published on 2011 LSOA boundaries.
#' These are joined to 2021 LSOA codes via the ONS best-fit lookup. Where a 2021 LSOA
#' encompasses multiple 2011 LSOAs (boundary merges), the mean IMD score is used as an
#' approximation. This is reasonable given that LSOAs are designed to contain similar
#' population sizes.
#'
#' \strong{Urban/rural crosswalk}: The 2011 Rural/Urban Classification is similarly
#' crosswalked to 2021 boundaries. LSOAs that correspond to multiple 2011 entries
#' (and therefore cannot be unambiguously classified) are set to \code{NA}.
#'
#' \strong{Duplicate LSOAs}: Four LSOAs span two Local Authorities (Ryedale/Scarborough)
#' due to an electoral ward boundary. The function resolves this by retaining the row
#' where the LSOA 2021 name contains the LAD name, effectively assigning each LSOA to
#' its primary Local Authority.
#'
#' \strong{IMD deciles}: Deciles are calculated within region, so a decile of 1
#' indicates the most deprived LSOA within that region. Wales is treated as its own
#' region because WIMD and English IMD scores are not directly comparable.

# Define function to make LSOA-level lookup data -------------------------------

make_lsoa_lookup_data <- function(path_lsoa_size,
                                  path_imd_eng,
                                  path_imd_wales,
                                  path_lsoa11_lsoa21_lookup,
                                  path_ethnicity,
                                  path_region,
                                  path_ward,
                                  path_urban_rural,
                                  path_age){

  # Load LSOA size dataset
  data_lsoa_size <- vroom(here(path_lsoa_size), col_select = c("LSOA21CD",
                                                               "Extent of the Realm (Area in KM2)")) %>%
    
    # Rename variables
    rename("lsoa21cd" = "LSOA21CD",
           "area_in_km2" = "Extent of the Realm (Area in KM2)")
  
  # Load English IMD dataset
  data_imd_eng <- read_excel(here(path_imd_eng),
                             sheet = "IoD2019 Scores", col_names = TRUE) %>%
    
    # Select relevant columns
    select("LSOA code (2011)",
           "Index of Multiple Deprivation (IMD) Score") %>%
    
    # Rename columns
    rename("lsoa11cd" = "LSOA code (2011)",
           "imd_score" = "Index of Multiple Deprivation (IMD) Score")
  
  # Load Welsh IMD dataset
  data_imd_wales <- read_ods(here(path_imd_wales),
                             sheet = "Data", range = "A4:D1913", col_names = TRUE) %>%
    
    # Clean names
    clean_names() %>%
    
    # Select relevant columns
    select(lsoa_code, wimd_2019) %>%
    
    # Rename columns
    rename("lsoa11cd" = "lsoa_code",
           "imd_score" = "wimd_2019")
  
  # Bind England and Wales IMD datasets together
  data_imd <- bind_rows(data_imd_eng,
                        data_imd_wales)
  
  # Load LSOA11 code to LSOA21 code lookup data
  data_lsoa11cd_lsoa21cd_lookup <- vroom(here(path_lsoa11_lsoa21_lookup),
                                         col_select = c("LSOA11CD",
                                                        "LSOA21CD")) %>%
    
    # Clean names
    clean_names()
  
  # Left join LSOA21 codes with IMD data using the 2011 codes - this will generate
  # a dataset of IMD scores using the 2021 LSOA codes, with IMD score defined on the
  # 2011 LSOA codes
  data_imd <- data_lsoa11cd_lsoa21cd_lookup %>%
    
    # Left join to retain all LSOA21 codes
    left_join(data_imd, by = "lsoa11cd") %>%
    
    # Where multiple 2011 LSOAs correspond to one 2021 LSOA, I take the 
    # average IMD score across all corresponding 2011 LSOAs (this should give a 
    # reasonable approximation since all LSOAs are similar sized populations by 
    # definition)
    summarise(imd_score = mean(imd_score, na.rm = TRUE),
              .by = lsoa21cd)
  
  # Load ethnicity dataset
  data_ethnicity <- vroom(here(path_ethnicity)) %>%
    
    # Clean names
    clean_names() %>%
    
    # Rename columns
    rename(lsoa21cd = lower_layer_super_output_areas_code,
           ethnic_grp = ethnic_group_20_categories) %>%
    
    # Generate new indicator for count of people from white ethnic background by small area
    mutate(white_eth = case_when(ethnic_grp %in% c("White: English, Welsh, Scottish, Northern Irish or British",
                                                   "White: Irish",
                                                   "White: Gypsy or Irish Traveller",
                                                   "White: Roma",
                                                   "White: Other White") ~ observation, 
                                 .default = 0)) %>%
    
    # Group by LSOA level to aggregate
    group_by(lsoa21cd) %>%
    
    # Aggregate dataset to LSOA level with total sum of people, people from white background
    # and percentage of people from white background
    summarise(num_people = sum(observation),
              
              num_white = sum(white_eth)) %>%
    
    mutate(white_pct = (num_white/num_people)*100)
  
  # Load ward-level data and merge with region-country lookup
  data_region <- vroom(here(path_region)) %>% 
    
    # Clean names
    clean_names() %>%
    
    # Select relevant columns
    select(wd22cd,
           lad22cd,
           lad22nm,
           rgn22nm,
           ctry22nm)
  
  data_ward <- vroom(here(path_ward)) %>%
    
    # clean names
    clean_names() %>%
    
    # Select relevant columns
    select(lsoa21cd, lsoa21nm, wd22cd, wd22nm) %>%
    
    # Left join to region data
    left_join(data_region, by = "wd22cd")
  
  # There are four duplicated LSOAs - this is because the electoral ward of
  # Hunmanby and Sherburn is shared between the LAs of Ryedale and Scarborough.
  # Here, I filter the duplicated LSOAs by retaining the row where the LSOA21 name
  # Matches the LAD22 name - e.g. 'Ryedale 004C' would be assigned to 'Ryedale'
  data_ward_dupes <- get_dupes(data_ward, lsoa21cd) %>%
    
    select(!dupe_count) %>%
    
    # Detect string for Local Authority not within the LSOA name (then use 
    # to filter full list of LSOAs above)
    filter(!str_detect(lsoa21nm, lad22nm))
  
  # Filter rows from 'data_ward' based on the dataframe of duplicated values
  # using anti join
  data_ward <- data_ward %>%
    
    anti_join(data_ward_dupes)
  
  # Load urban/rural classifier
  data_urban_rural <- vroom(here(path_urban_rural), col_select = c("LSOA11CD",
                                                                        "RUC11")) %>%
    
    clean_names()
  
  # Merge with 2021 LSOA lookup dataset and remove NA values
  data_urban_rural <- data_lsoa11cd_lsoa21cd_lookup %>%
    
    # Left join to urban/rural data to preserve 2021 LSOA codes
    left_join(data_urban_rural, by = "lsoa11cd") %>%
    
    # Remove lsoa11 col
    select(!lsoa11cd) %>%
    
    # Remove LSOAs where more than one entry (duplicated when merging
    # 2011 LSOA codes with 2021 LSOA codes - unclear how to categorise them)
    mutate(n = n(),
           .by = lsoa21cd) %>%
    
    filter(n == 1) %>%
    
    select(!n)
  
  # Load LSOA median age dataset
  data_age <- read_excel(path_age, sheet = "Median age LSOA 2021", skip = 3) %>%
    
    # Clean names
    clean_names() %>%
    
    # Select relevant columns
    select(lsoa_2021_code,
           median_age_mid_2022) %>%
    
    # Rename cols
    rename(lsoa21cd = lsoa_2021_code)
  
  # Create LSOA-level lookup dataset
  
  data_lsoa_lookup <- data_lsoa_size %>%
    
    # Left join to IMD data (we only want to keep LSOA codes from the 'LSOA_size' 
    # dataset as these are the more recent 2021 codes)
    left_join(data_imd, by = "lsoa21cd") %>%
    
    # Left join to urban/rural classification
    left_join(data_urban_rural, by = "lsoa21cd") %>%
    
    # Left join to median age data
    left_join(data_age, by = "lsoa21cd") %>%
    
    # Full join to ethnicity data
    full_join(data_ethnicity, by = "lsoa21cd") %>%
    
    # Full join to ward names data
    full_join(data_ward, by = "lsoa21cd") %>%
    
    # Mutate region variable to treat Wales as a region (since IMD calculated differently)
    mutate(rgn22nm = ifelse(ctry22nm == "Wales", ctry22nm, rgn22nm)) %>%
    
    # Remove 'ctry22nm' column
    select(-ctry22nm) %>%
    
    # Mutate IMD and white ethnicity percentage deciles (as factors) by region
    mutate(imd_decile = ntile(desc(imd_score), n = 10),
           white_dec = ntile(desc(white_pct), n = 10),
           .by = "rgn22nm") %>%
    
    # Mutate characters to factors
    mutate(across(where(is.character), as.factor))
  
  return(data_lsoa_lookup)

}
