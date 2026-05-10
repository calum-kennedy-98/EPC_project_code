# Name of script: UtilityFunctions
# Description:  Defines a set of utility functions used across multiple analysis
#               scripts, including housing type summaries, correlation helpers,
#               spatial utilities, and table formatting functions
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 04-09-2024
# Latest update by: Calum Kennedy
# Latest update on: 04-09-2024

#' Compare property type distributions between OS AddressBase and EPC datasets
#'
#' @description
#' Computes the count and proportion of residential properties by Census 2021 housing
#' type in both the OS AddressBase and EPC datasets, and returns them in a single joined
#' data frame for comparison. Properties with missing house form are excluded from both
#' datasets before computing proportions.
#'
#' @param data_os A data frame of OS AddressBase residential records containing a
#'   \code{property_type_census} column with Census 2021 categories.
#' @param data_epc A data frame of EPC records containing a \code{uprn} column and a
#'   \code{property_type_census} column. Duplicate UPRNs are deduplicated before
#'   counting so that each unique property is counted once.
#'
#' @return A data frame with one row per Census housing type, containing:
#'   \describe{
#'     \item{\code{property_type_census}}{Factor. Housing type category.}
#'     \item{\code{n_os}, \code{perc_os}}{Integer and numeric. Count and proportion of
#'       OS AddressBase properties of each type.}
#'     \item{\code{n_epc}, \code{perc_epc}}{Integer and numeric. Count and proportion of
#'       EPC properties of each type (unique UPRNs only).}
#'   }

make_data_housing_type_os_epc <- function(data_os,
                                          data_epc){
  
  # Make data frame of percentage and N of housing type for OS data
  data_housing_type_os <- data_os %>%
    
    # Filter missing housing types, as these would not be used in calculations
    filter(property_type_census != "House form missing") %>%
    
    # Summarise row count by housing type
    summarise(n_os = n(), .by = property_type_census) %>%
    
    # Mutate to give percentage
    mutate(perc_os = n_os / sum(n_os))
  
  # Make data frame of percentage and N of housing types for EPC data
  data_housing_type_epc <- data_epc %>%
    
    # Keep only unique records
    distinct(uprn, .keep_all = TRUE) %>%
    
    # Filter missing housing types, as these would not be used in calculations
    filter(property_type_census != "House form missing") %>%
    
    # Summarise row count by housing type
    summarise(n_epc = n(), .by = property_type_census) %>%
    
    # Mutate to give percentage
    mutate(perc_epc = n_epc / sum(n_epc))
  
  # Bind data frames together
  data_housing_type_os_epc <- data_housing_type_os %>%
    
    left_join(data_housing_type_epc, by = "property_type_census")
                                    
}

#' Load OS AddressBase residential property data from a Parquet file
#'
#' @description
#' Reads a pre-processed OS AddressBase Parquet file containing residential property
#' records. The cleaning and filtering steps (active records, residential classification,
#' Census property type assignment) have been applied upstream and are not repeated here.
#'
#' @param data_os_path A character string giving the file path to the OS AddressBase
#'   Parquet file.
#'
#' @return A data frame of OS AddressBase residential property records as stored in
#'   the Parquet file.

get_os_data <- function(data_os_path){
  
  data_os <- read_parquet(data_os_path) #%>%
    
    # # Clean names
    # clean_names() %>%
    # 
    # # Keep only records which are still active (i.e. end_date is NA)
    # filter(is.na(end_date)) %>%
    # 
    # select(!end_date) %>%
    # 
    # # Filter only residential properties (classification code starts with 'R')
    # filter(str_sub(classification_code, 1, 1) == "R") %>%
    # 
    # # Mutate new variable capturing Census property types
    # mutate(property_type_census = factor(case_when(classification_code == "RD02" ~ "Detached",
    #                                     classification_code == "RD03" ~ "Semi Detached",
    #                                     classification_code == "RD04" ~ "Terrace",
    #                                     classification_code == "RD06" ~ "Flat",
    #                                     classification_code %in% c("RD01",
    #                                                                "RD07",
    #                                                                "RD08",
    #                                                                "RD10") ~ "Other accommodation",
    #                                     .default = "House form missing")))
  
}

#' Compute a correlation coefficient from a bootstrap-indexed data frame
#'
#' @description
#' A bootstrap-compatible statistic function that computes the correlation between
#' two named columns of a data frame using a row index vector supplied by
#' \code{boot::boot}. Designed to be passed as the \code{statistic} argument to
#' \code{boot}.
#'
#' @param data A data frame containing the columns named by \code{x_var} and
#'   \code{y_var}.
#' @param x_var A character string giving the name of the predictor column.
#' @param y_var A character string giving the name of the response column.
#' @param idx An integer vector of row indices defining the bootstrap sample.
#'   Supplied automatically by \code{boot::boot}.
#' @param correlation_method A character string specifying the correlation method.
#'   Passed to \code{cor}. One of \code{"pearson"}, \code{"spearman"}, or
#'   \code{"kendall"}.
#'
#' @return A numeric scalar: the correlation coefficient between \code{y_var} and
#'   \code{x_var} computed on the rows selected by \code{idx}.

get_corr <- function(data, 
                     x_var, 
                     y_var, 
                     idx, 
                     correlation_method){
  
  # Calculate correlation of specified df cols
  corr <- cor(data[[y_var]][idx], 
              data[[x_var]][idx],
              method = correlation_method)
  
  return(corr)
  
}

#' Compute the winter-minus-summer difference in correlation coefficients for bootstrap inference
#'
#' @description
#' A bootstrap-compatible statistic function that computes the difference between the
#' winter and summer Spearman (or other) correlation coefficients in a single bootstrap
#' call. This allows \code{boot::boot.ci} to produce a valid confidence interval for the
#' seasonal contrast directly, rather than combining two separate intervals. Designed to
#' be passed as the \code{statistic} argument to \code{boot::boot}.
#'
#' @param data A data frame containing the columns named by \code{x_var}, \code{y_var},
#'   and \code{season_var}.
#' @param x_var A character string giving the name of the predictor column.
#' @param y_var A character string giving the name of the response column.
#' @param season_var A character string giving the name of the season column. Must
#'   contain the values \code{"Winter"} and \code{"Summer"}.
#' @param idx An integer vector of row indices defining the bootstrap sample.
#'   Supplied automatically by \code{boot::boot}.
#' @param correlation_method A character string specifying the correlation method.
#'   Passed to \code{cor}. Typically \code{"spearman"}.
#'
#' @return A numeric scalar: the winter correlation coefficient minus the summer
#'   correlation coefficient, computed on the rows selected by \code{idx}.
#'
#' @details
#' The function subsets the bootstrapped data into winter and summer subsets using
#' \code{season_var}, then computes \code{cor} within each subset. A positive return
#' value indicates a stronger positive association in winter than in summer, which is
#' the expected direction if PM\eqn{_{2.5}} elevations are partly attributable to
#' domestic wood burning.

get_corr_diff <- function(data, 
                     x_var, 
                     y_var,
                     season_var,
                     idx, 
                     correlation_method){
  
  # Subset data based on idx selected
  data_idx <- data[idx,]
  
  # Extract data for winter and summer separately
  data_winter <- data_idx[data_idx[[season_var]] == "Winter",]
  data_summer <- data_idx[data_idx[[season_var]] == "Summer",]
  
  # Calculate correlation of specified df cols
  corr_winter <- cor(data_winter[[y_var]], 
              data_winter[[x_var]],
              method = correlation_method)
  
  # Calculate correlation of specified df cols
  corr_summer <- cor(data_summer[[y_var]], 
                     data_summer[[x_var]],
                     method = correlation_method)
  
  # Get difference between two correlation coefficients
  corr_diff = corr_winter - corr_summer
  
  return(corr_diff)
  
}

#' Load an ONS boundary shapefile and filter to England and Wales
#'
#' @description
#' Reads a polygon shapefile using \code{sf::read_sf}, standardises column names with
#' \code{janitor::clean_names}, and removes Scottish (prefix \code{"S"}) and Northern
#' Irish (prefix \code{"N"}) geographies based on the specified geography code column.
#'
#' @param shapefile_path A character string giving the file path to the \code{.shp}
#'   shapefile (e.g. LSOA, ward, or LAD boundaries from the ONS Open Geography Portal).
#' @param geography_var An unquoted column name in the shapefile containing the ONS
#'   geography code (e.g. \code{lsoa21cd}, \code{lad22cd}, \code{wd22cd}). Geographies
#'   whose codes begin with \code{"S"} (Scotland) or \code{"N"} (Northern Ireland) are
#'   excluded.
#'
#' @return An \code{sf} object containing only English and Welsh polygon geometries,
#'   with column names converted to snake_case by \code{clean_names}.

get_shapefile <- function(shapefile_path,
                          geography_var){
  
  # Get shapefile
  shp <- read_sf(shapefile_path) %>%
    
    # Clean names
    clean_names() %>%
    
    # Filter out Scottish/Northern Irish geographies
    filter(!str_sub({{geography_var}}, 1, 1) %in% c("S", "N"))
  
  return(shp)
  
}

#' Compute a specified percentile of a numeric vector
#'
#' @description
#' A thin wrapper around \code{quantile} that returns a single named numeric value
#' for the specified percentile, ignoring \code{NA} values. Used in
#' \code{make_choropleth_map} to compute winsorisation thresholds.
#'
#' @param variable A numeric vector.
#' @param percentile A numeric scalar in [0, 1] specifying the desired percentile
#'   (e.g. \code{0.05} for the 5th percentile, \code{0.95} for the 95th).
#'
#' @return A named numeric scalar: the value of \code{variable} at the specified
#'   percentile, with \code{NA} values excluded.

get_percentile <- function(variable, percentile){
  
  percentile <- quantile(variable, percentile, na.rm = TRUE)
  
  return(percentile)
  
}

#' Load and prepare Census 2021 housing type counts by LSOA with geographic identifiers
#'
#' @description
#' Reads the Census 2021 accommodation type table (TS044) and a set of geographic
#' lookup files to produce a data frame of housing stock counts by LSOA and property
#' type. The resulting data frame is used in \code{make_summary_data_by_group} to
#' reweight EPC-derived WF prevalence estimates to reflect the full housing stock
#' composition.
#'
#' @param path_data_housing_type_census A character string giving the file path to the
#'   Census 2021 accommodation type CSV (TS044, 8-category version). Must contain
#'   columns \code{"Accommodation type (8 categories)"}, \code{"Lower layer Super
#'   Output Areas Code"}, and \code{"Observation"}.
#' @param path_region A character string giving the file path to the ONS ward-to-region
#'   lookup CSV (Ward to LAD to County to Region to Country, December 2022).
#' @param path_ward A character string giving the file path to the ONS LSOA-to-ward
#'   lookup CSV (LSOA 2021 to Ward to LTLA, May 2022).
#'
#' @return A data frame with one row per unique LSOA-by-property-type combination,
#'   containing:
#'   \describe{
#'     \item{\code{lsoa21cd}}{Character. 2021 LSOA code.}
#'     \item{\code{property_type_census}}{Factor. Census 2021 housing type, recoded
#'       to match EPC categories: \code{"Detached"}, \code{"Semi Detached"},
#'       \code{"Terrace"}, \code{"Flat"}, \code{"Other accommodation"}.}
#'     \item{\code{n_properties}}{Integer. Number of properties of this type in this
#'       LSOA according to Census 2021. Rows with \code{n_properties == 0} are excluded.}
#'     \item{\code{property_type_h}}{Integer (0/1). 1 if the property type is a house
#'       (detached, semi-detached, or terrace), 0 otherwise.}
#'     \item{\code{property_type_perc}}{Numeric. Proportion of all properties in the
#'       LSOA that are of this type.}
#'     \item{\code{wd22cd}, \code{wd22nm}, \code{lad22cd}, \code{lad22nm},
#'       \code{rgn22nm}, \code{ctry22nm}}{Factor. Ward, LAD, and region identifiers
#'       (December 2022 boundaries).}
#'   }
#'
#' @details
#' Census accommodation categories are recoded to match the EPC classification scheme:
#' purpose-built flats are mapped to \code{"Flat"}; converted/shared housing,
#' commercial conversions, and caravans are mapped to \code{"Other accommodation"};
#' semi-detached and terraced houses are renamed to match EPC conventions. Multiple
#' Census rows that collapse to the same recoded category within an LSOA (e.g. multiple
#' "Other accommodation" subtypes) are summed before further calculations.
#'
#' Duplicate LSOAs arising from the ward boundary crossing two Local Authorities
#' (Ryedale/Scarborough) are resolved by the same name-matching logic used in
#' \code{make_lsoa_lookup_data}.

make_data_housing_type_census <- function(path_data_housing_type_census,
                                          path_region,
                                          path_ward){
  
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
  
  # Load ward lookup data from path
  data_ward <- vroom(here(path_ward)) %>%
    
    # clean names
    clean_names() %>%
    
    # Select relevant columns
    select(lsoa21cd, 
           lsoa21nm, 
           wd22cd, 
           wd22nm) %>%
    
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
  
  # Load housing type data from path and join to geographic identifiers
  data_housing_type_census <- vroom(path_data_housing_type_census, col_select = c("Accommodation type (8 categories)",
                                                                                  "Lower layer Super Output Areas Code",             
                                                                                  "Observation")) %>% 
    # Clean names
    clean_names() %>%
    
    # Mutate character to factors
    mutate(across(where(is.character), as.factor)) %>%
    
    # Rename variables
    rename(lsoa21cd = lower_layer_super_output_areas_code,
           property_type_census = accommodation_type_8_categories) %>%
    
    # Recast accommodation categories to match EPC data
    mutate(property_type_census = case_when(property_type_census == "In a purpose-built block of flats or tenement" ~ "Flat",
                                            property_type_census %in% c("Part of a converted or shared house, including bedsits",
                                                                        "Part of another converted building, for example, former school, church or warehouse",
                                                                        "In a commercial building, for example, in an office building, hotel or over a shop",
                                                                        "A caravan or other mobile or temporary structure") ~ "Other accommodation",
                                            property_type_census == "Semi-detached" ~ "Semi Detached",
                                            property_type_census == "Terraced" ~ "Terrace",
                                            .default = property_type_census)) %>%
    
    # SUmmarise across property type and LSOA (multiple rows for 'other accommodation')
    summarise(n_properties = sum(observation), .by = c("property_type_census",
                                                        "lsoa21cd")) %>%
    
    # Create indicator variable for property type = 'house' (for prevalence metric)
    mutate(property_type_h = case_when(property_type_census %in% c("Detached",
                                                                   "Semi Detached",
                                                                   "Terrace") ~ 1,
                                       .default = 0)) %>%
    
    # Keep all non-zero observations
    filter(n_properties > 0) %>%
    
    # Create new variable for proportion of all properties equal to each property type
    mutate(property_type_perc = n_properties / sum(n_properties), .by = "lsoa21cd") %>%
    
    left_join(data_ward, by = "lsoa21cd")
  
}

# Table formatting functions ---------------------------------------------------

#' Summarise wood fuel prevalence by property type and EPC sequence number
#'
#' @description
#' Filters the property-level EPC dataset to properties that have had exactly
#' \code{n_epc} certificates, restricts to houses (detached, semi-detached, terrace),
#' and returns a wide-format data frame showing WF prevalence and property counts by
#' property type and EPC sequence number (first, second, etc.).
#'
#' @param data A data frame of property-level EPC records with covariates, produced by
#'   \code{merge_data_epc_cleaned_covars}. Must contain columns \code{total_epc},
#'   \code{property_type_census}, \code{any_wood_h}, \code{epc_number}, and \code{uprn}.
#' @param n_epc A positive integer specifying the total number of EPC certificates to
#'   filter on (e.g. \code{2} to retain only properties with exactly two EPCs).
#'
#' @return A data frame in wide format with one row per Census house type category,
#'   containing columns \code{property_type_census}, \code{n_epc} (the filter value),
#'   and for each EPC sequence number: \code{wood_perc_h_{k}} (mean WF prevalence at
#'   EPC number \code{k}) and \code{n_{k}} (count of properties). Rows are sorted
#'   alphabetically by \code{property_type_census}.
#'
#' @details
#' This function is designed to be called via \code{make_summary_tabs_by_epc_number},
#' which applies it across a range of \code{n_epc} values and binds the results.

make_summary_tab_by_epc_number <- function(data,
                                           n_epc){
  
  summary_tab <- data %>%
    
    # Filter total EPC equal to specified number
    filter(total_epc == n_epc) %>%
    
    # Filter only houses
    filter(property_type_census %in% c("Detached",
                                       "Semi Detached",
                                       "Terrace")) %>%
    
    # Summarise wood fuel prevalence by EPC number and property type
    summarise(wood_perc_h = mean(any_wood_h, na.rm = TRUE),
              n = n(),
              .by = c(epc_number,   
                      property_type_census)) %>%
    
    # Pivot wider
    pivot_wider(id_cols = property_type_census,
                         names_from = epc_number,
                         values_from = c(wood_perc_h,
                                         n)) %>%
    
    # Make new column to indicate number of EPCs (to use as group var)
    mutate(n_epc = n_epc) %>%
    
    # Arrange alphabetically
    arrange(property_type_census)
  
  return(summary_tab)
  
}

#' Combine WF prevalence summary tables across multiple EPC sequence numbers
#'
#' @description
#' Calls \code{make_summary_tab_by_epc_number} iteratively for EPC counts 2 through
#' \code{max_n_epc} and binds the resulting wide-format tables into a single data frame.
#' The output is used to produce the repeat-EPC panel in the manuscript tables.
#'
#' @param data A data frame of property-level EPC records with covariates. Passed
#'   to each call of \code{make_summary_tab_by_epc_number}.
#' @param max_n_epc A positive integer specifying the maximum total number of EPC
#'   certificates to include. Tables are generated for \code{n_epc = 2, 3, ...,
#'   max_n_epc}. Properties with more than \code{max_n_epc} certificates are excluded
#'   due to small sample sizes.
#'
#' @return A data frame produced by \code{dplyr::bind_rows} across all EPC count
#'   strata, with \code{NA} for columns that do not exist at lower EPC counts (e.g.
#'   \code{wood_perc_h_4} is \code{NA} for rows where \code{n_epc == 2}).

make_summary_tabs_by_epc_number <- function(data,
                                            max_n_epc) {
  
  # Make list of tables by different EPC numbers (starting at 2 and ending at 'max_n_epc')
  tab_list <- lapply(rep(2:max_n_epc), make_summary_tab_by_epc_number, data = data)
  
  # Bind tables together, filling missing columns with NA
  tab_n_epc <- bind_rows(tab_list)
  
  return(tab_n_epc)
}
