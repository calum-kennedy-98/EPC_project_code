# Name of script: MakeSummaryDataByGroup
# Description: Defines function to make aggregate level dataset by arbitrary group vars
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 03-09-2024
# Latest update by: Calum Kennedy
# Latest update on: 20-09-2024
# Update notes: Changed function to refer to arbitrary grouping variables

# Comments ---------------------------------------------------------------------

# Define function to aggregate main data with all covariates to a summary dataset at an arbitrary
# geographical level (e.g. ward, LSOA, LAD, region)

#' Aggregate property-level EPC data to an arbitrary geographical level with predicted WF prevalence
#'
#' @description
#' Aggregates the enriched property-level EPC dataset (\code{data_epc_cleaned_covars})
#' to a summary cross-section at a user-specified geographical level (e.g. LSOA, ward,
#' local authority, or region). The function computes both observed EPC-based WF/SFA
#' prevalence and a Census-adjusted predicted prevalence that corrects for differential
#' EPC sampling rates across housing types.
#'
#' @param data_epc A data frame of property-level EPC records with covariates, produced
#'   by \code{merge_data_epc_cleaned_covars}. Must contain columns for WF/SFA indicators
#'   (\code{any_wood}, \code{any_sfa}, \code{any_wood_h}, \code{any_sfa_h}), the
#'   \code{most_recent} flag, LSOA codes, area and population covariates, and all
#'   variables named in \code{group_vars}.
#' @param data_housing_type_census A data frame of Census 2021 housing stock counts
#'   by LSOA and property type, produced by \code{make_data_housing_type_census}.
#'   Must contain columns \code{lsoa21cd}, the variable named in \code{housing_type_var},
#'   \code{n_properties}, \code{property_type_h} (1 for houses, 0 otherwise), and
#'   \code{property_type_perc}.
#' @param lsoa_var A character string giving the name of the LSOA-level geography
#'   column in \code{data_epc} (typically \code{"lsoa21cd"}). EPC-derived WF/SFA
#'   prevalence is always estimated at LSOA level before aggregation.
#' @param geo_level_var A character string giving the name of the target geography
#'   column for aggregation of predicted WF counts (e.g. \code{"lsoa21cd"},
#'   \code{"wd22cd"}, \code{"lad22cd"}, or \code{"rgn22nm"}).
#' @param housing_type_var A character string giving the name of the housing type
#'   column in both \code{data_epc} and \code{data_housing_type_census} on which to
#'   stratify prevalence estimates (typically \code{"property_type_census"}).
#' @param n_cutoff_conc_pred A positive integer. LSOA-by-housing-type cells with
#'   fewer than this many EPC observations are excluded from prevalence estimation
#'   before applying Census weights. This prevents unstable estimates from very
#'   small cells driving predictions.
#' @param group_vars A character vector of column names by which to group the final
#'   summary dataset. Typically includes \code{geo_level_var} and optionally
#'   \code{"rgn22nm"} to retain regional identifiers in the output.
#' @param most_recent_only A logical scalar. If \code{TRUE}, the main summary statistics
#'   (observed WF/SFA counts, socio-economic covariates) are computed using only the
#'   most recent EPC per property. The predicted prevalence section always uses only
#'   most recent EPCs regardless of this setting.
#'
#' @return A data frame with one row per unique combination of \code{group_vars},
#'   containing:
#'   \describe{
#'     \item{\code{wood_perc}, \code{sfa_perc}}{Numeric. Mean proportion of all
#'       properties (including flats) with WF or SFA heat sources in the EPC data.}
#'     \item{\code{any_wood}, \code{any_sfa}, \code{any_wood_h}, \code{any_sfa_h},
#'       \code{any_sfa_m}, \code{any_sfa_s}, \code{wood_m}, \code{wood_s},
#'       \code{pre_1950}}{Integer. Summed counts of properties with each indicator.}
#'     \item{\code{epc}}{Integer. Total number of EPC records in the group.}
#'     \item{\code{epc_house_total}}{Integer. Number of EPC records for houses
#'       (detached, semi-detached, or terrace).}
#'     \item{\code{wood_perc_h}, \code{sfa_perc_h}}{Numeric. Percentage of houses
#'       with WF or SFA heat sources, computed from observed EPC data.}
#'     \item{\code{wood_conc}, \code{sfa_conc}}{Numeric. Number of WF or SFA
#'       properties per km\eqn{^2}, computed from observed EPC data.}
#'     \item{\code{imd_score}, \code{imd_decile}, \code{white_pct},
#'       \code{median_age_mid_2022}, \code{urban}, \code{sca_area}}{Numeric. Mean
#'       socio-economic covariates across properties in the group. \code{sca_area} is
#'       1 if any part of the geography overlaps a Smoke Control Area.}
#'     \item{\code{num_people}, \code{area_in_km2}}{Numeric. Total population and
#'       area aggregated at LSOA level, then summed across LSOAs in the group.}
#'     \item{\code{wood_perc_h_predicted}, \code{wood_perc_h_predicted_normalised}}{Numeric.
#'       Census-adjusted estimate of WF prevalence in houses as a percentage and as a
#'       proportion (0--1), respectively. Computed by weighting LSOA-by-housing-type
#'       EPC prevalence estimates by Census 2021 house counts.}
#'     \item{\code{sfa_perc_predicted}, \code{sfa_perc_all_properties_predicted}}{Numeric.
#'       Census-adjusted SFA prevalence restricted to houses, and across all properties,
#'       respectively.}
#'     \item{\code{n_wood_predicted}, \code{n_sfa_predicted}}{Numeric. Predicted total
#'       number of WF or SFA heat sources in the geography (all property types combined).}
#'     \item{\code{wood_conc_pred}, \code{sfa_conc_pred}}{Numeric. Predicted WF or SFA
#'       concentration per km\eqn{^2}.}
#'     \item{\code{n_properties_census}}{Integer. Total Census 2021 housing stock count
#'       for the geography.}
#'   }
#'
#' @details
#' \strong{Predicted prevalence estimation}: EPC data do not provide a representative
#' sample of the housing stock because EPCs are triggered by property transactions and
#' improvements. Sampling rates differ by housing type (e.g. flats are under-represented
#' relative to houses). The predicted prevalence adjusts for this by computing
#' WF/SFA prevalence separately for each property type within each LSOA from the EPC
#' data, then applying those type-specific rates to the Census 2021 housing stock counts
#' within each target geography. This produces estimates that reflect the full
#' housing stock composition rather than the EPC sample composition.
#'
#' \strong{Aggregation}: Area and population are aggregated at LSOA level first (using
#' \code{distinct(lsoa21cd)}) before summing to the target geography, to avoid
#' double-counting when multiple EPC records exist for the same LSOA.

# Define function to make summary data by group --------------------------------

make_summary_data_by_group <- function(data_epc,
                                       data_housing_type_census,
                                       lsoa_var,
                                       geo_level_var,
                                       housing_type_var,
                                       n_cutoff_conc_pred,
                                       group_vars,
                                       most_recent_only){
  
  # Get mean WF/SF by housing type and LSOA (we always want to use the smallest level geography)
  data_wf_sf_predicted <- data_epc %>%
    
    # Filter most recent EPCs only
    filter(most_recent == TRUE) %>%
    
    # Get percentage of properties with WF/SF by property type and LSOA
    summarise(wood_perc = mean(any_wood, na.rm = TRUE),
              sfa_perc = mean(any_sfa, na.rm = TRUE),
              epc = n(),
              .by = c(lsoa_var,
                      housing_type_var)) %>%
    
    # Filter rows where have fewer than 'n_cutoff_conc_pred' data points
    filter(epc > n_cutoff_conc_pred)
  
  # Get dataset of predicted number of WF/SF heat sources by geography var 
  # (aggregate over all LSOAs within that geography)
  data_n_wood_predicted <- data_housing_type_census %>%
    
    left_join(data_wf_sf_predicted, by = c(lsoa_var,
                                           housing_type_var)) %>%
  
    summarise(n_wood_predicted = sum(wood_perc * n_properties, na.rm = TRUE),
           n_sfa_predicted = sum(sfa_perc * n_properties, na.rm = TRUE),
           wood_perc_h_predicted = sum(wood_perc * n_properties * property_type_h / sum(n_properties * property_type_h, na.rm = TRUE), na.rm = TRUE) * 100,
           wood_perc_h_predicted_normalised = sum(wood_perc * n_properties * property_type_h / sum(n_properties * property_type_h, na.rm = TRUE), na.rm = TRUE),
           sfa_perc_predicted = sum(sfa_perc * n_properties * property_type_h / sum(n_properties * property_type_h, na.rm = TRUE), na.rm = TRUE) * 100,
           sfa_perc_all_properties_predicted = sum(sfa_perc * n_properties / sum(n_properties, na.rm = TRUE), na.rm = TRUE) * 100,
           n_properties_census = sum(n_properties, na.rm = TRUE),
           .by = geo_level_var)
  
  # Generate dataset of area in km2 and population by LSOA for later joining
  data_area_pop <- data_epc %>%
    
    # Select distinct values for population/area
    distinct(lsoa21cd, .keep_all = TRUE) %>%
    
    # Summarise to generate aggregated data
    summarise(num_people = sum(num_people, na.rm = TRUE), # Calculate total population by geographical area
              area_in_km2 = sum(area_in_km2, na.rm = TRUE), # Calculate total area by geographical area 
              .by = group_vars)
  
  # If 'most_recent_only' is TRUE, filter data by 'most_recent' indicator
  if(most_recent_only == TRUE) data_epc <- data_epc %>% filter(most_recent == TRUE)
  
  # Aggregate data using specified group vars
  summary_data <- data_epc %>%
    
    # Remove observations where group indicator is missing
    filter(if_all(group_vars, ~ !is.na(.))) %>%
    
    # Aggregate summary variables by LSOA-year group
    summarise(
      
      # Percentage of all properties with WF/SF heat source
      wood_perc = mean(any_wood, na.rm = TRUE),
      sfa_perc = mean(any_sfa, na.rm = TRUE),
      
      # Sum relevant variables
      across(all_of(c("any_sfa_m",
                              "any_sfa_s",
                              "wood_m",
                              "wood_s",
                              "any_sfa",
                              "any_sfa_h",
                              "any_wood",
                              "any_wood_h",
                              "pre_1950")), ~ sum(., na.rm = TRUE)),
      
      # Total number of EPCs
      epc = n(),
              
      # Total number of EPCs on houses
      epc_house_total = sum(property_type_census %in% c("Detached",
                                                        "Semi Detached",
                                                        "Terrace"), na.rm = TRUE),
      
      # Average socio-economic indicators across smallest grouping variable
      imd_score = mean(imd_score, na.rm = TRUE), 
      imd_decile = mean(imd_decile, na.rm = TRUE),
      white_pct = mean(white_pct, na.rm = TRUE),
      median_age_mid_2022 = mean(median_age_mid_2022, na.rm = TRUE),
      urban = mean(urban, na.rm = TRUE),
      
      # Indicator for whether any part of geography is in an SCA
      # If there is any overlap with an SCA, the geography is classified as 1
      sca_area = case_when(mean(sca_area, na.rm = TRUE) > 0 ~ 1,
                           .default = 0),
      
      .by = group_vars) %>%
    
    # Join to area/population data
    full_join(data_area_pop, by = group_vars) %>%
    
    # Create new variables for concentration of SFAs per km2 and proportion of EPCs
    # with SFAs (restricted to houses)
    mutate(sfa_conc = any_sfa/area_in_km2,
           
           wood_conc = any_wood/area_in_km2,
           
           sfa_perc_h = (any_sfa_h/epc_house_total)*100,
           
           wood_perc_h = (any_wood_h/epc_house_total)*100,
           .by = group_vars) %>%
    
    # Convert NaNs to NAs
    mutate(across(where(is.numeric), ~ na_if(., NaN))) %>%
    
    # Left join to predicted number of WF heat sources
    left_join(data_n_wood_predicted, by = geo_level_var) %>%
    
    # Generate predicted WF/SF concentration
    mutate(sfa_conc_pred = n_sfa_predicted / area_in_km2,
           wood_conc_pred = n_wood_predicted / area_in_km2)
  
  return(summary_data)
}
