# Name of script: MakeLAEIData
# Description: Defines function to make merged grid square LAEI data for mapping,
#              combining OS AddressBase, UPRN-SCA lookup, and EPC data
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 30-11-2024
# Latest update by: Calum Kennedy
# Latest update on: 30-11-2024

#' Build a grid-square-level LAEI shapefile with predicted wood fuel counts and PM\eqn{_{2.5}} emissions
#'
#' @description
#' Merges the London Atmospheric Emissions Inventory (LAEI) grid square shapefile with
#' OS AddressBase residential property counts, UPRN-level geographic lookup data, and
#' EPC wood fuel indicators to produce a spatially referenced data frame of predicted
#' wood fuel (WF) heat source counts and domestic biomass PM\eqn{_{2.5}} emissions by
#' LAEI grid square. Only GLA (Greater London Authority) grid squares are retained.
#'
#' @param path_data_laei A character string giving the file path to the LAEI shapefile
#'   (\code{.shp}). The shapefile must contain columns \code{borough} (to filter GLA
#'   areas), \code{biomass19} (domestic biomass PM\eqn{_{2.5}} emissions in 2019 in
#'   kilotonnes), and \code{grid_id} (unique grid square identifier).
#' @param data_os A data frame of OS AddressBase residential property records with a
#'   \code{uprn} column and a \code{property_type_census} column.
#' @param data_uprn_sca_lookup A data frame produced by \code{make_uprn_sca_lookup},
#'   containing columns \code{uprn}, \code{rgn22cd}, \code{long}, and \code{lat}
#'   (Web Mercator coordinates).
#' @param data_epc_cleaned_covars A data frame produced by
#'   \code{merge_data_epc_cleaned_covars}, containing columns \code{uprn},
#'   \code{most_recent}, and \code{any_wood}.
#'
#' @return An \code{sf} object with one row per LAEI grid square (GLA only), containing:
#'   \describe{
#'     \item{\code{grid_id}}{Character. Unique LAEI grid square identifier.}
#'     \item{\code{n_wood_pred}}{Numeric. Predicted total number of WF heat sources in
#'       the grid square, computed as the sum over all property types of (Census-weighted
#'       OS property count) \eqn{\times} (EPC-derived WF prevalence for that type).}
#'     \item{\code{pm_25_emissions}}{Numeric. Mean domestic biomass PM\eqn{_{2.5}}
#'       emissions in 2019 across the properties in the grid square (kilotonnes/year).}
#'     \item{\code{geometry}}{The original LAEI polygon geometry in EPSG:3857 (Web
#'       Mercator).}
#'   }
#'
#' @details
#' The LAEI grid is filtered to GLA boroughs only (\code{borough != "Non GLA"}).
#' OS AddressBase records are joined to the UPRN lookup to obtain coordinates, then
#' restricted to London UPRNs (\code{rgn22cd == "E12000007"}). EPC WF indicators
#' (\code{any_wood}) from the most recent certificate per UPRN are left-joined to the
#' combined OS/UPRN dataset. Properties with missing coordinates or missing Census
#' property type are excluded before the spatial join to the LAEI grid.
#' Predicted WF counts are computed by applying grid-square-level EPC prevalence rates
#' (stratified by property type) to OS property counts, then summing across property types.
#' This approach mirrors the Census-reweighting methodology in \code{make_summary_data_by_group}.

# Define function to make summary data by group --------------------------------

make_laei_data <- function(path_data_laei,
                           data_os,
                           data_uprn_sca_lookup,
                           data_epc_cleaned_covars){
  
  # Get LAEI shapefile
  shp_laei <- read_sf(path_data_laei) %>%
    
    # Clean names
    clean_names() %>%
    
    # Keep GLA authorities
    filter(borough != "Non GLA") %>%
     
    # Select column for domestic biomass emissions in 2019
    select(biomass19, grid_id) %>%
    
    # Set crs to match EPC data
    st_transform(3857)
  
  # Keep relevant rows from UPRN lookup
  data_uprn_sca_lookup <- data_uprn_sca_lookup %>%
    
    select(uprn,
           rgn22cd,
           long,
           lat) %>%
    
    # Filter London UPRNs
    filter(rgn22cd == "E12000007")
  
  # Process EPC data
  data_epc_cleaned_covars_wood <- data_epc_cleaned_covars %>%
    
    # Remove non WF heat sources
    filter(most_recent == TRUE) %>%
    
    # Select relevant cols
    select(any_wood, uprn)
  
  # Merge OS, UPRN, and EPC data
  data_os_uprn_epc <- data_os %>%
    
    # Left join to UPRN data 
    left_join(data_uprn_sca_lookup, by = "uprn") %>%
    
    # Retain London UPRNs
    filter(rgn22cd == "E12000007") %>%
    
    # Remove region col
    select(!rgn22cd) %>%
    
    # Left join to EPC data
    left_join(data_epc_cleaned_covars_wood, by = "uprn") %>%
    
    # Filter if missing coordinates and if census property type missing 
    filter(!is.na(long) & !is.na(lat) & !is.na(property_type_census)) %>%
    
    # Set as sf
    st_as_sf(coords = c("long",
                        "lat")) %>%
    
    # Transform crs for consistency with LAEI shapefile
    st_set_crs(3857)
  
  # Merge shapefile with OS, UPRN and EPC lookup and generate summary variables
  shp_laei_merged <- shp_laei %>%
    
    # st join to OS, UPRN, EPC data by st_contains
    st_join(data_os_uprn_epc, join = st_contains) %>%
    
    # Group by grid square ID and Census property type
    group_by(grid_id,
             property_type_census) %>%
    
    # Get total N properties from OS data by grid square and property type,
    # Percentage of properties with WF by grid square and property type,
    # and retain PM2.5 emissions for later mapping
    summarise(n_properties = n(),
              n_epc = sum(!is.na(any_wood), na.rm = TRUE),
              wf_perc = mean(any_wood, na.rm = TRUE),
              pm_25_emissions = mean(biomass19, na.rm = TRUE)) %>%
    
    # Ungroup 
    ungroup() %>%
    
    # Calculate total predicted WF heat sources by property type as product of 
    # n_properties and WF percentage
    mutate(n_wood_pred = n_properties * wf_perc) %>%
    
    # Group by grid ID
    group_by(grid_id) %>%
    
    # Get total sum of predicted WF heat sources and mean PM2.5 emissions by grid square
    summarise(n_wood_pred = sum(n_wood_pred, na.rm = TRUE),
              pm_25_emissions = mean(pm_25_emissions, na.rm = TRUE))
  
  # Return merged shapefile
  return(shp_laei_merged)
  
}
