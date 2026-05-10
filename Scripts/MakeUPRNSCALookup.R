# Name of script: MakeUPRNSCALookup.R
# Description:  Loads statistical geographies and SCA data to merge with main EPC data
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 02-10-2024
# Latest update by: Calum Kennedy
# Latest update on: 02-10-2024
# Update notes: 

# Comments ---------------------------------------------------------------------


#' Build a UPRN-to-geography lookup enriched with Smoke Control Area status
#'
#' @description
#' Reads the ONS National Statistics UPRN Lookup (NSUL) Parquet file and passes it to
#' \code{merge_geo_data_sca} to produce a flat data frame linking each UPRN to its
#' statistical geography identifiers (LSOA, ward, LAD, region) and a binary indicator
#' for Smoke Control Area (SCA) membership. The resulting lookup is used in
#' \code{merge_data_epc_cleaned_covars} to enrich EPC records with geographic and
#' policy context.
#'
#' @param path_stat_geo_files A character string giving the file path to the ONS NSUL
#'   Parquet file (e.g. \code{"Data/raw/geo_files/nsul_lookup.parquet"}). This file
#'   must contain UPRN identifiers, BNG easting and northing columns, and statistical
#'   geography code columns (LSOA, ward, LAD, region).
#' @param sca_path_eng A character string giving the file path to the England SCA
#'   polygon shapefile. Passed directly to \code{merge_geo_data_sca}.
#' @param sca_path_wal A character string giving the file path to the Wales SCA
#'   polygon shapefile. Passed directly to \code{merge_geo_data_sca}.
#' @param long_var A character string giving the name of the BNG easting column in the
#'   NSUL lookup (e.g. \code{"gridgb1e"}). Passed to \code{merge_geo_data_sca}.
#' @param lat_var A character string giving the name of the BNG northing column in the
#'   NSUL lookup (e.g. \code{"gridgb1n"}). Passed to \code{merge_geo_data_sca}.
#'
#' @return A data frame with one row per UPRN containing statistical geography
#'   identifiers, a binary \code{sca_area} column, and Web Mercator \code{long} and
#'   \code{lat} coordinate columns. See \code{merge_geo_data_sca} for full details of
#'   the output structure.

# Define function to merge statistical geographies with SCA data ---------------

make_uprn_sca_lookup <- function(path_stat_geo_files,
                                 sca_path_eng,
                                 sca_path_wal,
                                 long_var,
                                 lat_var){
  
  # Get UPRN lookup datasets and merge using 'merge statistical geographies' function
  data_geo_uprn <- read_parquet(path_stat_geo_files)
  
  # Add in SCA status using 'merge_geo_data_sca' function
  data_uprn_sca_lookup <- merge_geo_data_sca(geo_data = data_geo_uprn,
                                             sca_path_eng = sca_path_eng,
                                             sca_path_wal = sca_path_wal,
                                             long_var = long_var,
                                             lat_var = lat_var)
  
  # Return merged dataset
  return(data_uprn_sca_lookup)
  
}