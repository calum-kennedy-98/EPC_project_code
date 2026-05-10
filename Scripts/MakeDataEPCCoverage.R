# Name of script: MergeStatisticalGeographies
# Description:  Defines function to generate a dataset of the percentage of 
# properties in the ONS UPRN lookup included in the main EPC data, by 
# an arbitrary statistical geography
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 13-09-2024
# Latest update by: Calum Kennedy
# Latest update on: 13-09-2024
# Update notes: 

# Comments ---------------------------------------------------------------------



#' Compute the proportion of properties with an EPC record by statistical geography
#'
#' @description
#' Joins the OS AddressBase UPRN dataset to the EPC data and calculates the
#' proportion of residential properties (by UPRN) that have at least one EPC
#' certificate, at an arbitrary statistical geography level. The resulting coverage
#' data frame is also joined to the LSOA-level covariate lookup to enable analysis
#' of geographic and socio-economic predictors of EPC coverage.
#'
#' @param data_epc A data frame of cleaned EPC records, produced by
#'   \code{clean_data_epc}. Must contain a \code{uprn} column.
#' @param data_os A data frame of OS AddressBase residential properties. Must contain
#'   a \code{uprn} column and, after joining to \code{data_uprn_sca_lookup}, a
#'   \code{lsoa21cd} column.
#' @param data_uprn_sca_lookup A data frame produced by \code{make_uprn_sca_lookup},
#'   linking UPRNs to statistical geography codes including \code{lsoa21cd}.
#' @param group_var An unquoted column name specifying the geographical level at which
#'   to aggregate EPC coverage (e.g. \code{lsoa21cd}).
#' @param path_lsoa_size A character string giving the file path to the LSOA area CSV.
#'   Passed to \code{make_lsoa_lookup_data}.
#' @param path_imd_eng A character string giving the file path to the English IMD 2019
#'   Excel workbook. Passed to \code{make_lsoa_lookup_data}.
#' @param path_imd_wales A character string giving the file path to the Welsh IMD 2019
#'   ODS file. Passed to \code{make_lsoa_lookup_data}.
#' @param path_lsoa11_lsoa21_lookup A character string giving the file path to the
#'   LSOA 2011-to-2021 best-fit lookup CSV. Passed to \code{make_lsoa_lookup_data}.
#' @param path_ethnicity A character string giving the file path to the Census 2021
#'   ethnicity CSV. Passed to \code{make_lsoa_lookup_data}.
#' @param path_region A character string giving the file path to the ward-to-region
#'   lookup CSV. Passed to \code{make_lsoa_lookup_data}.
#' @param path_ward A character string giving the file path to the LSOA-to-ward
#'   lookup CSV. Passed to \code{make_lsoa_lookup_data}.
#' @param path_urban_rural A character string giving the file path to the
#'   Rural/Urban Classification CSV. Passed to \code{make_lsoa_lookup_data}.
#' @param path_age A character string giving the file path to the LSOA median age
#'   Excel workbook. Passed to \code{make_lsoa_lookup_data}.
#'
#' @return A data frame with one row per unique value of \code{group_var}, containing:
#'   \describe{
#'     \item{\code{epc_coverage}}{Numeric. Percentage (0--100) of OS AddressBase
#'       UPRNs within the geography that have at least one EPC certificate.}
#'   }
#'   plus all covariate columns from \code{make_lsoa_lookup_data} joined by
#'   \code{lsoa21cd}.
#'
#' @details
#' OS AddressBase properties that cannot be linked to a statistical geography
#' (missing \code{lsoa21cd} after joining to the UPRN lookup) or that are in Scottish
#' LSOAs are excluded from the denominator. A unique property is identified by UPRN;
#' the binary indicator \code{epc_exists} is 1 if any EPC record with that UPRN exists
#' in \code{data_epc} (regardless of how many certificates). Coverage is thus defined
#' as the fraction of OS AddressBase UPRNs with at least one EPC, not the fraction of
#' all EPCs relative to properties.

# Define function to generate dataset of EPC coverage by geography -------------

make_data_epc_coverage <- function(data_epc,
                                   data_os,
                                   data_uprn_sca_lookup,
                                   group_var,
                                   path_lsoa_size,
                                   path_imd_eng,
                                   path_imd_wales,
                                   path_lsoa11_lsoa21_lookup,
                                   path_ethnicity,
                                   path_region,
                                   path_ward,
                                   path_urban_rural,
                                   path_age){
  
  # Make LSOA lookup data
  data_lsoa_lookup <- make_lsoa_lookup_data(path_lsoa_size,
                                            path_imd_eng,
                                            path_imd_wales,
                                            path_lsoa11_lsoa21_lookup,
                                            path_ethnicity,
                                            path_region,
                                            path_ward,
                                            path_urban_rural,
                                            path_age)
  
  # Load OS AddressBase dataset from specified path
  data_os_uprn <- data_os %>%
  
  # Left join OS data to statistical geographies
    left_join(data_uprn_sca_lookup, by = "uprn") %>%
    
    # Filter non-matched UPRNs (exclude if cannot be linked to a statistical geography)
    # and Scottish LSOAs
    filter(!is.na(lsoa21cd) & str_sub(lsoa21cd, 1, 1) != "S")
  
  # Keep distinct UPRNs in EPC data
  data_epc <- data_epc %>%
    
    distinct(uprn, .keep_all = TRUE) %>%
    
    # Make indicator variable for existence of EPC
    mutate(epc_exists = 1) %>%
    
    # Select relevant cols
    select(uprn, 
           epc_exists)
  
  # Left join UPRN lookup to EPC data
  data_epc_coverage <- data_os_uprn %>% 
    
    # Left join OS data to EPC data by UPRN
    left_join(data_epc, by = "uprn") %>%
    
    # Set 'epc_exists' to 0 if NA
    mutate(epc_exists = case_when(is.na(epc_exists) ~ 0,
                                  .default = epc_exists)) %>%
    
    # Summarise coverage by geography variable
    summarise(epc_coverage = mean(epc_exists, na.rm = TRUE) * 100,
              .by = {{group_var}}) %>%
  
  # Merge to LSOA lookup data
  left_join(data_lsoa_lookup, by = "lsoa21cd")
  
  return(data_epc_coverage)
  
}
