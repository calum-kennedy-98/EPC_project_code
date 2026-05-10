# Name of script: MergeDataEPCCleanedCovars.R
# Description:  Loads statistical geographies and other data to make LSOA-level 
# data to merge with main EPC data
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 03-09-2024
# Latest update by: Calum Kennedy
# Latest update on: 23-09-2024
# Update notes: 

# Comments ---------------------------------------------------------------------

# Loads statistical geography data and merges other data sources
# (e.g. ethnicity, IMD) into LSOA-level lookup dataset 'data_lsoa_lookup'
# I have used full joins to join the LSOA-level data, as at this stage want to 
# keep all data where possible, including missing data
# The resulting dataset is joined to the main EPC data to create a summary LSOA-level
# dataset 

#' Enrich cleaned EPC records with statistical geography identifiers and area-level covariates
#'
#' @description
#' Joins the cleaned EPC dataset to statistical geography identifiers (LSOA, ward, LAD,
#' region) and to an LSOA-level covariate lookup (IMD, ethnicity, age, urban/rural
#' classification). Two linkage strategies are used: EPC records with a UPRN are joined
#' via the UPRN-to-geography lookup, while records with a missing UPRN are joined via
#' postcode. The function also identifies the most recent EPC for each unique property
#' and assigns EPC sequence numbers.
#'
#' @param data A data frame of cleaned EPC records produced by \code{clean_data_epc}.
#'   Must contain columns \code{uprn}, \code{postcode}, and \code{inspection_date}.
#' @param data_uprn_sca_lookup A data frame produced by \code{make_uprn_sca_lookup},
#'   containing one row per UPRN with columns for statistical geography codes, the
#'   \code{sca_area} binary indicator, and \code{long}/\code{lat} Web Mercator
#'   coordinates.
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
#' @return A data frame with one row per EPC certificate, combining all columns from
#'   the cleaned EPC data with geography and covariate columns from the UPRN lookup
#'   and LSOA lookup. Key additional columns include:
#'   \describe{
#'     \item{\code{most_recent}}{Logical. \code{TRUE} if this is the most recent EPC
#'       for the UPRN (based on \code{inspection_date} order set in
#'       \code{clean_data_epc}). Always \code{TRUE} for records without a UPRN.}
#'     \item{\code{epc_number}}{Integer. Sequential EPC number for the property,
#'       with 1 denoting the earliest and \code{total_epc} the most recent.}
#'     \item{\code{total_epc}}{Integer. Total number of EPCs recorded for the UPRN.
#'       Always 1 for records without a UPRN.}
#'     \item{\code{urban}}{Integer (0/1/NA). 1 if the LSOA's 2011 Rural/Urban
#'       Classification contains "Urban", 0 if it contains "Rural", \code{NA} if
#'       the classification is missing.}
#'     \item{\code{sca_area}}{Integer (0/1). Smoke Control Area membership from the
#'       UPRN lookup.}
#'     \item{\code{long}, \code{lat}}{Numeric. Web Mercator coordinates.}
#'     \item{\code{lsoa21cd}, \code{wd22cd}, \code{lad22cd}, \code{rgn22nm}}{Geography
#'       identifiers from the UPRN/postcode lookup.}
#'     \item{\code{imd_score}, \code{imd_decile}, \code{white_pct},
#'       \code{median_age_mid_2022}, \code{area_in_km2}, \code{num_people}}{LSOA-level
#'       covariates from \code{make_lsoa_lookup_data}.}
#'   }
#'   Columns \code{postcode}, \code{inspection_date}, \code{pcds}, and \code{ruc11}
#'   are dropped from the output.
#'
#' @details
#' \strong{UPRN linkage}: EPC records with a non-missing UPRN are joined to the UPRN
#' lookup using \code{uprn} as the key, then full-joined to the LSOA covariate lookup
#' on \code{lsoa21cd} (to retain LSOAs with no EPC coverage). Scottish LSOAs
#' (identified by \code{rgn22cd == "S99999999"}) are excluded at this stage.
#'
#' \strong{Postcode fallback}: Records with a missing UPRN are joined to a postcode-level
#' summary of the UPRN lookup (\code{data_geo_pcds}) using the cleaned postcode as the
#' key. Postcodes that span multiple LSOAs (identified via \code{janitor::get_dupes})
#' are excluded from this lookup, as their LSOA cannot be determined unambiguously.
#' Records matched via postcode are assumed to be the most recent EPC for the property.
#'
#' \strong{Most-recent identification}: For UPRN-linked records, the most recent EPC is
#' identified using \code{data.table::rowid}, which returns 1 for the first row within
#' each UPRN group. Because \code{clean_data_epc} arranges records in descending
#' \code{inspection_date} order, the first row corresponds to the most recent certificate.

# Define function to merge covars with cleaned main EPC data -------------------

merge_data_epc_cleaned_covars <- function(data,
                                          data_uprn_sca_lookup,
                                          path_stat_geo_files,
                                          path_lsoa_size,
                                          path_imd_eng,
                                          path_imd_wales,
                                          path_lsoa11_lsoa21_lookup,
                                          path_ethnicity,
                                          path_region,
                                          path_ward,
                                          path_urban_rural,
                                          path_age){

  # Merge statistical geographies and SCA areas --------------------------------
  
  # Generate postcode-level lookup data
  data_geo_pcds <- data_uprn_sca_lookup %>%
    
    # Remove missing postcodes
    filter(!is.na(pcds)) %>%
    
    # Drop UPRN column
    select(-uprn) %>%
    
    # Retain distinct rows
    distinct()
  
  # Some postcodes are spread across multiple LSOAs - we exclude these from the
  # analysis since it is not possible to attribute them to a specific LSOA
  # Here we generate a dataset of duplicate postcodes in the lookup, and filter
  # them by performing an anti join with the main postcode lookup dataset
  data_geo_pcds_dupes <- get_dupes(data_geo_pcds, pcds)
  
  data_geo_pcds <- data_geo_pcds %>%
    
    # Anti join to filter postcodes split across multiple LSOAs
    anti_join(data_geo_pcds_dupes)
  
  # Make LSOA-level lookup data ------------------------------------------------
  
  data_lsoa_lookup <- make_lsoa_lookup_data(path_lsoa_size,
                                            path_imd_eng,
                                            path_imd_wales,
                                            path_lsoa11_lsoa21_lookup,
                                            path_ethnicity,
                                            path_region,
                                            path_ward,
                                            path_urban_rural,
                                            path_age)
  
  # Merge statistical geographies and secondary data onto main EPC data --------
  
  # Merge data with UPRNs
  data_epc_cleaned_covars_with_uprn <- data %>%
    
    # Filter non-missing UPRNs
    filter(!is.na(uprn)) %>%
    
    # Keep only distinct rows with UPRNs (we do not do this for those with missing UPRNs
    # since these entries may contain multiple properties with same postcode)
    distinct() %>%
    
    # Left join to statistical geographies by UPRN
    left_join(data_uprn_sca_lookup, by = "uprn") %>%
    
    # Remove Scottish LSOAs
    filter(rgn22cd != "S99999999") %>%
    
    # Full join to LSOA lookup data - full join to keep LSOAs even if no corresponding
    # entries in EPC data
    full_join(data_lsoa_lookup, by = "lsoa21cd")
  
  # Merge data without UPRNs using postcode
  data_epc_cleaned_covars_without_uprn <- data %>%
    
    # Filter only missing UPRNs
    filter(is.na(uprn)) %>%
    
    # Left join to statistical geographies by PCDS
    left_join(data_geo_pcds, by = c("postcode" = "pcds")) %>%
    
    # Remove Scottish LSOAs
    filter(rgn22cd != "S99999999") %>%
    
    # Left join to LSOA lookup data (only keep rows in main EPC data)
    left_join(data_lsoa_lookup, by = "lsoa21cd") %>%
    
    # Create 'most recent' variable (assume all EPCs without UPRN are most recent)
    mutate(most_recent = TRUE)
  
  # Set to data.table
  setDT(data_epc_cleaned_covars_with_uprn)
        
  # Generate new indicator variable for whether an EPC is the most recent 
  # for that UPRN. NOTE: the rows are pre-ordered by date in the 'CleanDataEPC' script
  data_epc_cleaned_covars_with_uprn <- data_epc_cleaned_covars_with_uprn[, most_recent := rowid(uprn) == 1]
  
  # Note: In the case of missing UPRNs, I keep all observations since we cannot
  # tell whether there are duplicate EPCs or not
  
  # Merge two data.tables to recreate main data.table
  data_epc_cleaned_covars <- bind_rows(data_epc_cleaned_covars_with_uprn,
                                       data_epc_cleaned_covars_without_uprn) %>%
    
    # Remove duplicate columns
    select(!ends_with(".y")) %>%
    
    rename_with(~ str_replace(., ".x", "")) %>%
    
    # Create indicator for EPC number by property (inverse of row_number() function)
    # and for total number of EPCs by property
    mutate(total_epc = max(row_number()),
           epc_number = total_epc - row_number() + 1,
           .by = uprn) %>%
    
    # Create new binary variable for urban/rural
    mutate(urban = case_when(grepl("Urban", ruc11) ~ 1,
                             grepl("Rural", ruc11) ~ 0,
                             .default = NA)) %>%
    
    # Remove unused variables
    select(!c(postcode,
              inspection_date,
              pcds, 
              ruc11)) %>%
    
    # Set character variables as factors
    mutate(across(where(is.character), as.factor))
  
  # Test remove ancilliary datasets
  rm(data_uprn_sca_lookup,
     data_epc_cleaned_covars_with_uprn,
     data_epc_cleaned_covars_without_uprn,
     data_geo_pcds,
     data_geo_pcds_dupes)
  
  # Return 'data_epc_cleaned_covars'
  return(data_epc_cleaned_covars)

}