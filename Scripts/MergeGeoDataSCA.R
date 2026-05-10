# Name of script: MergeGeoDataSCA.R
# Description:  Merges UPRN lookup to SCA areas
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 30-09-2024
# Latest update by: Calum Kennedy
# Latest update on: 30-09-2024
# Update notes: 

# Comments ---------------------------------------------------------------------



#' Assign Smoke Control Area status to a UPRN-level spatial dataset
#'
#' @description
#' Performs a spatial join between a UPRN-level point dataset and Smoke Control Area
#' (SCA) polygon shapefiles for England and Wales, producing a binary indicator
#' (\code{sca_area}) recording whether each UPRN falls within an SCA. Eastings/northings
#' in British National Grid (BNG) are also converted to Web Mercator (EPSG:3857)
#' longitude/latitude coordinates for subsequent mapping and spatial merging.
#'
#' @param geo_data A data frame containing UPRN-level records with columns for
#'   eastings and northings in British National Grid coordinates, as well as UPRN
#'   and statistical geography identifiers. Typically the raw ONS NSUL lookup
#'   (\code{Data/raw/geo_files/nsul_lookup.parquet}).
#' @param sca_path_eng A character string giving the file path to the England SCA
#'   polygon shapefile (\code{.shp}). This shapefile must contain a \code{type}
#'   column identifying feature type; only features with \code{type == "Smoke Control
#'   Area"} are used.
#' @param sca_path_wal A character string giving the file path to the Wales SCA
#'   polygon shapefile (\code{.shp}). All features in this shapefile are treated as
#'   SCA polygons.
#' @param long_var A character string giving the name of the column in \code{geo_data}
#'   containing BNG eastings (e.g. \code{"gridgb1e"}).
#' @param lat_var A character string giving the name of the column in \code{geo_data}
#'   containing BNG northings (e.g. \code{"gridgb1n"}).
#'
#' @return A data frame (no geometry column) with the same rows as \code{geo_data}
#'   plus two new columns:
#'   \describe{
#'     \item{\code{sca_area}}{Integer (0/1). 1 if the UPRN falls within a Smoke
#'       Control Area, 0 otherwise.}
#'     \item{\code{long}, \code{lat}}{Numeric. Web Mercator (EPSG:3857) x and y
#'       coordinates, used for downstream spatial operations and mapping.}
#'   }
#'   The original BNG coordinate columns (\code{long_var}, \code{lat_var}) and the
#'   \code{geometry} column are removed from the output.
#'
#' @details
#' The England and Wales SCA shapefiles are bound by row after harmonising their
#' columns to \code{geometry} and \code{type}. The CRS of \code{geo_data} is set to
#' match the SCA shapefile CRS (typically EPSG:27700, BNG) before the spatial join.
#' UPRNs that do not intersect any SCA polygon receive \code{sca_area = 0}.
#' Web Mercator coordinates are extracted after transforming to EPSG:3857 and are
#' stored in columns named \code{long} and \code{lat} for consistency with downstream
#' EPC buffer operations.

# Define function to merge SCA data to UPRN lookup -----------------------------

merge_geo_data_sca <- function(geo_data,
                               sca_path_eng,
                               sca_path_wal,
                               long_var,
                               lat_var){
  
  # Read SCA shapefile for England
  sca_data_eng <- read_sf(sca_path_eng) %>%
    
    # Select relevant cols
    select(geometry, type)
  
  # Read SCA data for Wales
  sca_data_wal <- read_sf(sca_path_wal) %>%
    
    # Select geometry column
    select(geometry) %>%
    
    # Generate new 'type' column indicating SCA
    mutate(type = "Smoke Control Area")
  
  # Bind rows together to create final SCA spatial data
  sca_data <- rbind(sca_data_eng,
                    sca_data_wal)
  
  # Create merged geo data frame
  geo_data_sca <- geo_data %>%
    
    # Set as sf object using long/lat variables
    st_as_sf(coords = c(long_var,
                        lat_var),
             remove = FALSE) %>%
    
    # Set CRS to match the SCA shapefile
    st_set_crs(st_crs(sca_data)) %>%
    
    # Left join with SCA shapefile, keeping all rows in UPRN lookup
    st_join(sca_data["type"], left = TRUE) %>%
    
    # Rows not in SCAs show up as NA - convert to 0/1s
    mutate(sca_area = case_when(type == "Smoke Control Area" ~ 1,
                                .default = 0)) %>%
    
    # Remove 'type' column
    select(!type) %>%
    
    # Mutate characters to factors
    mutate(across(where(is.character), as.factor))
  
  # Mutate coordinates to long/lat (CRS 4326) from BNG (CRS 27700) for plotting
  coords_long_lat <- geo_data_sca %>% 
    
    # Transform coordinate reference system
    st_transform(3857) %>%
    
    # Extract coordinate vector
    st_coordinates() %>%
    
    # Set as tibble
    as_tibble() %>%
    
    # Rename to long and lat
    rename(long = X,
           lat = Y)
  
  # Add coordinates to data frame as new columns and remove geometry and old coordinate cols
  geo_data_sca <- geo_data_sca %>%
    
    # Recast as data frame
    as.data.frame() %>%
    
    cbind(coords_long_lat) %>%
    
    select(!c(geometry, long_var, lat_var)) %>%
    
    # Mutate characters to factors
    mutate(across(where(is.character), as.factor))
  
  # Return final data frame
  return(geo_data_sca)
  
}
