# Name of script: SetSpatialPoints
# Description:  Defines function to convert a data frame with coordinate columns to a
#               British National Grid sf object
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 31-10-2024
# Latest update by: Calum Kennedy
# Latest update on: 31-10-2024

#' Convert a data frame with coordinate columns to a British National Grid sf object
#'
#' @description
#' Takes a data frame containing longitude and latitude columns in WGS84 (EPSG:4326)
#' and converts it to a simple features (\code{sf}) object projected in British National
#' Grid (EPSG:27700). BNG uses metres as its unit of distance, which is required for
#' metric spatial operations such as \code{st_buffer} and \code{st_join}.
#'
#' @param data A data frame or tibble containing columns for longitude and latitude
#'   in decimal degrees (WGS84, EPSG:4326).
#' @param longitude_var A character string giving the name of the column containing
#'   WGS84 longitude values.
#' @param latitude_var A character string giving the name of the column containing
#'   WGS84 latitude values.
#'
#' @return An \code{sf} object with POINT geometry in British National Grid (EPSG:27700).
#'   The coordinate columns named in \code{longitude_var} and \code{latitude_var} are
#'   replaced by the \code{geometry} column.
#'
#' @details
#' The coordinate reference system transformation proceeds in two steps: first the data
#' frame is cast to an \code{sf} object with CRS EPSG:4326 (WGS84), then transformed to
#' EPSG:27700 (British National Grid). This two-step approach is necessary because
#' \code{st_as_sf} requires an initial CRS assignment before transformation.

# Define function to set dataset to spatial points -----------------------------

set_spatial_points <- function(data,
                                 longitude_var,
                                 latitude_var){
  
  sf_data <- data %>%
    
    # Set as sf object
    st_as_sf(coords = c(longitude_var,
                        latitude_var)) %>%
    
    # Set default CRS
    st_set_crs(4326) %>%
    
    # Transform CRS
    st_transform(27700)
  
  # Return prepared sf
  return(sf_data)
  
}

