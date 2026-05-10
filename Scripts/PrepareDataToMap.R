# Name of script: PrepareDataToMap
# Description:  Defines function to merge a summary dataset to an sf shapefile,
#               producing a spatially-enriched object ready for choropleth mapping
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 12-09-2024
# Latest update by: Calum Kennedy
# Latest update on: 12-09-2024

#' Merge a summary data frame to a shapefile for choropleth mapping
#'
#' @description
#' Left-joins a flat summary data frame to an \code{sf} shapefile, preserving all
#' geometries in the shapefile and attaching the corresponding summary statistics.
#' The resulting object is ready for use with \code{make_choropleth_map}.
#'
#' @param fill_data A data frame containing the summary statistics to be mapped.
#'   Must contain a column matching \code{join_var}.
#' @param shapefile_data An \code{sf} object containing the polygon geometries for
#'   mapping. Must contain a column matching \code{join_var}. All rows in this object
#'   are retained in the output; rows with no match in \code{fill_data} will have
#'   \code{NA} for the summary columns.
#' @param join_var A character string giving the name of the shared key column on
#'   which to join, e.g. \code{"lsoa21cd"}, \code{"lad22cd"}, or \code{"wd22cd"}.
#'
#' @return An \code{sf} object with the same rows and geometry as \code{shapefile_data},
#'   with columns from \code{fill_data} appended by left join on \code{join_var}.
#'
#' @details
#' The join is performed as a left join from \code{shapefile_data}, so only geographies
#' present in the shapefile are retained. Both \code{fill_data} and \code{shapefile_data}
#' must be at the same geographical resolution (e.g. both LSOA-level); no aggregation
#' is performed.

# Define function to merge datasets ready to map -------------------------------

prepare_data_to_map <- function(fill_data,
                                shapefile_data,
                                join_var){
  
  # Generate merged dataset
  data_to_map <- shapefile_data %>%
    
    # Left join to preserve only the rows in the mapping geometries data
    left_join(fill_data, by = join_var)
  
  # Return data ready to map
  return(data_to_map)
  
}
