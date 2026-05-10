

#' Spatially join monitoring station data to London Atmospheric Emissions Inventory grid squares
#'
#' @description
#' Assigns each air quality monitoring station to its corresponding London Atmospheric
#' Emissions Inventory (LAEI) or National Atmospheric Emissions Inventory (NAEI) grid
#' square by a point-in-polygon spatial join, then appends grid-level wood fuel
#' emission and predicted concentration variables to the monitoring data. Stations
#' outside the inventory grid (i.e. outside London for the LAEI) are excluded.
#'
#' @param data_openair_epc A data frame of monitoring records with EPC-derived WF
#'   counts, produced by \code{merge_openair_epc_data}. Must contain columns
#'   \code{longitude} and \code{latitude} (WGS84, EPSG:4326 decimal degrees), as well
#'   as \code{code} and all temporal/count variables added by \code{merge_openair_epc_data}.
#' @param data_laei An \code{sf} object of LAEI or NAEI grid squares loaded in
#'   \code{_targets.R} via \code{st_read}. Must contain a \code{grid_id} column and
#'   the renamed columns \code{n_wood_pred} (predicted number of wood fuel heat
#'   sources per grid square) and \code{pm_25_emissions} (estimated PM\eqn{_{2.5}}
#'   emissions in kilotonnes per year). The CRS must be EPSG:3857 (Web Mercator), as
#'   set by the \code{rename} and \code{mutate} operations applied in \code{_targets.R}.
#'
#' @return A tibble (no geometry column) containing all columns from
#'   \code{data_openair_epc} for stations successfully matched to an inventory grid
#'   square, plus:
#'   \describe{
#'     \item{\code{grid_id}}{Character. Unique identifier for the matched inventory
#'       grid square.}
#'     \item{\code{n_wood_pred}}{Numeric. Predicted number of wood fuel heat sources
#'       in the grid square from the inventory model.}
#'     \item{\code{pm_25_emissions}}{Numeric. Estimated annual PM\eqn{_{2.5}} wood
#'       fuel emissions for the grid square (kilotonnes per year).}
#'     \item{\code{log_n_wood_pred}}{Numeric. Natural log of \code{n_wood_pred};
#'       0 when \code{n_wood_pred == 0}.}
#'   }
#'   Stations outside the inventory domain (i.e. with \code{grid_id == NA} after the
#'   join) are excluded from the output.
#'
#' @details
#' The function transforms monitoring station coordinates from WGS84 (EPSG:4326) to
#' Web Mercator (EPSG:3857) to match the CRS of the inventory shapefile before the
#' \code{st_join(join = st_within)} operation. The geometry column is dropped before
#' returning, so the result is a flat data frame suitable for use in
#' \code{make_patchwork_plot_openair}.

make_openair_epc_laei_data <- function(data_openair_epc,
                                       data_laei){
  
  data_openair_epc_laei <- data_openair_epc %>%
    
    # Filter if NA coordinates
    filter(!is.na(longitude) & !is.na(latitude)) %>%
    
    # Set as sf object
    st_as_sf(coords = c("longitude",
                        "latitude")) %>%
    
    # Set CRS
    st_set_crs(4326) %>%
    
    # Transform to match LAEI CRS
    st_transform(3857) %>%
    
    # Join to LAEI data
    st_join(data_laei, join = st_within) %>%
    
    # Remove unmatched grid ids (outside of London)
    filter(!is.na(grid_id)) %>%
    
    # Set as df
    as_tibble() %>%
    
    # Remove geometry variable
    select(!geometry)
  
  return(data_openair_epc_laei)
  
}