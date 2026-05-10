# Name of script: MergeOpenairEPCData
# Description:  Defines function to construct measure of concentration of WF heat
# sources around monitors derived from openair data
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 31-10-2024
# Latest update by: Calum Kennedy
# Latest update on: 31-10-2024
# Update notes: 

# Comments ---------------------------------------------------------------------



#' Merge air quality monitoring data with EPC-derived wood fuel counts within a spatial buffer
#'
#' @description
#' For each air quality monitoring station, counts the number of wood fuel (WF) heat
#' sources and total EPC properties falling within a circular buffer of specified radius.
#' These counts are joined back to the hourly monitoring data, enabling correlation
#' analysis between WF source density and PM\eqn{_{2.5}} concentrations. Temporal
#' variables for seasonal analysis (season, peak/non-peak hours, weekday/weekend) and
#' daily PM\eqn{_{2.5}} difference metrics are also computed.
#'
#' @param data_openair A data frame of air quality monitoring records produced by
#'   \code{get_openair_data}. Must contain columns \code{site}, \code{code},
#'   \code{longitude}, \code{latitude}, \code{source}, \code{site_type}, \code{date},
#'   \code{day}, \code{month}, \code{hour}, and \code{pm2.5}.
#' @param data_epc A data frame of property-level EPC records with covariates, produced
#'   by \code{merge_data_epc_cleaned_covars}. Must contain columns named by
#'   \code{long_var_epc} and \code{lat_var_epc} (Web Mercator coordinates), \code{most_recent},
#'   \code{any_wood}, and \code{sca_area}.
#' @param long_var_epc A character string giving the name of the Web Mercator longitude
#'   column in \code{data_epc} (typically \code{"long"}).
#' @param lat_var_epc A character string giving the name of the Web Mercator latitude
#'   column in \code{data_epc} (typically \code{"lat"}).
#' @param buffer_radius A positive numeric scalar specifying the radius of the circular
#'   buffer around each monitoring station, in metres (e.g. \code{1000} for 1 km).
#'   The analysis is run at 500 m, 1000 m, and 2000 m in the main pipeline to assess
#'   sensitivity to buffer size.
#'
#' @return A data frame with the same rows as \code{data_openair}, augmented with the
#'   following additional columns:
#'   \describe{
#'     \item{\code{n_wf}}{Integer. Number of most-recent EPC properties with \code{any_wood == 1}
#'       within the buffer around the monitoring station.}
#'     \item{\code{n}}{Integer. Total number of most-recent EPC properties within the buffer.}
#'     \item{\code{sca_area}}{Numeric. Mean proportion of EPC properties within the buffer
#'       that fall within a Smoke Control Area.}
#'     \item{\code{season}}{Character. Season label: \code{"Winter"} (Dec--Feb),
#'       \code{"Spring"} (Mar--May), \code{"Summer"} (Jun--Aug), or \code{"Autumn"}
#'       (Sep--Nov).}
#'     \item{\code{weekend}}{Integer (0/1). 1 if the observation falls on a Saturday or Sunday.}
#'     \item{\code{day_id}}{Integer. Day of year (1--366), used as a grouping key for
#'       computing daily summary statistics.}
#'     \item{\code{peak}}{Integer (0/1). 1 if the hour falls within the peak wood-burning
#'       period (19:00--01:00).}
#'     \item{\code{non_peak}}{Integer (0/1). 1 if the hour falls within the daytime
#'       non-peak period (05:00--17:00).}
#'     \item{\code{pm2.5_diff_peak}}{Numeric. Mean PM\eqn{_{2.5}} during peak hours minus
#'       mean PM\eqn{_{2.5}} during non-peak hours for the same monitoring site on the same
#'       calendar day. Positive values indicate elevated evening/overnight concentrations.}
#'     \item{\code{log_n_wf}}{Numeric. Natural log of \code{n_wf}; 0 when \code{n_wf == 0}.}
#'     \item{\code{log_n}}{Numeric. Natural log of \code{n}; 0 when \code{n == 0}.}
#'   }
#'
#' @details
#' EPC coordinates are stored in Web Mercator (EPSG:3857, metres) from the UPRN
#' lookup processing. The function transforms these to British National Grid (EPSG:27700,
#' also metres) before buffering, since \code{st_buffer} requires a projected CRS for
#' accurate distance calculations. Monitoring station coordinates (WGS84, EPSG:4326)
#' are similarly projected to EPSG:27700 via \code{set_spatial_points} before buffering.
#' Only the most recent EPC for each UPRN (\code{most_recent == TRUE}) is used in the
#' spatial count, to avoid double-counting properties that have had multiple surveys.
#' Stations with missing coordinates are excluded from the spatial join.

# Define function to merge openair data with EPC data --------------------------

merge_openair_epc_data <- function(data_openair,
                                    data_epc,
                                    long_var_epc,
                                    lat_var_epc,
                                    buffer_radius){
  
  # Get unique monitoring sites from openair
  unique_sites <- distinct(data_openair, site, .keep_all = TRUE) %>%
    
    # Filter if NA coordinates
    filter(!is.na(longitude) & !is.na(latitude))
  
  # Set unique sites as sf object using 'set_spatial_points' function
  unique_sites_sf <- set_spatial_points(unique_sites,
                                          "longitude",
                                          "latitude")
  
  # Get main EPC data and set as sf object, retaining only long/lat columns
  data_epc_sf <- data_epc %>% 
    
    select(long_var_epc,
           lat_var_epc,
           most_recent,
           any_wood,
           sca_area) %>%
    
    filter(most_recent == TRUE) %>%
    
    # Set as SF (need to initially set CRS 3857 as this was defined in previous
    # data cleaning, before transforming to BNG27700 as this is in metres)
    st_as_sf(coords = c(long_var_epc,
                        lat_var_epc), crs = 3857) %>%
    
    st_transform(27700)
  
  # Create an st_buffer object using the 'unique_sites_sf' object to capture
  # the number of WF heat sources around that site
  unique_sites_buffer <- st_buffer(unique_sites_sf, dist = buffer_radius)
  
  # Spatial join the EPC data to the generated buffer
  data_epc_buffer_joined <- data_epc_sf %>%
    
    # Join to EPC data using the 'st_within' command
    st_join(unique_sites_buffer, join = st_within)
  
  # Summarise count data by site for merging back to main openair data
  unique_sites_with_counts <- data_epc_buffer_joined %>%
    
    # Set as dataframe
    as.data.frame() %>%
    
    # Filter non-matched rows
    filter(!is.na(code)) %>%
    
    # Count number of WF and total number of heat sources within specified radius
    summarise(n_wf = sum(any_wood == 1, na.rm = TRUE),
              n = n(),
              sca_area = mean(sca_area, na.rm = TRUE),
              .by = code)
  
  # Left join counts data to main openair data
  data_openair_with_counts <- data_openair %>%
    
    left_join(unique_sites_with_counts, by = "code") %>%
    
    # Generate necessary variables for plotting
    mutate(season = case_when(month %in% c(6, 7, 8) ~ "Summer",
                              month %in% c(12, 1, 2) ~ "Winter",
                              month %in% c(3, 4, 5) ~ "Spring",
                              month %in% c(9, 10, 11) ~ "Autumn"),
           weekend = case_when(day %in% c("Saturday", 
                                         "Sunday") ~ 1, .default = 0),
           
           # Variable for each unique day of year (for calculating grouped variables below)
           day_id = lubridate::yday(date),
           
           # Indicator variable for peak burning times (1900 - 0100)
           peak = case_when(hour %in% c("19", 
                                        "20", 
                                        "21", 
                                        "22", 
                                        "23", 
                                        "00", 
                                        "01") ~ 1, .default = 0),
    
    # Indicator variable for non-peak burning
    non_peak = case_when(hour %in% c("05",
                                     "06",
                                     "07",
                                     "08",
                                     "09",
                                     "10",
                                     "11",
                                     "12",
                                     "13",
                                     "14",
                                     "15",
                                     "16",
                                     "17") ~ 1, .default = 0)) %>%
    
    # Get difference in mean PM during peak time vs. during non-peak time
    mutate(pm2.5_diff_peak = mean(pm2.5[peak==1], na.rm = TRUE) - mean(pm2.5[non_peak==1], na.rm = TRUE),
           log_n_wf = ifelse(n_wf > 0, log(n_wf), 0), 
           log_n = ifelse(n > 0, log(n), 0),
           .by = c(code, day_id))
  
  return(data_openair_with_counts)
}
