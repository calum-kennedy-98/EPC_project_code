# Name of script: GetOpenairData
# Description:  Defines function to generate dataset from R package openair
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 31-10-2024
# Latest update by: Calum Kennedy
# Latest update on: 31-10-2024
# Update notes: 

# Comments ---------------------------------------------------------------------



#' Download hourly PM\eqn{_{2.5}} air quality data from UK monitoring networks
#'
#' @description
#' Uses \code{openair::importMeta} and \code{openair::importUKAQ} to download
#' air quality monitoring data from one or more UK national networks (AURN, AQE,
#' WAQN) for a specified set of years and pollutants. Monitoring stations in
#' Scotland and Northern Ireland are excluded. Date-derived variables (day of week,
#' month, and hour) are appended to facilitate temporal filtering in downstream
#' analyses.
#'
#' @param source_list A character vector of monitoring network identifiers to
#'   include. Supported values are \code{"aurn"} (Automatic Urban and Rural Network),
#'   \code{"aqe"} (Air Quality England), and \code{"waqn"} (Welsh Air Quality
#'   Network). Multiple networks can be specified and their data will be combined.
#' @param year_list A numeric vector of calendar years for which to download data
#'   (e.g. \code{2022:2024}).
#' @param frequency A character string specifying the temporal resolution of the
#'   data. Passed to the \code{data_type} argument of \code{importUKAQ}. Defaults
#'   to \code{"hourly"}. Other supported values include \code{"daily"} and
#'   \code{"monthly"}.
#' @param pollutant_list A character vector of pollutant codes to download. Defaults
#'   to \code{"all"}. To restrict to PM\eqn{_{2.5}} only, pass \code{c("pm2.5")}.
#'   Pollutant codes follow the \code{openair} naming convention.
#'
#' @return A data frame with one row per monitoring site per time step, containing
#'   all columns returned by \code{importUKAQ} (including \code{date}, \code{code},
#'   \code{site}, \code{site_type}, \code{source}, \code{longitude}, \code{latitude},
#'   and the requested pollutant columns) plus three derived columns:
#'   \describe{
#'     \item{\code{day}}{Character. Full English name of the day of the week
#'       (e.g. \code{"Monday"}).}
#'     \item{\code{month}}{Integer. Calendar month (1--12).}
#'     \item{\code{hour}}{Character. Zero-padded hour of the day as a two-character
#'       string (e.g. \code{"09"}, \code{"23"}). Only present when
#'       \code{frequency = "hourly"}.}
#'   }
#'
#' @details
#' Station metadata are retrieved with \code{importMeta(..., all = TRUE)} and filtered
#' to exclude sites whose \code{zone} field contains \code{"Ireland"} or
#' \code{"Scotland"}. Only distinct site codes are passed to \code{importUKAQ},
#' with the corresponding \code{source} identifier to handle sites registered across
#' multiple networks. Monitor coordinates are included in the output via
#' \code{meta = TRUE}.

# Define function to get openair data ------------------------------------------

get_openair_data <- function(source_list,
                             year_list,
                             frequency = "hourly",
                             pollutant_list = "all"){
  
  # Get vector of site codes to pass to 'importUKAQ'
  sites <- importMeta(source = source_list,
                      all = TRUE) %>%
    
    # Filter out Scottish/Irish monitoring stations
    filter(!str_detect(zone, paste0(c("Ireland", "Scotland"), collapse = "|"))) %>%
    
    # Keep distinct codes
    distinct(code, .keep_all = TRUE)
  
  # Get openair data
  openair_data <- importUKAQ(site = sites$code,
                             year = year_list,
                             data_type = frequency,
                             pollutant = pollutant_list,
                             meta = TRUE,
                             source = sites$source) %>%
    
    # Create new variable for day of week and month
    mutate(day = weekdays(date),
           month = lubridate::month(date))
  
  # If frequency is 'hourly' - create new column called 'hour' for hour of day
  if(frequency == "hourly") openair_data <- openair_data %>% mutate(hour = format(date, "%H"))
  
  return(openair_data)

} 
