# Name of script: MakeChoroplethMap
# Description:  Defines function to produce choropleth map of UK given arbitrary dataset
# and geographical resolution using ggplot2 package
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 23-09-2024
# Latest update by: Calum Kennedy
# Latest update on: 23-09-2024
# Update notes: 

# Comments ---------------------------------------------------------------------

# Defines function to produce choropleth map of UK at arbitrary geographical resolution, 
# given an arbitrary dataset using ggplot2 package

#' Produce a choropleth map of England and Wales at an arbitrary geographical resolution
#'
#' @description
#' Creates a \code{ggplot2} choropleth map using a viridis colour scale, optionally
#' filtering low-sample areas and winsorising extreme values. An optional secondary
#' polygon layer (e.g. Local Authority boundaries) can be drawn on top of the fill
#' layer for reference.
#'
#' @param fill_data An \code{sf} object containing polygon geometries and the variable
#'   to be mapped as a fill colour. Typically produced by \code{prepare_data_to_map}.
#' @param fill_var An unquoted column name in \code{fill_data} to use as the fill
#'   aesthetic (e.g. \code{wood_perc_h_predicted}).
#' @param filter_low_n A logical scalar. If \code{TRUE}, rows with fewer than
#'   \code{n_threshold} observations in \code{n_var} are excluded before plotting.
#'   Defaults to \code{FALSE}.
#' @param n_var An unquoted column name in \code{fill_data} containing the observation
#'   count for each geography (e.g. \code{epc}). Only used when
#'   \code{filter_low_n = TRUE}.
#' @param n_threshold A positive numeric scalar. Geographies with \code{n_var} less
#'   than or equal to this value are excluded. Only used when \code{filter_low_n = TRUE}.
#' @param boundary_data An \code{sf} object with polygon geometries to draw as a
#'   boundary overlay on top of the choropleth fill. Pass \code{NULL} to suppress
#'   the boundary layer.
#' @param fill_palette A character string specifying the viridis palette to use.
#'   One of \code{"viridis"}, \code{"magma"}, \code{"plasma"}, \code{"inferno"},
#'   \code{"cividis"}, \code{"rocket"}, or \code{"turbo"}. Defaults to
#'   \code{"inferno"}.
#' @param scale_lower_lim A numeric scalar specifying the lower limit of the colour
#'   scale. Values below this limit are clipped to the lower limit colour. Pass
#'   \code{NULL} to use the data minimum.
#' @param scale_upper_lim A numeric scalar specifying the upper limit of the colour
#'   scale. Values above this limit are clipped to the upper limit colour. Pass
#'   \code{NULL} to use the data maximum.
#' @param winsorise A logical scalar. If \code{TRUE}, values of \code{fill_var} below
#'   the \code{lower_perc} percentile or above the \code{upper_perc} percentile are
#'   capped at those percentile values before plotting. Defaults to \code{FALSE}.
#' @param lower_perc A numeric scalar in [0, 1] specifying the lower winsorisation
#'   percentile (e.g. \code{0.05} for the 5th percentile). Only used when
#'   \code{winsorise = TRUE}.
#' @param upper_perc A numeric scalar in [0, 1] specifying the upper winsorisation
#'   percentile (e.g. \code{0.95} for the 95th percentile). Only used when
#'   \code{winsorise = TRUE}.
#' @param legend_title A character string or expression to use as the colour bar
#'   title in the legend. Expressions can be produced with \code{expression()} to
#'   include mathematical notation (e.g. \code{expression(km^{2})}).
#' @param legend_position A character string specifying the position of the colour
#'   bar within the plot. Passed to the \code{position} argument of
#'   \code{ggplot2::guide_colourbar}. Typical values are \code{"inside"},
#'   \code{"bottom"}, or \code{"right"}.
#'
#' @return A \code{ggplot} object containing the choropleth map. The map uses
#'   \code{theme_void} with no axis labels or grid lines. The colour scale is
#'   inverted (darker colours correspond to higher values) via \code{direction = -1}.
#'
#' @details
#' Winsorisation is applied to the \code{fill_var} column in a copy of \code{fill_data}
#' before plotting; the underlying data are not modified. This is useful for variables
#' with long-tailed distributions where extreme values would compress the colour scale
#' and obscure geographic variation in the bulk of the data.

# Define map function ----------------------------------------------------------

make_choropleth_map <- function(fill_data,
                                fill_var,
                                filter_low_n = FALSE,
                                n_var = NULL,
                                n_threshold = NULL,
                                boundary_data,
                                fill_palette = "inferno",
                                scale_lower_lim = NULL,
                                scale_upper_lim = NULL,
                                winsorise = FALSE,
                                lower_perc = NULL,
                                upper_perc = NULL,
                                legend_title,
                                legend_position){
  
  # If 'n_var' is specified, filter data to plot based on number of obs higher than 'n_threshold'
  if(filter_low_n) fill_data <- fill_data %>% filter({{n_var}} > n_threshold)
  
  # If 'winsorise' is TRUE, winsorise upper and lower percentiles of fill variable (default is 5th and 95th percentile)
  if(winsorise) fill_data <- mutate(fill_data, "{{fill_var}}" := case_when({{fill_var}} > get_percentile({{fill_var}}, upper_perc) ~ get_percentile({{fill_var}}, upper_perc),
                                                                           {{fill_var}} < get_percentile({{fill_var}}, lower_perc) ~ get_percentile({{fill_var}}, lower_perc),
                                                                           .default = {{fill_var}}))
  
  
  choropleth_map <- ggplot(fill_data
                           ) +
    
    geom_sf(aes(fill = {{fill_var}}),
            colour = NA
            ) +
    
    scale_fill_viridis(option = fill_palette,
                       direction = -1,
                       limits = c(scale_lower_lim,
                                  scale_upper_lim)
    ) +
    
    theme_void() +
    
    theme(legend.title = element_text(size = 10),
          legend.text = element_text(size = 8),
          legend.justification.inside = c(1, 1),
          plot.title = element_text(face = "bold")) +
    
    guides(fill = guide_colourbar(position = legend_position,
                                  title = legend_title))
  
  return(choropleth_map)
  
}
