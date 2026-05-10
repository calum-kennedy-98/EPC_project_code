# Name of script: MakeGroupedScatterPlot
# Description:  Defines function to produce scatter plot of key variables by grouping variables
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 02-09-2024
# Latest update by: Calum Kennedy
# Latest update on: 02-09-2024
# Update notes: Updated code to modularise into separate scripts

# Comments ---------------------------------------------------------------------

#' Produce a bubble scatter plot with colour and size aesthetics
#'
#' @description
#' Creates a \code{ggplot2} bubble scatter plot in which point colour encodes a
#' categorical grouping variable (e.g. region) and point size encodes a continuous
#' variable (e.g. population). The global project colour palette (\code{cbbPalette})
#' and scatter plot theme (\code{scatter_plot_opts}) defined in \code{LoadEnv.R}
#' are applied.
#'
#' @param data A data frame containing the variables to be plotted.
#' @param x_var An unquoted column name in \code{data} for the x-axis variable.
#' @param y_var An unquoted column name in \code{data} for the y-axis variable.
#' @param group_var An unquoted column name in \code{data} used to define point
#'   groupings. Currently unused in the \code{aes} mapping (size and colour are
#'   controlled by \code{size_var} and \code{colour_var}), but retained for
#'   compatibility with downstream filtering.
#' @param colour_var An unquoted column name in \code{data} for the categorical
#'   variable to map to point colour (e.g. region name).
#' @param size_var An unquoted column name in \code{data} for the continuous
#'   variable to map to point size (e.g. \code{num_people}).
#' @param legend_position A character string specifying where to place the legend
#'   (e.g. \code{"bottom"}, \code{"right"}, \code{"none"}).
#'
#' @return A \code{ggplot} object. Axis labels, titles, and additional guide
#'   customisation should be added via \code{labs}, \code{guides}, and
#'   \code{ggtitle} after the function call.
#'
#' @details
#' Point transparency is fixed at \code{alpha = 0.7}. Size scale labels use
#' \code{scales::label_comma} to format large population values. Colour legend
#' override sets point size to 2 to make the colour legend readable regardless of
#' the data-driven size mapping.

# Define function to produce scatter plot by group variable --------------------

make_grouped_scatter_plot <- function(data, 
                                      x_var, 
                                      y_var, 
                                      group_var, 
                                      colour_var, 
                                      size_var,
                                      legend_position){
  
  scatter_plot <- data %>%
    
    # Call ggplot
    ggplot() +
    
    # Add scatter points
    geom_point(aes(x = {{x_var}},
                   y = {{y_var}},
                   colour = {{colour_var}},
                   size = {{size_var}}),
               alpha = 0.7) +
    
    scale_size_continuous(labels = label_comma()) +
    
    scale_colour_manual(values = cbbPalette) +
    
    scatter_plot_opts +
    
    guides(colour = guide_legend(override.aes = list(size = 2))) +
    
    theme(legend.position = legend_position)
  
  return(scatter_plot)
  
}