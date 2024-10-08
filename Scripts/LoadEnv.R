# Name of script: 0_LoadEnv
# Description:  Loads environment for extraction/cleaning of Eenergy Performance Certificate data 
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 12-08-2024
# Latest update by: Calum Kennedy
# Latest update on: 03-09-2024
# Update notes: Updated code to modularise into separate scripts

# Comments ---------------------------------------------------------------------

# Loads environment for analysis of EPC data
# Loads necessary packages
# Defines global options for output plots and figures

# Set global plot options ------------------------------------------------------

scatter_plot_opts <- list(ggthemes::theme_tufte(base_family = "arial"),
                          ggplot2::scale_size(range = c(2, 10)),
                          ggplot2::theme(legend.position = "bottom",
                                legend.box = "vertical"))

line_plot_opts <- list(ggthemes::theme_tufte(base_family = "arial"),
                       ggplot2::scale_size(range = c(2, 10)),
                       ggplot2::theme(legend.position = "bottom",
                                      legend.box = "vertical"),
                       ggplot2::scale_colour_viridis_d(),
                       ggplot2::geom_line(size = 1))