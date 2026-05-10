# Name of script: MakePatchworkPlot
# Description:  Defines function to combine an arbitrary list of ggplot objects into
#               a single patchwork figure
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 02-10-2024
# Latest update by: Calum Kennedy
# Latest update on: 02-10-2024

#' Combine a list of ggplot objects into a patchwork figure
#'
#' @description
#' Uses \code{patchwork::plot_layout} to assemble an arbitrary list of \code{ggplot}
#' objects into a single composite figure. The list is collapsed using \code{Reduce}
#' with the \code{+} operator, enabling the function to handle any number of panels
#' without requiring them to be specified individually.
#'
#' @param list A list of \code{ggplot} objects to combine. The order of elements
#'   determines the panel order in the output figure.
#' @param legend_position A character string specifying the legend position applied
#'   to all panels via the \code{&} operator (e.g. \code{"bottom"}, \code{"right"},
#'   \code{"none"}). Defaults to \code{NULL}, which leaves each panel's legend position
#'   unchanged.
#' @param ... Additional arguments passed to \code{patchwork::plot_layout}, such as
#'   \code{ncol}, \code{nrow}, \code{guides} (e.g. \code{"collect"} or \code{"keep"}),
#'   and \code{widths}.
#'
#' @return A \code{patchwork} object that can be saved with \code{ggsave} or further
#'   modified with \code{patchwork} operators (\code{+}, \code{/}, \code{&}).
#'
#' @details
#' Setting \code{guides = "collect"} in \code{...} merges duplicate legends across
#' panels, while \code{guides = "keep"} preserves each panel's legend independently.
#' When panels have asymmetric colour scales (e.g. maps with different fill ranges),
#' \code{"keep"} should be used.

# Define function to produce patchwork of ggplot objects using 'reduce' --------

make_patchwork_plot <- function(list, 
                                legend_position = NULL,
                                ...){
  
  # Make patchwork object by applying `+` operator to list of objects
  patchwork <- Reduce(`+`, list) +
    
    # Set plot layout
    plot_layout(...) &
    
    # Optional legend position
    theme(legend.position = legend_position,
          legend.box = "vertical")
  
  return(patchwork)
  
}