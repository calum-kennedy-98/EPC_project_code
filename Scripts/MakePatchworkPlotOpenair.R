# Name of script: MakePatchworkPlotOpenair
# Description:  Defines function to produce a three-panel patchwork of openair
#               scatter plots showing average PM2.5, weekday peak-minus-non-peak
#               PM2.5 difference, and weekend peak-minus-non-peak PM2.5 difference
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 15-01-2025
# Latest update by: Calum Kennedy
# Latest update on: 20-01-2025

#' Produce a three-panel patchwork of seasonal PM\eqn{_{2.5}} scatter plots for openair data
#'
#' @description
#' Calls \code{make_scatter_plot_openair} three times with different \code{y_var} and
#' \code{days} arguments to produce a standardised three-panel figure: (A) average
#' PM\eqn{_{2.5}} across all days; (B) peak-minus-non-peak PM\eqn{_{2.5}} difference
#' on weekdays; (C) peak-minus-non-peak PM\eqn{_{2.5}} difference on weekends. The
#' three panels are combined into a single \code{patchwork} figure with shared legends
#' and collected axis titles.
#'
#' @param data_openair A data frame of merged monitoring and EPC records, produced by
#'   \code{merge_openair_epc_data} or \code{make_openair_epc_laei_data}.
#' @param x_var An unquoted column name in \code{data_openair} for the x-axis predictor
#'   variable (e.g. \code{log_n_wf}, \code{pm_25_emissions}, \code{log_n}).
#' @param pm2.5_var An unquoted column name containing mean PM\eqn{_{2.5}} values
#'   (typically \code{pm2.5}), used as the y-axis variable in panel A.
#' @param pm2.5_diff_peak_var An unquoted column name containing the daily
#'   peak-minus-non-peak PM\eqn{_{2.5}} difference (typically \code{pm2.5_diff_peak}),
#'   used as the y-axis variable in panels B and C.
#' @param x_lab A character string or \code{bquote}/\code{expression} object used as
#'   the x-axis label for all three panels.
#' @param site_type A character string passed to \code{make_scatter_plot_openair}
#'   for filtering monitoring stations by site type (e.g. \code{"Urban Background"}).
#' @param source_list A character vector of monitoring network codes to include.
#'   Passed to \code{make_scatter_plot_openair} (e.g. \code{c("aurn", "aqe")}).
#' @param correlation_method A character string specifying the correlation method.
#'   Passed to \code{make_scatter_plot_openair} (typically \code{"spearman"}).
#' @param bootstrap_method A character string specifying the bootstrap CI method.
#'   Passed to \code{make_scatter_plot_openair} (typically \code{"bca"}).
#' @param n_rep A positive integer giving the number of bootstrap replicates.
#' @param conf_int A numeric scalar in (0, 1) giving the confidence level.
#'
#' @return A \code{patchwork} object with three panels arranged in a single row
#'   (\code{ncol = 3}), with a shared legend at the bottom and collected y-axis
#'   titles. The object can be passed directly to \code{ggsave}.
#'
#' @details
#' Panel A uses all seven days of the week; panel B uses Monday--Friday only; panel C
#' uses Saturday--Sunday only. This split isolates weekend burning behaviour (typically
#' recreational) from weekday patterns. The \code{&} operator applies
#' \code{scatter_plot_opts} and shared theme settings (legend position, y-axis angle,
#' legend text size) uniformly across all three panels.

# Define function to make patchwork plot calling the 'make_scatter_plot_openair'
# function with merged openair data --------------------------------------------

make_patchwork_plot_openair <- function(data_openair,
                                        x_var,
                                        pm2.5_var,
                                        pm2.5_diff_peak_var,
                                        x_lab,
                                        site_type,
                                        source_list,
                                        correlation_method,
                                        bootstrap_method,
                                        n_rep,
                                        conf_int){
  
  # make scatter plot of x_var against average PM2.5 levels in summer and winter
  plot_avg_pm2.5 <- make_scatter_plot_openair(data_openair,
                                              x_var = {{x_var}},
                                              y_var = {{pm2.5_var}},
                                              days = c("Monday",
                                                       "Tuesday",
                                                       "Wednesday",
                                                       "Thursday",
                                                       "Friday",
                                                       "Saturday",
                                                       "Sunday"),
                                              {{site_type}},
                                              {{source_list}},
                                              correlation_method = correlation_method,
                                              bootstrap_method = bootstrap_method,
                                              n_rep = n_rep,
                                              conf_int = conf_int) +
    
    ggtitle("Average") +
    
    labs(x = x_lab,
         y = expression("Average PM"["2.5"]))
  
  # Make scatter plot of x_var against difference between peak vs. non-peak PM2.5 on weekdays
  plot_diff_peak_pm2.5_weekdays <- make_scatter_plot_openair(data_openair,
                                                             x_var = {{x_var}},
                                                             y_var = {{pm2.5_diff_peak_var}},
                                                             days = c("Monday",
                                                                      "Tuesday",
                                                                      "Wednesday",
                                                                      "Thursday",
                                                                      "Friday"),
                                                             {{site_type}},
                                                             {{source_list}},
                                                             correlation_method = correlation_method,
                                                             bootstrap_method = bootstrap_method,
                                                             n_rep = n_rep,
                                                             conf_int = conf_int) +
    
    ggtitle("Weekday") +
    
    labs(x = x_lab,
         y = expression("Peak PM"[2.5] - "non-peak PM"[2.5]))
  
  # Make scatter plot of x_var against difference between peak vs. non-peak PM2.5 on weekdays
  plot_diff_peak_pm2.5_weekends <- make_scatter_plot_openair(data_openair,
                                                             x_var = {{x_var}},
                                                             y_var = {{pm2.5_diff_peak_var}},
                                                             days = c("Saturday",
                                                                      "Sunday"),
                                                             {{site_type}},
                                                             {{source_list}},
                                                             correlation_method = correlation_method,
                                                             bootstrap_method = bootstrap_method,
                                                             n_rep = n_rep,
                                                             conf_int = conf_int) +
    
    ggtitle("Weekend") +
    
    labs(x = x_lab,
         y = expression("Peak PM"[2.5] - "non-peak PM"[2.5]))
  
  # Compile into patchwork plot
  patchwork_plot_openair <- plot_avg_pm2.5 + 
    plot_diff_peak_pm2.5_weekdays + 
    plot_diff_peak_pm2.5_weekends +
    
    # Set plot layout options
    plot_layout(ncol = 3,
                guides = "collect",
                axis_titles = "collect") &
    
    scatter_plot_opts &
    
    theme(legend.position = "bottom",
          axis.title.y=element_text(angle=90),
          legend.title = element_blank(),
          legend.text = element_text(size = 12))
  
}