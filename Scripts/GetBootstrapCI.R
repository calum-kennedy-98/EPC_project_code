# Name of script: GetBootstrapCI
# Description:  Defines function to compute a bootstrap confidence interval for an
#               arbitrary statistic (typically a correlation coefficient) stratified
#               by season
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 21-01-2025
# Latest update by: Calum Kennedy
# Latest update on: 21-01-2025

#' Compute a bootstrap confidence interval for a correlation coefficient within a season
#'
#' @description
#' Filters a data frame to a single season and uses \code{boot::boot} and
#' \code{boot::boot.ci} to compute a bootstrap confidence interval for a user-supplied
#' statistic function (typically \code{get_corr}). The random seed is fixed at 123 to
#' ensure reproducibility across pipeline runs.
#'
#' @param data A data frame containing columns \code{x_var}, \code{y_var}, and
#'   \code{season}. Typically the site-level seasonal summary output from
#'   \code{make_scatter_plot_openair}.
#' @param x_var A character string giving the name of the column to use as the
#'   predictor variable in the correlation.
#' @param y_var A character string giving the name of the column to use as the
#'   response variable in the correlation.
#' @param season A character string specifying the season to subset. Must match a
#'   value in the \code{season} column of \code{data} (e.g. \code{"Winter"} or
#'   \code{"Summer"}).
#' @param boot_func A function to compute the statistic of interest from a bootstrap
#'   sample. Must accept arguments \code{data}, \code{idx}, \code{x_var},
#'   \code{y_var}, and \code{correlation_method} (e.g. \code{get_corr}).
#' @param n_rep A positive integer specifying the number of bootstrap replicates
#'   (e.g. \code{10000}).
#' @param conf_int A numeric scalar in (0, 1) giving the confidence level
#'   (e.g. \code{0.95} for a 95\% interval).
#' @param correlation_method A character string specifying the correlation method
#'   passed to \code{cor}. One of \code{"pearson"}, \code{"spearman"}, or
#'   \code{"kendall"}.
#' @param bootstrap_method A character string specifying the bootstrap confidence
#'   interval type passed to \code{boot.ci}. One of \code{"norm"}, \code{"basic"},
#'   \code{"perc"}, or \code{"bca"}. The BCa (\code{"bca"}) method is recommended
#'   as it is bias-corrected and acceleration-adjusted.
#'
#' @return A named numeric vector with three elements:
#'   \describe{
#'     \item{\code{correlation_coefficient}}{The central estimate (original sample
#'       statistic), rounded to 2 decimal places.}
#'     \item{\code{lower_bound}}{The lower confidence bound, rounded to 2 decimal
#'       places.}
#'     \item{\code{upper_bound}}{The upper confidence bound, rounded to 2 decimal
#'       places.}
#'   }
#'
#' @details
#' The lower and upper bounds are extracted from the penultimate and final elements
#' of the fourth list element of the \code{boot.ci} output, which is the standard
#' position for the BCa interval limits. This indexing is consistent across all
#' \code{boot.ci} methods that return a 5-element vector (normal, basic, percentile,
#' BCa). The seed is set with \code{set.seed(123)} immediately before each call to
#' \code{boot} to guarantee reproducibility.

# Define function to get bootstrap confidence interval -------------------------

get_bootstrap_ci <- function(data, 
                             x_var, 
                             y_var, 
                             season, 
                             boot_func, 
                             n_rep, 
                             conf_int, 
                             correlation_method,
                             bootstrap_method){
  
  # Set seed for reproducibility
  set.seed(123)
  
  # Filter by season
  data_for_boot <- data %>% filter(season == {{season}})
  
  # Get vector of correlation coefficients from bootstrapped samples
  corr_boot <- boot(data_for_boot, 
                    statistic = boot_func, 
                    R = n_rep, 
                    x_var = x_var, 
                    y_var = y_var,
                    correlation_method = correlation_method) 
  
  # Get 95% CI using method specified in 'method'
  corr_boot_ci <- boot.ci(corr_boot, 
                          conf = conf_int, 
                          type = bootstrap_method)
  
  # Get central estimate for correlation coefficient
  correlation_coefficient <- round(corr_boot_ci$t0, digits = 2)
  
  # Get lower confidence band ((length - 1)th element of vector)
  lower_bound <- round(corr_boot_ci[[4]][length(corr_boot_ci[[4]]) - 1], digits = 2)
                      
  # Get upper confidence band ((length)th element of vector)
  upper_bound <- round(corr_boot_ci[[4]][length(corr_boot_ci[[4]])], digits = 2)
  
  # Combine into single vector
  output <- c("correlation_coefficient" = correlation_coefficient,
                "lower_bound" = lower_bound,
                "upper_bound" = upper_bound)
  
  # Return confidence interval
  return(output) 

}