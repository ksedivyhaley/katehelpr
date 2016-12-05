#' katehelpr: Tools for Analysis of Two-Variable Data
#'
#' This package can be used to analyse any data in which observations are
#' described by a primary and secondary categorical variable, and a
#' numeric measurement or "response". Within groups defined by the secondary
#' variable, it compares observations with different primary variables to a
#' reference group. For example, the package could be used to compare mouse weights
#' (response) when mice are fed different diets (secondary variable), comparing
#' mutant mice to wild type (primary variable with reference level).
#'
#' Included functions:
#'
#' - tidy_pol(), which tidies data from a specific human-friendly format
#'
#' - analyse_pol(), which calculates mean & standard deviation of groups and
#' performs t-tests.
#'
#' - get_pstar(), which converts numeric p-values to star symbols for graphing
#'
#' - plot_pol(), which graphs data in a format produced by analyse_pol()
#'
#' tidy_pol(), analyse_pol(), and plot_pol() can be used in a pipe to go from
#' untidy data to a finished plot.
#'
#' The tools were originally developed for studies on macrophage polarization,
#' in which a polarizing prestimulation is followed by a secondary stimulation,
#' and the effect of polarization on response to the secondary stimulation is of
#' interest.
#'
#'
#' @name katehelpr
#' @docType package
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  {
  utils::globalVariables(c("data", "data.x", "data.y", "p.star",
                           "p.value", "primary", "ref_data", "sd",
                           "secondary", "stim", "t.test", "test",
                           "tidy"))
}
