#' Tidy Primary-Secondary Data
#'
#' Takes a data.frame or tibble in a specific human-friendly format
#' and tidies it into the conventional format suitable for use with
#' analyse_pol()
#'
#' @param data data.frame where each column represents a treatment group,
#' the first row contains treatment names in form primary-secondary
#' (eg 'Ctrl-LPS'), and subsequent rows contain individual observations.
#' @param sep string: separator character, default "-". Uses regular
#' expressions, so be careful! For example, "." must be input as "\\."
#'
#' @return tibble with columns primary <chr>, secondary <chr> and
#' response <int> or <dbl>
#' @export
#' @importFrom magrittr %>%
#'
#' @examples
#' data <- tibble::tibble(
#'   'Ctrl-Ctrl' = 1:3,
#'   'M1-Ctrl'= 4:6,
#'   'Ctrl-LPS' = 4:6,
#'   'M1-LPS' = 11:13
#' )
#' tidy_pol(data)
#'
#' data2 <- tibble::tibble(
#'   'Ctrl.Ctrl' = 1:3,
#'   'M1.Ctrl'= 4:6,
#'   'Ctrl.LPS' = 4:6,
#'   'M1.LPS' = 11:13
#' )
#' tidy_pol(data2, sep="\\.")
tidy_pol <- function(data, sep="-") {

  #testing for numeric data - fails downstream otherise
  numeric_cols <- sum(sapply(data, is.numeric))
  if (numeric_cols < ncol(data)){
    stop("Input data must be numeric. ", ncol(data)-numeric_cols,
         " column(s) are not numeric.")
  }

  #test that column names have expected separator character
  names_no_sep <- !sapply(colnames(data), function(name) grepl(sep, name))
  if(sum(names_no_sep) != 0){
    stop("Column names must contain separator ", sep, "; column(s) ",
         which(names_no_sep),
         "'", names(names_no_sep)[names_no_sep], "'",
         " do/does not.")
  }

  data %>%
    tidyr::gather(key = "stim", value = "response") %>%
    tidyr::separate(stim, into=c("primary", "secondary"), sep=sep)
}
