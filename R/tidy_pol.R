#' Tidy Polarization Data
#'
#' Takes data frame where each column represents a treatment group
#' with the first row being treatment names in form
#' polarization-secondary (eg 'Ctrl-LPS') and subsequent rows
#' containing individual observations. Returns tidied tibble
#' with columns polarization <chr>, secondary <chr> and
#' response <int>
#'
#' @param data data frame
#'
#' @return tibble
#' @export
#' @importFrom magrittr %>%
#'
#' @examples
#' data <- tibble::tibble(
#' 'Ctrl-Ctrl' = 1:3,
#' 'M1-Ctrl'= 1:3,
#' 'Ctrl-LPS' = 4:6,
#' 'M1-LPS' = 11:13
#' )
#' tidy_pol(data)
tidy_pol <- function(data) {

  #testing for numeric data - fails downstream otherise
  numeric_cols <- sum(sapply(data, is.numeric))
  if (numeric_cols < ncol(data)){
    stop("Input data must be numeric. ", ncol(data)-numeric_cols,
         " column(s) are not numeric.")
  }

  #test that column names have expected separator character
  names_no_sep <- !sapply(colnames(data), function(name) grepl("-", name))
  if(sum(names_no_sep) != 0){
    stop("Column names must contain separator '-'; column(s) ",
         which(names_no_sep),
         "'", names(names_no_sep)[names_no_sep], "'",
         " do/does not.")
  }

  data %>%
    tidyr::gather(key = "stim", value = "response") %>%
    tidyr::separate(stim, into=c("polarization", "secondary"), sep="-")
}
