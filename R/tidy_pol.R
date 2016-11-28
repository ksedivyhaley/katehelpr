tidy_pol <- function(file) {

  data <- readr::read_csv(file)

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
