#' Analyze Primary-Secondary Data
#'
#' Takes a data.frame with observations from multiple primary-secondary
#' conditions and calculates the mean and standard deviation for each
#' primary-secondary group. Also performs a t.test on each group relative
#' to the reference primary group with the same secondary stimulation.
#' Result is suitable for use with plot_pol().
#'
#' @param df data.frame: should have columns 'primary', 'secondary', and 'response'
#' @param reference string: Name of the reference level within the 'primary'
#' variable/column. Default is "Ctrl"
#' @param na.rm logical: indicates how mean() and sd() will treat NAs downstream
#'
#' @return tibble with columns primary, secondary, mean, sd, and p.value.
#' @export
#'
#' @examples
#' df <- tibble::tibble(
#'   'primary' = rep(rep(c("Ctrl", "M1"), each=3), 2),
#'   'secondary'= rep(c("Ctrl", "LPS"), each=6),
#'   'response' = c(1, 2, 3, 4, 5, 6, 4, NA, 6, 11, 12, 13)
#' )
#' analyse_pol(df)
#' analyse_pol(df, na.rm = FALSE)
#'
#' df2 <- tibble::tibble(
#'   'primary' = rep(rep(c("WT", "Mutant"), each=3), 2),
#'   'secondary'= rep(c("Diet1", "Diet2"), each=6),
#'   'response' = c(1, 2, 3, 4, 5, 6, 4, 5, 6, 11, 12, 13)
#' )
#' analyse_pol(df2, reference = "WT")
analyse_pol <- function(df, reference = "Ctrl", na.rm = TRUE) {

  if(!is.data.frame(df)){
    stop(paste(c("df must be a data frame for analysis. Class", class(df),
                 "supplied."), collapse=" "))
  }

  #check that the data frame has necessary columns for downstream analysis
  cols_needed <- c("primary", "secondary")
  cols_missing <- !(cols_needed %in% colnames(df))
  if(sum(cols_missing) > 0){
    stop(paste(c("df is missing column(s)",
                 cols_needed[cols_missing]), collapse=" "))
  }

  if(!is.character(reference) | !length(reference)==1){
    stop(paste(c("reference level must be a length 1 character vector. Length",
                 length(reference), class(reference), "supplied."), collapse=" "))
  }

  if(!(reference %in% df$primary)){
    stop("reference level must occur at least once in primary.")
  }

  nested_data <- df %>%
    dplyr::group_by(primary, secondary) %>%
    tidyr::nest()

  #This allows me to join a copy of this ctrl data to my main tibble
  ref_pol <- nested_data %>%
    dplyr::filter(primary==reference) %>%
    dplyr::select(-primary) #Redundant - it's all ref

  nested_data %>%
    dplyr::inner_join(ref_pol, by="secondary") %>%
    dplyr::rename(data = data.x,
                  ref_data = data.y) %>%  # refer to in t.test()
    dplyr::mutate(data = purrr::map(data, `[[`, 1), # produces vectors for t.test()
                  ref_data = purrr::map(ref_data, `[[`, 1),
                  mean = purrr::map_dbl(data, mean, na.rm = na.rm),
                  sd = purrr::map_dbl(data, sd, na.rm = na.rm),
                  test = purrr::map2(data, ref_data, t.test),
                  tidy = purrr::map(test, broom::tidy)) %>%
  tidyr::unnest(tidy) %>%
  dplyr::select(primary, secondary, mean, sd, p.value)

}
