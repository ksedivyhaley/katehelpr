#' Translate p-value to Star Symbols
#'
#' Translates a numeric p-value into a significance symbol according to
#' p > 0.05          : ""
#' 0.05 <= p < 0.01  : "*"
#' 0.01 <= p < 0.001 : "**"
#' 0.001 <= p        : "***"
#'
#' @param pval double, or vector of doubles: between 0 and 1
#'
#' @return factor: "" "*" "**" or "***"
#' @export
#'
#' @examples
#' get_pstar(0.8)
#' get_pstar(0.05)
#' get_pstar(0.00002)
#' get_pstar(c(0.8, 0.05, 0.0002))
get_pstar <- function(pval) {

  if (!is.numeric(pval)){
    stop("p-value must be numeric. Value of class ", class(pval), " supplied.")
  }
  sapply(pval, function(p){
  if (p > 1 | p < 0){
    stop("p-value must be between 0 and 1. Value ", p, " supplied.")
  }
  }
  )

  breaks <- c(0, 0.001, 0.01, 0.05, 1)
  labels <- c("***", "**", "*", "")
  cut(pval, breaks, labels)
}
