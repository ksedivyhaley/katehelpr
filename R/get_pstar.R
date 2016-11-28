get_pstar <- function(pval) {

  if (!is.numeric(pval)){
    stop("p-value must be numeric. Value of class ", class(pval), " supplied.")
  }
  if (pval > 1 | pval < 0){
    stop("p-value must be between 0 and 1. Value ", pval, " supplied.")
  }

  breaks <- c(0, 0.001, 0.01, 0.05, 1)
  labels <- c("***", "**", "*", "")
  cut(pval, breaks, labels)
}
