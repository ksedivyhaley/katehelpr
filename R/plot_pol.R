#' Plot Bar Graph with Statistical Summaries
#'
#' Produces a bar graph with the primary variable being represented as facets
#' and the secondary variable represented by position along the x-axis and colour.
#' Group mean is plotted, along with error bars representing the standard deviation
#' and an optional label indicating statistical significance (numeric p-value
#' or stars, see get_pstar). Axis labels may be customised to represent
#' the specific data used.
#'
#' @param df data.frame: Requires columns "primary", "secondary", "mean",
#' and "sd", as well as an optional p.value or p.star column if labels used.
#' @param xlab string: x axis label, name of the type of observation of the
#' primary variable (eg mutation)
#' @param ylab string: y axis label, name of response measured
#' @param label string or NULL: p.value and p.star values are currently supported.
#' If only a p.value can be found, p.star can be derived within the function. If
#' an unsupported label type is given, no label will be used and a warning message
#' will be produced.
#'
#' @return plot
#' @export
#'
#' @examples
#' df <- tibble::tibble(
#'   'primary' = c("Ctrl", "M1", "Ctrl", "M1"),
#'   'secondary'= c("Ctrl", "Ctrl", "M1", "M1"),
#'   'mean' = c(2, 5, 5, 12),
#'   'sd' = c(1, 1, 1.414, 1),
#'   'p.value' = c(1, 0.21, 1, 0.04)
#' )
#' plot_pol(df)
#' plot_pol(df, ylab="Inflammation", label="p.value")
#' plot_pol(df, ylab="Inflammation", label="p.star")
#' plot_pol(df, ylab="Inflammation", label="sd") #produces warning, no label
plot_pol <- function(df, xlab = "Polarization",
                     ylab="Response", label=NULL) {

  # Error Checking ----------------------------------------------------------

  if(!is.data.frame(df)){
    stop(paste(c("df must be a data frame for analysis. Class", class(df),
                 "supplied."), collapse=" "))
  }

  #check to see if any required columns are missing
  cols_needed <- c("primary", "secondary", "mean", "sd")
  cols_missing <- !(cols_needed %in% colnames(df))
  if(sum(cols_missing) > 0){
    stop(paste(c("df is missing column(s)", cols_needed[cols_missing]), collapse=" "))
  }

  if(!is.character(xlab) | !length(xlab)==1){
    stop(paste(c("xlab must be a length 1 character vector. Length",
                 length(xlab), class(xlab), "supplied."), collapse=" "))
  }

  if(!is.character(ylab) | !length(ylab)==1){
    stop(paste(c("ylab must be a length 1 character vector. Length",
                 length(ylab), class(ylab), "supplied."), collapse=" "))
  }

  #if the label is null, it's effectively 'present' in df - otherwise check.
  ifelse(is.null(label), label_present <- TRUE,
         label_present <- (label %in% colnames(df)))

  # Plotting ----------------------------------------------------------------

  #check if necessary & possible to derive p.star before begin plotting
  #otherwise can't add p.star to the data for ggplot
  if(label=="p.star" && !label_present && ("p.value" %in% colnames(df))){
    df <- df %>%
      dplyr::mutate(p.star = get_pstar(p.value))
    label_present <- (label %in% colnames(df)) #should now be present
  }

  #save the graph, without labels
  p <- ggplot2::ggplot(df, ggplot2::aes(x=primary, y=mean, fill=primary)) +
    ggplot2::facet_wrap(~secondary) +
    ggplot2::geom_bar(stat="identity") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin=mean-sd, ymax=mean+sd),
                           width=.2,
                           position=ggplot2::position_dodge(.9)) +
    ggplot2::labs(x = xlab, y = ylab) +
    ggplot2::theme(legend.position="none")  #redundant with x-axis label

  #add the label afterwards to avoid repeating main graph in conditional statements
  above_error_bar <- 2*max(df$sd)
  if(is.null(label)){
    p
  } else if(label=="p.value" && label_present){
    p +
      ggplot2::geom_text(ggplot2::aes(label=paste0("p=", round(p.value, 4))),
                         nudge_y = above_error_bar)
  } else if(label=="p.star" && label_present){
    p +
      ggplot2::geom_text(ggplot2::aes(label=p.star), nudge_y = above_error_bar)
  } else {
    warning("Unrecognized or missing label ", label)
    p
  } #this is my gentle failure

}
