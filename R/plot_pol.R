plot_pol <- function(df, response_type, label=NULL) {

  if(!is.data.frame(df)){
    stop(paste(c("df must be a data frame for analysis. Class", class(df),
                 "supplied."), collapse=" "))
  }

  #check to see if any required columns are missing
  cols_needed <- c("polarization", "secondary", "mean", "sd")
  cols_missing <- !(cols_needed %in% colnames(df))
  if(sum(cols_missing) > 0){
    stop(paste(c("df is missing column(s)", cols_needed[cols_missing]), collapse=" "))
  }

  if(!is.character(response_type) | !length(response_type)==1){
    stop(paste(c("response_type must be a length 1 character vector. Length",
                 length(response_type), class(response_type), "supplied."), collapse=" "))
  }

  #if the label is null, it's effectively 'present' in df - otherwise check.
  ifelse(is.null(label), label_present <- TRUE,
         label_present <- (label %in% colnames(df)))

  #check if necessary & possible to derive p.star before begin plotting
  #otherwise can't add p.star to the data for ggplot
  if(label=="p.star" && !label_present && ("p.value" %in% colnames(df))){
    df <- df %>%
      mutate(p.star = sapply(p.value, get_pstar))
    label_present <- (label %in% colnames(df)) #should now be present
  }

  #save the graph, without labels
  p <- ggplot(df, aes(x=polarization, y=mean, fill=polarization)) +
    facet_wrap(~secondary) +
    geom_bar(stat="identity") +
    geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd),
                  width=.2,
                  position=position_dodge(.9)) +
    labs(x = "Polarization", y = response_type) +
    theme(legend.position="none")  #redundant with x-axis label

  #add the label afterwards to avoid repeating main graph in conditional statements
  above_error_bar <- 2*max(df$sd)
  if(is.null(label)){
    p
  } else if(label=="p.value" && label_present){
    p +
      geom_text(aes(label=paste0("p=", round(p.value, 4))), nudge_y = above_error_bar)
  } else if(label=="p.star" && label_present){
    p +
      geom_text(aes(label=p.star), nudge_y = above_error_bar)
  } else {
    warning("Unrecognized or missing label ", label)
    p
  } #this is my gentle failure

}
