analyse_pol <- function(df, na.rm=TRUE) {

  if(!is.data.frame(df)){
    stop(paste(c("df must be a data frame for analysis. Class", class(df),
                 "supplied."), collapse=" "))
  }

  #check that the data frame has necessary columns for downstream analysis
  cols_needed <- c("polarization", "secondary")
  cols_missing <- !(cols_needed %in% colnames(df))
  if(sum(cols_missing) > 0){
    stop(paste(c("df is missing column(s)",
                 cols_needed[cols_missing]), collapse=" "))
  }

  nested_data <- df %>%
    group_by(polarization, secondary) %>%
    nest()

  #This allows me to join a copy of this ctrl data to my main tibble
  ctrl_pol <- nested_data %>%
    filter(polarization=="Ctrl") %>%
    select(-polarization) #Redundant - it's all Ctrl

  analysed_data <- nested_data %>%
    inner_join(ctrl_pol, by="secondary") %>%
    rename(data = data.x,
           ctrl_data = data.y) %>%  # refer to in t.test()
    mutate(data = map(data, `[[`, 1), # produces vectors for t.test()
           ctrl_data = map(ctrl_data, `[[`, 1),
           mean = map_dbl(data, mean, na.rm = na.rm),
           sd = map_dbl(data, sd, na.rm = na.rm),
           test = map2(data, ctrl_data, t.test),
           tidy = map(test, tidy)) %>%
    unnest(tidy) %>%
    select(polarization, secondary, mean, sd, p.value)
}
