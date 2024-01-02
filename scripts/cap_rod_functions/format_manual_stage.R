
format_manual_stage <- function(manual_df){
  #grab all the sites in the dataset
  sites <- unique(manual_df$site_code)
  #we need to make first and last measurements for subsetting later on
  first_last <- function(site){
    mini <- manual_df%>%
      mutate(DT = force_tz(DT, tzone = "MST"))%>%
      #arrange by datetime
      arrange(DT)%>%
      #subset by site
      filter(site_code == site)%>%
      #create first and last measurements with last measurement becoming first measurement for next site visit
      mutate(first_meas_DT = DT, 
             first_meas_cm = H_cm)
    mini<- mini%>%
      mutate(#lead will take the next value in the column and default will set the last value to the last measurement for the season
             last_meas_DT = lead(first_meas_DT, default = (mini$first_meas_DT[nrow(mini)])),#DT + 1, 
             last_meas_cm =  lead(first_meas_cm, default = (mini$first_meas_cm[nrow(mini)]))
      )
  }
  # map over all sites and bind them together
  formatted_df <- map_dfr(sites, first_last)
  #return the formatted df
  return(formatted_df)
}
