
# 15 min timestep
# subset data based on site/DTs in flag_values 
# add flag codes and notes to df
apply_flag_15min <- function(site, FLAG, start_dt, end_dt, notes) {
  
  correct_and_flagged_df <- corrected_df %>%
    dplyr::filter(corrected_df$site_code %in% site) %>%
    dplyr::filter(between(DT, start_dt, end_dt)) %>%
    mutate(flag_type = FLAG, 
           note = notes)
  
  return(correct_and_flagged_df)
  
}

# daily timestep
# change apply_flag to work on daily timestep rather than by DT
apply_flag_daily <- function(site, FLAG, start_date, end_date, notes) {
  
  correct_and_flagged_daily_df <- daily_means_stage %>%
    dplyr::filter(date >= start_date & date < end_date) %>%
    dplyr::filter(site_code == site) %>%
    mutate(flag_type = FLAG, 
           note = notes)
  
  return(correct_and_flagged_daily_df)
  
}