
# 15 min timestep
# subset data based on site/DTs in flag_values 
# add flag codes and notes to df
apply_flag_15min <- function(site, FLAG, start_dt, end_dt, notes) {
  
  correct_and_flagged_df <- corrected_df %>%
    dplyr::filter(corrected_df$site_code %in% site) %>%
    mutate(flag_type = case_when(between(DT, start_dt, end_dt) ~ FLAG, 
                                   TRUE ~ "PASS"), 
           note = case_when(between(DT, start_dt, end_dt) ~ notes, 
                               TRUE ~ NA))
  
  return(correct_and_flagged_df)
  
}

# daily timestep
# change apply_flag to work on daily timestep rather than by DT
apply_flag_daily <- function(site, FLAG, start_date, end_date, notes) {
  
  flagged_daily_df <- daily_means_stage %>%
    dplyr::filter(site_code == site) %>%
    mutate(flag_type = case_when( (date >= start_date & date < end_date) ~ FLAG, 
                                 TRUE ~ "PASS"), 
           note = case_when((date >= start_date & date < end_date) ~ notes, 
                            TRUE ~ NA))
  
  return(flagged_daily_df)
  
}
