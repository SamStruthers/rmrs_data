

#15 min timeseries data Q calculations
calc_Q_for_sensor_stage_15min <- function(site) {
  
  site_rc <- rating_curve_coeffs %>%
    dplyr::filter(site_code == site)
  
  # filter corrected stage df for site
  stage_Q_df <- final_stage_df %>%
    dplyr::filter(site_code == site) %>%
    # grab a, b, c and sigma for the given site from rc summary
    mutate(a = site_rc$a[1],
           b = site_rc$b[1],
           r2 = site_rc$r2[1],
           year = year(date),
           # calculate Q for the site based on a, b, c and stage given
           Q_cfs = case_when(
             #IF there is no rating curve (< 3 measurements), do not calculate Q
             is.na(a) ~ NA, 
             # if the stage is below the lowest stage where Q was measured, 
             # make it the lowest Q value measured; extrapolation with this 
             # few field Q's is unwise
             !is.na(corrected_stage_cm) & corrected_stage_cm <= min(site_rc$H_cm) ~ min(site_rc$q_cfs), 
             # similar idea to above but with max stage measured
             !is.na(corrected_stage_cm) & corrected_stage_cm >= max(site_rc$H_cm) ~ max(site_rc$q_cfs),
             # if within the bounds of measured stage values, use a & b to calc Q
             !is.na(corrected_stage_cm) ~ (a*(corrected_stage_cm)^b),
             # if the stage is below the lowest stage where Q was measured, 
             # make it the lowest Q value measured; extrapolation with this 
             # few field Q's is unwise
             is.na(corrected_stage_cm) & sensor_stage_cm <= min(site_rc$H_cm) ~ min(site_rc$q_cfs), 
             # similar idea to above but with max stage measured
             is.na(corrected_stage_cm) & sensor_stage_cm >= max(site_rc$H_cm) ~ max(site_rc$q_cfs),
             # if within the bounds of measured stage values, use a & b to calc Q 
             is.na(corrected_stage_cm) ~ (a*(sensor_stage_cm)^b),
             # Default case when none of the above conditions are met
             TRUE ~ NA_real_), 
           # some rating curves are not robust and thus will extrapolate beyond measured Q for stages lower than the max Q
           # this corrects for that by setting the Q to the max Q if the calculated Q is greater than the max Q
           Q_cfs = case_when(Q_cfs > max(site_rc$q_cfs) ~ max(site_rc$q_cfs), 
                             TRUE ~ Q_cfs),
           # calculate Q for the site based on a, b, c and stage given
           extrapolated_Q_cfs = case_when(!is.na(corrected_stage_cm) ~ (a*(corrected_stage_cm)^b),
                                          is.na(corrected_stage_cm) ~ (a*(sensor_stage_cm)^b)), 
           #Add a flag if Q was calculated from sensor stage rather than corrected stage
           q_flag = case_when(is.na(corrected_stage_cm) ~ "FLAG",
                              TRUE ~ NA_character_))

  
  return(stage_Q_df)
  
}

#DAILY
calc_Q_for_sensor_stage_daily <- function(site) {
  # subset manual data by site
  site_rc <- rating_curve_coeffs %>%
    dplyr::filter(site_code == site)
  
  # filter corrected stage df for site
  daily_stage_Q_df <- daily_means_stage %>%
    dplyr::filter(site_code == site) %>%
    # grab a, b, c and sigma for the given site from rc summary
    mutate(a = site_rc$a[1],
           b = site_rc$b[1],
           r2 = site_rc$r2[1],
           year = year(date),
           # calculate Q for the site based on a, b, c and stage given
           Q_cfs = case_when(
             #IF there is no rating curve (< 3 measurements), do not calculate Q
             is.na(a) ~ NA, 
             # if the stage is below the lowest stage where Q was measured, 
             # make it the lowest Q value measured; extrapolation with this 
             # few field Q's is unwise
             !is.na(mean_corrected_stage_cm) & mean_corrected_stage_cm <= min(site_rc$H_cm) ~ min(site_rc$q_cfs), 
             # similar idea to above but with max stage measured
             !is.na(mean_corrected_stage_cm) & mean_corrected_stage_cm >= max(site_rc$H_cm) ~ max(site_rc$q_cfs),
             # if within the bounds of measured stage values, use a & b to calc Q
             !is.na(mean_corrected_stage_cm) ~ (a*(mean_corrected_stage_cm)^b),
             # if the stage is below the lowest stage where Q was measured, 
             # make it the lowest Q value measured; extrapolation with this 
             # few field Q's is unwise
             is.na(mean_corrected_stage_cm) & mean_sensor_stage_cm <= min(site_rc$H_cm) ~ min(site_rc$q_cfs), 
             # similar idea to above but with max stage measured
             is.na(mean_corrected_stage_cm) & mean_sensor_stage_cm >= max(site_rc$H_cm) ~ max(site_rc$q_cfs),
             # if within the bounds of measured stage values, use a & b to calc Q 
             is.na(mean_corrected_stage_cm) ~ (a*(mean_sensor_stage_cm)^b),
             # Default case when none of the above conditions are met
             TRUE ~ NA_real_), 
           # some rating curves are not robust and thus will extrapolate beyond measured Q for stages lower than the max Q
           # this corrects for that by setting the Q to the max Q if the calculated Q is greater than the max Q
           Q_cfs = case_when(Q_cfs > max(site_rc$q_cfs) ~ max(site_rc$q_cfs), 
                             TRUE ~ Q_cfs),
           # calculate Q for the site based on a, b, c and stage given
           extrapolated_Q_cfs = case_when(!is.na(mean_corrected_stage_cm) ~ (a*(mean_corrected_stage_cm)^b),
                                          is.na(mean_corrected_stage_cm) ~ (a*(mean_sensor_stage_cm)^b)), 
           #Add a flag if Q was calculated from sensor stage rather than corrected stage
           q_flag = case_when(is.na(mean_corrected_stage_cm) ~ "FLAG",
                                          TRUE ~ NA_character_))
  
  
  return(daily_stage_Q_df)
  
}

