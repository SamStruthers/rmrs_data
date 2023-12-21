create_rating_curve <- function(site) {
  
  site_manual_h_q <- h_q_df %>%
    dplyr::filter(site_code == site) %>%
    mutate(logH = log10(H),
           logQ = log10(Q))
  
  # create linear model based on log of both functions
  # Q = m * H + b
  rc_lm <- lm(logQ ~ logH, data = site_manual_h_q)
  
  # summary of lm created
  # this is where coefficients will be pulled from
  
  # intercept == b
  # logH == a
  rc_summary <- summary(rc_lm)
  
  # pulling coefficients in to manual data frame
  site_manual_h_q <- site_manual_h_q %>%
    mutate(
      # translating coefficient back into non log scale
      a = (10 ^ (rc_summary$coefficients[1,1])),
      # already in non log scale 
      b = rc_summary$coefficients[2,1],
      # r^2 value
      r2 = rc_summary$r.squared,
      # using calculated coefficuient on manual H values to check for accuracy
      Q_cfs = (a*(H)^b))
  
  return(site_manual_h_q)
  
}

calc_Q_for_sensor_stage_15min <- function(site) {
  
  site_rc <- manual_H_Q_rc %>%
    dplyr::filter(site_code == site)
  
  # filter corrected stage df for site
  stage_Q_df <- final_stage_df %>%
    dplyr::filter(site_code == site) %>%
    # grab a, b, c and sigma for the given site from rc summary
    mutate(a = site_rc$a[1],
           b = site_rc$b[1],
           r2 = site_rc$r2[1],
           # calculate Q for the site based on a,b,c and stage given
           Q_cfs = 
             # if the stage is below the lowest stage where Q was measured, 
             # make it the lowest Q value measured; extrapolation with this 
             # few field Q's is unwise
             ifelse(corrected_stage_cm < min(site_rc$H), min(site_rc$Q),
                    # similar idea to above but with max stage measured        
                    ifelse(corrected_stage_cm > max(site_rc$H), max(site_rc$Q),
                           # if within the bounds of measured stage values, use a & b to calc Q 
                           (a*(corrected_stage_cm)^b))))
  
  return(stage_Q_df)
  
}

calc_Q_for_sensor_stage_daily <- function(site) {
  # subset manual data by site
  site_rc <- manual_H_Q_rc %>%
    dplyr::filter(site_code == site)
  
  # filter corrected stage df for site
  daily_stage_Q_df <- final_daily_means_stage %>%
    dplyr::filter(site_code == site) %>%
    # grab a, b, c and sigma for the given site from rc summary
    mutate(a = site_rc$a[1],
           b = site_rc$b[1],
           r2 = site_rc$r2[1],
           # calculate Q for the site based on a, b, c and stage given
           Q_cfs = 
             # if the stage is below the lowest stage where Q was measured, 
             # make it the lowest Q value measured; extrapolation with this 
             # few field Q's is unwise
             ifelse(mean_corrected_stage_cm < min(site_rc$H), min(site_rc$Q),
                    # similar Idea to above but with max stage measured        
                    ifelse(mean_corrected_stage_cm >max(site_rc$H), max(site_rc$Q),
                           # if within the bounds of measured stage values, use a & b to calc Q 
                           (a*(mean_corrected_stage_cm)^b))))
  
  return(daily_stage_Q_df)
  
}


EXTRAPOLATION_Q_sensor_15min <- function(site) {
  
  # subset manual data by site
  site_rc <- manual_H_Q_rc %>%
    dplyr::filter(site_code == site)
  
  # filter corrected stage df for site
  extrapolated_stage_Q_df <- final_stage_df %>%
    dplyr::filter(site_code == site) %>%
    # grab a, b, c and sigma for the given site from rc summary
    mutate(a = site_rc$a[1],
           b = site_rc$b[1],
           r2 = site_rc$r2[1],
           # calculate Q for the site based on a, b, c and stage given
           Q_cfs = (a*(corrected_stage_cm)^b))
  
  return(extrapolated_stage_Q_df)
  
}

EXTRAPOLATION_Q_sensor_daily <- function(site) {
  
  # subset manual data by site
  site_rc <- manual_H_Q_rc %>%
    dplyr::filter(site_code == site)
  
  # filter corrected stage df for site
  daily_extrapolated_stage_Q_df <- final_daily_means_stage%>%
    dplyr::filter(site_code == site)%>%
    # grab a, b, c and sigma for the given site from rc summary
    mutate(a = site_rc$a[1],
           b = site_rc$b[1],
           r2 = site_rc$r2[1],
           # calculate Q for the site based on a,b,c and stage given
           Q_cfs = (a*(mean_corrected_stage_cm)^b))
  
  return(daily_extrapolated_stage_Q_df)
  
}

