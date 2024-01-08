create_rating_curve <- function(site) {
  
  site_manual_h_q <- h_q_df %>%
    dplyr::filter(site_code == site) %>%
    mutate(logH = log10(H_cm),
           logQ = log10(q_cfs))
  
  # if there are less than 3 values, the linear model will not work
 if(nrow(site_manual_h_q) < 3) {
   site_rc_coeff <- site_manual_h_q %>%
     mutate(a = NA,b = NA,r2 = NA, calc_Q_cfs = NA)
   
   return(site_manual_h_q)
 } else {
   # create linear model based on log of both functions
   # Q = m * H + b
   # summary of lm created
   # this is where coefficients will be pulled from
  rc <- summary(lm(logQ ~ logH, data = site_manual_h_q))
  # y_intercept == b
  # logH == a

  # pulling coefficients in to manual data frame
  site_rc_coeff <- site_manual_h_q %>%
    mutate(
      # translating coefficient back into non log scale
      a = (10 ^ (rc$coefficients[1,1])),
      # already in non log scale 
      b = rc$coefficients[2,1],
      # r^2 value
      r2 = rc$r.squared,
      # using calculated coefficuient on manual H values to check for accuracy
      calc_Q_cfs = (a*(H_cm)^b))
  
  return(site_rc_coeff)
  
 }
}
