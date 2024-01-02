# function to adjust sensor stage values to match the start measurement  

adjust_stage <- function(site_name, start_date, end_date,
                         manual_start, manual_end) {
  
  stage_df_new <- stage_df %>%
    # subset df by site and DTs between site visits
    dplyr::filter(stage_df$site_code %in% site_name) %>%
    dplyr::filter(between(DT, start_date, end_date)) %>%
    # difference between the manual measurement and the average of 
    # the first 4 sensor readings in the subset (i.e. the next 4
    # sensor readings after the manual measurement)
    mutate(adjustment = manual_start - mean(head(water_height_cm_inst, n = 4), na.rm = TRUE) , 
           # adjust stage by adjustment difference
           adjusted_stage = water_height_cm_inst + adjustment)
  
  return(stage_df_new)
  
}

# function to give dataset a time correction factor for each subset
dr_factor_subset <- function(site_name, start_date, end_date, 
                             manual_start, manual_end) {
  
  factor_df <- stepped_df %>%
    # subset df by site and DTs between site visits
    dplyr::filter(stepped_df$site_code %in% site_name) %>%
    dplyr::filter(between(DT, start_date, end_date)) %>%
    # `dr_factor` needs DT to create the time factor
    mutate(time = format(DT, format = "%H:%M:%S"), 
           date = as.Date(DT, format = "%Y-%m-%d")) %>%
    # arranging subset to correct any factor errors
    arrange(DT) %>%
    # create time factor for subset of data
    dr_factor(., corrFactor = corrFac, dateVar = date, 
              timeVar = time, keepDateTime = TRUE )
  
  return(factor_df)
  
}

# function to correct drift over a subset by looking at measured 
# values and the previous four readings. data frame must have corrFac column to run

dr_correctOne_subset <- function(site_name, start_date, end_date, 
                                 manual_end, manual_start) {
  
  corrected_df <- factor_df %>%
    dplyr::filter(factor_df$site_code %in% site_name) %>%
    dplyr::filter(between(DT, start_date, end_date))
  
  # average of the last 4 measurements in the subset becomes 'calVal'
  # ie value that the sensor sees
  
  last_sensor_cm = mean(tail(corrected_df$adjusted_stage, n = 4), na.rm = TRUE)
  
  drift_corrected_df <- dr_correctOne(
    # dataframe to correct
    corrected_df,
    # column that we will be correcting
    sourceVar = adjusted_stage, 
    # column that we are creating
    cleanVar = corrected_stage_cm ,
    # sensor value nearest to our manual measurement
    calVal = last_sensor_cm , 
    # manual measurement that we want the subset of data to end on 
    calStd = manual_end , 
    # time correction factor from dr_factor
    factorVar = corrFac)
  
  return(drift_corrected_df)
  
}