# Calculate Q

calc_manual_q <- function(folder_path){
  
  #Find all xlsx files in the folder
  #ANY OPEN FILES WILL BREAK THIS
  manual_q_files <- list.files(path = folder_path, pattern = ".xlsx", full.names = TRUE, recursive = TRUE)
  
  read_q_file <- function(file_path){
    #parse site code and date from file name
    site_code <- file_path %>%
          str_extract("([^/]+)_([^_]+)(?=\\.xlsx)") %>%
          str_remove_all("[^A-Za-z]")
    
    parsed_date <- file_path %>%str_split("/") %>%
      map_chr(last) %>%
      str_extract("(\\d+)")

    
    #read in file
    manual_q <- read_xlsx(path = file_path)%>%
      # make column with next distance measurement
      mutate(dist_m = `Distance (m)`,
             depth_m = `Depth (m)`,
             vel_m_s = `Velocity (m/s)`,
        distance_lead = lead(dist_m),
             #calc the width between these points
             width_m = distance_lead - dist_m, 
             #calc area 
             area_m = width_m * depth_m, 
             # calc area and velocity for each section
             area_vel = area_m * vel_m_s)
    # calc q by summarizing area_vel
    q <- tibble(site = site_code, 
                date = as.Date(parsed_date, format = "%m%d%y"),
                q_cms = sum(manual_q$area_vel, na.rm = TRUE), 
                #convert to cfs
                q_cfs = q_cms*35.3147,
                #count number of measurements
                meas = length(manual_q$width_m),
                #flag if any measurements are above 10% of the total q
                Flag = ifelse(any(manual_q$area_vel > 0.1 * q_cms), "Flagged", "Not Flagged"))

    return(q)
  }
  #read in all files and calculate q for each
  q_calcs <- map_dfr(manual_q_files, .f = read_q_file)
  
  return(q_calcs)
}



