import_cap_rod <- function(folder_path){
  #get all the filenames in the folder
  filenames <-  list.files(folder_path, recursive = TRUE, full.names = TRUE)
  
  #grab data from each individual file and do some basic reformatting so that all datasets match
  grab_data <- function(file_name){
    #grab site name from filename
    file_parts <- strsplit(file_name, "/")[[1]]
    site <- strsplit(file_parts[5], "_")[[1]][1]
    #read in data
    stage_df <- read_csv(file_name)%>%
      #add site name to dataset
      mutate(site_code = site, 
             #convert datetime to DT object for R
             DT = as.POSIXct(.[[date_time_col]], format = "%m/%d/%y %H:%M"), 
             # wtrhgt__5 is instantaneous water height in mm
             # wtrhgt__6 is average water height (rolling average I believe) in mm
             water_height_cm_inst = wtrhgt__5 /10, 
             water_height_cm_ave = wtrhgt__6 /10)%>%
      #renaming columns to more logical titles
      rename(water_height_mm_inst = wtrhgt__5, 
             water_height_mm_ave = wtrhgt__6)
  }
  
  #Map over all filenames in folder to grab all the data
  all_stage <- map_dfr(filenames, grab_data)%>%
    select(site_code, DT, water_height_cm_inst, water_height_cm_ave)
  
  #return this dataset
  return(all_stage)
}