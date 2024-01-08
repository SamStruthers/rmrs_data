
# Function to read in collated isco excel files
read_isco_file <- function(raw_isco_filepath){
  
  # Grab the site name from the file path
  file_parts <- strsplit(raw_isco_filepath, "/")[[1]]
  site_code <- strsplit(file_parts[5], "_")[[1]][1]
  
  #read xlsx file removing first row (random header info)
  isco_data <- map_dfr(.x = raw_isco_filepath, ~read_xlsx(path = .x, skip = 1))%>%
    #remove extra header rows
    slice(4:n())
  #make all column names lower case
  colnames(isco_data) = tolower(colnames(isco_data))
  #grab just the columns we need
  isco_data_concise <- isco_data%>%
    select(timestamp, record, sc_us, temp_c, cdom_ave,cdom, turb_ave, turb) %>%
    mutate(
      #converting numeric datetime to DT mst
      DT_mst = with_tz(as.POSIXct((as.numeric(timestamp) * 86400), origin = "1899-12-30 6:30:01",  tz = "UTC"), tzone = "MST"),
      #Convert other rows to numeric from characters
      record = as.numeric(record),
      sc_us = as.numeric(sc_us),
      ISCO_temp_c = as.numeric(temp_c),
      cdom_ave = as.numeric(cdom_ave),
      cdom = as.numeric(cdom),
      turb_ave = as.numeric(turb_ave),
      turb = as.numeric(turb),
      # round this date to 5 min to be matched downstream
      DT_round_mst = floor_date(DT_mst, "5 minutes"), 
      site = site_code
    )%>%
    select(-c(temp_c, timestamp, DT_mst))
  #save collated data
  write_csv(isco_data_concise, paste0("data/collated/", site_code, "isco_data_2023.csv"))
  write_rds(isco_data_concise, paste0("data/collated/", site_code, "isco_data_2023.RDS"))
  
  return(isco_data_concise)
}


####----- DAT FILE Cleaner -----###

clean_collate_dat_files <- function(dat_files){
  
  
  clean_dat_files <- function(filename){
    # Grab the site name from the file path
    file_parts <- strsplit(filename, "/")[[1]]
    site_code <- strsplit(file_parts[length(file_parts)], "_")[[1]][1]
    
    #read in the file
    content <- read.table(file = filename, sep = "/", skip = 1  )
    #split by commas
    content_string <- strsplit(content$V1, split = ",")
    #extract column names from first row
    column_names <- content_string[[1]]
    # Remove extra rows
    data <- content_string[-c(1:3)]
    
    #convert to table and add column names
    unlist_data <- function(i){
      #unlist the data for each row
      unlisted <- unlist(data[[i]])
      #transpose this so that columns X rows are correct
      transposed_df <- as_tibble(t(unlisted))
      #add in the correct column names
      names(transposed_df) <- column_names
      return(transposed_df)
    }
    
    #map over all the rows
    df <- map_dfr(seq(1, length(data), 1), unlist_data)%>%
      mutate(Site = site_code)
    
    return(df)
  }
  
  #Example code to read in 2022 data
  collated_data <- map_dfr(dat_files, clean_dat_files)
  
  #make all column names lower case
  colnames(collated_data) = tolower(colnames(collated_data))
  #grab just the columns we need
  collated_concise <- collated_data%>%
    #remove duplicate data due to overwriting of .dat files
    distinct()%>%
    select(timestamp, record, sc_us, temp_c, cdom_ave,cdom, turb_ave, turb, site) %>%
    mutate(
      #converting numeric datetime to DT mst
      DT_mst = with_tz(ymd_hms(timestamp)),
      #Convert other rows to numeric from characters
      record = as.numeric(record),
      sc_us = as.numeric(sc_us),
      ISCO_temp_c = as.numeric(temp_c),
      cdom_ave = as.numeric(cdom_ave),
      cdom = as.numeric(cdom),
      turb_ave = as.numeric(turb_ave),
      turb = as.numeric(turb),
      # round this date to 5 min to be matched downstream
      DT_round_mst = floor_date(DT_mst, "5 minutes")
    )%>%
    select(-c(temp_c, timestamp, DT_mst))
  
  return(collated_concise)
}
