
# Function to read in isco excel files
read_isco_file <- function(raw_isco_filepath){
  
  # Grab the site name from the file path
  file_parts <- strsplit(raw_isco_filepath, "/")[[1]]
  site <- strsplit(file_parts[5], "_")[[1]][1]
  
  
  
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
      Site = site
    )%>%
    select(-c(temp_c, timestamp, DT_mst))
  #save collated data
  write_csv(isco_data_concise, paste0("data/collated/", site, "isco_data_2023.csv"))
  write_rds(isco_data_concise, paste0("data/collated/", site, "isco_data_2023.RDS"))
  
  return(isco_data_concise)
}
