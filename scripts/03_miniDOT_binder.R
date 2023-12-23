
dot_puller <- function(raw_folder_path){

  #List all .txt files in the folder path given
  txt_files_ls = list.files(path=paste0(raw_folder_path), pattern="*.txt", full.names=T, recursive = T)
  
  # Grab the site name from the file path
  file_parts <- strsplit(txt_files_ls, "/")[[1]]
  site <- file_parts[4]
  
  
  # Read the text files in
   txt_files <- lapply(txt_files_ls, function(x) {read.delim(file = x, header = F, sep =",", dec=".") %>% slice(-1:-2) %>%
                janitor::row_to_names(row_number=1) %>%
              # convert columns to what parameter they are
                select(seconds=1,
                       Temp_C=3,
                       DO_mgL=4) %>%
              #Convert seconds column to DT utc
                mutate(DT_utc=as_datetime(as.numeric(seconds), tz = "UTC"),
                       #Grab site from above
                       Site = site,
                       #convert temp and do to numeric values rather than characters
                       Temp_C=as.numeric(Temp_C),
                       DO_mgL=as.numeric(DO_mgL))})

  # Combine all individual text files to a single dataframe
   combined_do <- do.call("rbind", lapply(txt_files, as.data.frame))%>%
  #remove the seconds column
                  select(-seconds)
  #Save to CSV for future use
   write_csv(combined_do, paste0("data/cleaned/", site,'_miniDOT_data.csv'))
    #write_rds(x = combined_do, file =paste0("data/cleaned/", site,'_miniDOT_data.RDS' ))
    return(combined_do)
}


