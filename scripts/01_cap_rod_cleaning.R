import_adjust_caprod <- function(folder_path, field_stage, old_measurements, flagging_csv ){



# The function `import_cap_rod.R`, given a folder_path, will collate all stage data into a single data frame
# Make sure each file starts with the site name and is proceeded by a underscore
# Excel likes to mess with datetimes so check the data in excel first, if needing to, correcting the date time to the format: m/d/yy HH:MM
#Also make sure all the datasets have the same datetime column and set it below in date_time_col

# map over the folder path
stage_df <- import_cap_rod(folder_path = folder_path)
#read in manual measurements
manual_measurements <- read_xlsx(field_stage)%>%
  format_manual_stage()

#write raw collated file
write_csv(stage_df, "data/collated/stage_UNCLEAN.csv")




# 4 columns:
# start_dt <- when flag starts
# end_dt <- when flag ends
## make sure start != end, will duplicate data
# FLAG: flag code
# notes: any notes about the flag code or field notes
flag_values<- read_csv(flagging_csv, show_col_types = FALSE)%>%
  mutate(start_dt = as.POSIXct(start_dt, format = "%m/%d/%y %H:%M", tz = "MST"),
         end_dt = as.POSIXct(end_dt, format = "%m/%d/%y %H:%M", tz = "MST"))%>%
  select(site = site_code, start_dt, end_dt, FLAG, notes = Notes)


# apply flag in add flags.R
#map over flag values df
correct_and_flagged_df <- pmap_dfr(flag_values, apply_flag_15min)

####------ exporting corrected 15 min data ------ #####

final_stage_df<- correct_and_flagged_df %>%
  select(DT, 
         site_code, 
         sensor_stage_cm = water_height_cm_inst, 
         adjustment_cm = adjustment,
         adjusted_stage_cm = adjusted_stage,
         time_correction_value = corrFac, 
         corrected_stage_cm,
         flag_type, 
         notes = note,
         date, 
         time)

write.csv(final_stage_df,"data/final/corrected_15min_stage_2023.csv")




