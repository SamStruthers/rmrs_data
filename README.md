# rmrs_data
This repo will collate sensor data collected by RMRS in their Cameron Peak Fire Study. 
Sensor deployment was conducted by RMRS scientists at tributary sites in the Cameron Peak Burn Scar. 

# Workflow Design

All code is housed in `scripts` folder and all data will be housed in `data` folder.

`Data` is ignored from Github until publishing. Primary Data contact: Tim Fegel

Scripts should be run in ascending order (00-05) and will clean sensor data to remove outliers and match datetimes across sites/sensors
All datetimes will be converted to MST. 

