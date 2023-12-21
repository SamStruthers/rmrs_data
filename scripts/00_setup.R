#this file contains all the packages,metadata, groupings and color palettes that are used in downstream scripts

### ----- Load packages ----- ###
package_load <- function(package_names){
  for(i in 1:length(package_names)){
    if(!package_names[i] %in% installed.packages()){
      install.packages(package_names[i])
    }
    library(package_names[i],character.only = TRUE)
  }
}

#vector of packages
pack_req <- c( 
  # data wrangling packages
  "tidyverse","lubridate","padr","janitor","padr", "broom","arrow",
  #spatial packages
  "sf","terra","nhdplusTools", "tigris","raster", "leaflet","tmap",
  # plotting
  "ggpubr","ggthemes","scales","corrplot","gghighlight", "geomtextpath", "ggbeeswarm","plotly", "ggpmisc","flextable",
  # web scrapping
  "rjson", "rvest", "dataRetrieval", "httr", "jsonlite",
  #extra
  "devtools", "trend")
package_load(pack_req)


# install.packages("devtools")
#devtools::install_github("shaughnessyar/driftR",quiet = TRUE)
library(driftR)

# install.packages("devtools")
#devtools::install_github("anguswg-ucsb/cdssr",quiet = TRUE)
library(cdssr)

remove(pack_req, package_load)
#Simple function to negate %in%
`%nin%` = Negate(`%in%`)





