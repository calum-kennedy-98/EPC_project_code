# Name of script: MergeStatisticalGeographies
# Description:  Merges UPRN to statistical geography datasets to merge with main
# EPC data
# data to merge with main EPC data
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 06-09-2024
# Latest update by: Calum Kennedy
# Latest update on: 06-09-2024
# Update notes: 

# Comments ---------------------------------------------------------------------



# Define function to merge statistical geographies -----------------------------

merge_statistical_geographies <- function(path_stat_geo_files){
  
  # Statistical geography lookup (UPRN-level)
  data_geo_files <- list.files(here(path_stat_geo_files), pattern = "\\.csv$", full.names = TRUE)
  
  # Load statistical geographies data (UPRN level) -----------------------------
  
  # Sequentially load csv files using vroom
  data_geo_list <- lapply(data_geo_files, vroom, col_select = c("UPRN",
                                                                "PCDS",
                                                                "lsoa21cd",
                                                                "wd22cd",
                                                                "rgn22cd"))
  
  # Bind list of dataframes together
  data_geo <- bind_rows(data_geo_list) %>%
    
    # Clean names
    clean_names() %>%
    
    # Clean postcode variable to remove whitespace and set to lowercase
    mutate(pcds = tolower(str_replace_all(pcds, fixed(" "), "")))
  
  return(data_geo)
  
}
