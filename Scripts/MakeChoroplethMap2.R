# Name of script: MakeChoroplethMap2
# Description:  Defines function to produce choropleth map of UK given arbitrary dataset
# and geographical resolution using ggplot2 package
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 23-09-2024
# Latest update by: Calum Kennedy
# Latest update on: 23-09-2024
# Update notes: 

# Comments ---------------------------------------------------------------------

# Defines function to produce choropleth map of UK at arbitrary geographical resolution, 
# given an arbitrary dataset using ggplot2 package

# Define map function ----------------------------------------------------------

make_choropleth_map_2 <- function(fill_data,
                                  fill_var,
                                  boundary_data,
                                  fill_palette = "inferno",
                                  scale_lower_lim = NULL,
                                  scale_upper_lim = NULL,
                                  london_only = FALSE,
                                  winsorise = FALSE,
                                  lower_perc = 0.05,
                                  upper_perc = 0.95,
                                  legend_title){
  
  # If 'winsorise' is TRUE, winsorise upper and lower percentiles of fill variable (default is 5th and 95th percentile)
  if(winsorise) fill_data <- mutate(fill_data, "{{fill_var}}" := case_when({{fill_var}} > get_percentile({{fill_var}}, upper_perc) ~ get_percentile({{fill_var}}, upper_perc),
                                                                           {{fill_var}} < get_percentile({{fill_var}}, lower_perc) ~ get_percentile({{fill_var}}, lower_perc),
                                                                           .default = {{fill_var}}))
  
  # If 'london_only' == TRUE, filter fill data to only London polygons (else nothing)
  if(london_only) {
    
    fill_data <- filter(fill_data, rgn22nm == "London")
    boundary_data <- filter(boundary_data, str_sub(lad22cd, 1, 3) == "E09")
    
  }
  
  
  choropleth_map <- ggplot(fill_data
                           ) +
    
    geom_sf(aes(fill = {{fill_var}}),
            colour = NA
            ) +
    
    geom_sf(data = {{boundary_data}},
            fill = NA,
            lwd = 0.01,
            colour = "gray50"
            ) +
    
    scale_fill_viridis(option = fill_palette,
                       direction = -1,
                       limits = c(scale_lower_lim,
                                  scale_upper_lim)
    ) +
    
    theme_void() +
    
    labs(fill = legend_title)
  
  return(choropleth_map)
  
}
