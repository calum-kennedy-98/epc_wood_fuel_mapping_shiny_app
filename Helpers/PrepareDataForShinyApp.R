# Name of script: PrepareDataForShinyApp
# Description:  Script to prepare data to use in Shiny app
# and geographical resolution
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 17-10-2024
# Latest update by: Calum Kennedy
# Latest update on: 17-10-2024

# Load necessary packages ------------------------------------------------------

# Install packages using pacman
if(!require(pacman)){install.packages("pacman")}

p_load(shiny,
       stringr,
       vroom,
       ggplot2,
       leaflet,
       sf,
       janitor,
       dplyr,
       bslib,
       rmapshaper)

# Source helper functions ------------------------------------------------------

source(here("Helpers/UtilityFunctions.R"))

# Define percentiles for winsorising -------------------------------------------

lower_perc <- 0.05
upper_perc = 0.95

# Load necessary data ----------------------------------------------------------

# LSOA data
data_epc_lsoa_cross_section_to_map <- read_sf(here("Data/raw/data_epc_lsoa_cross_section_to_map.shp")) %>% 
  
  # Rename outcome variables
  rename(lsoa21nm = lso21nm,
         wood_conc_pred = wd_cnc_,
         wood_perc_h = wd_prc_)

# Ward data
data_epc_ward_cross_section_to_map <- read_sf(here("Data/raw/data_epc_ward_cross_section_to_map.shp")) %>% 
  
  # Rename outcome variables
  rename(objectid = objectd,
         wood_conc_pred = wd_cnc_,
         wood_perc_h = wd_prc_) %>%
  
  # Create unique name column by concatenating name + code (duplicate Ward names)
  mutate(wd22nm_cd = paste(wd22nm, wd22cd, sep = " "))

# LA data
data_epc_la_cross_section_to_map <- read_sf(here("Data/raw/data_epc_la_cross_section_to_map.shp")) %>% 
  
  # Rename outcome variables
  rename(wood_conc_pred = wd_cnc_,
         wood_perc_h = wd_prc_)

# Prepare data for Shiny app ---------------------------------------------------

# LSOA data
data_epc_lsoa_cross_section_to_map <- prepare_data_for_shiny_app(data = data_epc_lsoa_cross_section_to_map, 
                                                                 vars_to_select = c("lsoa21nm", 
                                                                                    "geometry", 
                                                                                    "wood_perc_h", 
                                                                                    "wood_conc_pred", 
                                                                                    "fid", 
                                                                                    "long", 
                                                                                    "lat"), 
                                                                 wood_conc_pred_var = wood_conc_pred, 
                                                                 wood_perc_h_var = wood_perc_h, 
                                                                 lower_perc = lower_perc, 
                                                                 upper_perc = upper_perc, 
                                                                 perc_polygons_to_keep = 0.2)

# Ward data
data_epc_ward_cross_section_to_map <- prepare_data_for_shiny_app(data = data_epc_ward_cross_section_to_map, 
                                                                 vars_to_select = c("wd22nm_cd", 
                                                                                    "geometry", 
                                                                                    "wood_perc_h", 
                                                                                    "wood_conc_pred", 
                                                                                    "objectid", 
                                                                                    "long", 
                                                                                    "lat"), 
                                                                 wood_conc_pred_var = wood_conc_pred, 
                                                                 wood_perc_h_var = wood_perc_h, 
                                                                 lower_perc = lower_perc, 
                                                                 upper_perc = upper_perc, 
                                                                 perc_polygons_to_keep = 0.4)

# LA data
data_epc_la_cross_section_to_map <- prepare_data_for_shiny_app(data = data_epc_la_cross_section_to_map, 
                                                               vars_to_select = c("lad22nm", 
                                                                                  "geometry", 
                                                                                  "wood_perc_h", 
                                                                                  "wood_conc_pred", 
                                                                                  "fid", 
                                                                                  "long", 
                                                                                  "lat"), 
                                                               wood_conc_pred_var = wood_conc_pred, 
                                                               wood_perc_h_var = wood_perc_h, 
                                                               lower_perc = lower_perc, 
                                                               upper_perc = upper_perc, 
                                                               perc_polygons_to_keep = 0.7)

# Save data to folder for faster loading into Shiny app ------------------------

st_write(data_epc_lsoa_cross_section_to_map, here("Data/cleaned/data_epc_lsoa_cross_section_to_map.shp"), append = FALSE)
st_write(data_epc_ward_cross_section_to_map, here("Data/cleaned/data_epc_ward_cross_section_to_map.shp"), append = FALSE)
st_write(data_epc_la_cross_section_to_map, here("Data/cleaned/data_epc_la_cross_section_to_map.shp"), append = FALSE)