# Name of script: UtilityFunctions
# Description:  Defines set of utility functions which are used in other scripts
# and geographical resolution
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 04-09-2024
# Latest update by: Calum Kennedy
# Latest update on: 04-09-2024
# Update notes: 

# Comments ---------------------------------------------------------------------



# Function to get the 'ith' percentile of a dataframe column -------------------

get_percentile <- function(variable, percentile){
  
  percentile <- quantile(variable, percentile, na.rm = TRUE)
  
  return(percentile)
  
}

# Define function to prepare data for Shiny app --------------------------------

prepare_data_for_shiny_app <- function(data,
                                       vars_to_select,
                                       wood_conc_pred_var,
                                       wood_perc_h_var,
                                       lower_perc,
                                       upper_perc,
                                       perc_polygons_to_keep){
  
  # Create new spatial data frame
  data_prepared <- data %>%
    
    # Select required variables
    select(vars_to_select) %>%
    
    # Create new variable for ranking relative to other geographies for two outcomes (ties allowed)
    mutate(wood_conc_pred_rank = dense_rank(desc({{wood_conc_pred_var}})),
           wood_perc_h_rank = dense_rank(desc({{wood_perc_h_var}}))) %>%
    
    # WInsorise wood_conc_pred variable based on function inputs to improve readability of maps
    mutate(wood_conc_winsorised = case_when({{wood_conc_pred_var}} > get_percentile({{wood_conc_pred_var}}, upper_perc) ~ get_percentile({{wood_conc_pred_var}}, upper_perc),
                                      {{wood_conc_pred_var}} < get_percentile({{wood_conc_pred_var}}, lower_perc) ~ get_percentile({{wood_conc_pred_var}}, lower_perc),
                                      .default = {{wood_conc_pred_var}})) %>%
    
    # Simplify polygons for faster app runtime
    ms_simplify(keep = perc_polygons_to_keep)
  
  # Return final data
  return(data_prepared)
  
}
