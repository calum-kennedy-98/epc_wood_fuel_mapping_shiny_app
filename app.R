### Name: app.R
### Author: Calum Kennedy
### Date created: 26-09-24
### Last update: 26-09-24

# Description ------------------------------------------------------------------

# Code to produce interactive R Shiny dashboard of the concentration and prevalence
# of EPCs with wood burning appliances by various geographies

# Source script to prepare data for app ----------------------------------------

if(!require(pacman)){install.packages("pacman")}

p_load(shiny,
       here,
       sf,
       dplyr,
       leaflet,
       bslib)

# Source functions -------------------------------------------------------------

setShapeStyle <- function( map, data = getMapData(map), layerId,
                           stroke = NULL, color = NULL,
                           weight = NULL, opacity = NULL,
                           fill = NULL, fillColor = NULL,
                           fillOpacity = NULL, dashArray = NULL,
                           smoothFactor = NULL, noClip = NULL,
                           options = NULL
){
  options <- c(list(layerId = layerId),
               options,
               filterNULL(list(stroke = stroke, color = color,
                               weight = weight, opacity = opacity,
                               fill = fill, fillColor = fillColor,
                               fillOpacity = fillOpacity, dashArray = dashArray,
                               smoothFactor = smoothFactor, noClip = noClip
               )))
  # evaluate all options
  options <- evalFormula(options, data = data)
  # make them the same length (by building a data.frame)
  options <- do.call(data.frame, c(options, list(stringsAsFactors=FALSE)))
  
  layerId <- options[[1]]
  style <- options[-1] # drop layer column
  
  #print(list(style=style))
  leaflet::invokeMethod(map, data, "setStyle", "shape", layerId, style);
}

# Load necessary data ----------------------------------------------------------

# Map boundary data (LA level)
la_boundaries <- read_sf(here("Data/raw/la_boundaries.shp"))

# Combine England and Wales shapefiles into one shapefile
sca_boundaries <- read_sf(here("Data/cleaned/sca_boundaries.shp"))

# LSOA data
data_epc_lsoa_cross_section_to_map <- read_sf(here("Data/cleaned/data_epc_lsoa_cross_section_to_map.shp")) %>% 
  
  # Rename outcome variables
  rename(lsoa21nm = lso21nm,
         wood_conc_pred = wd_cnc_p,
         wood_conc_pred_winsorised = wd_cnc_w,
         wood_perc_h = wd_prc_,
         wood_conc_pred_rank = wd_cn__,
         wood_perc_h_rank = wd_pr__) %>%
  
  # Convert layer ID to character (necessary for Java methods)
  mutate(fid = as.character(fid))

# Ward data
data_epc_ward_cross_section_to_map <- read_sf(here("Data/cleaned/data_epc_ward_cross_section_to_map.shp")) %>% 
  
  # Rename outcome variables
  rename(objectid = objectd,
         wood_conc_pred = wd_cnc_p,
         wood_conc_pred_winsorised = wd_cnc_w,
         wood_perc_h = wd_prc_,
         wd22nm_cd = wd22nm_,
         wood_conc_pred_rank = wd_cn__,
         wood_perc_h_rank = wd_pr__) %>%
  
  # Convert layer ID to character (necessary for Java methods)
  mutate(objectid = as.character(objectid))

# LA data
data_epc_la_cross_section_to_map <- read_sf(here("Data/cleaned/data_epc_la_cross_section_to_map.shp")) %>% 
  
  # Rename outcome variables
  rename(wood_conc_pred = wd_cnc_p,
         wood_conc_pred_winsorised = wd_cnc_w,
         wood_perc_h = wd_prc_,
         wood_conc_pred_rank = wd_cn__,
         wood_perc_h_rank = wd_pr__) %>%
  
  # Convert layer ID to character (necessary for Java methods)
  mutate(fid = as.character(fid))

# Set up global colour palette and proxy to update map based on user input -----
colour_pal_lsoa_conc <- colorNumeric(palette = "inferno",
                                     domain = data_epc_lsoa_cross_section_to_map$wood_conc_pred_winsorised,
                                     reverse = TRUE)

colour_pal_lsoa_perc <- colorNumeric(palette = "inferno",
                                     domain = data_epc_lsoa_cross_section_to_map$wood_perc_h,
                                     reverse = TRUE)

colour_pal_ward_conc <- colorNumeric(palette = "inferno",
                                     domain = data_epc_ward_cross_section_to_map$wood_conc_pred_winsorised,
                                     reverse = TRUE)

colour_pal_ward_perc <- colorNumeric(palette = "inferno",
                                     domain = data_epc_ward_cross_section_to_map$wood_perc_h,
                                     reverse = TRUE)

colour_pal_la_conc <- colorNumeric(palette = "inferno",
                                   domain = data_epc_la_cross_section_to_map$wood_conc_pred_winsorised,
                                   reverse = TRUE)

colour_pal_la_perc <- colorNumeric(palette = "inferno",
                                   domain = data_epc_la_cross_section_to_map$wood_perc_h,
                                   reverse = TRUE)

# Define UI --------------------------------------------------------------------

ui <- fluidPage(
  
  tags$head(
    # add in methods from https://github.com/rstudio/leaflet/pull/598
    tags$script(HTML(
      '
window.LeafletWidget.methods.setStyle = function(category, layerId, style){
  var map = this;
  if (!layerId){
    return;
  } else if (!(typeof(layerId) === "object" && layerId.length)){ // in case a single layerid is given
    layerId = [layerId];
  }

  //convert columnstore to row store
  style = HTMLWidgets.dataframeToD3(style);
  //console.log(style);

  layerId.forEach(function(d,i){
    var layer = map.layerManager.getLayer(category, d);
    if (layer){ // or should this raise an error?
      layer.setStyle(style[i]);
    }
  });
};

window.LeafletWidget.methods.setRadius = function(layerId, radius){
  var map = this;
  if (!layerId){
    return;
  } else if (!(typeof(layerId) === "object" && layerId.length)){ // in case a single layerid is given
    layerId = [layerId];
    radius = [radius];
  }

  layerId.forEach(function(d,i){
    var layer = map.layerManager.getLayer("marker", d);
    if (layer){ // or should this raise an error?
      layer.setRadius(radius[i]);
    }
  });
};
'
    ))
  ),
  
  # Create navigation panes
  navset_card_tab(
    
    # Set full_screen to TRUE
    full_screen = TRUE,
    
    # Set main title
    title = h1(strong("Mapping the predicted concentration and prevalence of wood fuel heat sources across England and Wales"),
               style = "font-size:18px;"),
    
    # Create navigation panel for LSOA
    nav_panel("LSOA",
              
              # Create sidebar layout within navigation panel
              layout_sidebar(
                
                # Generate sidebar
                sidebar = sidebar(
                  
                  # Set initial state to open
                  open = "open",
                  
                  # Selectize input for LSOA
                  selectizeInput(
                    inputId = "selected_lsoa",
                    label = "Select LSOA",
                    choices = NULL
                  ),
                  
                  # Select input for data type
                  selectInput(
                    inputId = "selected_data_lsoa",
                    label = "Select data layer",
                    choices = c("Concentration",
                                "Prevalence")
                  ),
                  
                  checkboxInput(inputId = "show_sca_boundaries_lsoa",
                                label = "Show Smoke Control Areas",
                                value = TRUE),
                  
                  textOutput("lsoa_text")),
                
                # Display map
                card(
                  leafletOutput("lsoa_map")
                )),
    ),
    
    # Create navigation panel for Electoral Ward
    nav_panel("Electoral Ward",
              
              # Create sidebar layout within navigation panel
              layout_sidebar(
                
                # Generate sidebar
                sidebar = sidebar(
                  
                  # Set initial state to open
                  open = "open",
                  
                  # Selectize input for Electoral Ward
                  selectizeInput(
                    inputId = "selected_ward",
                    label = "Select Ward",
                    choices = NULL
                  ),
                  
                  # Select input for data type
                  selectInput(
                    inputId = "selected_data_ward",
                    label = "Select data layer",
                    choices = c("Concentration",
                                "Prevalence")
                  ),
                  
                  checkboxInput(inputId = "show_sca_boundaries_ward",
                                label = "Show Smoke Control Areas",
                                value = TRUE),
                  
                  textOutput("ward_text")),
                
                # Display map
                card(
                  leafletOutput("ward_map")
                )),
    ),
    
    # Create navigation panel for Local Authority District
    nav_panel("Local Authority",
              
              # Create sidebar layout within navigation panel
              layout_sidebar(
                
                # Generate sidebar
                sidebar = sidebar(
                  
                  # Set initial state to open
                  open = "open",
                  
                  # Selectize input for Local Authority
                  selectizeInput(
                    inputId = "selected_la",
                    label = "Select Local Authority",
                    choices = NULL
                  ),
                  
                  # Select input for data type
                  selectInput(
                    inputId = "selected_data_la",
                    label = "Select data layer",
                    choices = c("Concentration",
                                "Prevalence")
                  ),
                  
                  checkboxInput(inputId = "show_sca_boundaries_la",
                                label = "Show Smoke Control Areas",
                                value = TRUE),
                  
                  textOutput("la_text")),
                
                # Display map
                card(
                  leafletOutput("la_map")
                )),
    ),
    
  )
)

# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  
  # LSOA Level -----------------------------------------------------------------
  
  proxy_lsoa <- leafletProxy("lsoa_map", data = data_epc_lsoa_cross_section_to_map) 
  
  # Set initial value for selectize input
  updateSelectizeInput(session, 
                       'selected_lsoa', 
                       choices = data_epc_lsoa_cross_section_to_map$lsoa21nm,
                       selected = "City of London 001A",
                       server = TRUE)
  
  # Create text object to display ranking
  output$lsoa_text <- renderText({
    
    paste0(input$selected_lsoa," is ranked ", 
           data_epc_lsoa_cross_section_to_map$wood_conc_pred_rank[data_epc_lsoa_cross_section_to_map$lsoa21nm == input$selected_lsoa], 
           " out of ", length(data_epc_lsoa_cross_section_to_map$fid), 
           " LSOAs for concentration of wood fuel heat sources and ",
           data_epc_lsoa_cross_section_to_map$wood_perc_h_rank[data_epc_lsoa_cross_section_to_map$lsoa21nm == input$selected_lsoa], 
           " for percentage of houses with wood fuel heat sources.",
           " The predicted concentration of wood fuel heat sources is ",
           round(data_epc_lsoa_cross_section_to_map$wood_conc_pred[data_epc_lsoa_cross_section_to_map$lsoa21nm == input$selected_lsoa], 1),
           " per km2, and ",
           round(data_epc_lsoa_cross_section_to_map$wood_perc_h[data_epc_lsoa_cross_section_to_map$lsoa21nm == input$selected_lsoa], 0),
           "% of homes have a wood fuel heat source.")
    
  })
  
  # Create base map of concentration by LSOA
  output$lsoa_map <- renderLeaflet({
    
    leaflet(data_epc_lsoa_cross_section_to_map) %>%
      
      addTiles() %>%
      
      addMapPane("data", zIndex = 430) %>%
      
      addMapPane("sca", zIndex = 420) %>%
      
      # Add LA boundaries as polygons
      addPolygons(data = la_boundaries,
                  fillOpacity = 0,
                  color = "grey",
                  weight = 1) %>%
      
      addPolygons(data = sca_boundaries,
                  fillOpacity = 0.25,
                  color = "blue",
                  weight = 1,
                  group = "sca",
                  options = pathOptions(pane = "sca")) %>%
      
      addPolygons(data = data_epc_lsoa_cross_section_to_map,
                  weight = 0,
                  fillColor = ~colour_pal_lsoa_conc(wood_conc_pred_winsorised),
                  layerId = ~fid,
                  group = "data",
                  options = pathOptions(pane = "data")) 
    
  })
  
  # Observer event to display SCA boundaries
  observeEvent(input$show_sca_boundaries_lsoa, {

    if(input$show_sca_boundaries_lsoa == FALSE){

      proxy_lsoa %>%

        hideGroup("sca")

    } else {

      proxy_lsoa %>%

        showGroup("sca")

    }

  })
  
  # Set up event to change map fill based on user data selection
  observeEvent(input$selected_data_lsoa, {
    
    if(input$selected_data_lsoa == "Concentration"){
      
      proxy_lsoa  %>%
        
        setShapeStyle(layerId = ~fid, 
                      smoothFactor = 0,
                      fillColor = ~colour_pal_lsoa_conc(wood_conc_pred_winsorised),
                      weight = 0,
                      opacity = 0.7,
                      fillOpacity = 0.5)
      
    } else {
      
      proxy_lsoa  %>%
        
        setShapeStyle(layerId = ~fid, 
                      smoothFactor = 0,
                      fillColor = ~colour_pal_lsoa_perc(wood_perc_h),
                      weight = 0,
                      opacity = 0.7,
                      fillOpacity = 0.5)
      
    }
    
  })
  
  # Set up an event to zoom map to the selected lsoa in the user input
  observeEvent(input$selected_lsoa, {
    
    if(!is.null(input$lsoa_map_bounds) & input$selected_lsoa != "" & !is.null(input$selected_lsoa)) {
      
       if(!between(data_epc_lsoa_cross_section_to_map$long[data_epc_lsoa_cross_section_to_map$lsoa21nm == input$selected_lsoa],
                 input$lsoa_map_bounds[["west"]],
                 input$lsoa_map_bounds[["east"]]) |
        !between(data_epc_lsoa_cross_section_to_map$lat[data_epc_lsoa_cross_section_to_map$lsoa21nm == input$selected_lsoa],
                 input$lsoa_map_bounds[["south"]],
                 input$lsoa_map_bounds[["north"]])) {

         proxy_lsoa %>%
           
           flyTo(lng = data_epc_lsoa_cross_section_to_map$long[data_epc_lsoa_cross_section_to_map$lsoa21nm == input$selected_lsoa],
                 lat = data_epc_lsoa_cross_section_to_map$lat[data_epc_lsoa_cross_section_to_map$lsoa21nm == input$selected_lsoa],
                 zoom = 12)

       }
      
      proxy_lsoa %>%
        
        removeShape(layerId = "selected_lsoa_polygon") %>%
        
        addPolylines(data = data_epc_lsoa_cross_section_to_map[data_epc_lsoa_cross_section_to_map$lsoa21nm == input$selected_lsoa,],
                     color = "blue",
                     layerId = "selected_lsoa_polygon")
      
      }
    
  })
  
  # Update lsoa selection based on map click
  observeEvent(input$lsoa_map_shape_click, {
    
    event <- input$lsoa_map_shape_click
    
    lsoa <- data_epc_lsoa_cross_section_to_map$lsoa21nm[data_epc_lsoa_cross_section_to_map$fid == event$id]
    
    updateSelectizeInput(session,
                         "selected_lsoa",
                         selected = lsoa,
                         choices = data_epc_lsoa_cross_section_to_map$lsoa21nm,
                         server = TRUE) 
    
  })
  
  # Ward Level -----------------------------------------------------------------
  
  proxy_ward <- leafletProxy("ward_map", data = data_epc_ward_cross_section_to_map)
  
  # Set initial value for selectize input
  updateSelectizeInput(session, 
                       'selected_ward', 
                       choices = data_epc_ward_cross_section_to_map$wd22nm_cd,
                       selected = "Aldersgate E05009288",
                       server = TRUE)
  
  # Create text object to display ranking
  output$ward_text <- renderText({
    
    paste0(input$selected_ward," is ranked ", 
           data_epc_ward_cross_section_to_map$wood_conc_pred_rank[data_epc_ward_cross_section_to_map$wd22nm_cd == input$selected_ward], 
           " out of ", length(data_epc_ward_cross_section_to_map$objectid), 
           " Electoral Wards for concentration of wood fuel heat sources and ",
           data_epc_ward_cross_section_to_map$wood_perc_h_rank[data_epc_ward_cross_section_to_map$wd22nm_cd == input$selected_ward], 
           " for percentage of houses with wood fuel heat sources.",
           " The predicted concentration of wood fuel heat sources is ",
           round(data_epc_ward_cross_section_to_map$wood_conc_pred[data_epc_ward_cross_section_to_map$wd22nm_cd == input$selected_ward], 1),
           " per km2, and ",
           round(data_epc_ward_cross_section_to_map$wood_perc_h[data_epc_ward_cross_section_to_map$wd22nm_cd == input$selected_ward], 0),
           "% of homes have a wood fuel heat source.")
    
  })
  
  # Create base map
  output$ward_map <- renderLeaflet({
    
    leaflet(data_epc_ward_cross_section_to_map) %>%
      
      addTiles() %>%
      
      addMapPane("data", zIndex = 430) %>%
      
      addMapPane("sca", zIndex = 420) %>%
      
      # Add LA boundaries as polygons
      addPolygons(data = la_boundaries,
                  fillOpacity = 0,
                  color = "grey",
                  weight = 1) %>%
      
      addPolygons(data = sca_boundaries,
                  fillOpacity = 0.25,
                  color = "blue",
                  weight = 1,
                  group = "sca",
                  options = pathOptions(pane = "sca")) %>%
      
      addPolygons(data = data_epc_ward_cross_section_to_map,
                  smoothFactor = 0,
                  layerId = ~objectid,
                  fillColor = ~colour_pal_ward_conc(wood_conc_pred_winsorised),
                  weight = 0,
                  opacity = 0.7,
                  fillOpacity = 0.5,
                  group = "data",
                  options = pathOptions(pane = "data"))
    
  })
  
  # Observer event to display SCA boundaries
  observeEvent(input$show_sca_boundaries_ward, {
    
    if(input$show_sca_boundaries_ward == FALSE){
      
      proxy_ward %>%
        
        hideGroup("sca")
      
    } else {
      
      proxy_ward %>%
        
        showGroup("sca")
      
    }
    
  })
  
  # Set up event to change map fill based on user data selection
  observeEvent(input$selected_data_ward, {
    
    if(input$selected_data_ward == "Concentration"){
      
      proxy_ward %>%
        
        setShapeStyle(layerId = ~objectid, 
                      smoothFactor = 0,
                      fillColor = ~colour_pal_ward_conc(wood_conc_pred_winsorised),
                      weight = 0,
                      opacity = 0.7,
                      fillOpacity = 0.5)
        
      
    } else {
      
      proxy_ward %>%
        
        setShapeStyle(layerId = ~objectid, 
                      smoothFactor = 0,
                      fillColor = ~colour_pal_ward_perc(wood_perc_h),
                      weight = 0,
                      opacity = 0.7,
                      fillOpacity = 0.5)
      
      
    }
    
  })
  
  # Set up an event to zoom map to the selected ward in the user input
  observeEvent(input$selected_ward, {
    
    if(!is.null(input$ward_map_bounds) & input$selected_ward != "" & !is.null(input$selected_ward)) {
      
      if(!between(data_epc_ward_cross_section_to_map$long[data_epc_ward_cross_section_to_map$wd22nm_cd == input$selected_ward],
                  input$ward_map_bounds[["west"]],
                  input$ward_map_bounds[["east"]]) |
         !between(data_epc_ward_cross_section_to_map$lat[data_epc_ward_cross_section_to_map$wd22nm_cd == input$selected_ward],
                  input$ward_map_bounds[["south"]],
                  input$ward_map_bounds[["north"]])) {
    
    proxy_ward %>%
      
      flyTo(lng = data_epc_ward_cross_section_to_map$long[data_epc_ward_cross_section_to_map$wd22nm_cd == input$selected_ward],
            lat = data_epc_ward_cross_section_to_map$lat[data_epc_ward_cross_section_to_map$wd22nm_cd == input$selected_ward],
            zoom = 12)
        
      }
      
      proxy_ward %>%
        
        removeShape(layerId = "selected_ward_polygon") %>%
        
        addPolylines(data = data_epc_ward_cross_section_to_map[data_epc_ward_cross_section_to_map$wd22nm_cd == input$selected_ward,],
                     color = "blue",
                     layerId = "selected_ward_polygon")
      
    }
    
  })
  
  # Update ward selection based on map click
  observeEvent(input$ward_map_shape_click, {
    
    event <- input$ward_map_shape_click
    
    ward <- data_epc_ward_cross_section_to_map$wd22nm_cd[data_epc_ward_cross_section_to_map$objectid == event$id]
    
    updateSelectizeInput(session,
                         "selected_ward",
                         selected = ward,
                         choices = data_epc_ward_cross_section_to_map$wd22nm_cd,
                         server = TRUE) 
    
  })
  
  # LA level -------------------------------------------------------------------
  
  proxy_la <- leafletProxy("la_map", data = data_epc_la_cross_section_to_map)
  
  # Set initial value for selectize input
  updateSelectizeInput(session, 
                       'selected_la', 
                       choices = data_epc_la_cross_section_to_map$lad22nm,
                       selected = "City of London",
                       server = TRUE)
  
  # Create text object to display ranking
  output$la_text <- renderText({
    
    paste0(input$selected_la," is ranked ", 
           data_epc_la_cross_section_to_map$wood_conc_pred_rank[data_epc_la_cross_section_to_map$lad22nm == input$selected_la], 
           " out of ", length(data_epc_la_cross_section_to_map$fid), 
           " Local Authorities for concentration of wood fuel heat sources and ",
           data_epc_la_cross_section_to_map$wood_perc_h_rank[data_epc_la_cross_section_to_map$lad22nm == input$selected_la], 
           " for percentage of houses with wood fuel heat sources.",
           " The predicted concentration of wood fuel heat sources is ",
           round(data_epc_la_cross_section_to_map$wood_conc_pred[data_epc_la_cross_section_to_map$lad22nm == input$selected_la], 1),
           " per km2, and ",
           round(data_epc_la_cross_section_to_map$wood_perc_h[data_epc_la_cross_section_to_map$lad22nm == input$selected_la], 0),
           "% of homes have a wood fuel heat source.")
    
  })
  
  # Create base map
  output$la_map <- renderLeaflet({
    
    leaflet(data_epc_la_cross_section_to_map) %>%
      
      addTiles() %>%
      
      addMapPane("data", zIndex = 430) %>%
      
      addMapPane("sca", zIndex = 420) %>%
      
      # Add LA boundaries as polygons
      addPolygons(data = la_boundaries,
                  fillOpacity = 0,
                  color = "grey",
                  weight = 1) %>%
      
      addPolygons(data = sca_boundaries,
                  fillOpacity = 0.25,
                  color = "blue",
                  weight = 1,
                  group = "sca",
                  options = pathOptions(pane = "sca")) %>%
      
      addPolygons(data = data_epc_la_cross_section_to_map,
                  smoothFactor = 0,
                  layerId = ~fid,
                  fillColor = ~colour_pal_la_conc(wood_conc_pred_winsorised),
                  weight = 0,
                  opacity = 0.7,
                  fillOpacity = 0.5,
                  group = "data",
                  options = pathOptions(pane = "data"))
    
  })
  
  # Set up event to change map fill based on user data selection
  observeEvent(input$selected_data_la, {
    
    if(input$selected_data_la == "Concentration"){
      
      proxy_la %>%
        
        setShapeStyle(layerId = ~fid, 
                      smoothFactor = 0,
                      fillColor = ~colour_pal_la_conc(wood_conc_pred_winsorised),
                      weight = 0,
                      opacity = 0.7,
                      fillOpacity = 0.5)
      
    } else {
      
      proxy_la %>%
        
        setShapeStyle(layerId = ~fid, 
                      smoothFactor = 0,
                      fillColor = ~colour_pal_la_perc(wood_perc_h),
                      weight = 0,
                      opacity = 0.7,
                      fillOpacity = 0.5)
      
    }
    
  })
  
  # Observer event to display SCA boundaries
  observeEvent(input$show_sca_boundaries_la, {
    
    if(input$show_sca_boundaries_la == FALSE){
      
      proxy_la %>%
        
        hideGroup("sca")
      
    } else {
      
      proxy_la %>%
        
        showGroup("sca")
      
    }
    
  })
  
  # Set up an event to zoom map to the selected la in the user input
  observeEvent(input$selected_la, {
    
    if(!is.null(input$la_map_bounds) & input$selected_la != "" & !is.null(input$selected_la)) {
      
      if(!between(data_epc_la_cross_section_to_map$long[data_epc_la_cross_section_to_map$lad22nm == input$selected_la],
                  input$la_map_bounds[["west"]],
                  input$la_map_bounds[["east"]]) |
         !between(data_epc_la_cross_section_to_map$lat[data_epc_la_cross_section_to_map$lad22nm == input$selected_la],
                  input$la_map_bounds[["south"]],
                  input$la_map_bounds[["north"]])) {
    
    proxy_la %>%
      
      flyTo(lng = data_epc_la_cross_section_to_map$long[data_epc_la_cross_section_to_map$lad22nm == input$selected_la],
            lat = data_epc_la_cross_section_to_map$lat[data_epc_la_cross_section_to_map$lad22nm == input$selected_la],
            zoom = 12)
        
      }
      
      proxy_la %>%
        
        removeShape(layerId = "selected_la_polygon") %>%
        
        addPolylines(data = data_epc_la_cross_section_to_map[data_epc_la_cross_section_to_map$lad22nm == input$selected_la,],
                     color = "blue",
                     layerId = "selected_la_polygon")
  
    }
    
  })
  
  # Update la selection based on map click
  observeEvent(input$la_map_shape_click, {
    
    event <- input$la_map_shape_click
    
    la <- data_epc_la_cross_section_to_map$lad22nm[data_epc_la_cross_section_to_map$fid == event$id]
    
    updateSelectizeInput(session,
                         "selected_la",
                         selected = la,
                         choices = data_epc_la_cross_section_to_map$lad22nm,
                         server = TRUE) 
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)