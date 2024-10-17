### Name: app_conc_london.R
### Author: Calum Kennedy
### Date created: 26-09-24
### Last update: 26-09-24

# Description ------------------------------------------------------------------

# Code to produce interactive R Shiny dashboard of the concentration of EPCs with
# wood burning appliances by various geographies. The app includes a choropleth map
# and a line chart showing the change in prevalence over the period 2009-2024.
# The map and line chart automatically update when a new Ward is selected. At the moment
# I am mapping London only, but can expand to other cities at some point

# Comments ---------------------------------------------------------------------

# Setup ------------------------------------------------------------------------

# Install packages using pacman
if(!require(pacman)){install.packages("pacman")}

p_load(shiny,
       targets,
       stringr,
       vroom,
       here,
       ggplot2,
       leaflet,
       sf,
       janitor,
       dplyr,
       bslib,
       rmapshaper)

# Source necessary scripts -----------------------------------------------------

source(here("Scripts/GetMappingBoundaries.R"))
source(here("Scripts/UtilityFunctions.R"))
source(here("Scripts/LoadEnv.R"))

# Define percentiles for winsorising -------------------------------------------

lower_perc <- 0.05
upper_perc = 0.95

# Load necessary data ----------------------------------------------------------

# Map boundary data (LA level)
la_boundaries <- get_mapping_boundaries("lad22cd")

# Data for mapping -------------------------------------------------------------

tar_load(data_epc_lsoa_cross_section_to_map)
tar_load(data_epc_ward_cross_section_to_map)
tar_load(data_epc_la_cross_section_to_map)

# Simplify mapping data using rmapshaper ---------------------------------------

# LSOA data
data_epc_lsoa_cross_section_to_map <- data_epc_lsoa_cross_section_to_map %>%
  
  select(lsoa21nm, geometry, wood_perc_h, wood_conc_pred, fid, lat, long) %>%
  
  mutate(wood_conc_pred_rank = dense_rank(desc(wood_conc_pred)),
         wood_perc_h_rank = dense_rank(desc(wood_perc_h))) %>%
  
  mutate(wood_conc_pred = case_when(wood_conc_pred > get_percentile(wood_conc_pred, upper_perc) ~ get_percentile(wood_conc_pred, upper_perc),
                                                wood_conc_pred < get_percentile(wood_conc_pred, lower_perc) ~ get_percentile(wood_conc_pred, lower_perc),
                                                .default = wood_conc_pred)) %>%
  
  ms_simplify(keep = 0.25)

# Ward data
data_epc_ward_cross_section_to_map <- data_epc_ward_cross_section_to_map %>%
  
  select(wd22nm, wd22cd, geometry, wood_perc_h, wood_conc_pred, objectid, lat, long) %>%
  
  mutate(wood_conc_pred_rank = dense_rank(desc(wood_conc_pred)),
         wood_perc_h_rank = dense_rank(desc(wood_perc_h))) %>%
  
  mutate(wood_conc_pred = case_when(wood_conc_pred > get_percentile(wood_conc_pred, upper_perc) ~ get_percentile(wood_conc_pred, upper_perc),
                               wood_conc_pred < get_percentile(wood_conc_pred, lower_perc) ~ get_percentile(wood_conc_pred, lower_perc),
                               .default = wood_conc_pred)) %>%
  
  mutate(wd22nm_cd = paste(wd22nm, wd22cd, sep = " ")) %>%
  
  ms_simplify(keep = 0.5)

# LA data
data_epc_la_cross_section_to_map <- data_epc_la_cross_section_to_map %>%
  
  select(lad22nm, geometry, wood_perc_h, wood_conc_pred, fid, lat, long) %>%
  
  mutate(wood_conc_pred_rank = dense_rank(desc(wood_conc_pred)),
         wood_perc_h_rank = dense_rank(desc(wood_perc_h))) %>%
  
  mutate(wood_conc_pred = case_when(wood_conc_pred > get_percentile(wood_conc_pred, upper_perc) ~ get_percentile(wood_conc_pred, upper_perc),
                                    wood_conc_pred < get_percentile(wood_conc_pred, lower_perc) ~ get_percentile(wood_conc_pred, lower_perc),
                                    .default = wood_conc_pred)) %>%
  
  ms_simplify(keep = 0.75)

# Define UI --------------------------------------------------------------------

ui <- page_fillable(
  
  # Create navigation panes
  navset_card_tab(
    
    # Set full_screen to TRUE
    full_screen = TRUE,
    
    # Set main title
    title = h1(strong("Predicted concentration of wood fuel\nheat sources using EPCs and Census data"),
               style = "font-size:18px;"),
    
    # Create navigation panel for LSOA
    nav_panel("LSOA",
              
              # Create sidebar layout within navigation panel
              layout_sidebar(
                
                # Generate sidebar
                sidebar = sidebar(
                  
                  # Selectize input for LSOA
                  selectizeInput(
                    inputId = "selected_lsoa",
                    label = "Select LSOA",
                    choices = NULL
                ),
                
                # Select input for data type
                selectInput(
                  inputId = "selected_data_lsoa",
                  label = "Select data",
                  choices = c("Concentration",
                            "Prevalence")
                ),
                
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
                  
                  # Selectize input for Electoral Ward
                  selectizeInput(
                    inputId = "selected_ward",
                    label = "Select Ward",
                    choices = NULL
                  ),
                  
                  # Select input for data type
                  selectInput(
                    inputId = "selected_data_ward",
                    label = "Select data",
                    choices = c("Concentration",
                                "Prevalence")
                  ),
                  
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
                
                # Selectize input for Local Authority
                selectizeInput(
                  inputId = "selected_la",
                  label = "Select Local Authority",
                  choices = NULL
                ),
                
                # Select input for data type
                selectInput(
                  inputId = "selected_data_la",
                  label = "Select data",
                  choices = c("Concentration",
                              "Prevalence")
                ),
                
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
  
  # Set up global colour palette and proxy to update map based on user input
  colour_pal_lsoa_conc <- colorNumeric(palette = "inferno",
                                  domain = data_epc_lsoa_cross_section_to_map$wood_conc_pred,
                                  reverse = TRUE)
  
  colour_pal_lsoa_perc <- colorNumeric(palette = "inferno",
                                       domain = data_epc_lsoa_cross_section_to_map$wood_perc_h,
                                       reverse = TRUE)
  
  proxy_lsoa <- leafletProxy("lsoa_map")
  
  # Set up a reactive value for lsoa to use to filter map/plot
  lsoa_reactive <- reactiveVal()
  
  # Set an initial value for lsoa reactive so chart doesn't begin blank
  lsoa_reactive("City of London 001A")
  
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
         " LSOAs for concentration of wood fuel heat sources per km2 and ",
         data_epc_lsoa_cross_section_to_map$wood_perc_h_rank[data_epc_lsoa_cross_section_to_map$lsoa21nm == input$selected_lsoa], 
         " out of ", length(data_epc_lsoa_cross_section_to_map$fid),
         " LSOAs for percentage of houses with wood fuel heat sources")
    
  })
  
  # Create base map of concentration by LSOA
  output$lsoa_map <- renderLeaflet({
    
    leaflet(data_epc_lsoa_cross_section_to_map) %>%
      
      addTiles() %>%
      
      # Add LA boundaries as polygons
      addPolygons(data = la_boundaries,
                  fillOpacity = 0,
                  color = "grey",
                  weight = 1) %>%
      
      # Add lsoa polygons and colour by concentration of EPCs with wood burning heat sources
      addPolygons(data = data_epc_lsoa_cross_section_to_map,
                  smoothFactor = 0,
                  fillColor = ~colour_pal_lsoa_conc(wood_conc_pred),
                  weight = 0,
                  opacity = 0.7,
                  fillOpacity = 0.5,
                  popup = ~paste(lsoa21nm),
                  layerId = ~fid)
    
  })
  
  # Set up event to change map display based on user data selection
  observeEvent(input$selected_data_lsoa, {
    
    if(input$selected_data_lsoa == "Concentration"){
      
      proxy_lsoa %>%
        
        clearShapes() %>%
        
        # Add lsoa polygons and colour by concentration of EPCs with wood burning heat sources
        addPolygons(data = data_epc_lsoa_cross_section_to_map,
                    smoothFactor = 0,
                    fillColor = ~colour_pal_lsoa_conc(wood_conc_pred),
                    weight = 0,
                    opacity = 0.7,
                    fillOpacity = 0.5,
                    popup = ~paste(lsoa21nm),
                    layerId = ~fid)
      
    } else {
      
      proxy_lsoa %>%
        
        clearShapes() %>%
        
        # Add lsoa polygons and colour by concentration of EPCs with wood burning heat sources
        addPolygons(data = data_epc_lsoa_cross_section_to_map,
                    smoothFactor = 0,
                    fillColor = ~colour_pal_lsoa_conc(wood_perc_h),
                    weight = 0,
                    opacity = 0.7,
                    fillOpacity = 0.5,
                    popup = ~paste(lsoa21nm),
                    layerId = ~fid)
      
    }
    
  })
  
  # Set up an event to zoom map to the selected lsoa in the user input
  observeEvent(input$selected_lsoa, {
    
    lsoa_reactive(input$selected_lsoa) 
    
    proxy_lsoa %>%
      
      flyTo(lng = data_epc_lsoa_cross_section_to_map$long[data_epc_lsoa_cross_section_to_map$lsoa21nm == input$selected_lsoa],
            lat = data_epc_lsoa_cross_section_to_map$lat[data_epc_lsoa_cross_section_to_map$lsoa21nm == input$selected_lsoa],
            zoom = 10)
    
  })
  
  # Update lsoa selection based on map click
  observeEvent(input$lsoa_map_shape_click, {
    
    event <- input$lsoa_map_shape_click
    
    lsoa <- data_epc_lsoa_cross_section_to_map$lsoa21nm[data_epc_lsoa_cross_section_to_map$fid == event$id]
    
    lsoa_reactive(lsoa) 
    
  })
  
  # Ward Level -----------------------------------------------------------------
  
  # Set up global colour palette and proxy to update map based on user input
  colour_pal_ward_conc <- colorNumeric(palette = "inferno",
                                      domain = data_epc_ward_cross_section_to_map$wood_conc_pred,
                                      reverse = TRUE)
  
  colour_pal_ward_perc <- colorNumeric(palette = "inferno",
                                       domain = data_epc_ward_cross_section_to_map$wood_perc_h,
                                       reverse = TRUE)
  
  proxy_ward <- leafletProxy("ward_map")
  
  # Set up a reactive value for ward to use to filter map/plot
  ward_reactive <- reactiveVal()
  
  # Set an initial value for Ward reactive so chart doesn't begin blank
  ward_reactive("Waddon E05011487")
  
  # Set initial value for selectize input
  updateSelectizeInput(session, 
                       'selected_ward', 
                       choices = data_epc_ward_cross_section_to_map$wd22nm_cd,
                       selected = "Waddon E05011487",
                       server = TRUE)
  
  # Create text object to display ranking
  output$ward_text <- renderText({
    
    paste0(input$selected_ward," is ranked ", 
           data_epc_ward_cross_section_to_map$wood_conc_pred_rank[data_epc_ward_cross_section_to_map$wd22nm_cd == input$selected_ward], 
           " out of ", length(data_epc_ward_cross_section_to_map$objectid), 
           " Electoral Wards for concentration of wood fuel heat sources per km2 and ",
           data_epc_ward_cross_section_to_map$wood_perc_h_rank[data_epc_ward_cross_section_to_map$wd22nm_cd == input$selected_ward], 
           " out of ", length(data_epc_ward_cross_section_to_map$objectid),
           " Electoral Wards for percentage of houses with wood fuel heat sources")
    
  })
  
  # Create base map
  output$ward_map <- renderLeaflet({
    
    leaflet(data_epc_ward_cross_section_to_map) %>%
      
      addTiles() %>%
      
      # Add LA boundaries as polygons
      addPolygons(data = la_boundaries,
                  fillOpacity = 0,
                  color = "grey",
                  weight = 1)
    
  })
  
  # Set up event to change map fill based on user data selection
  observeEvent(input$selected_data_ward, {
    
    if(input$selected_data_ward == "Concentration"){
      
      proxy_ward %>%
        
        # Clear previous polygons
        clearShapes() %>%
        
        # Add polygons and colour by concentration of EPCs with wood burning heat sources
        addPolygons(data = data_epc_ward_cross_section_to_map,
                    smoothFactor = 0,
                    fillColor = ~colour_pal_ward_conc(wood_conc_pred),
                    weight = 0,
                    opacity = 0.7,
                    fillOpacity = 0.5,
                    popup = ~paste(wd22nm_cd), 
                    layerId = ~objectid)
      
    } else {
      
      proxy_ward %>%
        
        # Clear previous polygons
        clearShapes() %>%
        
        # Add polygons and colour by concentration of EPCs with wood burning heat sources
        addPolygons(data = data_epc_ward_cross_section_to_map,
                    smoothFactor = 0,
                    fillColor = ~colour_pal_ward_perc(wood_perc_h),
                    weight = 0,
                    opacity = 0.7,
                    fillOpacity = 0.5,
                    popup = ~paste(wd22nm_cd),
                    layerId = ~objectid)
      
    }
    
  })
  
  # Set up an event to zoom map to the selected ward in the user input
  observeEvent(input$selected_ward, {
    
    ward_reactive(input$selected_ward)
    
    proxy_ward %>%
      
      flyTo(lng = data_epc_ward_cross_section_to_map$long[data_epc_ward_cross_section_to_map$wd22nm_cd == input$selected_ward],
            lat = data_epc_ward_cross_section_to_map$lat[data_epc_ward_cross_section_to_map$wd22nm_cd == input$selected_ward],
            zoom = 10)
    
  })
  
  # Update ward selection based on map click
  observeEvent(input$ward_map_shape_click, {
    
    event <- input$ward_map_shape_click
    
    ward <- data_epc_ward_cross_section_to_map$wd22nm_cd[data_epc_ward_cross_section_to_map$objectid == event$id]
    
    ward_reactive(ward) 
    
  })
  
  # LA level -------------------------------------------------------------------
  
  # Set up global colour palettes and proxy to update map based on user input
  colour_pal_la_conc <- colorNumeric(palette = "inferno",
                                domain = data_epc_la_cross_section_to_map$wood_conc_pred,
                                reverse = TRUE)
  
  colour_pal_la_perc <- colorNumeric(palette = "inferno",
                                     domain = data_epc_la_cross_section_to_map$wood_perc_h,
                                     reverse = TRUE)
  
  proxy_la <- leafletProxy("la_map")
  
  # Set up a reactive value for la to use to filter map/plot
  la_reactive <- reactiveVal()
  
  # Set an initial value for la reactive so chart doesn't begin blank
  la_reactive("Haringey")
  
  # Set initial value for selectize input
  updateSelectizeInput(session, 
                       'selected_la', 
                       choices = data_epc_la_cross_section_to_map$lad22nm,
                       selected = "Haringey",
                       server = TRUE)
  
  # Create text object to display ranking
  output$la_text <- renderText({
    
    paste0(input$selected_la," is ranked ", 
           data_epc_la_cross_section_to_map$wood_conc_pred_rank[data_epc_la_cross_section_to_map$lad22nm == input$selected_la], 
           " out of ", length(data_epc_la_cross_section_to_map$fid), 
           " Local Authorities for concentration of wood fuel heat sources per km2 and ",
           data_epc_la_cross_section_to_map$wood_perc_h_rank[data_epc_la_cross_section_to_map$lad22nm == input$selected_la], 
           " out of ", length(data_epc_la_cross_section_to_map$fid),
           " Local Authorities for percentage of houses with wood fuel heat sources")
    
  })
  
  # Create base map
  output$la_map <- renderLeaflet({
    
    leaflet(data_epc_la_cross_section_to_map) %>%
      
      addTiles() %>%
      
      # Add LA boundaries as polygons
      addPolygons(data = la_boundaries,
                  fillOpacity = 0,
                  color = "grey",
                  weight = 1)
    
  })
  
  # Set up event to change data input based on click
  observeEvent(input$selected_data_la, {
    
    if(input$selected_data_la == "Concentration"){
      
      proxy_la %>%
        
        # Clear previous polygons
        clearShapes() %>%
        
        # Add polygons and colour by concentration of EPCs with wood burning heat sources
        addPolygons(data = data_epc_la_cross_section_to_map,
                    smoothFactor = 0,
                    fillColor = ~colour_pal_la_conc(wood_conc_pred),
                    weight = 0,
                    opacity = 0.7,
                    fillOpacity = 0.5,
                    popup = ~paste(lad22nm), # Add popup here with rank?
                    layerId = ~fid)
      
    } else {
      
      proxy_la %>%
        
        # Clear previous polygons
        clearShapes() %>%
        
        # Add polygons and colour by concentration of EPCs with wood burning heat sources
        addPolygons(data = data_epc_la_cross_section_to_map,
                    smoothFactor = 0,
                    fillColor = ~colour_pal_la_perc(wood_perc_h),
                    weight = 0,
                    opacity = 0.7,
                    fillOpacity = 0.5,
                    popup = ~paste(lad22nm),
                    layerId = ~fid)
      
    }
    
  })
  
  # Set up an event to zoom map to the selected la in the user input
  observeEvent(input$selected_la, {
    
    la_reactive(input$selected_la)
    
    proxy_la %>%
      
      flyTo(lng = data_epc_la_cross_section_to_map$long[data_epc_la_cross_section_to_map$lad22nm == input$selected_la],
            lat = data_epc_la_cross_section_to_map$lat[data_epc_la_cross_section_to_map$lad22nm == input$selected_la],
            zoom = 10)
    
  })
  
  # Update la selection based on map click
  observeEvent(input$la_map_shape_click, {
    
    event <- input$la_map_shape_click
    
    la <- data_epc_la_cross_section_to_map$lad22nm[data_epc_la_cross_section_to_map$fid == event$id]
    
    la_reactive(la) 
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
