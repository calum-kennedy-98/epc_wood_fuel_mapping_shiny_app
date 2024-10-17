### Name: app.R
### Author: Calum Kennedy
### Date created: 26-09-24
### Last update: 26-09-24

# Description ------------------------------------------------------------------

# Code to produce interactive R Shiny dashboard of the concentration and prevalence
# of EPCs with wood burning appliances by various geographies

# Source script to prepare data for app ----------------------------------------

if(!require(pacman)){install.packages("pacman")}

p_load(here)

source(here("Helpers/PrepareDataForShinyApp.R"))

# Define UI --------------------------------------------------------------------

ui <- page_fillable(
  
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
                  weight = 1) 
    
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
                    fillColor = ~colour_pal_lsoa_perc(wood_perc_h),
                    weight = 0,
                    opacity = 0.7,
                    fillOpacity = 0.5,
                    popup = ~paste(lsoa21nm),
                    layerId = ~fid)
      
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
  
  # Set up global colour palette and proxy to update map based on user input
  colour_pal_ward_conc <- colorNumeric(palette = "inferno",
                                       domain = data_epc_ward_cross_section_to_map$wood_conc_pred,
                                       reverse = TRUE)
  
  colour_pal_ward_perc <- colorNumeric(palette = "inferno",
                                       domain = data_epc_ward_cross_section_to_map$wood_perc_h,
                                       reverse = TRUE)
  
  proxy_ward <- leafletProxy("ward_map")
  
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
                  weight = 1) %>%
      
      addPolygons(data = data_epc_ward_cross_section_to_map,
                  smoothFactor = 0,
                  fillColor = ~colour_pal_ward_conc(wood_conc_pred),
                  weight = 0,
                  opacity = 0.7,
                  fillOpacity = 0.5,
                  popup = ~paste(wd22nm_cd), 
                  layerId = ~objectid)
    
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
  
  # Set up global colour palettes and proxy to update map based on user input
  colour_pal_la_conc <- colorNumeric(palette = "inferno",
                                     domain = data_epc_la_cross_section_to_map$wood_conc_pred,
                                     reverse = TRUE)
  
  colour_pal_la_perc <- colorNumeric(palette = "inferno",
                                     domain = data_epc_la_cross_section_to_map$wood_perc_h,
                                     reverse = TRUE)
  
  proxy_la <- leafletProxy("la_map")
  
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
                  weight = 1) %>%
      
      # Add polygons and colour by concentration of EPCs with wood burning heat sources
      addPolygons(data = data_epc_la_cross_section_to_map,
                  smoothFactor = 0,
                  fillColor = ~colour_pal_la_conc(wood_conc_pred),
                  weight = 0,
                  opacity = 0.7,
                  fillOpacity = 0.5,
                  popup = ~paste(lad22nm), 
                  layerId = ~fid)
    
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
                    popup = ~paste(lad22nm), 
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