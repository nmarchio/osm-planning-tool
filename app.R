
# global ----------------------------------------------------------------

# Packages
library(devtools)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(forcats)
library(sf)
library(sp)
library(osmdata)
library(rgdal)
library(mapview)

# library(raster)
# library(memoise)
# library(promises)
# library(future)
# plan(multiprocess)
# library(tidygraph)

url <- 'https://github.com/nmarchio/osm-planning-tool/blob/master/freetown.RData?raw=true'
download.file(url, destfile= "sle.RData", mode = "wb")
load("sle.RData")

# Boundary boxes
bbox_list <- list('C.K.G - Freetown, Sierra Leone' = c(-13.26239,-13.24243,8.474558,8.488714),
                  'Waterfront communities - Lagos, Nigeria' = c(3.3528302631,3.3917659856,6.4650446851,6.482144034),
                  'West Point - Monrovia, Liberia' = c(-10.8115950032,-10.7805516589,6.3192609536,6.3409750256),
                  'Malabon - Manila, Philippines' = c(120.9526113007,120.9720626272,14.6548062731,14.674809902),
                  'Canaan - Port-au-Prince, Haiti' = c(-72.3197132646,-72.2141325408, 18.6354273757,18.6765784905),
                  'Dharavi - Mumbai, India' = c(72.84803101,72.8654472637,19.0365929141,19.0488042035),
                  'Rocinha - Rio de Janeiro, Brasil' = c(-43.2558354349, -43.240807566,-22.9931642826,-22.9836546585),
                  'Ciudad Nezahualcóyotl - Ciudad de México, México' = c(-99.06799135, -98.96587437,19.36621323,19.4406311529),
                  'Orangi Town - Karachi, Pakistan' = c(66.9894659945, 67.019582407,24.9345039855,24.9640053108),
                  'Khayelitsha - Cape Town, South Africa' = c(18.6519186575,18.7157664391,-34.0667541339,-34.0367738578),
                  'Kibera - Nairobi, Kenya' = c(36.7700362138,36.8062182657,-1.318913533,-1.3031807865),
                  'Epworth - Harare, Zimbabwe' = c(31.12492652,31.1800024405,-17.92837714,-17.863677222),
                  'Ilala District - Dar-es-Salaam, Tanzania' = c(39.2139383738, 39.245485821, -6.8511294828,-6.8303274359),
                  'Old Fadama - Accra, Ghana' = c(-0.2295671004,-0.2182425436,5.5419797951,5.5563023039))

# Assemble coordinates into a bounding box SF dataframe
if(exists('bbpoly_df')){ rm(bbpoly_df) }
for (t in names(bbox_list) ){
  geometry <- st_bbox(c(xmin = bbox_list[[t]][1], xmax = bbox_list[[t]][2], ymax = bbox_list[[t]][3], ymin = bbox_list[[t]][4]), crs = st_crs(4326)) %>% st_as_sfc()
  geometry <- st_sf(name = t, geometry)
  if(exists('bbpoly_df')){ bbpoly_df <- rbind(bbpoly_df, geometry) } else { bbpoly_df <- geometry }
}

# OSM API Query Function
osm_query <- function(xmin_bound, ymin_bound, xmax_bound, ymax_bound){
  
  bbsearch <- st_bbox(c(xmin = xmin_bound,
                        xmax = xmax_bound, 
                        ymin = ymin_bound,
                        ymax = ymax_bound), crs = st_crs(4326)) %>% st_as_sfc()
  
  withProgress(message = 'Extracting vector layers . . . . . . .', value = .1, {
    
    # Initialize placeholder vector objects
    df = data.frame(osm_id=0, name='', vector_group = 'Amenities')
    empty_line <- st_sf(geometry = st_sfc(st_linestring(rbind(c(xmin_bound,ymin_bound), c(xmin_bound+.0001,ymin_bound-.0001)))), crs = st_crs(4326))
    empty_poly <- st_sf(geometry = st_sfc(st_polygon(list(rbind(c(xmin_bound,ymin_bound), c(xmin_bound+.0001,ymin_bound),c(xmin_bound+.0001,ymin_bound+.0001), c(xmin_bound,ymin_bound+.0001),c(xmin_bound,ymin_bound)))  )), crs = st_crs(4326))
    
    osm_points_vectors <- df %>% mutate(lat = as.numeric(xmin_bound), lon = as.numeric(ymin_bound)) %>%
      add_row(osm_id=0, name='', vector_group = 'Amenities', lat = xmin_bound, lon = ymax_bound) %>% st_as_sf(coords = c("lon", "lat"), crs = 4326) 
    osm_lines_vectors <- st_sf(rbind(data.frame(df %>% mutate(vector_group = 'Streets'), empty_line),
                                     data.frame(df %>% mutate(vector_group = 'Barriers'), empty_line))) 
    osm_polygons_vectors <- st_sf(rbind(data.frame(df %>% mutate(vector_group = 'Amenities'), empty_poly),
                                        data.frame(df %>% mutate(vector_group = 'Barriers'), empty_poly)))
    osm_polygons_buildings <- st_sf(data.frame(df %>% mutate(vector_group = 'Buildings'), empty_poly) )
    
    # Download all spatial features with a name field
    incProgress(detail = '10% complete')
    try(osm_full <- opq(bbox = c(xmin_bound, ymin_bound, xmax_bound, ymax_bound)) %>%
          add_osm_feature(key = 'name') %>%
          osmdata_sf())
    incProgress(0.1, detail = '25% complete')
    
    # Download Linestrings
    #future_line <- future({
    withProgress(message = 'Extracting linestrings . . . . . . . . .', value = .1, {
      incProgress(0.1, detail = 'Ingesting networks')
      try(osm_lines_vectors <- osm_full %>%
            pluck("osm_lines") %>% 
            st_intersection(x = ., y = bbsearch) %>% 
            mutate(vector_group = case_when(!is.na(highway) | grepl("street|road|path|route", tolower(.$name)) ~ "Streets",
                                            !is.na(waterway) | !is.na(natural) | grepl("rail", tolower(.$name)) ~ 'Barriers',
                                            is.na(.$name) ~ 'Other line',
                                            TRUE ~ as.character('Other line')))  %>% 
            filter(vector_group != 'Other line') %>%
            select(one_of(c("osm_id","name","vector_group"))) %>% 
            rbind(.,osm_lines_vectors))
      incProgress(0.1, detail = 'Ingesting streets')
      try(osm_lines_vectors <- opq(bbox = c(xmin_bound, ymin_bound, xmax_bound, ymax_bound)) %>%
            add_osm_feature(key = 'highway') %>%
            osmdata_sf() %>%
            pluck("osm_lines") %>% 
            filter(!(osm_id %in% unique(osm_lines_vectors$osm_id))) %>%
            st_intersection(x = ., y = bbsearch) %>%
            mutate(vector_group = 'Streets') %>%
            select(one_of(c("osm_id","name", "vector_group"))) %>%
            rbind(.,osm_lines_vectors))
      incProgress(0.1, detail = 'Ingesting waterways')
      try(osm_lines_vectors <- opq(bbox = c(xmin_bound, ymin_bound, xmax_bound, ymax_bound)) %>%
            add_osm_feature(key = 'waterway') %>%
            osmdata_sf() %>%
            pluck("osm_lines") %>%
            filter(!(osm_id %in% unique(osm_lines_vectors$osm_id))) %>%
            st_intersection(x = ., y = bbsearch) %>%
            mutate(name = case_when(!is.na(name) ~ name,
                                    TRUE ~ as.character('Waterway')),
                   vector_group = 'Barriers') %>%
            select(one_of(c("osm_id","name", "vector_group"))) %>%
            rbind(.,osm_lines_vectors))
      incProgress(0.1, detail = 'Ingesting natural barriers')
      try(osm_lines_vectors <- opq(bbox = c(xmin_bound, ymin_bound, xmax_bound, ymax_bound)) %>%
            add_osm_feature(key = 'natural') %>%
            osmdata_sf() %>%
            pluck("osm_lines") %>%
            st_intersection(x = ., y = bbsearch) %>%
            mutate(osm_id = '', 
                   name = 'Natural',
                   vector_group = 'Barriers') %>%
            select(one_of(c("osm_id","name", "vector_group"))) %>%
            rbind(.,osm_lines_vectors))
      incProgress(0.1, detail = 'Ingesting power lines')
      try(osm_lines_vectors <- opq(bbox = c(xmin_bound, ymin_bound, xmax_bound, ymax_bound)) %>%
            add_osm_feature(key = 'power') %>%
            osmdata_sf() %>%
            pluck("osm_lines") %>%
            st_intersection(x=. , y=bbsearch) %>%
            mutate(osm_id = '',
                   name = 'Power line',
                   vector_group = 'Barriers') %>%
            select(one_of(c("osm_id","name","vector_group")))%>%
            rbind(.,osm_lines_vectors))
      incProgress(0.1, detail = 'Extraction successful')
    })
    #return(osm_lines_vectors)})
    
    incProgress(0.2, detail = '50% complete')
    
    # Download Footprints
    #future_footprint <- future({
    withProgress(message = 'Extracting building polygons . . .', value = .1, {
      incProgress(0.4, detail = 'Ingesting building footprints')
      try(osm_polygons_buildings <- opq(bbox = c(xmin_bound, ymin_bound, xmax_bound, ymax_bound)) %>%
            add_osm_feature(key = 'building') %>%
            osmdata_sf() %>%
            pluck("osm_polygons") %>%
            st_convex_hull() %>% 
            st_difference() %>% 
            st_intersection(x=.,y= bbsearch) %>%
            mutate(vector_group = 'Buildings')%>%
            select(one_of(c("osm_id","name", "vector_group")))  %>%
            rbind(.,osm_polygons_buildings))
      incProgress(0.9, detail = 'Extraction successful')
    })
    #return(osm_polygons_buildings)})
    incProgress(0.2, detail = '75% complete')
    
    # Download Polygons
    #future_polygon <- future({
    withProgress(message = 'Extracting polygons . . . . . . . .', value = .1, {
      incProgress(0.1, detail = 'Ingesting landuse polygons')
      try(osm_polygons_vectors <- osm_full %>%
            pluck("osm_polygons") %>% 
            st_intersection(x = ., y = bbsearch) %>% 
            mutate(vector_group = case_when(grepl('dumpsite|cemetery|landfill|trash|hazard|garbage|brownfield|military', tolower(.$name)) ~ "Barriers",
                                            is.na(.$name) ~ 'Amenities',
                                            TRUE ~ as.character('Amenities'))) %>%
            select(one_of(c("osm_id","name","vector_group"))) %>%
            rbind(.,osm_polygons_vectors))
      incProgress(0.1, detail = 'Segmenting landuse polygons')
      try(osm_polygons_vectors <- opq(bbox = c(xmin_bound, ymin_bound, xmax_bound, ymax_bound)) %>%
            add_osm_feature(key = 'landuse') %>%
            osmdata_sf() %>%
            pluck("osm_polygons") %>%
            filter(!(landuse %in% c('residential','commercial')),
                   !(osm_id %in% unique(osm_polygons_vectors$osm_id))) %>%
            st_intersection(x=. , y=bbsearch) %>%
            mutate(name = case_when(is.na(name) ~ landuse,
                                    TRUE ~ as.character(name)),
                   vector_group =  case_when(grepl('dumpsite|cemetery|landfill|trash|hazard|garbage|brownfield|military', tolower(name)) ~ "Barriers",
                                             TRUE ~ as.character('Amenities'))) %>%
            select(one_of(c("osm_id","name","vector_group"))) %>%
            rbind(.,osm_polygons_vectors))
      incProgress(0.1, detail = 'Ingesting amenities polygons')
      try(osm_polygons_vectors <- opq(bbox = c(xmin_bound, ymin_bound, xmax_bound, ymax_bound)) %>%
            add_osm_feature(key = 'amenity') %>%
            osmdata_sf()   %>%
            pluck("osm_polygons") %>%
            filter(!(osm_id %in% unique(osm_polygons_vectors$osm_id))) %>%
            st_intersection(x=.,y= bbsearch) %>%
            mutate(vector_group =  'Amenities') %>%
            select(one_of(c("osm_id","name","vector_group")))  %>%
            rbind(.,osm_polygons_vectors))
      incProgress(0.1, detail = 'Cleaning up landuse polygons')
      try(osm_polygons_vectors <- osm_polygons_vectors %>%
            mutate(area = round(as.numeric(st_area(geometry)/st_area(bbsearch)),3)) %>%
            filter(area <= .5) %>%
            select(one_of(c("osm_id","name","vector_group"))))
      incProgress(0.1, detail = 'Extraction successful')
    })
    #return(osm_polygons_vectors)})
    incProgress(0.2, detail = '95% complete')
    
    # Download Points
    #future_point <- future({
    withProgress(message = 'Extracting points . . . . . . . . .', value = .1, {
      incProgress(0.4, detail = 'Ingesting amenities points')
      amenity_corpus <- 'hotel|hostel|shop|gym|bakery|station|toilet|bathroom|restroom|bus|house|bath|clinic|market|lodge|embassy|farm|bank|ymca|headquarter|water|vocation|boutique|art |gallery|food|space|society|hospital|center|church|school|restaurant|primary|secondary|hq'
      try(osm_points_vectors <- osm_full %>%
            pluck("osm_points") %>% 
            st_intersection(x = ., y = bbsearch) %>%
            mutate(vector_group = case_when(!is.na(amenity) | grepl(amenity_corpus, tolower(.$name)) ~ 'Amenities',
                                            TRUE ~ as.character('Other point'))) %>%
            filter(vector_group != 'Other point') %>%
            select(one_of(c("osm_id","name","vector_group"))) %>%
            rbind(.,osm_points_vectors))
      incProgress(0.6, detail = 'Extraction successful')
    })
    #return(osm_points_vectors)})
    
    # osm_lines_streets <-future_line %...>% filter(vector_group == 'Streets')
    # osm_lines_barriers <-  future_line %...>% filter(vector_group == 'Barriers')
    # osm_polygons_barriers <- future_footprint %...>% filter(vector_group == 'Barriers')
    # osm_polygons_amenities <- future_polygon %...>% filter(vector_group == 'Amenities')
    # osm_points_amenities <- future_point %...>% filter(vector_group == 'Amenities')
    
    try(osm_lines_streets <- osm_lines_vectors %>% filter(vector_group == 'Streets'))
    try(osm_lines_barriers <-  osm_lines_vectors %>% filter(vector_group == 'Barriers'))
    try(osm_polygons_barriers <- osm_polygons_vectors %>% filter(vector_group == 'Barriers'))
    try(osm_polygons_amenities <- osm_polygons_vectors %>% filter(vector_group == 'Amenities'))
    try(osm_points_amenities <- osm_points_vectors %>% filter(vector_group == 'Amenities'))
    
    osm_sf_list <- list('osm_polygons_buildings' = osm_polygons_buildings,
                        'osm_lines_streets' = osm_lines_streets,
                        'osm_lines_barriers' =  osm_lines_barriers,
                        'osm_polygons_barriers' = osm_polygons_barriers,
                        'osm_polygons_amenities' = osm_polygons_amenities,
                        'osm_points_amenities' = osm_points_amenities )
    
    incProgress(0.1, detail = 'Vector layer extraction successful')
  })
  
  return(osm_sf_list)
}

# Memoise / cache results when same inputs occurr
osm_query_cache <- memoise(osm_query)

# Running a Shiny app object
app <- shinyApp(
  
  ui <- fluidPage(
    useShinyjs(), 
    leafletOutput("leafmap", height = "90vh"),
    column(2, br(), actionButton("actionOSM", "Add Layers", value = TRUE, icon = icon("vector-square")) ),
    column(2, br(), actionButton("actionReblock", "Run Reblock", value = TRUE, icon = icon("th")) ),
    column(5, br(), downloadButton('downloadData', 'Download .kml') )
  ),

  #server <- function(input, output, session) {
  server <- function(input, output) {
    
    # Load initial data
    disable("actionReblock")
    disable("downloadData")
    disable("actionOSM")
    #osmRV <- reactiveValues(osm_vectors = osm_query(xmin_bound=bbox_list[[1]][1], ymin_bound = bbox_list[[1]][3], xmax_bound = bbox_list[[1]][2], ymax_bound = bbox_list[[1]][4]))
    osmRV <- reactiveValues(osm_vectors = osm_vectors)
    bbRV <- reactiveValues(osm_view = c(bbox_list[[1]][1], bbox_list[[1]][3], bbox_list[[1]][2], bbox_list[[1]][4]))
    drawRV <- reactiveValues(draw_output = NULL)
    enable("actionReblock")
    enable("actionOSM")
    
    # Read in OSM vector data
    observeEvent(input$actionOSM, {
      disable("actionOSM")
      disable("actionReblock")
      disable("downloadData")
      osmRV$osm_vectors <- osm_query_cache(xmin_bound = input$leafmap_bounds$west,
                                           ymin_bound = input$leafmap_bounds$south,
                                           xmax_bound = input$leafmap_bounds$east,
                                           ymax_bound = input$leafmap_bounds$north)
      
      bbRV$osm_view <- c(input$leafmap_bounds$west,
                         input$leafmap_bounds$south,
                         input$leafmap_bounds$east,
                         input$leafmap_bounds$north)
      
      enable("actionOSM")
      enable("actionReblock")
      enable("downloadData")
      #zoomRV$osm_zoom <- st_centroid((st_bbox(c(xmin = input$leafmap_bounds$west, xmax = input$leafmap_bounds$east, ymax = input$leafmap_bounds$north, ymin = input$leafmap_bounds$south), crs = st_crs(4326)) %>% st_as_sfc())))
    })
    
    # Activate Reblocking algorithm
    observeEvent(input$actionReblock, {
      disable("actionOSM")
      disable("actionReblock")
      disable("downloadData")
      Sys.sleep(2)
      enable("actionOSM")
      enable("actionReblock")
      enable("downloadData")
    })
    
    isolate(osmRV$osm_vectors)
    isolate(bbRV$osm_view)
    
    # Render Leaflet map
    output$leafmap <- renderLeaflet({
      withProgress(message = 'Rendering map', value = .4, {
        leafobject <- leaflet() %>%
          #setView(zoomRV$osm_zoom[1], zoomRV$osm_zoom[2], zoom = 16) %>%
          addHomeButton(ext = raster::extent(bbpoly_df$geometry[14] %>% as_Spatial()), layer.name = paste0(bbpoly_df$name[14])) %>%
          addHomeButton(ext = raster::extent(bbpoly_df$geometry[13] %>% as_Spatial()), layer.name = paste0(bbpoly_df$name[13])) %>%
          addHomeButton(ext = raster::extent(bbpoly_df$geometry[12] %>% as_Spatial()), layer.name = paste0(bbpoly_df$name[12])) %>%
          addHomeButton(ext = raster::extent(bbpoly_df$geometry[11] %>% as_Spatial()), layer.name = paste0(bbpoly_df$name[11])) %>%
          addHomeButton(ext = raster::extent(bbpoly_df$geometry[10] %>% as_Spatial()), layer.name = paste0(bbpoly_df$name[10])) %>%
          addHomeButton(ext = raster::extent(bbpoly_df$geometry[9] %>% as_Spatial()), layer.name = paste0(bbpoly_df$name[9])) %>%
          addHomeButton(ext = raster::extent(bbpoly_df$geometry[8] %>% as_Spatial()), layer.name = paste0(bbpoly_df$name[8])) %>%
          addHomeButton(ext = raster::extent(bbpoly_df$geometry[7] %>% as_Spatial()), layer.name = paste0(bbpoly_df$name[7])) %>%
          addHomeButton(ext = raster::extent(bbpoly_df$geometry[6] %>% as_Spatial()), layer.name = paste0(bbpoly_df$name[6])) %>%
          addHomeButton(ext = raster::extent(bbpoly_df$geometry[5] %>% as_Spatial()), layer.name = paste0(bbpoly_df$name[5])) %>%
          addHomeButton(ext = raster::extent(bbpoly_df$geometry[4] %>% as_Spatial()), layer.name = paste0(bbpoly_df$name[4])) %>%
          addHomeButton(ext = raster::extent(bbpoly_df$geometry[3] %>% as_Spatial()), layer.name = paste0(bbpoly_df$name[3])) %>%
          addHomeButton(ext = raster::extent(bbpoly_df$geometry[2] %>% as_Spatial()), layer.name = paste0(bbpoly_df$name[2])) %>%
          addHomeButton(ext = raster::extent(bbpoly_df$geometry[1] %>% as_Spatial()), layer.name = paste0(bbpoly_df$name[1])) %>%
          addTiles(group = "OSM",options = providerTileOptions(minZoom = 15, maxZoom = 18)) %>%
          addProviderTiles(provider = "Hydda.Full", group = "Color",options = providerTileOptions(minZoom = 15, maxZoom = 18)) %>%
          addProviderTiles(provider = "Esri.WorldTopoMap", group = "Topography", options = providerTileOptions(minZoom = 15, maxZoom = 18)) %>%
          addProviderTiles(provider ='Esri.WorldImagery', group = "Satellite", options = providerTileOptions(minZoom = 15, maxZoom = 18)) %>%
          addProviderTiles(provider ="CartoDB.PositronNoLabels", group = "Positron",options = providerTileOptions(minZoom = 15, maxZoom = 18)) %>%
          addDrawToolbar(targetGroup = "drawnPoly", rectangleOptions = F, markerOptions = F, circleOptions=F,
                         editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()), 
                         circleMarkerOptions = drawCircleMarkerOptions(color = "green"),
                         polylineOptions = drawPolylineOptions(shapeOptions = drawShapeOptions(color = 'green', fillOpacity = 0, weight = 4, opacity = .6)),
                         polygonOptions = drawPolygonOptions(showArea=TRUE, repeatMode=F, shapeOptions=drawShapeOptions(color = 'green', fillColor="green", clickable = TRUE))) %>%
          addLayersControl(baseGroups = c("Positron","Color","Satellite","Topography","OSM"),
                           options = layersControlOptions(collapsed = TRUE),
                           position = "bottomleft",
                           overlayGroups = c("Barriers", "Streets", "Buildings", "Amenities")) %>%
          addPolygons(data = osmRV$osm_vectors$osm_polygons_buildings, weight = 1, color = "black", group = "Buildings") %>%
          addPolylines(data = osmRV$osm_vectors$osm_lines_barriers, label = ~name, color = "red", group = "Barriers") %>%
          addPolygons(data = osmRV$osm_vectors$osm_polygons_barriers, label = ~name, weight = 1, color = "red", group = "Barriers") %>%
          addPolylines(data = osmRV$osm_vectors$osm_lines_streets, label = ~name, color = 'purple', highlight = highlightOptions(color = "green"), group = "Streets") %>%
          addPolygons(data = osmRV$osm_vectors$osm_polygons_amenities, label = ~name, weight = 1, color = "blue", group = "Amenities") %>%
          addCircleMarkers(data = osmRV$osm_vectors$osm_points_amenities, label = ~name, radius = 5, weight =1 , fillOpacity = 0.5, stroke = FALSE, color = "blue", group = "Amenities") %>%
          fitBounds(bbRV$osm_view[1],bbRV$osm_view[2],bbRV$osm_view[3],bbRV$osm_view[4]) 
        
        incProgress(0.9, detail = 'Map processing')
      })
      leafobject
    })
    
    # Add new vector features
    observeEvent(input$leafmap_draw_new_feature,{
      feature <- input$leafmap_draw_new_feature
      if(feature$geometry$type == "Point") {
        gis_new <-st_point(matrix(unlist(feature$geometry$coordinates),ncol=2,byrow=TRUE))}
      if(feature$geometry$type == "LineString") {
        gis_new <-st_linestring(matrix(unlist(feature$geometry$coordinates),ncol=2,byrow=TRUE)) }
      if(feature$geometry$type == "Polygon") {
        gis_new <- st_polygon(list(matrix(unlist(feature$geometry$coordinates),ncol=2,byrow=TRUE))) }
      if(exists('geomcollection_output') ){
        geomcollection_input <- st_sf(id=feature$properties$`_leaflet_id`,
                                      type=feature$geometry$type, 
                                      label='label', 
                                      geometry = st_sfc(gis_new),
                                      crs = st_crs(4326))
        geomcollection_output <<- rbind(geomcollection_output, geomcollection_input)
        enable("downloadData")
        #drawRV$draw_output <- geomcollection_output
      } else {
        geomcollection_output <<- st_sf(id=feature$properties$`_leaflet_id`,
                                        type=feature$geometry$type, 
                                        label='label', 
                                        geometry = st_sfc(gis_new),
                                        crs = st_crs(4326))
        enable("downloadData")
        #drawRV$draw_output <- geomcollection_output
      }
    })
    
    # Modify existing vector features
    observeEvent(input$leafmap_draw_edited_features, {
      print("Edited Features")
      feature_edit <- input$leafmap_draw_edited_features
      print(input$leafmap_draw_edited_features)
      for(i in seq_along(feature_edit$features)) {
        if(feature_edit$features[[i]]$geometry$type == "Point") {
          gis_edit <- st_point(matrix(unlist(feature_edit$features[[i]]$geometry$coordinates),ncol=2,byrow=TRUE)) }
        if(feature_edit$features[[i]]$geometry$type== "LineString") {
          gis_edit <- st_linestring(matrix(unlist(feature_edit$features[[i]]$geometry$coordinates),ncol=2,byrow=TRUE)) }
        if(feature_edit$features[[i]]$geometry$type == "Polygon") {
          gis_edit <- st_polygon(list(matrix(unlist(feature_edit$features[[i]]$geometry$coordinates),ncol=2,byrow=TRUE))) }
        if(exists('geomcollection_fulledit') ){
          geomcollection_edit <- st_sf(id=feature_edit$features[[i]]$properties$`_leaflet_id`,
                                       type=feature_edit$features[[i]]$geometry$type,
                                       label='label', 
                                       geometry = st_sfc(gis_edit),
                                       crs = st_crs(4326))
          geomcollection_fulledit <<- rbind(geomcollection_fulledit, geomcollection_edit)
        } else {
          geomcollection_fulledit <<- st_sf(id=feature_edit$features[[i]]$properties$`_leaflet_id`,
                                            type=feature_edit$features[[i]]$geometry$type,
                                            label='label', 
                                            geometry = st_sfc(gis_edit),
                                            crs = st_crs(4326))
        }
      }
      geomcollection_output[match(geomcollection_fulledit$id, geomcollection_output$id), 2:4] <<- geomcollection_fulledit[2:4]
      enable("downloadData")
      #drawRV$draw_output <- geomcollection_output
    })
    
    # Delete vector features
    observeEvent(input$leafmap_draw_deleted_features, {
      print("Deleted Features")
      feature_delete <- input$leafmap_draw_deleted_features
      deleted_id_list <- c()
      for(i in seq_along(feature_delete$features)) {
        deleted_id_list <- c(deleted_id_list, feature_delete$features[[i]]$properties$`_leaflet_id`)
      }
      geomcollection_output <<- geomcollection_output %>% filter(!id %in% deleted_id_list)
      if(nrow(geomcollection_output) == 0) {disable("downloadData")}
      #drawRV$draw_output <- geomcollection_output
    })
    
    # Download user-created vectors to a KML file
    output$downloadData<-downloadHandler(
      filename = 'reblockExport.zip',
      content = function(file) {
        if (length(Sys.glob("reblockExport.*"))>0){
          file.remove(Sys.glob("reblockExport.*"))
        }
        st_write(st_cast(geomcollection_output, "GEOMETRYCOLLECTION"), "reblockExport.kml")
        #req(drawRV())
        #st_write(st_cast(drawRV$draw_output, "GEOMETRYCOLLECTION"), "reblockExport.kml")
        zip(zipfile='reblockExport.zip', files=Sys.glob("reblockExport.*"))
        file.copy("reblockExport.zip", file)
        if (length(Sys.glob("reblockExport.*"))>0){
          file.remove(Sys.glob("reblockExport.*"))
        }
      }
    )
  }
)

#runApp(app)
