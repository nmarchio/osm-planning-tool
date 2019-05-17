
# server.R ------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Load initial data
  disable("actionReblock")
  disable("downloadData")
  disable("actionOSM")
  osmRV <- reactiveValues(osm_vectors = osm_query(xmin_bound=xmin_1, ymin_bound = ymin_1, xmax_bound = xmax_1, ymax_bound = ymax_1))
  bbRV <- reactiveValues(osm_view = c(xmin_1, ymin_1, xmax_1, ymax_1))
  drawRV <- reactiveValues(draw_output = NULL)
  enable("actionReblock")
  enable("actionOSM")
  
  # Read in OSM vector data
  observeEvent(input$actionOSM, {
    disable("actionOSM")
    disable("actionReblock")
    disable("downloadData")
    osmRV$osm_vectors <- osm_query(xmin_bound = input$leafmap_bounds$west,
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
  #isolate(drawRV$draw_output)
  
  # Render Leaflet map
  output$leafmap <- renderLeaflet({
    withProgress(message = 'Rendering map', value = .4, {
      leafobject <- leaflet() %>%
        #setView(zoomRV$osm_zoom[1], zoomRV$osm_zoom[2], zoom = 16) %>%
        addHomeButton(ext = raster::extent(bbpoly_14 %>% as_Spatial()), layer.name = "Old Fadama - Accra, Ghana") %>%
        addHomeButton(ext = raster::extent(bbpoly_13 %>% as_Spatial()), layer.name = "Ilala District - Dar-es-Salaam, Tanzania") %>%
        addHomeButton(ext = raster::extent(bbpoly_12 %>% as_Spatial()), layer.name = "Epworth - Harare, Zimbabwe") %>%
        addHomeButton(ext = raster::extent(bbpoly_11 %>% as_Spatial()), layer.name = "Kibera - Nairobi, Kenya") %>%
        addHomeButton(ext = raster::extent(bbpoly_10 %>% as_Spatial()), layer.name = "Khayelitsha - Cape Town, South Africa") %>%
        addHomeButton(ext = raster::extent(bbpoly_8 %>% as_Spatial()), layer.name = "Ciudad Nezahualcóyotl - Ciudad de México, México") %>%
        addHomeButton(ext = raster::extent(bbpoly_7 %>% as_Spatial()), layer.name = "Rocinha - Rio de Janeiro, Brasil") %>%
        addHomeButton(ext = raster::extent(bbpoly_5 %>% as_Spatial()), layer.name = "Canaan - Port-au-Prince, Haiti") %>%
        addHomeButton(ext = raster::extent(bbpoly_9 %>% as_Spatial()), layer.name = "Orangi Town - Karachi, Pakistan") %>%
        addHomeButton(ext = raster::extent(bbpoly_6 %>% as_Spatial()), layer.name = "Dharavi - Mumbai, India") %>%
        addHomeButton(ext = raster::extent(bbpoly_4 %>% as_Spatial()), layer.name = "Malabon - Manila, Pilipinas") %>%
        addHomeButton(ext = raster::extent(bbpoly_3 %>% as_Spatial()), layer.name = "West Point - Monrovia, Liberia") %>%
        addHomeButton(ext = raster::extent(bbpoly_2 %>% as_Spatial()), layer.name = "Waterfront communities - Lagos, Nigeria") %>%
        addHomeButton(ext = raster::extent(bbpoly_1 %>% as_Spatial()), layer.name = "C.K.G. - Freetown, Sierra Leone") %>%
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

shinyApp(ui, server)
