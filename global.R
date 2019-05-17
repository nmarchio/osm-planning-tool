
# global.R ----------------------------------------------------------------

# Packages
library(osmdata)
library(leaflet)
library(leaflet.extras)
library(sp)
library(sf)
library(tidyverse)
library(forcats)
library(shiny)
library(shinydashboard)
library(rgdal)
library(mapview)
library(shinyjs)

# C.K.G - Freetown, Sierra Leone
xmin_1 = -13.26239
xmax_1 = -13.24243
ymin_1 = 8.474558
ymax_1 = 8.488714
bbpoly_1 <- st_bbox(c(xmin = xmin_1, xmax = xmax_1, ymax = ymax_1, ymin = ymin_1), crs = st_crs(4326)) %>% st_as_sfc()

# Waterfront communities - Lagos, Nigeria 
xmin_2 = 3.3528302631
xmax_2 = 3.3917659856
ymin_2 = 6.4650446851
ymax_2 = 6.482144034
bbpoly_2 <- st_bbox(c(xmin = xmin_2, xmax = xmax_2, ymax = ymax_2, ymin = ymin_2), crs = st_crs(4326)) %>% st_as_sfc()

# West Point - Monrovia, Liberia
xmin_3 = -10.8115950032
xmax_3 = -10.7805516589
ymin_3 = 6.3192609536
ymax_3 = 6.3409750256
bbpoly_3 <- st_bbox(c(xmin = xmin_3, xmax = xmax_3, ymax = ymax_3, ymin = ymin_3), crs = st_crs(4326)) %>% st_as_sfc()

# Malabon - Manila, Phillipines
xmin_4 = 120.9526113007
xmax_4 = 120.9720626272
ymin_4 = 14.6548062731
ymax_4 = 14.674809902
bbpoly_4 <- st_bbox(c(xmin = xmin_4, xmax = xmax_4, ymax = ymax_4, ymin = ymin_4), crs = st_crs(4326)) %>% st_as_sfc()

# Canaan - Port-au-Prince, Haiti
xmin_5 = -72.3197132646
xmax_5 = -72.2141325408
ymin_5 = 18.6354273757
ymax_5 = 18.6765784905
bbpoly_5 <- st_bbox(c(xmin = xmin_5, xmax = xmax_5, ymax = ymax_5, ymin = ymin_5), crs = st_crs(4326)) %>% st_as_sfc()

# Dharavi - Mumbai, India
xmin_6 = 72.84803101
xmax_6 = 72.8654472637
ymin_6 = 19.0365929141
ymax_6 = 19.0488042035
bbpoly_6 <- st_bbox(c(xmin = xmin_6, xmax = xmax_6, ymax = ymax_6, ymin = ymin_6), crs = st_crs(4326)) %>% st_as_sfc()

# Rocinha - Rio de Janeiro, Brasil
xmin_7 = -43.2558354349
xmax_7 = -43.240807566
ymin_7 = -22.9931642826
ymax_7 = -22.9836546585
bbpoly_7 <- st_bbox(c(xmin = xmin_7, xmax = xmax_7, ymax = ymax_7, ymin = ymin_7), crs = st_crs(4326)) %>% st_as_sfc()

# Ciudad Nezahualcóyotl - Ciudad de México, México
xmin_8 = -99.06799135
xmax_8 = -98.96587437
ymin_8 = 19.36621323
ymax_8 = 19.4406311529
bbpoly_8 <- st_bbox(c(xmin = xmin_8, xmax = xmax_8, ymax = ymax_8, ymin = ymin_8), crs = st_crs(4326)) %>% st_as_sfc()

# Orangi Town - Karachi, Pakistan
xmin_9 = 66.9894659945
xmax_9 = 67.019582407
ymin_9 = 24.9345039855
ymax_9 = 24.9640053108
bbpoly_9 <- st_bbox(c(xmin = xmin_9, xmax = xmax_9, ymax = ymax_9, ymin = ymin_9), crs = st_crs(4326)) %>% st_as_sfc()

# Khayelitsha - Cape Town, South Africa
xmin_10 = 18.6519186575
xmax_10 = 18.7157664391
ymin_10 = -34.0667541339
ymax_10 = -34.0367738578
bbpoly_10 <- st_bbox(c(xmin = xmin_10, xmax = xmax_10, ymax = ymax_10, ymin = ymin_10), crs = st_crs(4326)) %>% st_as_sfc()

# Kibera - Nairobi, Kenya
xmin_11 = 36.7700362138
xmax_11 = 36.8062182657
ymin_11 = -1.318913533
ymax_11 = -1.3031807865
bbpoly_11 <- st_bbox(c(xmin = xmin_11, xmax = xmax_11, ymax = ymax_11, ymin = ymin_11), crs = st_crs(4326)) %>% st_as_sfc()

# Epworth - Harare, Zimbabwe
xmin_12 = 31.12492652
xmax_12 = 31.1800024405
ymin_12 = -17.92837714
ymax_12 = -17.863677222
bbpoly_12 <- st_bbox(c(xmin = xmin_12, xmax = xmax_12, ymax = ymax_12, ymin = ymin_12), crs = st_crs(4326)) %>% st_as_sfc()

# Ilala District - Dar-es-Salaam, Tanzania
xmin_13 = 39.2139383738
xmax_13 = 39.245485821
ymin_13 = -6.8511294828
ymax_13 = -6.8303274359
bbpoly_13 <- st_bbox(c(xmin = xmin_13, xmax = xmax_13, ymax = ymax_13, ymin = ymin_13), crs = st_crs(4326)) %>% st_as_sfc()

# Old Fadama - Accra, Ghana
xmin_14 = -0.2295671004
xmax_14 = -0.2182425436
ymin_14 = 5.5419797951
ymax_14 = 5.5563023039
bbpoly_14 <- st_bbox(c(xmin = xmin_14, xmax = xmax_14, ymax = ymax_14, ymin = ymin_14), crs = st_crs(4326)) %>% st_as_sfc()

# OSM API Query
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
    
    incProgress(detail = '10% complete')
    try(osm_full <- opq(bbox = c(xmin_bound, ymin_bound, xmax_bound, ymax_bound)) %>%
          add_osm_feature(key = 'name') %>%
          osmdata_sf())
    incProgress(0.1, detail = '25% complete')
    withProgress(message = 'Extracting linestrings . . . . . . . . .', value = .1, {
      incProgress(0.1, detail = 'Ingesting networks')
      # Download Linestrings
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
      #plot(osm_lines_vectors)
      
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
      #plot(osm_lines_vectors)
      
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
      
      #plot(osm_lines_vectors)
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
      #plot(osm_lines_vectors)
      incProgress(0.1, detail = 'Extraction successful')
    })
    
    # Download Footprints
    incProgress(0.2, detail = '50% complete')
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
      #plot(osm_polygons_buildings)
      incProgress(0.9, detail = 'Extraction successful')
    })
    
    incProgress(0.2, detail = '75% complete')
    # Download Polygons
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
      #plot(osm_polygons_vectors)
      
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
      #plot(osm_polygons_vectors)
      
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
      #plot(osm_polygons_vectors)
      
      incProgress(0.1, detail = 'Cleaning up landuse polygons')
      try(osm_polygons_vectors <- osm_polygons_vectors %>%
            mutate(area = round(as.numeric(st_area(geometry)/st_area(bbsearch)),3)) %>%
            filter(area <= .5) %>%
            select(one_of(c("osm_id","name","vector_group"))))
      #plot(osm_polygons_vectors)
      incProgress(0.1, detail = 'Extraction successful')
    })
    incProgress(0.2, detail = '95% complete')
    # Donwload Points
    withProgress(message = 'Finishing up . . . . . . . . .', value = .1, {
      incProgress(0.1, detail = 'Refining data')
      amenity_corpus <- 'hotel|hostel|shop|gym|bakery|station|toilet|bathroom|restroom|bus|house|bath|clinic|market|lodge|embassy|farm|bank|ymca|headquarter|water|vocation|boutique|art |gallery|food|space|society|hospital|center|church|school|restaurant|primary|secondary|hq'
      try(osm_points_vectors <- osm_full %>%
            pluck("osm_points") %>% 
            st_intersection(x = ., y = bbsearch) %>%
            mutate(vector_group = case_when(!is.na(amenity) | grepl(amenity_corpus, tolower(.$name)) ~ 'Amenities',
                                            TRUE ~ as.character('Other point'))) %>%
            filter(vector_group != 'Other point') %>%
            select(one_of(c("osm_id","name","vector_group"))) %>%
            rbind(.,osm_points_vectors))
      
      incProgress(0.1, detail = 'Validating geometries')
      
      try(osm_lines_streets <- osm_lines_vectors %>% filter(vector_group == 'Streets'))
      try(osm_lines_barriers <-  osm_lines_vectors %>% filter(vector_group == 'Barriers'))
      try(osm_polygons_barriers <- osm_polygons_vectors %>% filter(vector_group == 'Barriers'))
      try(osm_polygons_amenities <- osm_polygons_vectors %>% filter(vector_group == 'Amenities'))
      try(osm_points_amenities <- osm_points_vectors %>% filter(vector_group == 'Amenities'))
      
      incProgress(0.1, detail = 'Finishing up')
      
    })
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
