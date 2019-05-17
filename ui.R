# ui.R ----------------------------------------------------------

ui <- fluidPage(
  useShinyjs(), 
  leafletOutput("leafmap", height = "90vh"),
  column(2, br(),
         actionButton("actionOSM", "Add Layers", value = TRUE, icon = icon("vector-square")) ),
  column(2, br(),
         actionButton("actionReblock", "Run Reblock", value = TRUE, icon = icon("th")) ), #play-circle
  column(5, br(),
         downloadButton('downloadData', 'Download .kml') )
)
