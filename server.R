server <- function(input, output) {
  
  fComm <- reactive({
    req(codeInsee())
    
    fComm <- flux %>% filter(idcom == codeInsee())
    return(fComm)
  })
  
  codeInsee <- reactive({
    
    if(is.null(input$mymap_click)) return() # retourne un élément vide si on n'a pas encore cliqué sur la carte
    
    coords <- c(input$mymap_click$lng, input$mymap_click$lat)
    codeInsee <- comms %>% getCommCenter(coords)
    
    return(codeInsee)
  })
  
  proxy <- leafletProxy("mymap")
  
  observe({
    req(codeInsee())
    myComm <- comms %>% filter(INSEE_COM == codeInsee())
    
    # Ajout du marqueur
    proxy %>% 
      clearMarkers() %>% 
      addMarkers(data = myComm %>% st_centroid)
    
    # Ajoute le contour de la commune
    proxy %>% 
      clearShapes() %>% 
      addPolygons(data = myComm,
                  color = paletteCerema$secondaire$orange, 
                  weight = 1, 
                  smoothFactor = 0.5,
                  opacity = 1, 
                  fillOpacity = 0.3,
                  fillColor = paletteCerema$secondaire$orange,
                  highlightOptions = highlightOptions(color = paletteCerema$secondaire$orange, 
                                                      weight = 2,
                                                      fillOpacity = 0.1,
                                                      bringToFront = TRUE))
    
    # Aller vers la commune
    bb <- st_bbox(myComm)
    proxy %>%
      flyToBounds(lng1 = as.numeric(bb$xmin),
                  lat1 = as.numeric(bb$ymin),
                  lng2 = as.numeric(bb$xmax),
                  lat2 = as.numeric(bb$ymax))
  })
  
  observeEvent(input$communes, {
    codeInsee <- input$communes
    bb <- comms %>% filter(INSEE_COM == codeInsee) %>% st_bbox %>% as.numeric
    proxy %>% flyToBounds(lng1 = bb[1], lat1 = bb[2], lng2 = bb[3], lat2 = bb[4])
  })
  
  output$foo <- renderPrint({
    codeInsee()
  })
  
  output$mymap <- renderLeaflet({
    
    bb <- st_bbox(comms) %>% as.numeric
    
    leaflet() %>%
      addTiles(group = "OSM") %>%
      addProviderTiles(providers$CartoDB.PositronOnlyLabels, group = "Villes") %>% 
      addTiles("http://wxs.ign.fr/choisirgeoportail/wmts?REQUEST=GetTile&SERVICE=WMTS&VERSION=1.0.0&STYLE=normal&TILEMATRIXSET=PM&FORMAT=image/png&LAYER=GEOGRAPHICALGRIDSYSTEMS.PLANIGNV2&TILEMATRIX={z}&TILEROW={y}&TILECOL={x}",
               options = c(WMSTileOptions(tileSize = 256),
                           providerTileOptions(minZoom = 1, maxZoom = 15)),
               attribution='<a target="_blank" href="https://www.geoportail.gouv.fr/">Geoportail France</a>',
               group = "Plan IGN"
      ) %>%
      addTiles("http://wxs.ign.fr/choisirgeoportail/wmts?REQUEST=GetTile&SERVICE=WMTS&VERSION=1.0.0&STYLE=normal&TILEMATRIXSET=PM&FORMAT=image/jpeg&LAYER=ORTHOIMAGERY.ORTHOPHOTOS&TILEMATRIX={z}&TILEROW={y}&TILECOL={x}",
               options = c(WMSTileOptions(tileSize = 256),
                           providerTileOptions(minZoom = 1, maxZoom = 22)),
               attribution='<a target="_blank" href="https://www.geoportail.gouv.fr/">Geoportail France</a>',
               group = "Photo aérienne"
      ) %>%
      addLayersControl(baseGroups    = c("Photo aérienne", "Plan IGN", "OSM"),
                       overlayGroups = "Villes",
                       options       = layersControlOptions(collapsed = FALSE)) %>%
      fitBounds(lng1 = bb[1], lat1 = bb[2], lng2 = bb[3], lat2 = bb[4])
  })
  
  output$uiCommune <- renderUI({
    
    if(is.null(codeInsee())) return(tagList(icon("mouse-pointer"), "Cliquez sur la carte pour afficher les statistiques"))
    
    div(h3(fComm()$idcomtxt, glue("({fComm()$idcom})"), 
           style=glue("color:{paletteCerema$secondaire$orange};")), 
        tags$span(tags$span(fComm()$artcom0920, " %", 
                            style=glue("font-weight:700;
                                                color:white;
                                                background-color:{paletteCerema$secondaire$orange};
                                                padding:5px;
                                                border-radius:2px;
                                                margin-right:5px;")), 
                  " de surface artificialisée entre 2009 et 2020"),
        style="margin-bottom:20px;"
    )
  })
  
  output$streamAndTreemap <- renderUI({
    req(fComm())
    
    myStream <- flux %>% makeStream(codeInsee())
    
    div(
      div(
        tags$span(tags$span(format(fComm()$nafart0920, 
                                   big.mark=" ", 
                                   scientific = FALSE), 
                            " hectares", 
                            style=glue("font-weight:700;
                                               color:white;
                                               background-color:{paletteCerema$principale$bleu};
                                               padding:5px;
                                               border-radius:2px;
                                               margin-right:5px;")), 
                  " artificialisés entre 2009 et 2020"),
        myStream,
        style="margin-bottom:20px;"
      ),
      div(plotlyOutput("treemap")), 
      style="margin-bottom:20px;")
  })
  
  output$treemap <- renderPlotly({
    req(codeInsee())
    flux %>% makeTreemap(codeInsee())  
  })
}