library(shiny)
library(tidyverse)
library(sf)
library(glue)
library(streamgraph)
library(rjson)
library(leaflet)
library(plotly)

# Flux sur PACA
flux <- read_csv("data/obs_artif_conso_com_2009_2020_V2.csv", na = c("", "NULL")) %>% 
    filter(idreg == "93")

# Contours de communes
comms <<- readRDS("data/comms.rds") %>% 
    filter(INSEE_REG == 93)

# Liste communes
communes <- flux$idcom
names(communes) <- glue("{flux$idcomtxt} ({flux$idcom})")
communes <- communes[order(names(communes))]
communes2 <- c("", communes)
names(communes2) <- c("Aller vers une commune", names(communes))

# Palette 
colorBlue    <- hcl(h = 220, c = 50, l = 80, fixup = TRUE)
colorRed     <- hcl(h = 4, c = 50, l = 80, fixup = TRUE)
colorMagenta <- hcl(h = 300, c = 50, l = 80, fixup = TRUE)
colorGrey    <- hcl(h = 0, c = 0, l = 80, fixup = TRUE)

myPalette <<- c("blue" = colorBlue,
                "red" = colorRed,
                "magenta" = colorMagenta,
                "grey" = colorGrey)

# Palette de couleurs
paletteCerema <- fromJSON(file = "palette_cerema.json")

ui <- fluidPage(

    # Application title
    titlePanel("Artificialisation de 2009 à 2020"),

    # Sidebar
    sidebarLayout(
        sidebarPanel(
            selectInput("communes", label = NULL, choices = communes2, selected = NULL),
            leafletOutput("mymap")
        ),

        mainPanel(
            # verbatimTextOutput("foo"),
            uiOutput("uiCommune"),
            uiOutput("streamAndTreemap")
        )
    )
)

server <- function(input, output) {
    
    getStatsFlux <- function(flux, codeInsee) {
        
        myCols <-  names(flux)[grep("^art[0-9]{2}\\S+[0-9]{2}$", names(flux))]
        
        # Filtre par commune
        df <- flux %>% filter(idcom == codeInsee) 
        
        # Colonnes intéressantes
        df <- df[, c("idcom", "idcomtxt", myCols)]
        
        # Long format
        df <- df %>% gather("variable", # key
                            "value",    # value
                            myCols) # variables
        
        # Année et type
        df$year <- gsub("art([0-9]{2})(\\S+)[0-9]{2}", "20\\1", df$variable)
        df$type <- gsub("art([0-9]{2})(\\S+)[0-9]{2}", "\\2", df$variable)
        
        # Renomme les valeurs de type
        df <- df %>% mutate(type = case_when(
            type == "hab" ~ "Habitat",
            type == "act" ~ "Activité",
            type == "mix" ~ "Mixte",
            type == "inc" ~ "Inconnu"))
        
        # Réagence les colonnes
        df <- df[, c("idcom", "idcomtxt", "year", "type", "value")]
        
        return(df)
    }
    
    makeStream <- function(flux, codeInsee) {
        
        df <- flux %>% getStatsFlux(codeInsee)
        
        # L'ordre des inverse dans les streamgraphs
        df$type <- factor(df$type, levels = c("Inconnu", "Mixte", "Activité", "Habitat"))
        
        # Plot
        df %>%
            streamgraph("type", "value", "year", sort = FALSE, height = '350px') %>%
            sg_axis_x(1, "Année", "%Y") %>% 
            sg_fill_manual(rev(myPalette))
    }
    
    makeTreemap <- function(flux, codeInsee) {
        df <- flux %>% filter(idcom == codeInsee)
        
        df <- df %>% gather("variable",
                            "value",
                            c("arthab0920", "artact0920", "artmix0920", "artinc0920"))
        
        df$variable <- case_when(
            df$variable == "arthab0920" ~ "Habitat",
            df$variable == "artact0920" ~ "Activité",
            df$variable == "artmix0920" ~ "Mixte",
            df$variable == "artinc0920" ~ "NC"
        )
        
        # Par. treemap
        labels = df$variable
        parents = rep("", nrow(df))
        values = df$value
        
        fig <- plot_ly(
            type="treemap",
            labels=labels,
            parents=parents,
            values=values,
            marker=list(colors = myPalette))
        
        fig
    }
    
    fComm <- reactive({
        req(codeInsee())
        
        fComm <- flux %>% filter(idcom == codeInsee())
        return(fComm)
    })
    
    codeInsee <- reactive({
        
        if(is.null(input$mymap_click)) return() # retourne un élément vide si on n'a pas encore cliqué sur la carte
        
        getCommCenter <- function(comms, coords) {
            pt <- coords %>% st_point %>% st_sfc %>% st_set_crs(4326)
            i <- st_intersects(comms, pt)
            w <- which(sapply(i, function(x) length(x) != 0))
            codeInsee <- comms$INSEE_COM[w]
            return(codeInsee)
        }
        
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

# Run the application 
shinyApp(ui = ui, server = server)
