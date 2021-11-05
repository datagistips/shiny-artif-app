library(shiny)
library(tidyverse)
library(sf)
library(glue)
library(streamgraph)
library(rjson)
library(leaflet)

# Flux sur PACA
flux <- read_csv("data/obs_artif_conso_com_2009_2020_V2.csv", na = c("", "NULL")) %>% 
    filter(idreg == "93")

# Contours de communes
comms <<- readRDS("data/comms.rds") %>% 
    filter(INSEE_REG == 93)

# Liste communes
communes <- flux$idcom
names(communes) <- glue("{flux$idcomtxt} ({flux$idcom})")

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
            selectInput("communes", label = NULL, choices = communes, selected = NULL),
            leafletOutput("mymap")
        ),

        mainPanel(
            verbatimTextOutput("foo"),
            uiOutput("uiCommune"),
            uiOutput("streamPlot")
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
    
    fComm <- reactive({
        fComm <- flux %>% filter(idcom == input$communes)
        return(fComm)
    })
    
    output$foo <- renderPrint({
        input$mymap_click
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

    output$streamPlot <- renderUI({
        
        codeInsee <- input$communes
        myStream <- flux %>% makeStream(codeInsee)
        
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
            div(
                tags$span("Habitat", style = glue("background-color:{myPalette['blue']};padding:10px;")),
                tags$span("Activité", style = glue("background-color:{myPalette['red']};padding:10px;")),
                tags$span("Mixte", style = glue("background-color:{myPalette['magenta']};padding:10px;")),
                tags$span("Inconnu", style = glue("background-color:{myPalette['grey']};padding:10px;")),
                style="text-align:center"
            ))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
