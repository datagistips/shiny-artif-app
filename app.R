library(shiny)
library(tidyverse)
library(sf)
library(glue)

# Flux sur PACA
flux <- read_csv("data/obs_artif_conso_com_2009_2020_V2.csv", na = c("", "NULL")) %>% 
    filter(idreg == "93")

# Contours de communes
comms <<- readRDS("data/comms.rds") %>% 
    filter(INSEE_REG == 93)

# Liste communes
communes <- flux$idcom
names(communes) <- glue("{flux$idcomtxt} ({flux$idcom})")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Artificialisation de 2009 à 2020"),

    # Sidebar
    sidebarLayout(
        sidebarPanel(
            selectInput("communes", label = NULL, choices = communes, selected = NULL),
        ),

        # Show a plot of the generated distribution
        mainPanel(
          dataTableOutput("tbResults")
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

    output$tbResults <- renderDataTable({
        codeInsee <- input$communes
        df <- flux %>% getStatsFlux(codeInsee)
        return(df)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
