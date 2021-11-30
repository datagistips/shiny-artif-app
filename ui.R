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
  ),
  hr(),
  tags$div(
    includeMarkdown("footer.md"), 
    style="font-size:0.8em")
)