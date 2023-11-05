#' langApp
#'
#' Loads a langApp
#'
#' @param ... this param is for other unspecified params.
#' 
#' @import readr
#' @import dplyr
#' @import purrr
#' @import usethis
#' @import devtools
#' @import testthat
#' @import shiny
#' @import leaflet
#' @import plotly
#' @import DT
#' 
#' @export
#' 
#'
#' @author Krzysztof Solowiej <krsolowiej@gmail.com>


langApp <- function(...) {

# data <- read_csv("./dataset/endangered_languages.csv")
data <- read_csv("https://raw.githubusercontent.com/KrzysztofSolowiej/langApp/master/dataset/endangered_languages.csv")

# Define UI 
ui <- fluidPage(
  titlePanel("Endangered languages table"),
  sidebarPanel(
    
    checkboxGroupInput(
      inputId = "degree",
      label = "Select degree of endangerment", 
      choices = unique(data$`Degree of endangerment`),
      selected = unique(data$`Degree of endangerment`),
      inline = TRUE
    ),
    
    sliderInput('speak_num',  'Select min number of speakers',
                min = 0, max = 7500000, value = 0
    )
  ),
  tabsetPanel(id='my_tabsetPanel',
              tabPanel('Table',
                       DT::DTOutput('lang_table') 
              ),
              tabPanel('Map',
                       leafletOutput('map')   
              )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$lang_table <- DT::renderDT({
    data %>% 
      filter(`Degree of endangerment` == input$degree) %>%
      filter(`Number of speakers` >= input$speak_num)
  })
  
  
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      setView(-93.65, 42.0285, zoom = 6)
  })
  
  observe({
    
    input$my_tabsetPanel
    
    tab1 <- leafletProxy('map', data = data
                         # %>% filter(`Degree of endangerment` == input$degree)
                         # %>% filter(`Number of speakers` >= input$speak_num)
    ) %>%
      
      clearMarkers() %>% 
      addCircleMarkers(lng = ~Longitude, lat = ~Latitude, radius = ~sqrt(sqrt(`Number of speakers`)), popup = ~ `Description of the location`) %>%
      addMarkers(lng = ~Longitude, lat = ~Latitude, label = ~`Name in English`)
    
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server, ...)
}
