library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggthemes)
library(shinythemes)


gs4_auth(
  cache = ".token",
  email ="sabrinalpedersen@gmail.com"
)


ui <- dashboardPage(title = "Titel i browseren", skin = "blue",
                    dashboardHeader(title = "Titel i øverste venstre hjørne"),
                    
                    dashboardSidebar(collapsed = TRUE,
                                     sidebarMenu(
                                       menuItem("Menu 1", tabName = "dashboard", icon = icon("virus")),
                                       menuItem(tagList(shiny::icon("linkedin"),"LinkedIn"), href = "https://www.linkedin.com/in/simon-eilersen/"))),
                    
                    dashboardBody(
                      fluidRow(
                        valueBoxOutput(width = 3, "vb1"), 
                        valueBoxOutput(width = 3, "vb2")), # Summen af længder på width må ikke overstige 12. Så ryger de ned på næste linje, hvilket ikke er meningen :-)
                      fluidRow(tabBox(title = tagList(shiny::icon("chart-line"), "Tabset 1"), id = "tabset1", height = "520px", width = 12, 
                                      tabPanel(tagList(shiny::icon("people-arrows"),"Tab Panel 1"), "", box(title = "", width = 12, plotOutput("tabpanelid1", height = 400))),
                                      tabPanel(tagList(shiny::icon("percent"),"Tab Panel 2"), "", box(title = "", width = 12, plotOutput("tabpanelid2", height = 400)))))) # Her kan I lave et ggplot eller plotly. Brug plotlyOutput, hvis plotly
)



server <- function(input, output, session) {
  
  output$vb1 <- renderValueBox({
    
    number <- 100
    
    valueBox(value = number, "Value Box 1", subtitle = paste0("Her er et fiktivt tal for vb1"), icon = icon("people-arrows"), 
             color = "aqua")
    
  })
  
  output$vb2 <- renderValueBox({
    
    valueBox(value = paste0(10, " %"), "Value Box 2", subtitle = paste0("Her er et fiktivt tal for vb2"), icon = icon("percent"),
             color = "blue")
    
  })
  
  
  output$tabpanelid1 <- renderPlot({
    
    # Eksempelvis et ggplot
    
  })
  
  output$tabpanel2 <- renderPlot({
    
    # Eksempelvis et ggplot
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
