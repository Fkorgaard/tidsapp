install.packages("bsicons")


library(shiny)
library(tidyverse)
library(bslib)
library(bsicons)

library(googlesheets4)

gs4_auth(
  cache = ".token",
  email = "lasseoestergaard10@gmail.com"
)


#Skal ikke sættes når app'en skal deployes. Det er kun til lokal testning

#setwd("C:/Users/sibe/OneDrive - EaDania/Datavisualisering/2025/GitHub/03_lektion_shiny/03_lektion")

day <- readRDS("C:/Users/Lasse/OneDrive/Skrivebord/dataanalyse/2semester/Datavisualisering/GitHub/Shiny/lasseshiny/day.Rds")


#day <- readRDS("model/day.Rds") 



# Define UI for application that draws a histogram
ui <- fluidPage(
  
  title = "Datavisualisering",
  
  # Application title
  titlePanel("EA Dania 2025"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      # Logo --------------------------------------------------------------------
      
      
      div(img(height = 65, width = 80, src = "dania_logo.png"), # Her indsætter jeg et logo
          style = "text-align: center;"),
      
      
      # Day selector ------------------------------------------------------------
      
      numericInput("sel_day",
                   "Select a number to return a day",
                   min = "1",
                   max = "7",
                   value = "1"),
      
      
      # Slider ------------------------------------------------------------------
      
      
      uiOutput("slider_ui")
      ,
      
      actionButton("tilføj", "Tilføj observation"),
      br(),
      actionButton("indlæs", "Indlæs data")),
    
    # Main panel --------------------------------------------------------------
    
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(tabPanel("Day",
                           tableOutput("tabel"),
                           uiOutput("day")),
                  tabPanel("Plot 1",
                           plotOutput("plot1")),
                  tabPanel("Plot 2",
                           sliderInput("hej",
                                       "Test",
                                       min = 1,
                                       max = 10,
                                       step = 1,
                                       value = 5),
                           hr(),
                           plotOutput("plot2")))
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Aktiv min actionbutton
  
  observeEvent(input$tilføj, {
    
    newRow <- data.frame(day = input$sel_day,
                         year = input$bins)
    
    print(newRow)
    
    sheet_append("xxxx", newRow, sheet = 1) # Kun ID
    
  })
  
  
  df <- eventReactive(input$indlæs, {
    
    read_sheet("xxxxxxx", # Fuld url
               range = "Ark1")
    
  })
  
  output$tabel <- renderDataTable({
    
    print(df())
    
  })
  
  
  
  # Value box day
  
  output$day <- renderUI({
    
    day <- day()
    
    weekday <- day(input$sel_day)
    
    bslib::value_box(title = "The chosen day is:",
                     value = weekday,
                     showcase = bs_icon("bank2"),
                     theme = "bg-danger")
    
  })
  
  
  output$slider_ui <- renderUI({
    
    req(passat())
    
    passat <- passat()
    
    sliderInput("bins",
                "Number of bins:",
                min = min(passat$year, na.rm = TRUE),
                max = max(passat$year, na.rm = TRUE),
                value = 30)
    
  })
  
  
  output$plot1 <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white',
         xlab = 'Waiting time to next eruption (in mins)',
         main = 'Histogram of waiting times')
  })
  
  
  output$plot2 <- renderPlot({
    
    #print(passat())
    
    # passat <- readxl::read_excel("data/passat.xlsx")
    # 
    # print(passat)
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white',
         xlab = 'Waiting time to next eruption (in mins)',
         main = 'Histogram of waiting times')
  })
  
  
  # Reactive ----------------------------------------------------------------
  
  
  
  passat <- reactive({
    
    passat <- readxl::read_excel("data/passat.xlsx") %>%
      filter(year >= 2014)
    
    #rds <- readxl::read_excel("data/passat.xlsx")
    
    
  })
  
  day <- reactive({
    
    day <- readRDS("model/day.Rds")
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

