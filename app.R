library(shiny)
library(tidyverse)
library(bslib)
library(bsicons)
library(googlesheets4)
library(lubridate)
library(DT)
library(ggplot2)
library(shinydashboard)
library(auth0)

#file.edit("~/.Renviron")

options(shiny.host = "127.0.0.1", shiny.port = 8080)


readRenviron(".Renviron")



# Google Sheets authentication
gs4_auth(
  cache = ".token",
  email = "lasseoestergaard10@gmail.com"
)

SHEET_ID <- "1lWgN35lGp9s3UE4OGxu14S9sde6QTkkNXwbkDa4PasI"

# Funktion til at hente data fra Google Sheets
load_data <- function() {
  read_sheet(SHEET_ID, range = "Ark1!A:F")
}

ui <- dashboardPage(
  dashboardHeader(title = "Gruppe 8"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Tidsregistrering", tabName = "tidsregistrering", icon = icon("clock")),
      menuItem("Indsigt", tabName = "indsigt", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tabItems(
      # Side 1: Tidsregistrering
      tabItem(tabName = "tidsregistrering",
              fluidPage(
                titlePanel("Tidsregistrering"),
                sidebarLayout(
                  sidebarPanel(
                    div(img(height = 65, width = 80, src = "gruppe8.jpg"), 
                        style = "text-align: center;"),
                    selectInput("person", "Vælg en person:",
                                choices = c("Lasse", "Frederik", "Sabrina", "Isabel")),
                    selectInput("fag", "Vælg et fag:",
                                choices = c("Datakildeforståelse", "Programmering", "Data visualisering", "Jura")),
                    selectInput("projekt", "Vælg et projekt:",
                                choices = c("1. semestersprojekt", "2. semestersprojekt")),
                    actionButton("start_btn", "Start"),
                    actionButton("stop_btn", "Stop")
                  ),
                  mainPanel(
                    textOutput("valg_resultat"),
                    tags$h2(textOutput("digital_timer"), 
                            style = "text-align: center; font-size: 50px; font-weight: bold; color: #007BFF;"),
                    tags$div(id = "timer_display", 
                             style = "text-align: center; font-size: 60px; font-weight: bold; color: #FF5733;"),
                    textOutput("knap_status")
                  )
                )
              )
      ),
      # Side 2: Indsigt
      tabItem(tabName = "indsigt",
              fluidPage(
                titlePanel("Indsigt i tidsregistrering"),
                fluidRow(
                  valueBoxOutput("total_tid_box"),
                  valueBoxOutput("mest_tid_fag"),
                  valueBoxOutput("mest_tid_person")
                ),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("indsigt_person", "Vælg person:", choices = NULL),
                    actionButton("refresh_data", "Opdater data")
                  ),
                  mainPanel(
                    h3("Oversigt over registreringer"),
                    DTOutput("data_tabel"),
                    h3("Tid brugt per person"),
                    plotOutput("tid_per_person"),
                    h3("Fordeling af tid per fag"),
                    plotOutput("tid_per_fag")
                  )
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  # Reaktive værdier for tidsregistrering
  start_time <- reactiveVal(NULL)
  timer_running <- reactiveVal(FALSE)
  elapsed_time <- reactiveVal(0)
  
  # Reaktiv dataset
  dataset <- reactiveVal(load_data())
  
  # Funktion til at formatere tid
  format_time <- function(seconds) {
    hours <- seconds %/% 3600
    minutes <- (seconds %% 3600) %/% 60
    secs <- seconds %% 60
    sprintf("%02d:%02d:%02d", hours, minutes, secs)
  }
  
  # Digital timer
  timer <- reactiveTimer(1000)
  observe({
    if (timer_running()) {
      timer()
      current_time <- as.numeric(difftime(Sys.time(), start_time(), units = "secs"))
      elapsed_time(current_time)
      
      # Opdater timer display
      output$digital_timer <- renderText({
        format_time(as.integer(current_time))
      })
      
      # Direkte JS manipulation for mere flydende timer
      session$sendCustomMessage("updateTimer", list(
        time = format_time(as.integer(current_time))
      ))
    }
  })
  
  # Start knap
  observeEvent(input$start_btn, {
    start_time(Sys.time())
    timer_running(TRUE)
    elapsed_time(0)
    output$knap_status <- renderText("Tidsregistrering startet!")
  })
  
  # Stop knap
  observeEvent(input$stop_btn, {
    req(start_time())
    timer_running(FALSE)
    
    # Beregn varighed
    varighed <- as.numeric(difftime(Sys.time(), start_time(), units = "secs"))
    varighed_formatted <- format_time(as.integer(varighed))
    
    # Opdater Google Sheets
    new_row <- tibble(
      Person = input$person, 
      Fag = input$fag, 
      Projekt = input$projekt, 
      Start = format(start_time(), "%Y-%m-%d %H:%M:%S"),
      Stop = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      Varighed = varighed
    )
    
    sheet_append(SHEET_ID, new_row)
    
    output$knap_status <- renderText(paste("Tidsregistrering stoppet! Varighed:", varighed_formatted))
    
    # Genindlæs dataset
    dataset(load_data())
  })
  
  # Indsigt side
  observe({
    updateSelectInput(session, "indsigt_person", choices = unique(dataset()$Person))
  })
  
  observeEvent(input$refresh_data, {
    dataset(load_data())
  })
  
  # Data tabel
  output$data_tabel <- renderDT({
    req(dataset())
    datatable(dataset(), options = list(pageLength = 5))
  })
  
  # Plot af tid per person
  output$tid_per_person <- renderPlot({
    req(dataset())
    dataset() %>%
      group_by(Person) %>%
      summarise(Total_tid = sum(as.numeric(Varighed))) %>%
      ggplot(aes(x = Person, y = Total_tid, fill = Person)) +
      geom_bar(stat = "identity") +
      labs(title = "Total tid brugt per person", x = "Person", y = "Tid (sekunder)") +
      theme_minimal()
  })
  
  # Plot af tid per fag
  output$tid_per_fag <- renderPlot({
    req(dataset())
    dataset() %>%
      group_by(Fag) %>%
      summarise(Total_tid = sum(as.numeric(Varighed))) %>%
      ggplot(aes(x = Fag, y = Total_tid, fill = Fag)) +
      geom_bar(stat = "identity") +
      labs(title = "Total tid brugt per fag", x = "Fag", y = "Tid (sekunder)") +
      theme_minimal()
  })
  
  # Value boxes
  output$total_tid_box <- renderValueBox({
    valueBox(
      round(sum(as.numeric(dataset()$Varighed)), 0), 
      "Samlet tid (sekunder)", 
      icon = icon("clock"), 
      color = "blue"
    )
  })
  
  output$mest_tid_fag <- renderValueBox({
    top_fag <- dataset() %>%
      group_by(Fag) %>%
      summarise(Total_tid = sum(as.numeric(Varighed))) %>%
      arrange(desc(Total_tid)) %>%
      slice(1)
    valueBox(
      round(top_fag$Total_tid, 0), 
      paste("Mest tid brugt på", top_fag$Fag), 
      icon = icon("book"), 
      color = "green"
    )
  })
  
  output$mest_tid_person <- renderValueBox({
    top_person <- dataset() %>%
      group_by(Person) %>%
      summarise(Total_tid = sum(as.numeric(Varighed))) %>%
      arrange(desc(Total_tid)) %>%
      slice(1)
    valueBox(
      round(top_person$Total_tid, 0), 
      paste("Mest tid brugt af", top_person$Person), 
      icon = icon("user"), 
      color = "red"
    )
  })
}

# Tilføj JavaScript til at opdatere timer display
tags$script("
  Shiny.addCustomMessageHandler('updateTimer', function(message) {
    $('#timer_display').text(message.time);
  });
")

auth0::shinyAppAuth0(ui = ui, server = server)


