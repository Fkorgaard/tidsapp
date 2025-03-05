# Load necessary libraries
library(shiny)
library(tidyverse)
library(bslib)
library(bsicons)
library(googlesheets4)
library(lubridate)
library(DT)  # Interaktive tabeller
library(ggplot2)  # Grafer

# Google Sheets authentication
gs4_auth(
  cache = ".token",
  email = "lasseoestergaard10@gmail.com"
)

SHEET_ID <- "1lWgN35lGp9s3UE4OGxu14S9sde6QTkkNXwbkDa4PasI"

df <- read_sheet(SHEET_ID, range = "Ark1")

# Funktion til at hente data fra Google Sheets
load_data <- function() {
  read_sheet(SHEET_ID, range = "Ark1!A:F")  # Henter kun relevante kolonner
}


# Define UI for application
ui <- navbarPage(
  title = "Gruppe 8",
  
  # Side 1: Tidsregistrering
  tabPanel("Tidsregistrering",
           sidebarLayout(
             sidebarPanel(
               div(img(height = 65, width = 80, src = "www/gruppe8.jpg"), 
                   style = "text-align: center;"),
               
               selectInput("person", "Vælg en person:",
                           choices = c("Alle", "Lasse", "Frederik", "Sabrina", "Isabel")),
               
               selectInput("fag", "Vælg et fag:",
                           choices = c("Datakildeforståelse", "Programmering", "Data visualisering", "Jura")),
               
               selectInput("projekt", "Vælg et projekt:",
                           choices = c("1. semestersprojekt", "2. semestersprojekt")),
               
               actionButton("start_btn", "Start"),
               actionButton("stop_btn", "Stop")
             ),
             
             mainPanel(
               textOutput("valg_resultat"),
               tags$h2(textOutput("digital_timer"), style = "text-align: center; font-size: 50px; font-weight: bold; color: #007BFF;"),
               tags$div(style = "margin-top: 20px;"),
               tags$div(style = "width: 100%; background-color: #ddd; height: 30px; border-radius: 10px;",
                        tags$div(id = "progress_bar", 
                                 style = "width: 0%; height: 100%; background-color: #007BFF; text-align: center; 
                                   line-height: 30px; color: white; border-radius: 10px;")),
               textOutput("knap_status")
             )
           )
  ),
  
  # Side 2: Indsigt
  tabPanel("Indsigt",
           fluidPage(
             titlePanel("Indsigt i tidsregistrering"),
             
             sidebarLayout(
               sidebarPanel(
                 selectInput("indsigt_person", "Vælg person:", choices = NULL),
                 actionButton("refresh_data", "Opdater data")
               ),
               
               mainPanel(
                 h3("Oversigt over registreringer"),
                 DTOutput("data_tabel"),  # Viser hele datasættet
                 
                 h3("Tid brugt per person"),
                 plotOutput("tid_per_person")  # Graf
               )
             )
           )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  start_time <- reactiveVal(NULL)
  stop_time <- reactiveVal(NULL)
  timer_running <- reactiveVal(FALSE)
  
  # Reactive dataset der henter data fra Google Sheets
  dataset <- reactiveVal(load_data())
  
  observeEvent(input$refresh_data, {
    dataset(load_data())  # Opdater data når brugeren trykker "Opdater data"
  })
  
  # Opdater dropdown-menu i "Indsigt"
  observe({
    updateSelectInput(session, "indsigt_person", choices = unique(dataset()$Person))
  })
  
  # Funktion til at opdatere Google Sheets
  update_sheet <- function(person, fag, projekt, start, stop, varighed) {
    new_row <- tibble(Person = person, Fag = fag, Projekt = projekt, Start = start, Stop = stop, Varighed = varighed)
    sheet_append(SHEET_ID, new_row)
  }
  
  # Output til at vise valgte værdier
  output$valg_resultat <- renderText({
    paste("Du har valgt:", input$person, ",", input$fag, "og", input$projekt)
  })
  
  # Start-knap trykkes
  observeEvent(input$start_btn, {
    start_time(Sys.time())  
    stop_time(NULL)
    timer_running(TRUE)
    
    output$knap_status <- renderText("Tidsregistrering startet!")
  })
  
  # Stop-knap trykkes
  observeEvent(input$stop_btn, {
    req(start_time())  
    
    stop_time(Sys.time())  
    timer_running(FALSE)
    
    elapsed_time <- as.numeric(difftime(stop_time(), start_time(), units = "secs"))
    minutes <- as.integer(elapsed_time %/% 60)
    seconds <- as.integer(elapsed_time %% 60)
    elapsed_formatted <- sprintf("%02d:%02d", minutes, seconds)
    
    update_sheet(input$person, input$fag, input$projekt, 
                 format(start_time(), "%Y-%m-%d %H:%M:%S"), 
                 format(stop_time(), "%Y-%m-%d %H:%M:%S"), 
                 elapsed_formatted)
    
    output$knap_status <- renderText(paste("Tidsregistrering stoppet! Varighed:", elapsed_formatted))
    
    session$sendCustomMessage(type = "updateProgress", list(value = 0))
  })
  
  # Opdaterer den digitale timer
  output$digital_timer <- renderText({
    req(timer_running())
    invalidateLater(1000, session)
    elapsed <- as.numeric(difftime(Sys.time(), start_time(), units = "secs"))
    minutes <- as.integer(elapsed %/% 60)
    seconds <- as.integer(elapsed %% 60)
    sprintf("%02d:%02d", minutes, seconds)
  })
  
  # Viser hele datasættet i en tabel
  output$data_tabel <- renderDT({
    datatable(dataset(), options = list(pageLength = 5))
  })
  
  # Graf: Tid brugt per person
  output$tid_per_person <- renderPlot({
    dataset() %>%
      group_by(Person) %>%
      summarise(Total_tid = sum(as.numeric(gsub(" min.*", "", Varighed)))) %>%
      ggplot(aes(x = Person, y = Total_tid, fill = Person)) +
      geom_bar(stat = "identity") +
      labs(title = "Total tid brugt per person", x = "Person", y = "Tid (minutter)") +
      theme_minimal()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


