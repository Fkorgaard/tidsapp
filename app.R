# Load necessary libraries
library(shiny)
library(tidyverse)
library(bslib)
library(bsicons)
library(googlesheets4)
library(lubridate)

# Google Sheets authentication
#gs4_auth(
 # cache = ".token",
  #email = "lasseoestergaard10@gmail.com"
#)

SHEET_ID <- "1lWgN35lGp9s3UE4OGxu14S9sde6QTkkNXwbkDa4PasI"

# Define UI for application
ui <- fluidPage(
  
  title = "Gruppe 8",
  
  # Application title
  titlePanel("Tidsregistrering"),
  
  # Sidebar layout
  sidebarLayout(
    sidebarPanel(
      
      # Logo
      div(img(height = 65, width = 80, src = "gruppe8.jpg"), 
          style = "text-align: center;"),
      
      # Dropdown-menuer
      selectInput("person", "Vælg en person:",
                  choices = c("Alle", "Lasse", "Frederik", "Sabrina", "Isabel")),
      
      selectInput("fag", "Vælg et fag:",
                  choices = c("Fag1", "Fag2", "Fag3", "Fag4", "Fag5", "Fag6")),
      
      selectInput("projekt", "Vælg et projekt:",
                  choices = c("1. semestersprojekt", "2. semestersprojekt")),
      
      # Knapper
      actionButton("start_btn", "Start"),
      actionButton("stop_btn", "Stop")
      
    ),
    
    mainPanel(
      textOutput("valg_resultat"),
      textOutput("timer"),
      textOutput("knap_status")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  start_time <- reactiveVal(NULL)
  stop_time <- reactiveVal(NULL)
  timer_running <- reactiveVal(FALSE)
  
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
    start_time(Sys.time())  # Registrerer starttid
    stop_time(NULL)
    timer_running(TRUE)
    
    output$knap_status <- renderText("Tidsregistrering startet!")
  })
  
  # Stop-knap trykkes
  observeEvent(input$stop_btn, {
    req(start_time())  # Sørger for, at der er en starttid før stop registreres
    
    stop_time(Sys.time())  # Registrerer stoptid
    timer_running(FALSE)
    
    # Beregner varigheden (sikrer at vi bruger heltal)
    elapsed_time <- as.numeric(difftime(stop_time(), start_time(), units = "secs"))
    minutes <- as.integer(elapsed_time %/% 60)
    seconds <- as.integer(elapsed_time %% 60)
    elapsed_formatted <- sprintf("%d min %d sek", minutes, seconds)
    
    # Registrerer data i Google Sheets
    update_sheet(input$person, input$fag, input$projekt, 
                 format(start_time(), "%Y-%m-%d %H:%M:%S"), 
                 format(stop_time(), "%Y-%m-%d %H:%M:%S"), 
                 elapsed_formatted)
    
    output$knap_status <- renderText(paste("Tidsregistrering stoppet! Varighed:", elapsed_formatted))
  })
  
  # Opdaterer tælleren
  output$timer <- renderText({
    req(timer_running())
    invalidateLater(1000, session)  # Opdaterer hver sekund
    elapsed <- as.numeric(difftime(Sys.time(), start_time(), units = "secs"))
    paste("Tid gået:", elapsed, "sekunder")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
