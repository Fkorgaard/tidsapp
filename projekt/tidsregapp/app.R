# Installer nødvendige pakker hvis de ikke allerede er installeret
# install.packages(c("shiny", "shinydashboard", "dplyr", "ggplot2", "lubridate", "DT", "googlesheets4", "googledrive"))

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(lubridate)
library(DT)
library(googlesheets4)
library(googledrive)

# Google Sheet indstillinger - erstat med din egen Google Sheet ID
google_sheet_id <- "https://docs.google.com/spreadsheets/d/1lWgN35lGp9s3UE4OGxu14S9sde6QTkkNXwbkDa4PasI/edit?usp=drive_link" # Erstat med dit eget sheet ID

# Funktion til at indlæse data fra Google Sheets
load_data_from_gsheets <- function() {
  # Autentifikation - du skal have opsat OAuth credentials
  # Dette udføres første gang du kører appen
  googledrive::drive_auth()
  googlesheets4::gs4_auth()
1
  
  tryCatch({
    # Indlæs data fra Google Sheet
    data <- read_sheet(google_sheet_id)
    
    # Sikre at dato er i korrekt format
    if("dato" %in% colnames(data)) {
      data$dato <- as.Date(data$dato)
    }
    
    # Sikre at timer er numerisk
    if("timer" %in% colnames(data)) {
      data$timer <- as.numeric(data$timer)
    }
    
    return(data)
  }, error = function(e) {
    # Hvis der opstår en fejl
    warning(paste("Fejl ved indlæsning fra Google Sheets:", e$message))
    return(data.frame(
      dato = as.Date(character()),
      person = character(),
      projekt = character(),
      emne = character(),
      start_tid = character(),
      slut_tid = character(),
      timer = numeric()
    ))
  })
}

# UI definition
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Tidsregistrerings Dashboard"),
  
  dashboardSidebar(
    selectInput("person", "Vælg Person:", 
                choices = c("Alle", "Frederik", "Lasse", "Isabel", "Sabrina")),  # Bliver opdateret dynamisk
    dateRangeInput("datointerval", "Vælg Datointerval:",
                   start = Sys.Date() - 30,
                   end = Sys.Date()),
    selectInput("projekt", "Vælg Projekt:",
                choices = c("Alle", "Projekt A", "Projekt B", "Projekt C")),  # Bliver opdateret dynamisk
    selectInput("emne", "Vælg Emne:",
                choices = c("Alle", "A", "B", "C")),  # Bliver opdateret dynamisk
    
    hr(),
    h4("Tidsregistrering"),
    selectInput("reg_person", "Person:", choices = ""),  # Bliver opdateret dynamisk
    selectInput("reg_projekt", "Projekt:", choices = ""),  # Bliver opdateret dynamisk
    selectInput("reg_emne", "Emne:", choices = ""),  # Bliver opdateret dynamisk
    dateInput("reg_dato", "Dato:", value = Sys.Date()),
    textInput("reg_start", "Start tid (HH:MM):", value = format(Sys.time(), "%H:%M")),
    textInput("reg_slut", "Slut tid (HH:MM):", value = format(Sys.time() + 3600, "%H:%M")),
    actionButton("gem_tid", "Gem tidsregistrering", icon = icon("save"), 
                 style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
    actionButton("opdater_data", "Opdater data", icon = icon("sync"), 
                 style = "color: #fff; background-color: #5cb85c; border-color: #4cae4c")
  ),
  
  dashboardBody(
    # Top banner med KPI'er per person
    fluidRow(
      box(
        title = "Samlet tidsregistrering per person (timer)",
        status = "primary",
        width = 12,
        solidHeader = TRUE,
        collapsible = TRUE,
        plotOutput("person_kpi_banner", height = "100px")
      )
    ),
    
    # Øverste række med 5 værdifelter
    fluidRow(
      valueBoxOutput("total_timer", width = 2),
      valueBoxOutput("gns_timer_dag", width = 2),
      valueBoxOutput("mest_aktive_projekt", width = 3),
      valueBoxOutput("mest_aktive_emne", width = 2),
      valueBoxOutput("dage_arbejdet", width = 3)
    ),
    
    # Anden række med graf og tabel
    fluidRow(
      tabBox(
        title = "Visualiseringer",
        width = 12,
        tabPanel("Tidsforbrug over tid", plotOutput("tidsplot")),
        tabPanel("Fordeling på projekter", plotOutput("projektplot")),
        tabPanel("Fordeling på emner", plotOutput("emneplot")),
        tabPanel("KPI Dashboard", 
                 plotOutput("kpi_dashboard_plot"))
      )
    ),
    
    # Tredje række med detaljeret tabel
    fluidRow(
      box(
        title = "Detaljeret Tidsregistrering",
        width = 12,
        DTOutput("tidsregistrering_tabel")
      )
    )
  )
)

# Server logik
server <- function(input, output, session) {
  
  # Reaktiv værdi til at gemme data
  tidsdata <- reactiveVal(data.frame())
  
  # Indlæs data ved opstart
  observe({
    data <- load_data_from_gsheets()
    tidsdata(data)
    
    # Opdater dropdowns baseret på indlæst data
    if(nrow(data) > 0) {
      # Opdater personer dropdown
      personer <- unique(data$person)
      updateSelectInput(session, "person", choices = c("Alle", personer))
      updateSelectInput(session, "reg_person", choices = personer)
      
      # Opdater projekter dropdown
      projekter <- unique(data$projekt)
      updateSelectInput(session, "projekt", choices = c("Alle", projekter))
      updateSelectInput(session, "reg_projekt", choices = projekter)
      
      # Opdater emner dropdown
      emner <- unique(data$emne)
      updateSelectInput(session, "reg_emne", choices = emner)
      
      # Opdater datointerval med min/max datoer fra data
      updateDateRangeInput(session, "datointerval", 
                           start = min(data$dato, na.rm = TRUE),
                           end = max(data$dato, na.rm = TRUE))
    }
  })
  
  # Manuel opdatering af data
  observeEvent(input$opdater_data, {
    showNotification("Opdaterer data fra Google Sheets...", type = "message")
    data <- load_data_from_gsheets()
    tidsdata(data)
    
    # Opdater dropdowns igen
    if(nrow(data) > 0) {
      personer <- unique(data$person)
      updateSelectInput(session, "person", choices = c("Alle", personer))
      updateSelectInput(session, "reg_person", choices = personer)
      
      projekter <- unique(data$projekt)
      updateSelectInput(session, "projekt", choices = c("Alle", projekter))
      updateSelectInput(session, "reg_projekt", choices = projekter)
      
      emner <- unique(data$emne)
      updateSelectInput(session, "reg_emne", choices = emner)
    }
    
    showNotification("Data opdateret!", type = "message")
  })
  
  # Funktion til at uploade data til Google Sheets
  upload_to_gsheets <- function(data) {
    tryCatch({
      # Tilføj ny række til Google Sheet
      sheet_append(google_sheet_id, data)
      return(TRUE)
    }, error = function(e) {
      # Hvis der opstår en fejl
      showNotification(paste("Fejl ved upload til Google Sheets:", e$message), 
                       type = "error", duration = 10)
      return(FALSE)
    })
  }
  
  # Filtrer data baseret på input
  filtered_data <- reactive({
    data <- tidsdata()
    
    if(nrow(data) == 0) {
      return(data.frame())
    }
    
    if (input$person != "Alle") {
      data <- data %>% filter(person == input$person)
    }
    
    data <- data %>% 
      filter(dato >= input$datointerval[1] & dato <= input$datointerval[2])
    
    if (input$projekt != "Alle") {
      data <- data %>% filter(projekt == input$projekt)
    }
    
    return(data)
  })
  
  # Samlet data for alle personer
  all_persons_data <- reactive({
    data <- tidsdata()
    
    if(nrow(data) == 0) {
      return(data.frame(person = character(), total_timer = numeric()))
    }
    
    data %>%
      filter(dato >= input$datointerval[1] & dato <= input$datointerval[2]) %>%
      group_by(person) %>%
      summarise(total_timer = sum(timer, na.rm = TRUE))
  })
  
  # KPI plot for top banner
  output$person_kpi_banner <- renderPlot({
    data <- all_persons_data()
    
    if(nrow(data) == 0) {
      return(NULL)
    }
    
    ggplot(data, aes(x = person, y = total_timer, fill = person)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(total_timer, 1)), vjust = -0.5, size = 5) +
      theme_minimal() +
      theme(legend.position = "none",
            axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid = element_blank(),
            plot.margin = unit(c(0, 0, 0, 0), "cm")) +
      labs(x = "", y = "")
  })
  
  # KPI Dashboard plot
  output$kpi_dashboard_plot <- renderPlot({
    data <- filtered_data()
    
    if(nrow(data) == 0) {
      return(NULL)
    }
    
    data_projekt <- data %>%
      group_by(projekt) %>%
      summarise(timer = sum(timer, na.rm = TRUE))
    
    data_person <- data %>%
      group_by(person) %>%
      summarise(timer = sum(timer, na.rm = TRUE))
    
    data_emner <- data %>%
      group_by(emne) %>%
      summarise(timer = sum(timer, na.rm = TRUE))
    
    # Opret et layout med 3 plots
    par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
    
    # Plot 1: Tidsforbrug per projekt
    barplot(data_projekt$timer, names.arg = data_projekt$projekt, 
            col = "skyblue", main = "Timer per projekt", 
            cex.names = 0.8, las = 2)
    
    # Plot 2: Tidsforbrug per person
    barplot(data_person$timer, names.arg = data_person$person, 
            col = "lightgreen", main = "Timer per person",
            cex.names = 0.8, las = 2)
    
    # Plot 3: Tidsforbrug per emne
    barplot(data_emner$timer, names.arg = data_emner$emne, 
            col = "pink", main = "Timer per emne",
            cex.names = 0.8, las = 2)
    
    # Plot 4: Efficiency rate (eksempel)
    personer_effektivitet <- data %>%
      group_by(person) %>%
      summarise(
        planlagt_timer = n_distinct(dato) * 7.5,  # Antag 7.5 timer er planlagt per dag
        faktisk_timer = sum(timer, na.rm = TRUE),
        effektivitet = min(faktisk_timer / planlagt_timer * 100, 100)
      )
    
    barplot(personer_effektivitet$effektivitet, 
            names.arg = personer_effektivitet$person,
            col = "orange", main = "Effektivitetsrate (%)",
            cex.names = 0.8, las = 2)
  })
  
  # Øverste værdibokse
  output$total_timer <- renderValueBox({
    data <- filtered_data()
    
    if(nrow(data) == 0) {
      total <- 0
    } else {
      total <- sum(data$timer, na.rm = TRUE)
    }
    
    valueBox(
      paste0(round(total, 1), " timer"),
      "Total arbejdstid",
      icon = icon("clock"),
      color = "blue"
    )
  })
  
  output$gns_timer_dag <- renderValueBox({
    data <- filtered_data()
    
    if (nrow(data) > 0) {
      gns <- mean(data$timer, na.rm = TRUE)
      valueBox(
        paste0(round(gns, 1), " timer"),
        "Gennemsnit pr dag",
        icon = icon("calculator"),
        color = "green"
      )
    } else {
      valueBox(
        "0 timer",
        "Gennemsnit pr dag",
        icon = icon("calculator"),
        color = "green"
      )
    }
  })
  
  output$mest_aktive_projekt <- renderValueBox({
    data <- filtered_data()
    
    if (nrow(data) > 0) {
      projekt_sum <- data %>%
        group_by(projekt) %>%
        summarise(total = sum(timer, na.rm = TRUE)) %>%
        arrange(desc(total))
      
      if (nrow(projekt_sum) > 0) {
        valueBox(
          projekt_sum$projekt[1],
          "Mest aktive projekt",
          icon = icon("project-diagram"),
          color = "purple"
        )
      } else {
        valueBox(
          "Ingen data",
          "Mest aktive projekt",
          icon = icon("project-diagram"),
          color = "purple"
        )
      }
    } else {
      valueBox(
        "Ingen data",
        "Mest aktive projekt",
        icon = icon("project-diagram"),
        color = "purple"
      )
    }
  })
  
  output$mest_aktive_emne <- renderValueBox({
    data <- filtered_data()
    
    if (nrow(data) > 0) {
      emne_sum <- data %>%
        group_by(emne) %>%
        summarise(total = sum(timer, na.rm = TRUE)) %>%
        arrange(desc(total))
      
      if (nrow(emne_sum) > 0) {
        valueBox(
          emne_sum$emne[1],
          "Mest aktive emne",
          icon = icon("tasks"),
          color = "yellow"
        )
      } else {
        valueBox(
          "Ingen data",
          "Mest aktive emne",
          icon = icon("tasks"),
          color = "yellow"
        )
      }
    } else {
      valueBox(
        "Ingen data",
        "Mest aktive emne",
        icon = icon("tasks"),
        color = "yellow"
      )
    }
  })
  
  output$dage_arbejdet <- renderValueBox({
    data <- filtered_data()
    
    if (nrow(data) > 0) {
      dage <- n_distinct(data$dato)
      valueBox(
        dage,
        "Antal arbejdsdage",
        icon = icon("calendar"),
        color = "red"
      )
    } else {
      valueBox(
        0,
        "Antal arbejdsdage",
        icon = icon("calendar"),
        color = "red"
      )
    }
  })
  
  # Graf over tidsforbrug over tid
  output$tidsplot <- renderPlot({
    data <- filtered_data()
    
    if(nrow(data) == 0) {
      return(NULL)
    }
    
    data <- data %>%
      group_by(dato, person) %>%
      summarise(timer = sum(timer, na.rm = TRUE), .groups = "drop")
    
    ggplot(data, aes(x = dato, y = timer, color = person)) +
      geom_line() +
      geom_point() +
      labs(title = "Tidsforbrug over tid",
           x = "Dato",
           y = "Timer") +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  # Graf over projekter
  output$projektplot <- renderPlot({
    data <- filtered_data()
    
    if(nrow(data) == 0) {
      return(NULL)
    }
    
    data <- data %>%
      group_by(projekt, person) %>%
      summarise(timer = sum(timer, na.rm = TRUE), .groups = "drop")
    
    ggplot(data, aes(x = projekt, y = timer, fill = person)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Tidsforbrug per projekt",
           x = "Projekt",
           y = "Timer") +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  # Graf over emner
  output$emneplot <- renderPlot({
    data <- filtered_data()
    
    if(nrow(data) == 0) {
      return(NULL)
    }
    
    data <- data %>%
      group_by(emne, person) %>%
      summarise(timer = sum(timer, na.rm = TRUE), .groups = "drop")
    
    ggplot(data, aes(x = emne, y = timer, fill = person)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Tidsforbrug per emne",
           x = "Emne",
           y = "Timer") +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  # Detaljeret tabel
  output$tidsregistrering_tabel <- renderDT({
    data <- filtered_data()
    
    if(nrow(data) == 0) {
      return(data.frame())
    }
    
    data %>%
      select(dato, person, projekt, emne, start_tid, slut_tid, timer) %>%
      arrange(dato, person)
  }, options = list(pageLength = 10))
  
  # Gem tidsregistrering
  observeEvent(input$gem_tid, {
    # Valider input
    start_tid <- input$reg_start
    slut_tid <- input$reg_slut
    
    # Beregn timer
    start_time <- as.POSIXct(paste(input$reg_dato, start_tid), format = "%Y-%m-%d %H:%M")
    end_time <- as.POSIXct(paste(input$reg_dato, slut_tid), format = "%Y-%m-%d %H:%M")
    
    if (is.na(start_time) || is.na(end_time)) {
      showNotification("Ugyldig tidsformat. Brug HH:MM format.", type = "error")
      return()
    }
    
    if (timer_diff <= 0) {
      showNotification("Slut tid skal være efter start tid.", type = "error")
      return()
    }
    
    # Beregn timeforskel
    timer_diff <- as.numeric(difftime(end_time, start_time, units = "hours"))
    
    # Opret nyt datasæt
    new_entry <- data.frame(
      dato = input$reg_dato,
      person = input$reg_person,
      projekt = input$reg_projekt,
      emne = input$reg_emne,
      start_tid = start_tid,
      slut_tid = slut_tid,
      timer = round(timer_diff, 2),
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
    
    # Upload til Google Sheets
    result <- upload_to_gsheets(new_entry)
    
    if (result) {
      showNotification("Tidsregistrering gemt i Google Sheets", type = "success")
      
      # Opdater lokale data ved at hente igen fra Google Sheets
      data <- load_data_from_gsheets()
      tidsdata(data)
      
      # Reset input felter
      updateTextInput(session, "reg_start", value = format(Sys.time(), "%H:%M"))
      updateTextInput(session, "reg_slut", value = format(Sys.time() + 3600, "%H:%M"))
    }
  })
}

# Kør Shiny app
shinyApp(ui, server)