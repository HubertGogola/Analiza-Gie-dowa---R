library(shiny)
library(dplyr)
library(ggplot2)

# Wczytanie danych
data <- read.csv("BTC-USD.csv")

# Konwersja kolumny Date na format daty
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")

# UI aplikacji
ui <- fluidPage(
  titlePanel("Analiza danych giełdowych"),
  
  sidebarLayout(
    sidebarPanel(
      # Zakres dat
      dateRangeInput("date_range", "Wybierz zakres dat:",
                     start = min(data$Date),
                     end = max(data$Date)),
      
      # Wybór zmiennej
      selectInput("variable", "Wybierz zmienną do analizy:",
                  choices = c("Open", "High", "Low", "Close", "Adj_Close", "Volume")),
      
      # Informacja dla użytkownika
      helpText("Wybierz zakres dat i parametr, aby zobaczyć zmiany w czasie.")
    ),
    
    mainPanel(
      # Wykres
      plotOutput("line_plot"),
      
      # Tabela danych
      tableOutput("data_table")
    )
  )
)

# Serwer aplikacji
server <- function(input, output) {
  
  # Filtrowanie danych na podstawie zakresu dat
  filtered_data <- reactive({
    data %>%
      filter(Date >= input$date_range[1] & Date <= input$date_range[2])
  })
  
  # Generowanie wykresu
  output$line_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = Date, y = .data[[input$variable]])) +
      geom_line(color = "blue", size = 1) +
      labs(title = paste("Wykres dla zmiennej:", input$variable),
           x = "Data",
           y = input$variable) +
      theme_minimal() 
  })
  
  # Wyświetlanie tabeli danych
  output$data_table <- renderTable({
    filtered_data() %>%
      select(Date, Open, High, Low, Close, Adj_Close, Volume)
  })
}

# Uruchomienie aplikacji
shinyApp(ui, server)