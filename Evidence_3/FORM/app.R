library(shiny)
library(shinydashboard)
library(readr)
library(ggplot2)
library(forecast)
library(randomForest)
library(caret)
library(e1071)
library(rpart)
library(rpart.plot)

# Cargar librerías necesarias
library(readr)
library(dplyr)
library(rpart)
library(forecast)
library(cluster)
library(caret)
library(rpart.plot)

# Cargar datos de ventas
ventas <- read_csv("path/to/ventas.csv")

# Cargar datos de empleados
empleados <- read_csv("path/to/empleados.csv")


ui <- dashboardPage(
  dashboardHeader(title = "Predicción de Ventas y Clasificación de Empleados"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Predicción de Ventas", tabName = "ventas", icon = icon("line-chart")),
      menuItem("Clasificación de Empleados", tabName = "empleados", icon = icon("users"))
    )
  ),
  dashboardBody(
    tabItems(
      # Pestaña de Predicción de Ventas
      tabItem(tabName = "ventas",
              fluidRow(
                box(title = "Cargar Datos CSV", status = "primary", solidHeader = TRUE,
                    fileInput("file1", "Elige CSV", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                    tags$hr(),
                    checkboxInput("header", "Encabezado", TRUE),
                    radioButtons("sep", "Separador", choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = ",")
                ),
                box(title = "Datos Manuales", status = "primary", solidHeader = TRUE,
                    numericInput("ano", "Año", value = 2024),
                    numericInput("mes", "Mes", value = 1, min = 1, max = 12),
                    numericInput("ventas_totales", "Ventas Totales", value = 0),
                    numericInput("total_carton", "Total Carton", value = 0),
                    numericInput("total_retornable", "Total Retornable", value = 0),
                    numericInput("servicios", "Servicios", value = 0),
                    actionButton("add", "Agregar")
                )
              ),
              fluidRow(
                box(title = "Predicciones", status = "primary", solidHeader = TRUE,
                    plotOutput("plotVentas"),
                    tableOutput("tableVentas")
                )
              )
      ),
      # Pestaña de Clasificación de Empleados
      tabItem(tabName = "empleados",
              fluidRow(
                box(title = "Cargar Datos CSV", status = "primary", solidHeader = TRUE,
                    fileInput("file2", "Elige CSV", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                    tags$hr(),
                    checkboxInput("header2", "Encabezado", TRUE),
                    radioButtons("sep2", "Separador", choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = ",")
                )
              ),
              fluidRow(
                box(title = "Clasificación de Empleados", status = "primary", solidHeader = TRUE,
                    verbatimTextOutput("summaryEmpleados"),
                    plotOutput("plotEmpleados")
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  ventasData <- reactiveVal(data.frame(Año = numeric(), Mes = numeric(), Ventas_totales = numeric(), Total_carton = numeric(), Total_retornable = numeric(), Servicios = numeric()))
  
  observeEvent(input$file1, {
    req(input$file1)
    data <- read.csv(input$file1$datapath, header = input$header, sep = input$sep)
    ventasData(data)
  })
  
  observeEvent(input$add, {
    newRow <- data.frame(Año = input$ano, Mes = input$mes, Ventas_totales = input$ventas_totales, Total_carton = input$total_carton, Total_retornable = input$total_retornable, Servicios = input$servicios)
    ventasData(rbind(ventasData(), newRow))
  })
  
  output$plotVentas <- renderPlot({
    req(ventasData())
    data <- ventasData()
    if(nrow(data) > 0) {
      ventas_ts <- ts(data$Ventas_totales, start = c(min(data$Año), min(data$Mes)), frequency = 12)
      plot(forecast(auto.arima(ventas_ts), h = 12), main = "Predicciones de Ventas Totales")
    }
  })
  
  output$tableVentas <- renderTable({
    req(ventasData())
    ventasData()
  })
  
  empleadosData <- reactiveVal()
  
  observeEvent(input$file2, {
    req(input$file2)
    data <- read.csv(input$file2$datapath, header = input$header2, sep = input$sep2)
    empleadosData(data)
  })
  
  output$summaryEmpleados <- renderPrint({
    req(empleadosData())
    data <- empleadosData()
    modelo <- rpart(Estatus ~ ., data = data, method = "class")
    printcp(modelo)
  })
  
  output$plotEmpleados <- renderPlot({
    req(empleadosData())
    data <- empleadosData()
    modelo <- rpart(Estatus ~ ., data = data, method = "class")
    rpart.plot(modelo, type = 4, extra = 101)
  })
}

shinyApp(ui, server)



