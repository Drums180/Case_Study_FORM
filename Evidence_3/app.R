library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(rpart)
library(rpart.plot)

# Cargar los modelos entrenados
modelo_cart <- readRDS("modelo_cart.rds")
modelo_regresion <- readRDS("modelo_regresion.rds")

# Opciones para variables categóricas (ejemplo basado en los datos disponibles)
genero_opciones <- c("Masculino", "Femenino")
puesto_opciones <- c("Administrador", "Operador", "Supervisor") # Ajustar según tus datos
dpto_opciones <- c("Ventas", "Producción", "Recursos Humanos") # Ajustar según tus datos
municipio_opciones <- c("Monterrey", "Guadalajara", "Ciudad de México") # Ajustar según tus datos
estado_opciones <- c("Nuevo León", "Jalisco", "CDMX") # Ajustar según tus datos
estado_civil_opciones <- c("Soltero", "Casado", "Divorciado") # Ajustar según tus datos

# Opciones para variables categóricas de regresión
clientes_opciones <- c("Aislantes y Empaques", "Aptiv", "Avanzar Interior Products", "Denso", "Draexlmaier", 
                       "EFP Operations", "ElringKlinger", "Estapack", "Faurecia", "Gaim Regiomontana", 
                       "Grupo ABC", "Grupo Antolin", "Hella Automotive", "IACNA", "Inoac Polytec", 
                       "Isringhausen", "ITB Packaging", "Johnson Controls", "Katcon", "Meridian Technologies", 
                       "Michigan State University", "Mitchell Plastics", "Po Lighting", "Sanhua Automotive", 
                       "Stabilus", "Tesla", "Tokai Rika", "Transporte y Automatización de Materiales", 
                       "Ufi Filters", "Yanfeng", "ZKW")

mes_opciones <- as.character(1:12)
semana_anio_opciones <- as.character(1:52)

# UI
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
      tabItem(tabName = "ventas",
              fluidRow(
                box(title = "Ingreso Manual de Datos", width = 12,
                    selectInput("cliente", "Cliente", choices = clientes_opciones),
                    selectInput("mes", "Mes", choices = mes_opciones),
                    selectInput("semana_anio", "Semana del Año", choices = semana_anio_opciones),
                    actionButton("predict_ventas_manual", "Predecir")
                ),
                box(title = "Resultados del Modelo", width = 12, verbatimTextOutput("ventasSummary"))
              )
      ),
      tabItem(tabName = "empleados",
              fluidRow(
                box(title = "Ingreso Manual de Datos", width = 12,
                    selectInput("genero", "Género", choices = genero_opciones),
                    selectInput("puesto", "Puesto", choices = puesto_opciones),
                    selectInput("dpto", "Dpto", choices = dpto_opciones),
                    numericInput("sd", "SD", value = 0),
                    selectInput("municipio", "Municipio", choices = municipio_opciones),
                    selectInput("estado", "Estado", choices = estado_opciones),
                    selectInput("estado_civil", "Estado Civil", choices = estado_civil_opciones),
                    numericInput("mes_nacimiento", "Mes de Nacimiento", value = 1),
                    numericInput("mes_entrada", "Mes de Entrada", value = 1),
                    numericInput("edad", "Edad", value = 25),
                    actionButton("predict_empleado_manual", "Predecir")
                ),
                box(title = "Resultados del Modelo", width = 12, verbatimTextOutput("empleadosSummary"))
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Predicción de Ventas
  observeEvent(input$predict_ventas_manual, {
    datos_manual <- data.frame(
      Cliente = factor(input$cliente, levels = clientes_opciones),
      Mes = factor(input$mes, levels = mes_opciones),
      Semana_Año = factor(input$semana_anio, levels = semana_anio_opciones)
    )
    
    prediccion <- predict(modelo_regresion, newdata = datos_manual)
    
    output$ventasSummary <- renderPrint({
      paste("Predicción de Cantidad Semanal:", round(prediccion, 2))
    })
  })
  
  # Clasificación de Empleados
  observeEvent(input$predict_empleado_manual, {
    datos_manual <- data.frame(
      Género = factor(input$genero, levels = genero_opciones),
      Puesto = factor(input$puesto, levels = puesto_opciones),
      Dpto = factor(input$dpto, levels = dpto_opciones),
      SD = input$sd,
      Municipio = factor(input$municipio, levels = municipio_opciones),
      Estado = factor(input$estado, levels = estado_opciones),
      `Estado Civil` = factor(input$estado_civil, levels = estado_civil_opciones),
      Mes_Nacimiento = input$mes_nacimiento,
      Mes_Entrada = input$mes_entrada,
      Edad = input$edad
    )
    
    prediccion <- predict(modelo_cart, newdata = datos_manual, type = "class")
    
    output$empleadosSummary <- renderPrint({
      paste("Predicción de Estatus:", prediccion)
    })
  })
}

# Ejecutar la aplicación Shiny
shinyApp(ui, server)

