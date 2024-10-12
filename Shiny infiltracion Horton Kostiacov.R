# Cargar las librerías necesarias
library(shiny)
library(shinydashboard)
library(plotly)
library(minpack.lm)
library(DT)
library(nlstools)

# Definir la interfaz de usuario
ui <- dashboardPage(
  dashboardHeader(title = "Dibujo de Cilindro 3D"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Cilindro", tabName = "cilindro", icon = icon("cylinder")),
      menuItem("Gráfica de Dispersión", tabName = "dispersión", icon = icon("scatter-plot")),
      menuItem("Tabla de Infiltración", tabName = "tabla_infiltracion", icon = icon("table")),
      menuItem("Ajuste de Modelos", tabName = "ajuste_modelos", icon = icon("chart-line")),
      menuItem("Ayuda", tabName = "ayuda", icon = icon("question-circle"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .sliderInput .irs-bar {
          background-color: green;
        }
        .sliderInput .irs-bar-edge {
          background-color: green;
        }
        .sliderInput .irs-slider {
          background-color: green;
        }
      "))
    ),
    fluidRow(
      column(12, align = "center", h4("Elaborado por: Jorge Méndez Gonzalez | UAAAN | oct 2024 | Simulación de infiltración"))
    ),
    tabItems(
      tabItem(tabName = "cilindro",
              fluidRow(
                box(width = 8, plotlyOutput("cilindroPlot", height = "600px")),
                box(width = 4,
                    h4("Dimensiones del Cilindro"),
                    sliderInput("alturaCilindro", "Altura del Cilindro (cm):", 
                                min = 10, max = 100, value = 30),
                    sliderInput("radioCilindro", "Radio del Cilindro (cm):", 
                                min = 5, max = 50, value = 20),
                    h4("Altura del Líquido"),
                    sliderInput("alturaLiquido", "Altura del Líquido (cm):", 
                                min = 0, max = 30, value = 25),
                    h4("Color del Líquido"),
                    selectInput("colorLiquido", "Selecciona el color del líquido:",
                                choices = c("LightBlue" = "lightblue", "Blue" = "blue", "Green" = "green", "Red" = "red")),
                    h4("Intervalo de Tiempo (min):"),
                    sliderInput("intervaloTiempo", "Selecciona el intervalo (min):", 
                                min = 1, max = 30, value = 1),
                    numericInput("bajaAgua", "Baja del Agua (mm):", 
                                 value = 1, min = 0, max = 50),
                    actionButton("applyDecrease", "Aplicar Baja"),
                    actionButton("reset", "Reiniciar"),
                    tableOutput("tablaAltura")
                )
              )
      ),
      tabItem(tabName = "dispersión",
              fluidRow(
                box(width = 12, plotlyOutput("dispersiónPlot", height = "600px"))
              )
      ),
      tabItem(tabName = "tabla_infiltracion",
              fluidRow(
                box(width = 12,
                    h3("Tabla de Infiltración"),
                    selectInput("show_entries", "Mostrar entradas:", 
                                choices = c(5, 10, 15, 20, 30, 40, 50), selected = 10),
                    DTOutput("tablaInfiltracion")  # Cambiado a DTOutput
                )
              )
      ),
      tabItem(tabName = "ajuste_modelos",
              fluidRow(
                box(width = 12,
                    h3("Ajuste de Modelos de Infiltración"),
                    selectInput("modeloElegido", "Selecciona el modelo a ajustar:",
                                choices = c("Horton", "Kostiakov")),
                    numericInput("tiempoManual", "Ingresa el Tiempo (min) para hacer estimaciones con el modelo:", value = 1, min = 0),  
                    actionButton("ajustar_modelos", "Ajustar Modelo"),
                    verbatimTextOutput("summary_modelo"),
                    plotlyOutput("plot_modelo"),  
                    tableOutput("resultados_modelos"),
                    verbatimTextOutput("estimacion_manual"),
                    tableOutput("tabla_observados_estimados")  # Nueva tabla para observados y estimados
                )
              )
      ),
      tabItem(tabName = "ayuda",
              fluidRow(
                box(width = 12,
                    h3("Instrucciones para el Uso de la Aplicación"),
                    p("Esta aplicación permite visualizar un cilindro en 3D y simular la variación de la altura del líquido dentro de él."),
                    h4("Secciones de la Aplicación"),
                    tags$ul(
                      tags$li(h4("Cilindro:")),
                      tags$ul(
                        tags$li("Dimensiones del Cilindro: Ajusta la altura y el radio."),
                        tags$li("Altura del Líquido: Establece la altura inicial del líquido."),
                        tags$li("Color del Líquido: Selecciona el color del líquido."),
                        tags$li("Intervalo de Tiempo: Selecciona el intervalo de tiempo para el cálculo."),
                        tags$li("Baja del Agua: Simula una disminución en milímetros."),
                        tags$li("Tabla de Historial: Muestra un registro de los cambios.")
                      ),
                      tags$li(h4("Gráfica de Dispersión:")),
                      tags$ul(
                        tags$li("Representa los eventos y sus correspondientes velocidades de infiltración.")
                      )
                    ),
                    h4("Uso General"),
                    p("Ajusta los controles deslizantes según tus preferencias. Usa 'Aplicar Baja' para ver cómo cambia la altura. Si deseas volver a empezar, utiliza 'Reiniciar'. Observa cómo se actualiza la tabla y la gráfica con cada acción."),
                    h4("Notas Adicionales"),
                    p("Asegúrate de que las dimensiones seleccionadas sean coherentes para una mejor visualización."),
                    h4("- Modelos de Infiltración:"),
                    p(h4("- Modelo Horton:")),
                    p("- Ecuación: V = a * e^{-b * t} + c"),
                    p("- Componentes:"),
                    tags$ul(
                      tags$li("$V$: Velocidad de infiltración (cm/hr)"),
                      tags$li("$t$: Tiempo acumulado (min)"),
                      tags$li("$a$: Coeficiente que representa la capacidad máxima de infiltración."),
                      tags$li("$b$: Tasa a la que disminuye la infiltración con el tiempo."),
                      tags$li("$c$: Velocidad mínima de infiltración cuando el suelo está saturado.")
                    ),
                    p(h4("- Modelo Kostiakov:")),
                    p("- Ecuación: V = a * t^b"),
                    p("- Componentes:"),
                    tags$ul(
                      tags$li("$V$: Velocidad de infiltración (cm/hr)"),
                      tags$li("$t$: Tiempo acumulado (min)"),
                      tags$li("$a$: Coeficiente que representa la capacidad máxima de infiltración."),
                      tags$li("$b$: Exponente que indica la tasa de disminución de la infiltración.")
                    )
                )
              )
      )
    )
  )
)

# Definir la lógica del servidor
server <- function(input, output, session) {
  
  # Crear una variable reactiva para almacenar la altura del líquido
  alturaLiquido <- reactiveVal(25)
  
  # Crear una lista reactiva para almacenar los valores acumulativos
  historialAltura <- reactiveVal(data.frame(
    "Descripción"= character(0),
    "Lamina infiltrada (cm)"= numeric(0),
    "Intervalo de tiempo (min)"= numeric(0),
    "Tiempo acumulado (min)"= numeric(0),
    "Velocidad de Infiltración (cm/hr)"= numeric(0),
    "Estimacion_Modelo"= numeric(0)  
  ))
  
  # Función para generar el cilindro
  output$cilindroPlot <- renderPlotly({
    altura_cilindro <- input$alturaCilindro
    radio <- input$radioCilindro
    
    t <- seq(0, 2 * pi, length.out=100)
    z_cilindro <- seq(0, altura_cilindro, length.out=2)
    
    x <- outer(radio * cos(t), rep(1, length(z_cilindro)))
    y <- outer(radio * sin(t), rep(1, length(z_cilindro)))
    z <- outer(rep(1, length(t)), z_cilindro)
    
    plot_ly() %>% 
      add_surface(x=~x, y=~y, z=~z, 
                  showscale=FALSE, opacity=0.3, 
                  colorscale=list(c(0,1), c("gray","gray"))) %>% 
      add_surface(x=~x, y=~y, z=~pmin(z, alturaLiquido()), 
                  showscale=FALSE, opacity=0.8, 
                  colorscale=list(c(0,1), c(input$colorLiquido, input$colorLiquido))) %>% 
      layout(scene=list(
        zaxis=list(title="Altura (cm)", range=c(0, altura_cilindro)),
        xaxis=list(title="X (cm)", range=c(-radio, radio)),
        yaxis=list(title="Y (cm)", range=c(-radio, radio)),
        camera=list(eye=list(x=1.5,y=1.5,z=1))
      ))
  })
  
  # Observador para el slider de altura del líquido
  observe({
    alturaLiquido(input$alturaLiquido)
  })
  
  # Observador para aplicar la baja del agua
  observeEvent(input$applyDecrease,{
    disminucion_cm <- input$bajaAgua /10
    new_altura <- alturaLiquido() - disminucion_cm
    alturaLiquido(new_altura)
    
    nuevo_historial <- historialAltura()
    tiempo_acumulado <- ifelse(nrow(nuevo_historial)==0,0,sum(nuevo_historial$Intervalo.de.tiempo..min.)) + input$intervaloTiempo
    
    nuevo_historial <- rbind(nuevo_historial,
                             data.frame(
                               "Descripción"="Baja del agua",
                               "Lamina infiltrada (cm)"=disminucion_cm,
                               "Intervalo de tiempo (min)"=input$intervaloTiempo,
                               "Tiempo acumulado (min)"=tiempo_acumulado,
                               "Velocidad de Infiltración (cm/hr)"=(disminucion_cm/input$intervaloTiempo)*60,
                               "Estimacion_Modelo"=NA  
                             ))
    
    historialAltura(nuevo_historial)
  })
  
  # Observador para reiniciar
  observeEvent(input$reset,{
    alturaLiquido(25)
    historialAltura(data.frame(
      "Descripción"=character(0),
      "Lamina infiltrada (cm)"=numeric(0), 
      "Intervalo de tiempo (min)"=numeric(0), 
      "Tiempo acumulado (min)"=numeric(0), 
      "Velocidad de Infiltración (cm/hr)"=numeric(0),
      "Estimacion_Modelo"=numeric(0)  
    ))
  })
  
  # Tabla de historial de alturas
  output$tablaAltura <- renderTable({
    historialAltura()
  })
  
  # Renderizar la gráfica de dispersión
  output$dispersiónPlot <- renderPlotly({
    datos <- historialAltura()
    if(nrow(datos)==0){
      return(NULL)
    }
    
    plot_ly(data=datos,x=~Tiempo.acumulado..min.,y=~Velocidad.de.Infiltración..cm.hr., 
            type='scatter',mode='lines+markers', 
            marker=list(size=10,color='blue',opacity=0.5),
            line=list(color='blue')) %>%
      layout(title="Gráfica de Dispersión",
             xaxis=list(title="Tiempo acumulado (min)"),
             yaxis=list(title="Velocidad de Infiltración (cm/hr)"))
  })
  
  # Renderizar la tabla de infiltración
  output$tablaInfiltracion <- renderDT({
    datos <- historialAltura()
    if(nrow(datos)==0){
      return(data.frame("No hay datos disponibles"))
    }
    datatable(datos, extensions = "Buttons", 
              options = list(dom = "Bfrtip",
                             buttons = c("copy", "csv", "excel", "pdf"),
                             pageLength = input$show_entries))  # Se agrega la opción de mostrar entradas
  })
  
  # Ajuste de modelos
  observeEvent(input$ajustar_modelos,{
    datos <- historialAltura()
    if(nrow(datos)==0){
      output$resultados_modelos<- renderTable({data.frame ("No hay datos para ajustar modelos")})
      output$summary_modelo<- renderPrint({"No hay datos para ajustar modelos."})
      output$plot_modelo<- renderPlotly({NULL})  
      output$estimacion_manual<- renderPrint({"No hay datos para estimar."})  
      output$tabla_observados_estimados <- renderTable({data.frame("No hay datos para mostrar")})  # Nueva tabla vacía
      return()
    }
    
    modelo<- input$modeloElegido
    resultados<- data.frame(Modelo=character(),Coeficiente=I(list()),R2=numeric())
    
    # Resumen del modelo ajustado
    summary_modelo <- NULL
    estimaciones <- NULL
    coeficientes <- NULL
    
    # Función para ajustar el modelo seleccionado
    fit_model_horton <- function(formula, start) {
      tryCatch({
        model <- nlsLM(formula, data = datos, start = start)
        summary_modelo <<- summary(model)
        estimaciones <<- data.frame(Tiempo = datos$Tiempo.acumulado..min., Estimado = predict(model))
        coeficientes <<- coef(model)  
        resultados <<- rbind(resultados, data.frame(Modelo = "Horton",
                                                    Coeficiente = list(coef(model)), 
                                                    R2 = summary(model)$r.squared))
        
        for (i in 1:nrow(estimaciones)) {
          historialAltura()[nrow(historialAltura()), "Estimacion_Modelo"] <<- estimaciones$Estimado[i]
        }
        
        # Agregar estimaciones al historial
        nuevo_historial <- historialAltura()
        nuevo_historial$Estimacion_Modelo <- c(rep(NA, nrow(nuevo_historial) - nrow(estimaciones)), estimaciones$Estimado)
        historialAltura(nuevo_historial)
        
      }, error = function(e) {
        resultados <<- rbind(resultados, data.frame(Modelo = "Horton", Coeficiente = NA, R2 = NA))
      })
    }
    
    fit_model_kostiakov <- function() {
      formulaKost <- as.formula(Velocidad.de.Infiltración..cm.hr. ~ a * Tiempo.acumulado..min.^b)
      tryCatch({
        model <- nls(formulaKost, data = datos, start = list(a = 10, b = -0.5))
        summary_modelo <<- summary(model)
        estimaciones <<- data.frame(Tiempo = datos$Tiempo.acumulado..min., Estimado = predict(model))
        coeficientes <<- coef(model)  
        resultados <<- rbind(resultados, data.frame(Modelo = "Kostiakov",
                                                    Coeficiente = list(coef(model)), 
                                                    R2 = summary(model)$r.squared))
        
        for (i in 1:nrow(estimaciones)) {
          historialAltura()[nrow(historialAltura()), "Estimacion_Modelo"] <<- estimaciones$Estimado[i]
        }
        
        # Agregar estimaciones al historial
        nuevo_historial <- historialAltura()
        nuevo_historial$Estimacion_Modelo <- c(rep(NA, nrow(nuevo_historial) - nrow(estimaciones)), estimaciones$Estimado)
        historialAltura(nuevo_historial)
        
      }, error = function(e) {
        resultados <<- rbind(resultados, data.frame(Modelo = "Kostiakov", Coeficiente = NA, R2 = NA))
      })
    }
    
    if (modelo == "Horton") {
      fit_model_horton(Velocidad.de.Infiltración..cm.hr. ~ a * exp(-b * Tiempo.acumulado..min.) + c,
                       start = list(a = 1, b = 0.1, c = 0))
    } else if (modelo == "Kostiakov") {
      fit_model_kostiakov()
    }
    
    # Mostrar resultados
    output$resultados_modelos <- renderTable({
      resultados
    })
    
    # Mostrar resumen del modelo ajustado
    output$summary_modelo <- renderPrint({
      if (!is.null(summary_modelo)) {
        summary_modelo
      } else {
        "No se pudo ajustar el modelo."
      }
    })
    
    # Graficar los datos observados y estimados
    output$plot_modelo <- renderPlotly({
      plot_ly() %>%
        add_trace(data = datos, x = ~Tiempo.acumulado..min., y = ~Velocidad.de.Infiltración..cm.hr., 
                  type = 'scatter', mode = 'lines+markers', name = 'Observados', 
                  marker = list(color = 'blue')) %>%
        add_trace(data = estimaciones, x = ~Tiempo, y = ~Estimado,
                  type = 'scatter', mode = 'lines', name = 'Estimados',
                  line = list(color = 'red')) %>%
        layout(title = paste("Ajuste del Modelo:", modelo),
               xaxis = list(title = "Tiempo acumulado (min)"),
               yaxis = list(title = "Velocidad de Infiltración (cm/hr)"))
    })
    
    # Estimación manual
    observeEvent(input$tiempoManual, {
      tiempo_manual <- input$tiempoManual
      estimacion <- NA
      
      if (!is.null(coeficientes)) {
        if (modelo == "Horton") {
          a <- coeficientes[1]
          b <- coeficientes[2]
          c <- coeficientes[3]
          estimacion <- a * exp(-b * tiempo_manual) + c
        } else if (modelo == "Kostiakov") {
          a <- coeficientes[1]
          b <- coeficientes[2]
          estimacion <- a * tiempo_manual^b
        }
      }
      
      output$estimacion_manual <- renderPrint({
        if (!is.na(estimacion)) {
          paste("Estimación de infiltración para ", tiempo_manual, " min:", round(estimacion, 2), " cm/hr")
        } else {
          "No se pudo realizar la estimación."
        }
      })
    })
    
    # Mostrar tabla de observados y estimados
    output$tabla_observados_estimados <- renderTable({
      if (nrow(datos) > 0 && !is.null(estimaciones)) {
        data.frame(
          "Tiempo acumulado (min)" = datos$Tiempo.acumulado..min.,
          "Velocidad Observada (cm/hr)" = datos$Velocidad.de.Infiltración..cm.hr.,
          "Velocidad Estimada (cm/hr)" = estimaciones$Estimado
        )
      } else {
        data.frame("No hay datos para mostrar")
      }
    })
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)

                 
                 
                 