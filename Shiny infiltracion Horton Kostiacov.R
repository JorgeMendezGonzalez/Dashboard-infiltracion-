# Cargar las librerías necesarias
library(shiny)
library(shinydashboard)
library(plotly)
library(minpack.lm)
library(DT)
library(nlstools)

# Definir la interfaz de usuario
ui <- dashboardPage(
  dashboardHeader(title = "D-JMG"),
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
        .estimacion {
          color: green;
          font-size: 16px; /* Tamaño de fuente ajustado */
        }
      "))
    ),
    fluidRow(
      column(12, align = "center", h4("Simulación de infiltración"))
    ),
    tabItems(
      tabItem(tabName = "cilindro",
              fluidRow(
                box(width = 8, plotlyOutput("cilindroPlot", height = "600px")),
                box(width = 4,
                    h4("Dimensiones del Cilindro"),
                    sliderInput("alturaCilindro", "Altura del Cilindro (cm):", 
                                min = 10, max = 200, value = 30),
                    sliderInput("radioCilindro", "Radio del Cilindro (cm):", 
                                min = 5, max = 50, value = 20),
                    h4("Altura del Líquido"),
                    sliderInput("alturaLiquido", "Altura del Líquido (cm):", 
                                min = 0, max = 200, value = 25),
                    h4("Color del Líquido"),
                    selectInput("colorLiquido", "Selecciona el color del líquido:",
                                choices = c("LightBlue" = "lightblue", "Blue" = "blue", "Green" = "green", "Red" = "red")),
                    h4("Rellenar el Cilindro"),
                    sliderInput("cantidadRelleno", "Cantidad a Rellenar (cm):", 
                                min = 0, max = 30, value = 5),
                    actionButton("applyRelleno", "Aplicar Relleno"),
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
                    verbatimTextOutput("coeficientes_modelo"),  # Nuevo output para coeficientes
                    verbatimTextOutput("interpretacion_coeficientes"),  # Nuevo output para interpretación
                    plotlyOutput("plot_modelo"),  
                    tableOutput("resultados_modelos"),
                    textOutput("estimacion_personalizada", container = function(...) {
                      tags$span(class = "estimacion", ...)
                    }),  # Nuevo output para estimación personalizada
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
                        tags$li("Rellenar el Cilindro: Selecciona la cantidad a rellenar."),
                        tags$li("Intervalo de Tiempo: Selecciona el intervalo de tiempo para el cálculo."),
                        tags$li("Baja del Agua: Simula una disminución en milímetros."),
                        tags$li("Tabla de Historial: Muestra un registro de los cambios.")
                      ),
                      tags$li(h4("Gráfica de Dispersión:")),
                      tags$ul(
                        tags$li("Representa los eventos y sus correspondientes velocidades de infiltración.")
                      ),
                      tags$li(h4("Ajuste de Modelos:")),
                      tags$ul(
                        tags$li("Permite ajustar modelos de infiltración (Horton y Kostiakov) a los datos observados."),
                        tags$li("Muestra los coeficientes del modelo ajustado junto con su interpretación.")
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
  
  # Observador para aplicar el relleno del agua
  observeEvent(input$applyRelleno, {
    cantidadRelleno <- input$cantidadRelleno
    nueva_altura <- alturaLiquido() + cantidadRelleno
    alturaLiquido(nueva_altura)
    
    # Cambiar el color del líquido al rellenar
    updateSelectInput(session, "colorLiquido", selected = sample(c("lightblue", "blue", "green", "red"), 1))  # Cambia a un color diferente al rellenar
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
    updateSelectInput(session, "colorLiquido", selected = "lightblue")  # Reiniciar color del líquido
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
      output$coeficientes_modelo <- renderPrint({"No hay datos para ajustar modelos."})  # Nuevo output vacío
      output$interpretacion_coeficientes <- renderPrint({"No hay datos para ajustar modelos."})  # Nuevo output vacío
      output$plot_modelo<- renderPlotly({NULL})  
      output$tabla_observados_estimados <- renderTable({data.frame("No hay datos para mostrar")})  # Nueva tabla vacía
      output$estimacion_personalizada <- renderText({"No hay estimación disponible."})  # Nuevo output vacío
      return()
    }
    
    modelo<- input$modeloElegido
    resultados<- data.frame(Modelo=character(),Coeficiente=I(list()),R2=numeric())
    
    # Resumen del modelo ajustado
    summary_modelo <- NULL
    estimaciones <- NULL
    coeficientes <- NULL
    interpretaciones <- NULL
    
    # Función para ajustar el modelo seleccionado
    fit_model_horton <- function(formula, start) {
      tryCatch({
        model <- nlsLM(formula, data = datos, start = start)
        summary_modelo <<- summary(model)
        estimaciones <<- data.frame(Tiempo = datos$Tiempo.acumulado..min., Estimado = predict(model))
        coeficientes <<- coef(model)  
        interpretaciones <<- c(
          paste("a (capacidad máxima):", coeficientes['a']),
          paste("b (tasa de disminución):", coeficientes['b']),
          paste("c (velocidad mínima):", coeficientes['c'])
        )
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
        interpretaciones <<- c(
          paste("a (capacidad máxima):", coeficientes['a']),
          paste("b (exponente):", coeficientes['b'])
        )
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
    
    # Renderizar resultados
    output$resultados_modelos <- renderTable({
      resultados
    })
    
    output$summary_modelo <- renderPrint({
      if (!is.null(summary_modelo)) {
        print(summary_modelo)
      }
    })
    
    output$coeficientes_modelo <- renderPrint({
      if (!is.null(coeficientes)) {
        print(coeficientes)
      }
    })
    
    output$interpretacion_coeficientes <- renderPrint({
      if (!is.null(interpretaciones)) {
        paste(interpretaciones, collapse = "\n")
      }
    })
    
    # Predicción manual basada en el tiempo ingresado
    observeEvent(input$tiempoManual, {
      tiempo_ingresado <- input$tiempoManual
      if (modelo == "Horton" && !is.null(coeficientes)) {
        estimacion <- coeficientes['a'] * exp(-coeficientes['b'] * tiempo_ingresado) + coeficientes['c']
        output$estimacion_personalizada <- renderText({
          paste("Valor estimado (Horton) para", tiempo_ingresado, "min:", round(estimacion, 2))
        })
      } else if (modelo == "Kostiakov" && !is.null(coeficientes)) {
        estimacion <- coeficientes['a'] * tiempo_ingresado^coeficientes['b']
        output$estimacion_personalizada <- renderText({
          paste("Valor estimado (Kostiakov) para", tiempo_ingresado, "min:", round(estimacion, 2))
        })
      } else {
        output$estimacion_personalizada <- renderText({"Modelo no ajustado o coeficientes no disponibles."})
      }
    })
    
    output$plot_modelo <- renderPlotly({
      if (!is.null(estimaciones)) {
        plot_ly(data = estimaciones, x = ~Tiempo, y = ~Estimado, type = 'scatter', mode = 'lines', name = 'Estimado') %>%
          add_trace(data = datos, x = ~Tiempo.acumulado..min., y = ~Velocidad.de.Infiltración..cm.hr., mode = 'markers', name = 'Observado') %>%
          layout(title = "Ajuste del Modelo",
                 xaxis = list(title = "Tiempo acumulado (min)"),
                 yaxis = list(title = "Velocidad de Infiltración (cm/hr)"))
      }
    })
    
    output$tabla_observados_estimados <- renderTable({
      if (!is.null(estimaciones)) {
        data.frame(Observados = datos$Velocidad.de.Infiltración..cm.hr., Estimados = estimaciones$Estimado)
      }
    })
  })
}

# Ejecutar la aplicación
shinyApp(ui, server)

                 
                 
                 
