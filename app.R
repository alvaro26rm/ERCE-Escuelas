library(shiny)
library(DT)
library(readr)
library(ggplot2)
library(writexl)

# Archivo donde se guardarán los datos
data_file <- "resultados_docentes.csv"

# Código de administrador
codigo_admin <- "admin123"

# Datos de escuelas, grados y estudiantes
escuelas <- list(
  "1001" = "Escuela Caldera",
  "2001" = "Escuela Llano de Piedra"
)

grados <- c("3° Básico")

estudiantes_por_grado <- list(
  "Escuela Caldera" = c(
    "Abrego, Gilberto", "Acosta, Ronnie", "Aviles, Danna", "Barria, Nayelis",
    "Castillo, Jose", "Castillo, Isabella", "Cortes, Jeicoth", "Gonzalez, Ahynara",
    "Gutierrez, Erick", "Marcusi, Martin", "Miranda, Joiner", "Montenegro, Petter",
    "Pitti, Heybbrith", "Reyes, Violet", "Rios, Karina", "Rovira, Abby",
    "Sanchez, Juan", "Tapia, Rianth", "Tugri, Wilmer", "Villarreal, Luis"
  ),
  "Escuela Llano de Piedra" = c(
    "Huiting Pab", "Yuleimis Chávez", "Veronica Morales", "Isabella Samniego",
    "Fernando Rodríguez", "Yeison Rodríguez", "Eneilyn de León", "Librada Samaniego",
    "Brianna Velasco", "Victoria Sánchez", "Kristel Ruíz", "Ariel De Gracia",
    "Juan Mora", "Karielys González", "Enereldo Venado", "Xavier Aguirre",
    "Victor Aparicio"
  )
)

preguntas <- paste("Pregunta", 1:30)
opciones <- c("A", "B", "C", "D", "No responde", "Marca más de una")

# Cargar datos si ya existe el archivo
if (file.exists(data_file)) {
  resultados <- read_csv(data_file, show_col_types = FALSE)
} else {
  resultados <- data.frame(Docente = character(), Escuela = character(),
                           Grado = character(), Estudiante = character(),
                           matrix(NA, nrow = 0, ncol = 30, dimnames = list(NULL, preguntas)),
                           stringsAsFactors = FALSE)
}

ui <- navbarPage("Sistema de Evaluación",
                 tabPanel("Registro de Resultados",
                          sidebarLayout(
                            sidebarPanel(
                              passwordInput("codigo_escuela", "Código de Escuela"),
                              actionButton("verificar", "Ingresar"),
                              textOutput("nombre_escuela"),
                              conditionalPanel(
                                condition = "output.accesoPermitido",
                                textInput("docente", "Nombre del Docente"),
                                selectInput("grado", "Grado", choices = grados),
                                uiOutput("select_estudiantes"),
                                uiOutput("preguntas_respuestas"),
                                actionButton("agregar", "Agregar o Modificar"),
                                actionButton("guardar", "Guardar datos"),
                                downloadButton("descargar", "Exportar a Excel")
                              )
                            ),
                            mainPanel(
                              DTOutput("tabla")
                            )
                          )
                 ),
                 tabPanel("Análisis por Estudiante",
                          fluidRow(
                            column(width = 12, align = "center",
                                   selectInput("estudiante_analisis", "Seleccionar Estudiante", choices = NULL)
                            )
                          ),
                          fluidRow(
                            column(width = 6, align = "center",
                                   plotOutput("grafico_resultados_pie", height = "400px")
                            ),
                            column(width = 6, align = "left",
                                   h4("Resultados Generales del Estudiante"),
                                   verbatimTextOutput("texto_grafico_pie")
                            )
                          ),
                          fluidRow(
                            column(width = 6, align = "center",
                                   plotOutput("grafico_resultados_ejes", height = "400px")
                            ),
                            column(width = 6, align = "left",
                                   h4("Resultados por Eje Temático"),
                                   verbatimTextOutput("texto_grafico_ejes_est")
                            )
                          ),
                          fluidRow(
                            column(width = 6, align = "center",
                                   plotOutput("grafico_resultados_habilidades", height = "400px")
                            ),
                            column(width = 6, align = "left",
                                   h4("Resultados por Habilidad"),
                                   verbatimTextOutput("texto_grafico_habilidades_est")
                            )
                          )
                 ),
                 tabPanel("Análisis por Grado",
                          fluidRow(
                            column(width = 12, align = "center",
                                   selectInput("grado_analisis", "Seleccionar Grado", choices = grados)
                            )
                          ),
                          fluidRow(
                            column(width = 6, align = "center",
                                   plotOutput("grafico_grado", height = "600px")
                            ),
                            column(width = 6, align = "left",
                                   h4("Análisis de Respuestas por Pregunta"),
                                   verbatimTextOutput("texto_grafico_grado")
                            )
                          ),
                          fluidRow(
                            column(width = 6, align = "center",
                                   plotOutput("grafico_grado_torta", height = "400px")
                            ),
                            column(width = 6, align = "left",
                                   h4("Resultados Generales del Grado"),
                                   verbatimTextOutput("texto_grafico_torta")
                            )
                          ),
                          fluidRow(
                            column(width = 6, align = "center",
                                   plotOutput("grafico_grado_ejes", height = "400px")
                            ),
                            column(width = 6, align = "left",
                                   h4("Resultados por Eje Temático"),
                                   verbatimTextOutput("texto_grafico_ejes")
                            )
                          ),
                          fluidRow(
                            column(width = 6, align = "center",
                                   plotOutput("grafico_grado_habilidades", height = "400px")
                            ),
                            column(width = 6, align = "left",
                                   h4("Resultados por Habilidad"),
                                   verbatimTextOutput("texto_grafico_habilidades")
                            )
                          )
                 )
)

server <- function(input, output, session) {
  # Variable reactiva para controlar el acceso
  escuela_seleccionada <- reactiveVal(NULL)
  
  observeEvent(input$verificar, {
    if (input$codigo_escuela %in% names(escuelas)) {
      escuela_seleccionada(escuelas[[input$codigo_escuela]])
    } else {
      showNotification("Código incorrecto", type = "error")
    }
  })
  
  output$nombre_escuela <- renderText({
    req(escuela_seleccionada())
    paste("Escuela seleccionada:", escuela_seleccionada())
  })
  
  output$accesoPermitido <- reactive({ !is.null(escuela_seleccionada()) })
  outputOptions(output, "accesoPermitido", suspendWhenHidden = FALSE)
  
  output$select_estudiantes <- renderUI({
    req(escuela_seleccionada())
    selectInput("estudiante", "Estudiante", choices = estudiantes_por_grado[[escuela_seleccionada()]])
  })
  
  output$preguntas_respuestas <- renderUI({
    req(input$estudiante)
    lapply(preguntas, function(p) {
      selectInput(paste0("respuesta_", p), p, choices = opciones)
    })
  })
  
  datos <- reactiveVal(resultados)
  
  observeEvent(input$agregar, {
    df <- datos()
    index <- which(df$Estudiante == input$estudiante & df$Grado == input$grado & df$Escuela == escuela_seleccionada())
    
    if (length(index) > 0) {
      df[index, "Docente"] <- input$docente
      for (p in preguntas) {
        df[index, p] <- input[[paste0("respuesta_", p)]]
      }
    } else {
      nuevo_registro <- data.frame(Docente = input$docente, Escuela = escuela_seleccionada(),
                                   Grado = input$grado, Estudiante = input$estudiante,
                                   stringsAsFactors = FALSE)
      
      for (p in preguntas) {
        nuevo_registro[[p]] <- input[[paste0("respuesta_", p)]]
      }
      
      df <- rbind(df, nuevo_registro)
    }
    datos(df)
    updateSelectInput(session, "estudiante_analisis", choices = unique(df$Estudiante))
  })
  
  observeEvent(input$guardar, {
    write_csv(datos(), data_file)
    showNotification("Datos guardados correctamente", type = "message")
  })
  
  output$tabla <- renderDT({
    datatable(datos(), escape = FALSE, selection = "none", editable = FALSE)
  })
  
  # **Gráficos y textos para la sección de Análisis por Estudiante**
  output$grafico_resultados_pie <- renderPlot({
    req(input$estudiante_analisis)
    df <- datos()
    
    estudiante_data <- df[df$Estudiante == input$estudiante_analisis, preguntas]
    
    # **Gráfico de Torta: Correctas vs Incorrectas**
    correctas <- sum(estudiante_data == "A") / length(preguntas) * 100
    incorrectas <- 100 - correctas
    
    data_pie <- data.frame(
      Categoria = c("Correctas", "Incorrectas"),
      Porcentaje = c(correctas, incorrectas)
    )
    
    ggplot(data_pie, aes(x = "", y = Porcentaje, fill = Categoria)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), 
                position = position_stack(vjust = 0.5), size = 5, color = "white") +
      scale_fill_manual(values = c("Correctas" = "#31a354", "Incorrectas" = "#d95f0e")) +
      theme_void() +
      labs(title = "Resultados Generales del Estudiante")
  })
  
  output$texto_grafico_pie <- renderText({
    req(input$estudiante_analisis)
    df <- datos()
    
    estudiante_data <- df[df$Estudiante == input$estudiante_analisis, preguntas]
    
    # Calcular el porcentaje de respuestas correctas e incorrectas
    correctas <- sum(estudiante_data == "A") / length(preguntas) * 100
    incorrectas <- 100 - correctas
    
    # Texto explicativo
    paste(
      "Porcentaje de respuestas correctas:", round(correctas, 1), "%\n",
      "Porcentaje de respuestas incorrectas:", round(incorrectas, 1), "%"
    )
  })
  
  output$grafico_resultados_ejes <- renderPlot({
    req(input$estudiante_analisis)
    df <- datos()
    
    estudiante_data <- df[df$Estudiante == input$estudiante_analisis, preguntas]
    
    # **Gráfico de Barras: Correctas por Eje Temático**
    ejes <- c("Geometría", "Álgebra", "Números", "Estadística")
    preguntas_ejes <- list(
      Geometría = 1:8,
      Álgebra = 9:15,
      Números = 16:25,
      Estadística = 26:30
    )
    
    porcentaje_ejes <- sapply(preguntas_ejes, function(indices) {
      mean(estudiante_data[indices] == "A") * 100
    })
    
    data_barras_ejes <- data.frame(Eje = ejes, Porcentaje = porcentaje_ejes)
    
    ggplot(data_barras_ejes, aes(x = Eje, y = Porcentaje, fill = Eje)) +
      geom_bar(stat = "identity", width = 0.6) +
      geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), vjust = -0.5, size = 5) +
      scale_fill_manual(values = c("Geometría" = "#2b8cbe", "Álgebra" = "#d7301f", 
                                   "Números" = "#31a354", "Estadística" = "#756bb1")) +
      labs(title = "Porcentaje de Respuestas Correctas por Eje", x = "Eje Temático", y = "Porcentaje") +
      theme_minimal()
  })
  
  output$texto_grafico_ejes_est <- renderText({
    req(input$estudiante_analisis)
    df <- datos()
    
    estudiante_data <- df[df$Estudiante == input$estudiante_analisis, preguntas]
    
    # Definir ejes temáticos
    ejes <- c("Geometría", "Álgebra", "Números", "Estadística")
    preguntas_ejes <- list(
      Geometría = 1:8,
      Álgebra = 9:15,
      Números = 16:25,
      Estadística = 26:30
    )
    
    # Calcular el porcentaje de respuestas correctas por eje
    porcentaje_ejes <- sapply(preguntas_ejes, function(indices) {
      mean(estudiante_data[indices] == "A") * 100
    })
    
    # Texto explicativo
    paste(
      "Porcentaje de respuestas correctas por eje:\n",
      "Geometría:", round(porcentaje_ejes[1], 1), "%\n",
      "Álgebra:", round(porcentaje_ejes[2], 1), "%\n",
      "Números:", round(porcentaje_ejes[3], 1), "%\n",
      "Estadística:", round(porcentaje_ejes[4], 1), "%"
    )
  })
  
  output$grafico_resultados_habilidades <- renderPlot({
    req(input$estudiante_analisis)
    df <- datos()
    
    estudiante_data <- df[df$Estudiante == input$estudiante_analisis, preguntas]
    
    # **Gráfico de Barras: Dominio de Habilidades**
    habilidades <- c("Reconocimiento", "Problemas Simples", "Problemas Complejos")
    preguntas_habilidades <- list(
      Reconocimiento = c(1:5, 20:30),
      Problemas_Simples = c(6:10, 15:19),
      Problemas_Complejos = 11:14
    )
    
    porcentaje_habilidades <- sapply(preguntas_habilidades, function(indices) {
      mean(estudiante_data[indices] == "A") * 100
    })
    
    data_barras_habilidades <- data.frame(Habilidad = habilidades, Porcentaje = porcentaje_habilidades)
    
    ggplot(data_barras_habilidades, aes(x = Habilidad, y = Porcentaje, fill = Habilidad)) +
      geom_bar(stat = "identity", width = 0.6) +
      geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), vjust = -0.5, size = 5) +
      scale_fill_manual(values = c("Reconocimiento" = "#6baed6", "Problemas Simples" = "#fd8d3c", 
                                   "Problemas Complejos" = "#74c476")) +
      labs(title = "Porcentaje de Dominio por Habilidad", x = "Tipo de Habilidad", y = "Porcentaje") +
      theme_minimal()
  })
  
  output$texto_grafico_habilidades_est <- renderText({
    req(input$estudiante_analisis)
    df <- datos()
    
    estudiante_data <- df[df$Estudiante == input$estudiante_analisis, preguntas]
    
    # Definir habilidades
    habilidades <- c("Reconocimiento", "Problemas Simples", "Problemas Complejos")
    preguntas_habilidades <- list(
      Reconocimiento = c(1:5, 20:30),
      Problemas_Simples = c(6:10, 15:19),
      Problemas_Complejos = 11:14
    )
    
    # Calcular el porcentaje de respuestas correctas por habilidad
    porcentaje_habilidades <- sapply(preguntas_habilidades, function(indices) {
      mean(estudiante_data[indices] == "A") * 100
    })
    
    # Texto explicativo
    paste(
      "Porcentaje de dominio por habilidad:\n",
      "Reconocimiento:", round(porcentaje_habilidades[1], 1), "%\n",
      "Problemas Simples:", round(porcentaje_habilidades[2], 1), "%\n",
      "Problemas Complejos:", round(porcentaje_habilidades[3], 1), "%"
    )
  })
  
  # **Gráficos y textos para la sección de Análisis por Grado**
  output$grafico_grado <- renderPlot({
    req(input$grado_analisis)
    df <- datos()
    
    # Filtrar datos por grado seleccionado
    grado_data <- df[df$Grado == input$grado_analisis, preguntas]
    
    # Calcular el porcentaje de respuestas correctas por pregunta
    porcentaje_correctas <- colMeans(grado_data == "A", na.rm = TRUE) * 100
    
    # Crear un data frame para el gráfico
    data_barras <- data.frame(
      Pregunta = factor(preguntas, levels = preguntas),
      Porcentaje = porcentaje_correctas
    )
    
    # Gráfico de barras horizontales
    ggplot(data_barras, aes(x = Pregunta, y = Porcentaje)) +
      geom_bar(stat = "identity", fill = "#2b8cbe", width = 0.7) +
      geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), hjust = -0.1, size = 4) +
      coord_flip() +
      labs(title = "Porcentaje de Respuestas Correctas por Pregunta",
           x = "Pregunta", y = "Porcentaje de Respuestas Correctas") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 10))
  })
  
  output$texto_grafico_grado <- renderText({
    req(input$grado_analisis)
    df <- datos()
    
    # Filtrar datos por grado seleccionado
    grado_data <- df[df$Grado == input$grado_analisis, preguntas]
    
    # Calcular el porcentaje de respuestas correctas por pregunta
    porcentaje_correctas <- colMeans(grado_data == "A", na.rm = TRUE) * 100
    
    # Identificar las preguntas con mayor y menor tasa de respuestas correctas
    preguntas_max <- which(porcentaje_correctas == max(porcentaje_correctas))
    preguntas_min <- which(porcentaje_correctas == min(porcentaje_correctas))
    
    # Definir ejes temáticos
    ejes <- c("Geometría", "Álgebra", "Números", "Estadística")
    preguntas_ejes <- list(
      Geometría = 1:8,
      Álgebra = 9:15,
      Números = 16:25,
      Estadística = 26:30
    )
    
    # Función para obtener el eje de una pregunta
    obtener_eje <- function(pregunta) {
      if (pregunta %in% preguntas_ejes$Geometría) return("Geometría")
      if (pregunta %in% preguntas_ejes$Álgebra) return("Álgebra")
      if (pregunta %in% preguntas_ejes$Números) return("Números")
      if (pregunta %in% preguntas_ejes$Estadística) return("Estadística")
      return("Desconocido")
    }
    
    # Texto explicativo
    texto <- paste(
      "Preguntas con mayor tasa de respuestas correctas:\n",
      paste(preguntas[preguntas_max], "(", obtener_eje(preguntas_max), ")", collapse = ", "), "\n\n",
      "Preguntas con menor tasa de respuestas correctas:\n",
      paste(preguntas[preguntas_min], "(", obtener_eje(preguntas_min), ")", collapse = ", ")
    )
    
    texto
  })
  
  output$grafico_grado_torta <- renderPlot({
    req(input$grado_analisis)
    df <- datos()
    
    # Filtrar datos por grado seleccionado
    grado_data <- df[df$Grado == input$grado_analisis, preguntas]
    
    # Calcular el porcentaje de respuestas correctas e incorrectas
    correctas <- mean(grado_data == "A", na.rm = TRUE) * 100
    incorrectas <- 100 - correctas
    
    data_pie <- data.frame(
      Categoria = c("Correctas", "Incorrectas"),
      Porcentaje = c(correctas, incorrectas)
    )
    
    # Gráfico de torta
    ggplot(data_pie, aes(x = "", y = Porcentaje, fill = Categoria)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), 
                position = position_stack(vjust = 0.5), size = 5, color = "white") +
      scale_fill_manual(values = c("Correctas" = "#31a354", "Incorrectas" = "#d95f0e")) +
      theme_void() +
      labs(title = "Resultados Generales del Grado")
  })
  
  output$texto_grafico_torta <- renderText({
    req(input$grado_analisis)
    df <- datos()
    
    # Filtrar datos por grado seleccionado
    grado_data <- df[df$Grado == input$grado_analisis, preguntas]
    
    # Calcular el porcentaje de respuestas correctas e incorrectas
    correctas <- mean(grado_data == "A", na.rm = TRUE) * 100
    incorrectas <- 100 - correctas
    
    # Texto explicativo
    paste(
      "Porcentaje de respuestas correctas:", round(correctas, 1), "%\n",
      "Porcentaje de respuestas incorrectas:", round(incorrectas, 1), "%"
    )
  })
  
  output$grafico_grado_ejes <- renderPlot({
    req(input$grado_analisis)
    df <- datos()
    
    # Filtrar datos por grado seleccionado
    grado_data <- df[df$Grado == input$grado_analisis, preguntas]
    
    # Definir ejes temáticos
    ejes <- c("Geometría", "Álgebra", "Números", "Estadística")
    preguntas_ejes <- list(
      Geometría = 1:8,
      Álgebra = 9:15,
      Números = 16:25,
      Estadística = 26:30
    )
    
    # Calcular el porcentaje de respuestas correctas por eje
    porcentaje_ejes <- sapply(preguntas_ejes, function(indices) {
      mean(grado_data[indices] == "A", na.rm = TRUE) * 100
    })
    
    data_barras_ejes <- data.frame(Eje = ejes, Porcentaje = porcentaje_ejes)
    
    # Gráfico de barras por eje temático
    ggplot(data_barras_ejes, aes(x = Eje, y = Porcentaje, fill = Eje)) +
      geom_bar(stat = "identity", width = 0.6) +
      geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), vjust = -0.5, size = 5) +
      scale_fill_manual(values = c("Geometría" = "#2b8cbe", "Álgebra" = "#d7301f", 
                                   "Números" = "#31a354", "Estadística" = "#756bb1")) +
      labs(title = "Porcentaje de Respuestas Correctas por Eje", x = "Eje Temático", y = "Porcentaje") +
      theme_minimal()
  })
  
  output$texto_grafico_ejes <- renderText({
    req(input$grado_analisis)
    df <- datos()
    
    # Filtrar datos por grado seleccionado
    grado_data <- df[df$Grado == input$grado_analisis, preguntas]
    
    # Definir ejes temáticos
    ejes <- c("Geometría", "Álgebra", "Números", "Estadística")
    preguntas_ejes <- list(
      Geometría = 1:8,
      Álgebra = 9:15,
      Números = 16:25,
      Estadística = 26:30
    )
    
    # Calcular el porcentaje de respuestas correctas por eje
    porcentaje_ejes <- sapply(preguntas_ejes, function(indices) {
      mean(grado_data[indices] == "A", na.rm = TRUE) * 100
    })
    
    # Texto explicativo
    paste(
      "Porcentaje de respuestas correctas por eje:\n",
      "Geometría:", round(porcentaje_ejes[1], 1), "%\n",
      "Álgebra:", round(porcentaje_ejes[2], 1), "%\n",
      "Números:", round(porcentaje_ejes[3], 1), "%\n",
      "Estadística:", round(porcentaje_ejes[4], 1), "%"
    )
  })
  
  output$grafico_grado_habilidades <- renderPlot({
    req(input$grado_analisis)
    df <- datos()
    
    # Filtrar datos por grado seleccionado
    grado_data <- df[df$Grado == input$grado_analisis, preguntas]
    
    # Definir habilidades
    habilidades <- c("Reconocimiento", "Problemas Simples", "Problemas Complejos")
    preguntas_habilidades <- list(
      Reconocimiento = c(1:5, 20:30),
      Problemas_Simples = c(6:10, 15:19),
      Problemas_Complejos = 11:14
    )
    
    # Calcular el porcentaje de respuestas correctas por habilidad
    porcentaje_habilidades <- sapply(preguntas_habilidades, function(indices) {
      mean(grado_data[indices] == "A", na.rm = TRUE) * 100
    })
    
    data_barras_habilidades <- data.frame(Habilidad = habilidades, Porcentaje = porcentaje_habilidades)
    
    # Gráfico de barras por habilidad
    ggplot(data_barras_habilidades, aes(x = Habilidad, y = Porcentaje, fill = Habilidad)) +
      geom_bar(stat = "identity", width = 0.6) +
      geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), vjust = -0.5, size = 5) +
      scale_fill_manual(values = c("Reconocimiento" = "#6baed6", "Problemas Simples" = "#fd8d3c", 
                                   "Problemas Complejos" = "#74c476")) +
      labs(title = "Porcentaje de Dominio por Habilidad", x = "Tipo de Habilidad", y = "Porcentaje") +
      theme_minimal()
  })
  
  output$texto_grafico_habilidades <- renderText({
    req(input$grado_analisis)
    df <- datos()
    
    # Filtrar datos por grado seleccionado
    grado_data <- df[df$Grado == input$grado_analisis, preguntas]
    
    # Definir habilidades
    habilidades <- c("Reconocimiento", "Problemas Simples", "Problemas Complejos")
    preguntas_habilidades <- list(
      Reconocimiento = c(1:5, 20:30),
      Problemas_Simples = c(6:10, 15:19),
      Problemas_Complejos = 11:14
    )
    
    # Calcular el porcentaje de respuestas correctas por habilidad
    porcentaje_habilidades <- sapply(preguntas_habilidades, function(indices) {
      mean(grado_data[indices] == "A", na.rm = TRUE) * 100
    })
    
    # Texto explicativo
    paste(
      "Porcentaje de dominio por habilidad:\n",
      "Reconocimiento:", round(porcentaje_habilidades[1], 1), "%\n",
      "Problemas Simples:", round(porcentaje_habilidades[2], 1), "%\n",
      "Problemas Complejos:", round(porcentaje_habilidades[3], 1), "%"
    )
  })
}

shinyApp(ui, server)