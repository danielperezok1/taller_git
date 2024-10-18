library(shiny)
library(shinydashboard)
library(lattice)
library(gridExtra)
library(RColorBrewer)
library(dplyr)
library(plotly)
library(viridis)
library(DT)
library(ggplot2)
library(leaflet)
library(sf)
library(readr)
library(latexpdf)
library(tinytex)
library(rmarkdown)
library(shinyWidgets)


data <- read.csv("foliaresDT.csv")

calcularFrecuencias <- function(merged_data) {
  tabla_contingencia <- table(merged_data$cluster_prescripciones, merged_data$cluster_cosecha)
  frecuencias_por_ambiente <- prop.table(tabla_contingencia, margin = 1) * 100
  return(frecuencias_por_ambiente)
}

lotes <- c("Huinca","Canals","Laguna", "Tucuman")


maximo <- function(xname, yname, x_unidad_medida, y_unidad_medida, b0, b1_x, b2_y, b3_xx, b4_yy, b5_x_y, x, y, precio_tha, costoX, costoY, n_cortes) {
  valorespred <- expand.grid(Densidad = x, Fertilizante = y)
  valorespred <- transform(valorespred, Rendimiento = (
    b0 + b1_x * Densidad + b2_y * Fertilizante + b3_xx * Densidad^2 + b4_yy * Fertilizante^2 + b5_x_y * Densidad * Fertilizante))
  valorespred$Ingreso <- valorespred$Rendimiento * precio_tha
  valorespred$Costo <- valorespred$Fertilizante * costoY + valorespred$Densidad * costoX
  valorespred$MB <- valorespred$Ingreso - valorespred$Costo
  
  resultados <- list(
    "DOA" = valorespred[which.max(valorespred$Rendimiento),],
    "DOE" = valorespred[which.max(valorespred$MB),]
  )
  
  resultados_tabla <- do.call(rbind, resultados)
  
  myPlotRend <- contourplot(Rendimiento ~ Densidad * Fertilizante, data = valorespred, region = TRUE,
                            xlab = paste(xname, x_unidad_medida), ylab = paste(yname, y_unidad_medida),
                            main = "Rendimiento",
                            col.regions = viridis(n_cortes),  
                            labels = list(labels = round(seq(min(valorespred$Rendimiento), max(valorespred$Rendimiento), length.out = n_cortes), 1), cex = 1),
                            label.style = 'align', scales = list(cex = 1.2))
  
  myPlotMB <- contourplot(MB ~ Densidad * Fertilizante, data = valorespred, region = TRUE,
                          xlab = paste(xname, x_unidad_medida), ylab = paste(yname, y_unidad_medida),
                          main = "Margen Bruto USD/ha",
                          col.regions = viridis(n_cortes),  
                          labels = list(labels = round(seq(min(valorespred$MB), max(valorespred$MB), length.out = n_cortes), 1), cex = 1),
                          label.style = 'align', scales = list(cex = 1.2))
  
  list(resultados = resultados_tabla, myPlotRend = myPlotRend, myPlotMB = myPlotMB, valorespred = valorespred)
}

calcula_productor <- function(densidad_prod, fertilizante_prod, precio_tha, costoX, costoY, b0, b1_x, b2_y, b3_xx, b4_yy, b5_x_y) {
  rendimiento_prod <- b0 + b1_x * densidad_prod + b2_y * fertilizante_prod + b3_xx * densidad_prod^2 + b4_yy * fertilizante_prod^2 + b5_x_y * densidad_prod * fertilizante_prod
  ingreso_prod <- rendimiento_prod * precio_tha
  costo_prod <- fertilizante_prod * costoY + densidad_prod * costoX
  mb_prod <- ingreso_prod - costo_prod
  data.frame(Rendimiento = rendimiento_prod, MB = mb_prod)
}

# Cargar coeficientes desde un CSV
coeficientes_csv <- read_csv("coeficientes.csv")
coeficientes <- coeficientes_csv %>%
  group_by(Hibrido, Region) %>%
  summarise(
    alta = list(b0 = b0_alta, b1_x = b1_x_alta, b2_y = b2_y_alta, b3_xx = b3_xx_alta, b4_yy = b4_yy_alta, b5_x_y = b5_x_y_alta),
    media = list(b0 = b0_media, b1_x = b1_x_media, b2_y = b2_y_media, b3_xx = b3_xx_media, b4_yy = b4_yy_media, b5_x_y = b5_x_y_media),
    baja = list(b0 = b0_baja, b1_x = b1_x_baja, b2_y = b2_y_baja, b3_xx = b3_xx_baja, b4_yy = b4_yy_baja, b5_x_y = b5_x_y_baja)
  ) %>%
  ungroup() %>%
  split(.$Hibrido)



assign_labels <- function(cluster_centers) {
  ordered_indices <- order(cluster_centers)
  cluster_labels <- c("Baja", "Media", "Alta")[match(1:length(cluster_centers), ordered_indices)]
  return(cluster_labels)
}



ui <- dashboardPage(
  dashboardHeader(title = "Red Tester 2024"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Experta", tabName = "tab_red_experta", icon = icon("info-circle")),
      menuItem("NxDxA", tabName = "tab_modelos", icon = icon("chart-line")),
      menuItem("Resumen NxDxA", tabName = "tab_resumen", icon = icon("list")),
      menuItem("Comparación Global", tabName = "tab_comparacion_global", icon = icon("globe")),
      menuItem("Ambientes", tabName = "tab_ambientes", icon = icon("map")),
      menuItem("Foliares", tabName = "tab_datos_foliares", icon = icon("leaf")),
      menuItem("Exportar a PDF", tabName = "tab_exportar", icon = icon("file-pdf")),
      menuItem("Configuración", tabName = "tab_configuracion", icon = icon("cogs"))
    )
  ),
  dashboardBody(
    # Selector global de región, que estará visible en todas las pestañas excepto "Foliares"
    conditionalPanel(
      condition = "input.tabName != 'tab_datos_foliares'",
      fluidRow(
        column(12,
               div(style = "padding: 10px; background-color: #f0f0f0; border-bottom: 2px solid #006064;",
                   selectInput("region", "Seleccione la Región:", 
                               choices = c("Todas", unique(coeficientes_csv$Region)),
                               selected = "Todas")
               )
        )
      )
    )
    ,
    tabItems(
      # Tab Red Experta
      tabItem(tabName = "tab_red_experta",
              fluidPage(
                titlePanel("Red Experta"),
                h3("Resultados de la Red tester"),
                p("Esta aplicación Shiny ha sido desarrollada para visualizar las curvas de respuesta y la eficiencia de diversos híbridos en diferentes regiones y ambientes. Seleccione una región para comenzar a explorar los datos."),
                p("En esta pestaña puede seleccionar la región que desea visualizar. Esto afectará los datos presentados en las pestañas 'Modelos' y 'Resumen'.")
              )
      ),
      tabItem(tabName = "tab_exportar",
              fluidPage(
                titlePanel("Exportar Información a PDF"),
                selectInput("exportar_opcion", "Seleccione el contenido a exportar:",
                            choices = c("Modelos", "Resumen", "Comparación Global")),
                actionButton("exportar_pdf", "Exportar a PDF", icon = icon("file-pdf"))
              )
      ),
      # Tab Modelos
      tabItem(tabName = "tab_modelos",
              fluidPage(
                titlePanel(div(style = "background-color: #006064; color: white; padding: 15px; border-radius: 8px;", "Curvas de Respuesta probable")),
                fluidRow(
                  column(2,
                         div(style = "border-right: 1px solid #006064; height: 100%; padding: 10px;",
                             uiOutput("hibrido_selector"),
                             radioButtons("ambiente", "Seleccione el potencial de rendimiento:", 
                                          choices = c("Alta" = "alta", "Media"= "media", "Baja" = "baja")),
                             sliderInput("precio_tha", "Precio Tn (USD):", min = 100, max = 300, value = 180),
                             sliderInput("costoX", "Costo *1000semillas (USD):", min = 1.5, max = 2.7, value = 2.29),
                             sliderInput("costoY", "Costo kg Ferti (USD):", min = 0.4, max = 1.5, value = 0.8)
                         )
                  ),
                  column(6,
                         fluidRow(
                           column(6, plotOutput("plotRend", height = "300px")),
                           column(6, plotOutput("plotMB", height = "300px"))
                         ),
                         fluidRow(
                           plotlyOutput("plot3D", height = "300px")
                         )
                  ),
                  column(3,
                         div(style = "border-left: 1px solid #006064; height: 100%; padding: 10px;",
                             h3(style = "border-bottom: 1px solid #006064; padding-bottom: 5px;", "Resultados del ensayo"),
                             div(style = "border: 1px solid #26897E; background-color: #26897E; color: white; padding: 20px; border-radius: 8px; margin-bottom: 10px;",
                                 HTML("<center><b>Dosis optima agronómica</b></center>"),
                                 htmlOutput("doa_text")),
                             div(style = "border: 1px solid #BD0A36; background-color: #BD0A36; color: white; padding: 20px; border-radius: 8px; margin-bottom: 10px;",
                                 HTML("<center><b>Dosis optima económica</b></center>"),
                                 htmlOutput("doe_text")),
                             h3("Testigo de uso actual (TUA)"),
                             numericInput("densidad_prod", "Densidad (1000 semillas/ha):", value = 89, min = 30, max = 100),
                             numericInput("fertilizante_prod", "Fertilizante (kg/ha):", value = 348, min = 0, max = 500),
                             div(style = "border: 1px solid #26897E; background-color: #26897E; color: white; padding: 20px; border-radius: 8px; margin-top: 20px;",
                                 HTML("<center><b>Diferencia TUA/DOE</b></center>"),
                                 htmlOutput("diff_text"))
                         )
                  )
                )
              )
      ),
      # Tab Resumen
      tabItem(tabName = "tab_resumen",
              fluidPage(
                titlePanel(div(style = "background-color: #006064; color: white; padding: 15px; border-radius: 8px;", "Resultados de híbridos por ambientes")),
                fluidRow(
                  column(2,
                         div(style = "border-right: 1px solid #006064; height: 100%; padding: 10px;",
                             sliderInput("precio_tha_resumen", "Precio Tn (USD):", min = 100, max = 300, value = 180),
                             sliderInput("costoX_resumen", "Costo *1000semillas (USD):", min = 1.5, max = 2.7, value = 2.29),
                             sliderInput("costoY_resumen", "Costo kg Ferti (USD):", min = 0.4, max = 1.5, value = 0.8)
                         )
                  ),
                  column(10,
                         div(style = "width: 1000px; margin: 0 auto;",
                             DTOutput("tabla_resumen")
                         ),
                         div(style = "width: 1000px; margin: 0 auto;",
                             plotOutput("plot_euf", height = "600px")
                         )
                  )
                )
              )
      ), 
      # Tab Comparación Global
      tabItem(tabName = "tab_comparacion_global",
              fluidPage(
                titlePanel(div(style = "background-color: #006064; color: white; padding: 15px; border-radius: 8px;", "Comparación Global de Híbridos")),
                fluidRow(
                  column(12,
                         plotOutput("comparacion_global", height = "600px")
                  )
                )
              )
      ),
      tabItem(tabName = "tab_datos_foliares",
              fluidPage(
                titlePanel("Visualización de Datos Foliares"),
                sidebarLayout(
                  sidebarPanel(
                    
                    checkboxInput("todas_regiones", "Seleccionar Todas las Regiones", value = FALSE),
                    pickerInput("regiones", "Seleccionar Región:", choices = c("Todas", unique(data$Regiones)), selected = "Todas", multiple = TRUE, options = list(`actions-box` = TRUE, `live-search` = TRUE)),
                    pickerInput("cultivo", "Seleccionar Cultivo:", choices = unique(data$Cultivo), selected = unique(data$Cultivo), multiple = TRUE, options = list(`actions-box` = TRUE, `live-search` = TRUE))
                  ),
                  mainPanel(
                    plotOutput("barPlot"),
                    dataTableOutput("summaryTable")
                  )
                )
              )
      ),
      tabItem(tabName = "tab_ambientes",
              fluidPage(
                tags$head(
                  tags$style(HTML("
        .map-title { font-size: 15px; } 
        .first-row { margin-bottom: 25px; } 
        #grafico_contingencia { width: 600px; } 
        .banner { background-color: #006064; color: white; padding: 10px; text-align: center; font-size: 24px; font-weight: bold; } 
        .input-panel { border-right: 1px solid #006064; padding: 2px; width: 90%; } 
        .compact-slider .shiny-input-container { margin-bottom: 5px; }
        .compact-slider .shiny-input-slider { height: 10px; } 
        .compact-slider .control-label { font-size: 12px; } 
        .compact-slider input { font-size: 12px; padding: 2px; }
      "))
                ),
      div(class = "banner", "Evaluación de Ambientes"),
      titlePanel("Análisis geoespacial de lotes"),
      sidebarLayout(
        sidebarPanel(
          div(class = "input-panel compact-slider", # Añadimos la clase `compact-slider` para aplicar el estilo compacto
              selectInput("lote", "Seleccione el lote:", choices = lotes),
              sliderInput("precio_maiz", "Precio Maíz (USD/tn):", value = 200, min = 100, max = 500, step = 10),
              sliderInput("costo_semilla", "Semilla (*1000 semillas):", value = 2.6, min = 0, max = 5, step = 0.1),
              sliderInput("costo_ferti", "Fertilizante (USD/Kg):", value = 0.8, min = 0, max = 3, step = 0.5),
              numericInput("costo_arrancador", "Arrancador (USD/ha):", value = 70, min = 0, max = 200, step = 0.5),
              numericInput("costo_labores", "Labores (USD/ha):", value = 70, min = 0, max = 200, step = 0.5),
              numericInput("costo_fito", "Fitosanitarios (USD/ha):", value = 70, min = 0, max = 200, step = 0.5),
              numericInput("costo_arrendamiento", "Costo Arrendamiento (USD/ha):", value = 100, min = 0, max = 1000, step = 10),
              plotlyOutput("indice_ajuste")
          )
        ),
        mainPanel(
          fluidRow(class = "first-row",
                   column(3,
                          div(
                            tags$h2("Ambientes Prescripción", class = "map-title", style = "font-weight: bold;"),
                            leafletOutput("mapa_prescripciones"),
                            style = "width: 100%; height: 400px;"
                          )
                   ),
                   column(3,
                          div(
                            tags$h2("Ambientes Cosecha", class = "map-title", style = "font-weight: bold;"),
                            leafletOutput("mapa_cosecha"),
                            style = "width: 100%; height: 400px;"
                          )
                   ),
                   
                   column(3,
                          div(
                            tags$h2("Comparación", class = "map-title", style = "font-weight: bold;"),
                            leafletOutput("mapa_comparacion"),
                            style = "width: 100%; height: 400px;"
                          )
                   ),
                   column(3,
                          div(
                            tags$h2("Mapa de Margen Neto", class = "map-title", style = "font-weight: bold;"),
                            leafletOutput("mapa_margen_neto"),
                            style = "width: 100%; height: 400px;"
                          )
                   )
          ),
          fluidRow(
            column(6,
                   plotOutput("p_medias")
            ),
            column(6,
                   plotOutput("grafico_contingencia")
            )
          )
        )
      )
              )
      )
    )
  )
)


server <- function(input, output, session) {
  # Reactivo para filtrar los coeficientes por región
  coef_filtrados <- reactive({
    if (input$region == "Todas") {
      return(coeficientes_csv)
    } else {
      return(coeficientes_csv %>% filter(Region == input$region))
    }
  })
  
  # Actualizar los híbridos disponibles según la región seleccionada
  output$hibrido_selector <- renderUI({
    selectInput("hibrido", "Seleccione el híbrido:", choices = unique(coef_filtrados()$Hibrido))
  })
  
  observeEvent(input$exportar_pdf, {
    # Lógica para generar el PDF según la opción seleccionada
    contenido_a_exportar <- input$exportar_opcion
    
    # Verificar si el archivo Rmd existe antes de intentar renderizarlo
    if (file.exists("export_template.Rmd")) {
      output_path <- paste0(contenido_a_exportar, "_export.pdf")
      rmarkdown::render(
        "export_template.Rmd",  # Necesitas crear un template Rmarkdown para la exportación
        output_file = output_path,
        params = list(opcion = contenido_a_exportar),
        envir = new.env()
      )
      
      # Verificar si el PDF fue generado correctamente
      if (file.exists(output_path)) {
        showNotification(paste("El PDF de", contenido_a_exportar, "ha sido generado."), type = "message")
      } else {
        showNotification("Error al generar el archivo PDF.", type = "error")
      }
    } else {
      showNotification("El archivo 'export_template.Rmd' no existe. Por favor, crea este archivo para continuar.", type = "error")
    }
  })
  # Modificar las funciones que dependen de los coeficientes para usar `coef_filtrados()`
  valores_predichos <- reactive({
    req(input$hibrido, input$ambiente)  # Asegurarse de que los inputs no sean NULL
    
    coefs <- coef_filtrados() %>% filter(Hibrido == input$hibrido)
    
    if (nrow(coefs) == 0) {
      return(NULL)
    }
    
    coefs_ambiente <- coefs %>%
      select(b0 = paste0("b0_", input$ambiente),
             b1_x = paste0("b1_x_", input$ambiente),
             b2_y = paste0("b2_y_", input$ambiente),
             b3_xx = paste0("b3_xx_", input$ambiente),
             b4_yy = paste0("b4_yy_", input$ambiente),
             b5_x_y = paste0("b5_x_y_", input$ambiente)) %>%
      unlist(use.names = FALSE)
    
    if (length(coefs_ambiente) != 6 || any(is.na(coefs_ambiente))) {
      return(NULL)
    }
    
    maximo(
      xname = "Densidad",
      yname = "Fertilizante",
      x_unidad_medida = "( * 1000 semillas / ha)",
      y_unidad_medida = "(kg / ha)",
      b0 = coefs_ambiente[1],
      b1_x = coefs_ambiente[2],
      b2_y = coefs_ambiente[3],
      b3_xx = coefs_ambiente[4],
      b4_yy = coefs_ambiente[5],
      b5_x_y = coefs_ambiente[6],
      x = seq(45, 95, 1),
      y = seq(0, 300, 1),
      precio_tha = input$precio_tha,
      costoX = input$costoX,
      costoY = input$costoY,
      n_cortes = 10
    )
  })
  
  output$plotRend <- renderPlot({
    pred <- valores_predichos()
    if (is.null(pred)) {
      plot.new()
      title("Sin datos para este ambiente")
      return()
    }
    
    ggplot(pred$valorespred, aes(x = Densidad, y = Fertilizante, z = Rendimiento)) +
      geom_contour_filled() +
      labs(title = "Rendimiento",
           x = "Densidad ( * 1000 semillas / ha)",
           y = "Fertilizante (kg / ha)",
           fill = "Rendimiento") +
      theme_minimal()
  })
  
  output$plotMB <- renderPlot({
    pred <- valores_predichos()
    if (is.null(pred)) {
      plot.new()
      title("Sin datos para este ambiente")
      return()
    }
    
    ggplot(pred$valorespred, aes(x = Densidad, y = Fertilizante, z = MB)) +
      geom_contour_filled() +
      labs(title = "Margen Bruto USD/ha",
           x = "Densidad ( * 1000 semillas / ha)",
           y = "Fertilizante (kg / ha)",
           fill = "Margen Bruto") +
      theme_minimal()
  })
  
  output$plot3D <- renderPlotly({
    pred <- valores_predichos()
    if (is.null(pred)) {
      plot_ly() %>%
        layout(title = "Sin datos para este ambiente")
      return()
    }
    
    coefs <- coef_filtrados() %>% filter(Hibrido == input$hibrido) %>%
      select(b0 = paste0("b0_", input$ambiente),
             b1_x = paste0("b1_x_", input$ambiente),
             b2_y = paste0("b2_y_", input$ambiente),
             b3_xx = paste0("b3_xx_", input$ambiente),
             b4_yy = paste0("b4_yy_", input$ambiente),
             b5_x_y = paste0("b5_x_y_", input$ambiente)) %>%
      unlist(use.names = FALSE)
    
    if (length(coefs) != 6 || any(is.na(coefs))) {
      plot_ly() %>%
        layout(title = "Sin datos para este ambiente")
      return()
    }
    
    densidad <- seq(45, 95, length.out = 100)
    fertilizante <- seq(0, 300, length.out = 100)
    
    D <- outer(densidad, fertilizante, FUN = function(d, f) d)
    F <- outer(densidad, fertilizante, FUN = function(d, f) f)
    
    Rendimiento <- with(list(b0 = coefs[1], b1_x = coefs[2], b2_y = coefs[3], b3_xx = coefs[4], b4_yy = coefs[5], b5_x_y = coefs[6]),
                        b0 + b1_x * D + b2_y * F + b3_xx * D^2 + b4_yy * F^2 + b5_x_y * D * F)
    
    plot_ly() %>%
      add_surface(x = ~densidad, y = ~fertilizante, z = ~Rendimiento, colorscale = 'Viridis') %>%
      layout(scene = list(xaxis = list(title = 'Densidad'),
                          yaxis = list(title = 'Ferti (kg/ha)'),
                          zaxis = list(title = 'Rinde (tn/ha)')))
  })
  
  output$doa_text <- renderUI({
    pred <- valores_predichos()
    if (is.null(pred)) {
      HTML("<b>Sin datos para este ambiente</b>")
      return()
    }
    doa <- pred$resultados["DOA", ]
    HTML(paste0(
      "Rinde = <b>", round(doa$Rendimiento, 1), " tn/ha</b><br>",
      "Fertilizante = <b>", round(doa$Fertilizante, 0), " kg/ha</b><br>",
      "Densidad = <b>", round(doa$Densidad, 0), " semillas/ha</b>"
    ))
  })
  
  output$doe_text <- renderUI({
    pred <- valores_predichos()
    if (is.null(pred)) {
      HTML("<b>Sin datos para este ambiente</b>")
      return()
    }
    doe <- pred$resultados["DOE", ]
    HTML(paste0(
      "Rinde = <b>", round(doe$Rendimiento, 1), " tn/ha</b><br>",
      "Fertilizante = <b>", round(doe$Fertilizante, 0), " kg/ha</b><br>",
      "Densidad = <b>", round(doe$Densidad, 0), " semillas/ha</b>"
    ))
  })
  
  output$diff_text <- renderUI({
    pred <- valores_predichos()
    if (is.null(pred)) {
      HTML("<b>Sin datos para este ambiente</b>")
      return()
    }
    
    valores_maximos <- pred$resultados
    coefs <- coef_filtrados() %>% filter(Hibrido == input$hibrido) %>%
      select(b0 = paste0("b0_", input$ambiente),
             b1_x = paste0("b1_x_", input$ambiente),
             b2_y = paste0("b2_y_", input$ambiente),
             b3_xx = paste0("b3_xx_", input$ambiente),
             b4_yy = paste0("b4_yy_", input$ambiente),
             b5_x_y = paste0("b5_x_y_", input$ambiente)) %>%
      unlist(use.names = FALSE)
    
    if (length(coefs) != 6 || any(is.na(coefs))) {
      HTML("<b>Sin datos para este ambiente</b>")
      return()
    }
    
    prod_values <- calcula_productor(
      densidad_prod = input$densidad_prod,
      fertilizante_prod = input$fertilizante_prod,
      precio_tha = input$precio_tha,
      costoX = input$costoX,
      costoY = input$costoY,
      b0 = coefs[1],
      b1_x = coefs[2],
      b2_y = coefs[3],
      b3_xx = coefs[4],
      b4_yy = coefs[5],
      b5_x_y = coefs[6]
    )
    
    diferencia_rendimiento <- prod_values$Rendimiento - valores_maximos["DOE", "Rendimiento"]
    diferencia_mb <- prod_values$MB - valores_maximos["DOE", "MB"]
    
    HTML(paste0(
      "TUA = <b>", round(prod_values$Rendimiento, 1), " tn/ha</b><br>",
      "Dif_Rinde = <b>", round(diferencia_rendimiento, 2), " (tn/ha)</b><br>",
      "Dif_MB = <b>", round(diferencia_mb, 2), "</b>"
    ))
  })
  
  # Filtro por región en valores_resumen
  valores_resumen <- reactive({
    coefs_filtrados <- coef_filtrados()
    hibridos <- unique(coefs_filtrados$Hibrido)
    ambientes <- c("alta", "media", "baja")
    
    resultados <- data.frame(
      Hibrido = character(),
      Region = character(),
      Ambiente = character(),
      Rendimiento_DOA = numeric(),
      Ferti_DOA = numeric(),
      Densidad_DOA = numeric(),
      Rendimiento_DOE = numeric(),
      Ferti_DOE = numeric(),
      Densidad_DOE = numeric(),
      stringsAsFactors = FALSE
    )
    
    for (hibrido in hibridos) {
      for (ambiente in ambientes) {
        coefs <- coefs_filtrados %>% filter(Hibrido == hibrido)
        
        if (nrow(coefs) == 0) {
          next
        }
        
        region <- unique(coefs$Region)
        
        coefs_ambiente <- coefs %>%
          select(b0 = paste0("b0_", ambiente),
                 b1_x = paste0("b1_x_", ambiente),
                 b2_y = paste0("b2_y_", ambiente),
                 b3_xx = paste0("b3_xx_", ambiente),
                 b4_yy = paste0("b4_yy_", ambiente),
                 b5_x_y = paste0("b5_x_y_", ambiente)) %>%
          unlist(use.names = FALSE)
        
        if (length(coefs_ambiente) != 6 || any(is.na(coefs_ambiente))) {
          next
        }
        
        pred <- maximo(
          xname = "Densidad",
          yname = "Fertilizante",
          x_unidad_medida = "( * 1000 semillas / ha)",
          y_unidad_medida = "(kg / ha)",
          b0 = coefs_ambiente[1],
          b1_x = coefs_ambiente[2],
          b2_y = coefs_ambiente[3],
          b3_xx = coefs_ambiente[4],
          b4_yy = coefs_ambiente[5],
          b5_x_y = coefs_ambiente[6],
          x = seq(45, 95, 1),
          y = seq(0, 300, 1),
          precio_tha = input$precio_tha_resumen,
          costoX = input$costoX_resumen,
          costoY = input$costoY_resumen,
          n_cortes = 10
        )
        if (is.null(pred) || any(is.na(pred$resultados))) {
          next
        }
        doa <- pred$resultados["DOA", ]
        doe <- pred$resultados["DOE", ]
        
        resultados <- rbind(resultados, data.frame(
          Hibrido = hibrido,
          Region = region,
          Ambiente = ambiente,
          Rendimiento_DOA = doa$Rendimiento,
          Ferti_DOA = doa$Fertilizante,
          Densidad_DOA = doa$Densidad,
          Rendimiento_DOE = doe$Rendimiento,
          Ferti_DOE = doe$Fertilizante,
          Densidad_DOE = doe$Densidad
        ))
      }
    }
    
    resultados
  })
  
  output$tabla_resumen <- renderDT({
    datatable(valores_resumen(), selection = 'single', options = list(
      dom = 't',
      paging = FALSE,
      autoWidth = TRUE,
      columnDefs = list(list(className = 'dt-center', targets = '_all'))
    ), rownames = FALSE)
  })
  
  output$plot_euf <- renderPlot({
    req(input$tabla_resumen_rows_selected)
    seleccion <- input$tabla_resumen_rows_selected
    hibrido <- as.character(valores_resumen()[seleccion, "Hibrido"])
    ambiente <- as.character(valores_resumen()[seleccion, "Ambiente"])
    
    coefs <- coef_filtrados() %>% filter(Hibrido == hibrido) %>%
      select(b0 = paste0("b0_", ambiente),
             b1_x = paste0("b1_x_", ambiente),
             b2_y = paste0("b2_y_", ambiente),
             b3_xx = paste0("b3_xx_", ambiente),
             b4_yy = paste0("b4_yy_", ambiente),
             b5_x_y = paste0("b5_x_y_", ambiente)) %>%
      unlist(use.names = FALSE)
    
    if (is.null(coefs) || length(coefs) != 6 || any(is.na(coefs))) {
      plot.new()
      title("Sin datos para este ambiente")
      return()
    }
    
    pred <- maximo(
      xname = "Densidad",
      yname = "Fertilizante",
      x_unidad_medida = "( * 1000 semillas / ha)",
      y_unidad_medida = "(kg / ha)",
      b0 = coefs[1],
      b1_x = coefs[2],
      b2_y = coefs[3],
      b3_xx = coefs[4],
      b4_yy = coefs[5],
      b5_x_y = coefs[6],
      x = seq(45, 90, 5),  
      y = seq(0, 300, 100),  
      precio_tha = input$precio_tha_resumen,
      costoX = input$costoX_resumen,
      costoY = input$costoY_resumen,
      n_cortes = 15
    )
    
    valorespred <- pred$valorespred
    
    euf_data <- expand.grid(Fertilizante = seq(100, 300, 100), Densidad = seq(45, 90, 5))
    euf_data$EUF <- NA
    
    for (i in 1:nrow(euf_data)) {
      densidad <- euf_data$Densidad[i]
      ferti <- euf_data$Fertilizante[i]
      rto_f <- with(valorespred, Rendimiento[Densidad == densidad & Fertilizante == ferti])
      rto_0 <- with(valorespred, Rendimiento[Densidad == densidad & Fertilizante == 0])
      if (length(rto_f) > 0 & length(rto_0) > 0) {
        euf_data$EUF[i] <- (rto_f*1000 - rto_0*1000) / ferti
      }
    }
    
    ggplot(euf_data, aes(x = Fertilizante, y = Densidad, fill = EUF, label = round(EUF, 2))) +
      geom_tile() +
      geom_text(color = "white", size = 5) +
      scale_fill_viridis_c() +
      labs(title = paste("Eficiencia de Uso de Fertilizante para", hibrido, "en ambiente", ambiente),
           x = "Dosis de Fertilizante (kg/ha)",
           y = "Densidad (1000 semillas/ha)",
           fill = "EUF") +
      theme_minimal() +
      theme(plot.margin = unit(c(1,1,1,1), "cm"))
  })
  
  
  output$comparacion_global <- renderPlot({
    resumen <- valores_resumen()
    
    resumen <- resumen %>%
      mutate(Ferti_DOA_group = cut(Ferti_DOA, 
                                   breaks = c(-Inf, 100, 200, 300, 400, Inf), 
                                   labels = c("0-100", "101-200", "201-300", "301-400", "401+")))
    
    ggplot(resumen, aes(x = Densidad_DOA, y = Rendimiento_DOA, label = Hibrido)) +
      geom_point(aes(color = Ambiente, shape = Ferti_DOA_group), size = 4) +
      geom_text(vjust = -1, hjust = 0.5, size = 3) +
      scale_color_manual(values = c("alta" = "green", "media" = "yellow", "baja" = "red")) +
      scale_shape_manual(values = c("0-100" = 16, "101-200" = 17, "201-300" = 18, "301-400" = 19, "401+" = 15)) +
      geom_hline(yintercept = c(5, 10), linetype = "dotted", color = "gray") +
      scale_x_continuous(breaks = seq(65, 100, 5), labels = seq(65, 100, 5)) +
      scale_y_continuous(breaks = seq(0, 15, 1), labels = seq(0, 15, 1)) +
      labs(title = "Comparación Global de Híbridos",
           x = "Densidad (1000 semillas/ha)",
           y = "Rendimiento (tn/ha)",
           color = "Ambiente",
           shape = "Dosis Fertilizante (kg/ha)") +
      theme_minimal() +
      theme(axis.title = element_text(size = 14),
            axis.text = element_text(size = 12),
            plot.title = element_text(size = 16, face = "bold"),
            panel.grid.major = element_line(size = 0.8),
            panel.grid.minor = element_line(size = 0.5),
            plot.margin = unit(c(1,1,1,1), "cm"))
  })
  
  observe({
    req(input$lote)  # Verifica que input$lote no sea NULL antes de proceder
    
    lote <- input$lote
    
    if (lote == "Huinca") {
      archivo <- "Huinca.shp"
    } else if (lote == "Canals") {
      archivo <- "Canals.shp"
    } else if (lote == "Laguna") {
      archivo <- "Laguna.shp"
    } else if (lote == "Tucuman") {
      archivo <- "Tucuman.shp"
    } else {
      showNotification("Lote no encontrado", type = "error")  # En lugar de `stop()`, para evitar que el servidor falle
      return(NULL)
    }
    
    datos <- st_read(archivo)
    
    # Simplificar geometrías y corregir errores
    datos <- st_make_valid(datos)
    datos <- st_collection_extract(datos, "POLYGON")
    
    # Extraer coordenadas para los puntos
    datos_coords <- st_coordinates(st_centroid(datos))
    datos <- cbind(datos, datos_coords)
    
    # Imprimir los primeros registros para depuración
    print("Datos combinados:")
    print(head(datos))
    
    # Verificar datos faltantes
    print("Verificar datos faltantes:")
    print(sum(is.na(datos$Rinde)))
    print(sum(is.na(datos$Semilla)))
    
    assign_labels <- function(cluster_centers) {
      ordered_indices <- order(cluster_centers)
      cluster_labels <- c("Baja", "Media", "Alta")[match(1:length(cluster_centers), ordered_indices)]
      return(cluster_labels)
    }
    
    set.seed(222)
    
    # Filtrar datos válidos para cosecha
    valid_cosecha <- datos %>% filter(!is.na(Rinde))
    print(paste("Número de registros válidos para cosecha:", nrow(valid_cosecha)))
    print(head(valid_cosecha$Rinde))
    if (nrow(valid_cosecha) >= 3) {
      clusters_cosecha <- kmeans(valid_cosecha$Rinde, centers = 3)
      labels_cosecha <- assign_labels(clusters_cosecha$centers)
      datos$cluster_cosecha[!is.na(datos$Rinde)] <- labels_cosecha[clusters_cosecha$cluster]
    } else {
      print("Datos insuficientes para k-means en cosecha.")
      datos$cluster_cosecha <- NA
    }
    
    # Filtrar datos válidos para prescripciones
    valid_prescripciones <- datos %>% filter(!is.na(Semilla))
    print(paste("Número de registros válidos para prescripciones:", nrow(valid_prescripciones)))
    print(head(valid_prescripciones$Semilla))
    if (nrow(valid_prescripciones) >= 3) {
      clusters_prescripciones <- kmeans(valid_prescripciones$Semilla, centers = 3)
      labels_prescripciones <- assign_labels(clusters_prescripciones$centers)
      datos$cluster_prescripciones[!is.na(datos$Semilla)] <- labels_prescripciones[clusters_prescripciones$cluster]
    } else {
      print("Datos insuficientes para k-means en prescripciones.")
      datos$cluster_prescripciones <- NA
    }
    
    datos$cluster_cosecha <- factor(datos$cluster_cosecha, levels = c("Baja", "Media", "Alta"))
    datos$cluster_prescripciones <- factor(datos$cluster_prescripciones, levels = c("Baja", "Media", "Alta"))
    
    datos$comparacion <- ifelse(datos$cluster_cosecha == datos$cluster_prescripciones, "Igual", "Diferente")
    
    output$mapa_cosecha <- renderLeaflet({
      leaflet(datos) %>%
        addProviderTiles(providers$Esri.WorldImagery) %>%
        addCircleMarkers(lng = ~X, lat = ~Y, 
                         color = ~colorFactor(palette = c("red", "yellow", "green"), domain = datos$cluster_cosecha)(cluster_cosecha), 
                         fillOpacity = 1, radius = 3, popup = ~paste("Cluster:", cluster_cosecha)) %>%
        addLegend(position = "bottomright", colors = c("red", "yellow", "green"), labels = c("Baja", "Media", "Alta"), title = "Cosecha", opacity = 1) %>%
        fitBounds(lng1 = min(datos$X), lat1 = min(datos$Y), lng2 = max(datos$X), lat2 = max(datos$Y))
    })
    
    output$mapa_prescripciones <- renderLeaflet({
      leaflet(datos) %>%
        addProviderTiles(providers$Esri.WorldImagery) %>%
        addCircleMarkers(lng = ~X, lat = ~Y, 
                         color = ~colorFactor(palette = c("red", "yellow", "green"), domain = datos$cluster_prescripciones)(cluster_prescripciones), 
                         fillOpacity = 1, radius = 3, popup = ~paste("Cluster:", cluster_prescripciones)) %>%
        addLegend(position = "bottomright", colors = c("red", "yellow", "green"), labels = c("Baja", "Media", "Alta"), title = "Prescripciones", opacity = 1) %>%
        fitBounds(lng1 = min(datos$X), lat1 = min(datos$Y), lng2 = max(datos$X), lat2 = max(datos$Y))
    })
    
    output$mapa_comparacion <- renderLeaflet({
      leaflet(datos) %>%
        addProviderTiles(providers$Esri.WorldImagery) %>%
        addCircleMarkers(lng = ~X, lat = ~Y, 
                         color = ~colorFactor(palette = c("red", "green"), domain = datos$comparacion)(comparacion), 
                         fillOpacity = 1, radius = 3, popup = ~paste("Comparación:", comparacion)) %>%
        addLegend(position = "bottomright", colors = c("red", "green"), labels = c("Diferente", "Igual"), title = "Comparación", opacity = 1) %>%
        fitBounds(lng1 = min(datos$X), lat1 = min(datos$Y), lng2 = max(datos$X), lat2 = max(datos$Y))
    })
    
    output$p_medias <- renderPlot({
      ggplot(datos, aes(x = cluster_cosecha, y = Rinde, fill = cluster_cosecha)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        scale_fill_manual(values = c("Baja" = "red", "Media" = "yellow", "Alta" = "green")) +
        labs(title = "Rendimiento por Ambiente de Cosecha", y = "Rendimiento (tn)", x = "Ambientes") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15))
    })
    
    frecuencias_relativas <- calcularFrecuencias(datos)
    
    frecuencias_df <- as.data.frame(as.table(frecuencias_relativas))
    colnames(frecuencias_df) <- c("cluster_prescripciones", "cluster_cosecha", "frecuencia")
    
    output$grafico_contingencia <- renderPlot({
      ggplot(frecuencias_df, aes(x = cluster_prescripciones, y = cluster_cosecha, fill = frecuencia)) +
        geom_tile() +
        geom_text(aes(label = paste0(round(frecuencia, 2), "%")), size = 5) +
        scale_fill_gradientn(colours = c("red", "yellow", "green")) +
        labs(title = "% de coincidencias", x = "Ambientes Prescripción", y = "Ambientes Cosecha") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 16), plot.title = element_text(size = 12, face = "bold"), legend.title = element_blank(), legend.text = element_text(size = 14)) +
        scale_y_discrete(expand = c(0, 0))
    })
    
    
    # Calcular el margen neto
    datos_margen_neto <- reactive({
      datos <- datos
      req(input$precio_maiz, input$costo_semilla, input$costo_ferti, input$costo_labores, input$costo_arrendamiento)
      
      # Añadir el cálculo del margen neto
      datos <- datos %>%
        mutate(
          MargenNeto = (input$precio_maiz * Rinde) - 
            (input$costo_semilla + (ferti * input$costo_ferti) + input$costo_labores + input$costo_arrendamiento + input$costo_fito + input$costo_arrancador)
        )
      
      return(datos)
    })
    
    # Renderizar el mapa de margen neto
    
    output$mapa_margen_neto <- renderLeaflet({
      valid_data <- datos_margen_neto()
      
      # Definir cortes basados en quantiles
      if (min(valid_data$MargenNeto) < 0) {
        # Si hay valores negativos, asegurarse de que 0 sea un corte
        breaks <- c(quantile(valid_data$MargenNeto, probs = seq(0, 0.8, by = 0.2)), 0, 
                    quantile(valid_data$MargenNeto, probs = seq(0.2, 1, by = 0.2)))
        breaks <- sort(unique(breaks)) # Ordenar y eliminar duplicados
      } else {
        # Si todos los valores son positivos, simplemente usar quantiles
        breaks <- quantile(valid_data$MargenNeto, probs = seq(0, 1, by = 1/6))
      }
      
      total_area <- sum(st_area(valid_data))
      area_percents <- sapply(1:(length(breaks)-1), function(i) {
        subset_data <- valid_data[valid_data$MargenNeto >= breaks[i] & valid_data$MargenNeto < breaks[i+1], ]
        return(sum(st_area(subset_data)) / total_area * 100)
      })
      
      # Definir la paleta de colores
      pal <- colorBin("RdYlGn", domain = valid_data$MargenNeto, bins = breaks)
      
      # Crear un vector de colores basado en los cortes
      colors <- sapply(breaks[-length(breaks)], pal)
      
      # Construir etiquetas personalizadas
      legend_labels <- mapply(function(b1, b2, ap) {
        sprintf("%d - %d (%0.2f%%)", floor(b1), floor(b2), ap)
      }, breaks[-length(breaks)], breaks[-1], area_percents)
      
      # Crear mapa
      leaflet(valid_data) %>% 
        addProviderTiles(providers$Esri.WorldImagery) %>%  # Imagen satelital de fondo
        addPolygons(color = ~pal(MargenNeto),
                    weight = 1,
                    fillOpacity = 0.7,
                    label = ~sprintf("Margen Neto: USD %d", floor(MargenNeto)),
                    highlight = highlightOptions(weight = 3,
                                                 color = "#666",
                                                 fillOpacity = 0.9,
                                                 bringToFront = TRUE)) %>%
        addLegend(title = "Margen Neto",
                  labels = legend_labels,
                  colors = colors,
                  opacity = 0.8,
                  position = "bottomright",
                  labFormat = labelFormat(digits = 0, suffix = " USD"))
    })
    
    
    
    filteredData <- reactive({
      if (input$todas_regiones || "Todas" %in% input$regiones) {
        data %>%
          filter(Cultivo %in% input$cultivo)
      } else {
        data %>%
          filter(Regiones %in% input$regiones, Cultivo %in% input$cultivo)
      }
    })
    
    # Calcular el promedio de rendimiento y diferencia porcentual respecto al testigo
    summaryData <- reactive({
      data_filtered <- filteredData()
      testigo <- data_filtered %>% filter(Tratamiento == "Testigo")
      promedio_testigo <- mean(testigo$Rinde, na.rm = TRUE)
      
      data_filtered %>%
        group_by(Region = if (input$todas_regiones || "Todas" %in% input$region) "Global" else Region, Cultivo, Tratamiento) %>%
        summarise(Rinde_Promedio = mean(Rinde, na.rm = TRUE),
                  Diferencia_Porcentual = round(((Rinde_Promedio - promedio_testigo) / promedio_testigo) * 100, 1),
                  Dif = unique(Dif)) %>%
        distinct(Cultivo, Tratamiento, .keep_all = TRUE)
    })
    
    # Crear el gráfico de barras
    output$barPlot <- renderPlot({
      ggplot(summaryData(), aes(x = Tratamiento, y = Rinde_Promedio, fill = Tratamiento)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        geom_text(aes(label = paste0(round(Diferencia_Porcentual, 1), "%")), vjust = -0.5, fontface = "bold") +
        geom_text(aes(label = Dif), vjust = 1.5, color = "black") +
        geom_text(aes(label = round(Rinde_Promedio, 1)), vjust = 15, fontface = "bold", color = "black", position = position_dodge(width = 0.9)) +
        labs(title = "Rendimiento tn/ha",
             x = "Tratamiento",
             y = "Rinde tn/ha") +
        theme_minimal() + theme(panel.background = element_rect(fill = 'transparent', color = NA))+
        theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
              axis.text.y = element_text(size = 12),
              axis.title.x = element_text(size = 14),
              axis.title.y = element_text(size = 14))
    })
    
    # Crear la tabla resumen
    output$summaryTable <- renderDataTable({
      datatable(summaryData(), options = list(pageLength = 10))
    })
    
    
    
    
    
    output$indice_ajuste <- renderPlotly({
      # Calcular el índice de ajuste de prescripciones
      combinaciones_perfectas <- sum(frecuencias_relativas["Baja", "Baja"]) + 
        sum(frecuencias_relativas["Media", "Media"]) + 
        sum(frecuencias_relativas["Alta", "Alta"])
      
      transiciones <- sum(frecuencias_relativas["Media", "Alta"]) + 
        sum(frecuencias_relativas["Media", "Baja"])
      
      indice_ajuste <- (combinaciones_perfectas + transiciones) / 3 
      
      print(paste("Combinaciones perfectas:", combinaciones_perfectas))
      print(paste("Transiciones:", transiciones))
      print(paste("Índice de ajuste:", indice_ajuste))
      
      # Crear el gráfico de medidor radial o podemos mejorar ???                      
      fig <- plot_ly(
        type = "indicator",
        mode = "gauge+number",
        value = indice_ajuste,
        gauge = list(
          axis = list(range = list(0, 100)),
          steps = list(
            list(range = c(0, 20), color = "red"),
            list(range = c(20, 50), color = "yellow"),
            list(range = c(50, 70), color = "lightgreen"),
            list(range = c(70, 100), color = "green")
          ),
          threshold = list(
            line = list(color = "black", width = 4),
            thickness = 0.75,
            value = indice_ajuste
          )
        )
      )
      
      fig <- fig %>%
        layout(
          title = "Índice de Ajuste de Prescripciones"
        )
      
      fig
    })
  })
}

shinyApp(ui = ui, server = server)

