# libreras -----------------------------
library(shiny)
library(DT)
library(highcharter)
library(tidyverse)
library(glue)
library(shinyjs) #empaqueta funciones de jascript para complementar con html por ej. animaciones
library(clipr) #para copiar y pegar 

# tablas -------------------------------
# acá podemos meter alguna función o algo que nos traiga 

datos = iris


ui <- fluidPage( #fluid page layout con filas y columnas libres
  useShinyjs(),
  
  # titulo
  fluidRow(
    column(width =  12,# ocupa 12 columnas que es todo el ancho de la página
           h1(strong('Título de la app')), # una alternativa: h2(HTML("Project <b>Description</b>")
           br(),
           h3('Un subtitulo'),
           p('Texto de un parrafo y cosito como por ej. un claim de la organización'),
           align = 'center', #centra todos los elentos de column
           )
  ),
  
  hr(), # agregar una linea
  
  # primera fila de la ui
  fluidRow(
    column(width = 3,
           h2('Filtros'),
           selectInput(
             inputId = 'selectSpecie',
             label = 'Seleccioná la especie',
             choices = unique(iris$Species),
             selected = "setosa"
           ),
           actionButton(
             inputId = "boton",
             label = "Ver filas #"
             ),
           actionButton(
             inputId = "clip",
             label = "copiar datos"
           ),
           checkboxInput(
             inputId = "mostrar_boxplot",
             label = "Mostrar / ocultar boxplot",
             value = TRUE
           ),
           align = 'center',
           ),
    column(width = 9,
           h2('Título 2 de la columna 4 a 6'),
           hr(),
           DT::DTOutput('tabla'),
           br(),
           highchartOutput("boxplot"),
           align = "center"
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$mostrar_boxplot, {
    if (input$mostrar_boxplot == T) {
      show("boxplot")
    } else {
      hide("boxplot")
    }
  })
  
  # copiar y pegar 
  observeEvent(input$clip,
               {
                 clipr::write_clip(datosProcesados())
  })
  
  # definir un elemento reactivo que sirve para varios output con la misma  
  datosProcesados = reactive({
    especieSeleccionada = input$selectSpecie #define el input
    datos = datos [datos$Species == especieSeleccionada,] 
    datos # la ultima línea ejecuta el dato en cuestion
  })


# output 1 La tabla -------------------------------------------------------

  
  output$tabla <- DT::renderDataTable({
     
     datosProcesados = datosProcesados()
     DT::datatable(datosProcesados, caption = paste("Especie seleccionada:",input$selectSpecie))
    
  }) 

# output 2 Boxplot -------------------------------------------------------  
  output$boxplot <- renderHighchart({
    
    datosProcesados = datosProcesados()
    
      datosProcesados = datosProcesados|> pivot_longer(
      cols = 1:4, 
      values_to = "Valores", 
      names_to = "Medidas")
    
    datosProcesados$Species = NULL
    
    datosProcesados$Medidas = factor(datosProcesados$Medidas)
    datosProcesados = datosProcesados  |>  as_tibble()
    
    # grafico
    data_boxplot=data_to_boxplot(
      data = datosProcesados,
      variable = Valores,
      group_var = Medidas,
      group_var2 = Medidas,
      add_outliers = F,
      fillColor = c('#a6611a','#dfc27d','#80cdc1','#018571'),
      color="black"
    )
    
    highchart()%>%
      hc_xAxis(type ="category")%>%
      hc_add_series_list(data_boxplot)%>%
      hc_xAxis(title = list(text = "Medida"))%>%
      hc_yAxis(title = list(text = "Centímetros"))%>%
      hc_title(text= glue("Boxplot para medidas de {input$selectSpecie}")) %>%
      hc_subtitle(text= "Medidas de pétalo y sépalo") %>%
      hc_legend(enabled= FALSE)
    
  })
  
  
  
}

shinyApp(ui, server)
