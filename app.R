# libreras -----------------------------
library(shiny)
library(DT)

# tablas -------------------------------
# acá podemos meter alguna función o algo que nos traiga 

datos = iris


ui <- fluidPage( #fluid page layout con filas y columnas libres
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
           
           
           align = 'center'
           ),
    column(width = 9,
           h2('Título 2 de la columna 4 a 6'),
           DT::DTOutput('tabla'),
           align = "center"
    )
  )
)

server <- function(input, output, session) {

#todas las salidas empiezan con output$ y la definición de la salida que queremos que renderice 
# (o sea que se muestre) y adentro se pone {} y se manda la tabla y hay que decirle tambien donde 
# queremos que lo ubique  y todos los paquetes uno que va en el server y otra en el ui render y output por lo gral
  output$tabla <- DT::renderDataTable({
    
    especieSeleccionada = input$selectSpecie #define el input
    
    datos = datos [datos$Species == especieSeleccionada,] #
    
    DT::datatable(datos, caption = paste("Especie seleccionada:",especieSeleccionada))
    
    
    
  })   
  
}

shinyApp(ui, server)
