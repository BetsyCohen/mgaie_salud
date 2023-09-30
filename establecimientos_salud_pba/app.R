# SETUP -------------------------------------------------------------------

# Librerías

library(shiny)
library(tidyverse)
library(shinythemes)
library(bslib) 
library(leaflet)
library(gt)
library(sf)


# Insumos

establecimientos <- read_rds("insumos_app/establecimientos.rds")
rendimientos <- read_rds("insumos_app/rendimientos.rds")
poblacion <- read_rds("insumos_app/poblacion.rds")
geometrias_region_sanitaria <- read_rds("insumos_app/geometrias_region_sanitaria.rds")


# Funciones 

#paleta de colores para los tipos de financiamiento
getColor <- function(establecimientos_filtrados) {
  sapply(establecimientos$establ_tipo_financiamiento, function(establ_tipo_financiamiento) {
    if(establ_tipo_financiamiento %in% c("Municipal")) {
      "orange"
    } else if(establ_tipo_financiamiento %in% c("Provincial")) {
      "#22a954"
    } else {
      "#0072bb"
    } })
}





# UI ---------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Rendimientos de Establecimientos de Salud"),
  sidebarPanel(
    h2('Filtros'),
    selectizeInput(inputId = "select_anio", 
                   label   = "Seleccione el año:",
                   choices = unique(rendimientos$anio),
                   selected = NULL),
    selectizeInput(inputId = "select_region", 
                   label = "Seleccione la regiones sanitarias:",
                   choices = unique(rendimientos$region_sanitaria),
                   multiple = TRUE),
    selectizeInput(inputId = "select_financiamiento", 
                   label = "Seleccione el tipos de financiamiento:",
                   choices = unique(rendimientos$establ_tipo_financiamiento),
                   multiple = TRUE)
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Tabla Resumen", tableOutput("tabla_resumen")),
      tabPanel("Mapa", leafletOutput("mapa_leaflet"))
    )
  )
)



# SERVER ------------------------------------------------------------------


server <- function(input, output) {
  
  # Filtrar los datos según las selecciones del usuario
  
  #1er reactive datos_rendimiento_filtrados
  datos_rendimiento_filtrados <- reactive({
    # Primer insumo rendimientos  
    datos_rendimiento <- rendimientos |> 
      filter(anio == input$select_anio) |> 
      filter(region_sanitaria %in% input$select_region) |> 
      filter(establ_tipo_financiamiento %in% input$select_financiamiento)
    return(datos_rendimiento) # SIEMPRE EN EL RETURN VA EL NOMBRE DE LA VARIABLE QUE QUERES MOSTRAR, NO EL NOMBRE DEL REACTIVE
  })
  
  #2do reactive datos_poblacion_filtrados
  datos_poblacion_filtrados <- reactive({
    
    datos_poblacion <- poblacion |> 
      filter(anio == input$select_anio) |> 
      filter(region_sanitaria %in% input$select_region) |> 
      group_by(partido_nombre, sexo_label) |> 
      summarise(poblacion = sum(poblacion)) |> # ACA ESTABA MAL EN NOMBRE DE LA COLUMNA QUE SUMAS 
      pivot_wider(names_from = "sexo_label",
                  values_from = "poblacion") |> 
      as.data.frame()
    return(datos_poblacion)
  })
  
  
  
  
  # 3er reactive datos_tabla_filtrados e insumo para ouput tabla_resumen 
  datos_tabla_filtrados <- reactive({
    
    # se crean las dos variables insumo a partir del 1er y 2do reactive que van DENTRO de este 3er reactive
    
    datos_rend = datos_rendimiento_filtrados() # ASIGNA EL REACTIVE A UNA VARIABLE PARA FACILITAR EL PROCESAMIENTO POSTERIOR 
    datos_pob = datos_poblacion_filtrados() # ASIGNA EL REACTIVE A UNA VARIABLE PARA FACILITAR EL PROCESAMIENTO POSTERIOR
  
    datos_tabla_join <- datos_rend   |> 
      group_by(partido_nombre) |> 
      summarise(establecimientos = n_distinct(establecimiento_id),
                cons_medicas_media = round(mean(consultas_medicas,na.rm = TRUE)),
                camas_disp_media = round(mean(promedio_camas_disponibles,na.rm = TRUE)),
                pacientes_dias_media = round(mean(pacientes_dias,na.rm = TRUE))) %>%
      left_join(datos_pob, by = "partido_nombre") %>%
    return(datos_tabla_join)
    })

 
  # Crear output tabla resumen en gt
  output$tabla_resumen <- render_gt({
    tabla_resumen <- datos_tabla_filtrados() |> # insumo reactivo 
      gt()  |> 
      tab_header(paste("Indicadores para region(es) sanitaria N°", paste(input$select_region, collapse = ", "))) |>   # titulo reactivo
      gt::cols_label(partido_nombre = "Partido",
                 establecimientos = "Establecimientos",
                 cons_medicas_media = "Consultas médicas promedio",
                 camas_disp_media = "Camas disponibles promedio",
                 pacientes_dias_media = "Pacientes promedio por día") 
    return(tabla_resumen)
  })
  

  

  
  # puntos con los establecimientos
  establecimientos_filtrados <- reactive({
    
    establecimientos_filtrados_mapa <- establecimientos %>%
      filter(region_sanitaria %in% input$select_region)
    
    return(establecimientos_filtrados_mapa)
    
    })
  
  
  # geometrias de las regiones sanitarias
  geometrias_filtradas <- reactive({
    
    geometrias_rs <- geometrias_region_sanitaria %>%
      filter(region_sanitaria %in% input$select_region)
    
    return(geometrias_rs)
    
  })  
  
  
  # etiquetas
  etiqueta_filtradas <- reactive({
    
    establecimientos_filtrados_var <- establecimientos_filtrados()
    
    etiqueta <- sprintf(
      "<strong>%s</strong><br/>%s",
      establecimientos_filtrados_var$establ_nombre_original,
      establecimientos_filtrados_var$establ_domicilio
    ) %>%
      lapply(htmltools::HTML)
    return(etiqueta)
    
  }) 
  
  # iconos con las etiquetas_filtradas
  
  iconos_filtrados <- reactive({
  
    establecimientos_filtrados_var <- establecimientos_filtrados()
    
    icons <- awesomeIcons(
      icon = 'medkit',
      iconColor = 'white',
      library = 'ion',
      markerColor = getColor(establecimientos_filtrados_var)
    )
      
    
  })
  
  
  
  
  # Crear el mapa Leaflet
  output$mapa_leaflet <- renderLeaflet({
    mapa <- leaflet() %>%
      addTiles() %>%
      addAwesomeMarkers(
        data = establecimientos_filtrados(), # reactiva de puntos con establecimientos filtrados
        icon = iconos_filtrados(),# reactiva de iconos filtrados
        label = etiqueta_filtradas(),# reactiva de etiquetas filtradas
        clusterOptions = markerClusterOptions()
      ) %>%
      addPolygons(
        data = geometrias_filtradas(),# reactiva de geometrias de las regiones sanitarias filtradas
        fillColor = "blue",
        fillOpacity = 0.2,
        weight = 1,
        color = "white",
        dashArray = "2",
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        )
      )
    
    return(mapa) # FALTABA EL RETURNO <- Gracias :)
    
  })
}



# RUN APP -----------------------------------------------------------------


# Run the application 
shinyApp(ui = ui, server = server)
