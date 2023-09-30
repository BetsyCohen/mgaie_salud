## Preparacion de datos


# SETUP -------------------------------------------------------------------

# Librerías
# setwd("establecimientos_salud_pba")
library(shiny)
library(tidyverse)
library(shinythemes)
library(bslib) 
library(leaflet)
library(gt)
library(sf)




# Insumos


establecimientos<- read.csv("https://catalogo.datos.gba.gob.ar/dataset/91743f68-bc82-4475-baca-7d5d6908eee8/resource/c2d51824-c61d-4374-a014-cca5b76b082a/download/establecimientos-salud-publicos.csv", sep = ";") 
rendimientos <- read.csv("https://catalogo.datos.gba.gob.ar/dataset/219d6b32-2e34-40cb-913d-b05c47cab75f/resource/8c3130cb-61ad-4014-b829-503b214ba3c0/download/rendimientos-hospitalarios_2005-2022.csv")
mapa_partidos_pba <- read_sf("fuentes/departamento/departamento.shp") 
# tabla trabajada previamente fuente INDEC
poblacion <- read.csv("fuentes/proy_1025_depto_buenos_aires.csv",sep = ";", encoding = "latin1")


# ETL ---------------------------------------------------------------------

# seleccionar y renombrar tablas establecimiento
establecimientos <- establecimientos %>% 
  select(establecimiento_id = cpd,
         establ_nombre_original = nor,
         establ_nombre_geo = nam,
         lat,
         long,
         partido_id = cde,
         partido_nombre = nde,
         localidad_nombre = nba,
         region_sanitaria = nrs,
         establ_domicilio = dom,
         establ_tipo_generico = gna,
         establ_modalidad_atencion = mod,
         establ_mail = mai,
         establ_tipo_financiamiento = tes,
         establ_categoria = cat,
         establ_domicilio = dom) %>% 
  mutate(establecimiento_id = as.character(establecimiento_id))




# Tydeo de rendimientos
rendimientos <- rendimientos %>% 
  rename(partido_nombre = municipio_nombre,
         partido_id = municipio_id)%>% 
  mutate(anio = as.character(anio),
         establecimiento_id = as.character(establecimiento_id), 
         partido_id = sub('.', '', partido_id)) %>% 
# Nos quedamos con el rendimientos posteriores al 2010 de los establecimientos de los cuales tenemos su ubicación en la tabla establecimientos
  filter(!is.na(establecimiento_id %in% establecimientos$establecimiento_id) ) %>% # eliminar establecimientos sin georreferencia
  filter((as.numeric(anio) >2017))  # 2018 en adelante


# Agregar establ_tipo_financiamiento a la tabla de rendimientos

establ_tipo_financiamiento <- establecimientos %>% select(establecimiento_id, establ_tipo_financiamiento)

rendimientos <- rendimientos %>% left_join(establ_tipo_financiamiento, by = "establecimiento_id" )

rm(establ_tipo_financiamiento)

# Tidear proyecciones de población y pegarle las variables de filtro
poblacion <- poblacion %>% 
  pivot_longer(cols = -c(1:3),
               names_to = ("anio")) %>% 
  mutate(anio= as.numeric((str_remove(anio,"X")))) %>% 
  filter(anio >2017) %>% # 2018 en adelante
  mutate(anio = as.character(anio)) %>% 
  rename(partido_nombre = partido_label,
         poblacion = value)


# Armar una tabla con las regiones partidos y cantidad de establecimientos
establecimientos_por_partido_region <- rendimientos %>% 
  group_by(region_sanitaria,partido_id,partido_nombre) %>% 
  summarise(region_sanitaria = region_sanitaria,
            partido_id = partido_id,
            partido_nombre = partido_nombre,
            establecimientos_partido_suma = sum(n_distinct(establecimiento_id))) %>% 
  distinct(partido_id,.keep_all = T)  %>% 
  ungroup()
  

poblacion <- poblacion %>% 
  left_join(establecimientos_por_partido_region, by =  "partido_nombre") %>% 
  relocate(partido_id, .after =partido_nombre ) %>% 
  filter(!(is.na(partido_id)))


# Tydeo de mapa_partidos_pba
mapa_partidos_pba <- mapa_partidos_pba %>% 
  mutate(provincia = substr(as.character(in1), 1, 2), #Obtener segmento cod. prov
         partido_id = substring(as.character(in1), 3) #Obtener segmento cod. partido
         ) %>% 
  filter(provincia == "06") # nos quedamos con Buenos Aires


# Pegamos las regiones en el mapa de partidos
mapa_partidos_pba <- mapa_partidos_pba %>% 
  left_join(establecimientos_por_partido_region, by = "partido_id")

# Construimos las geometrías de las 12 regiones a partir de los partidos que las componen
geometrias_region_sanitaria <- mapa_partidos_pba %>%
  mutate(region_sanitaria = ifelse(nam == "Lezama", "XI", region_sanitaria)) %>%
  group_by(region_sanitaria) %>%
  summarize(
    geometry = st_union(geometry),
    region_sanitaria = region_sanitaria,
    establecimientos_partido_suma = establecimientos_partido_suma) %>% # Agregar la población total
  distinct(geometry, .keep_all = TRUE)

geometrias_region_sanitaria$establecimientos_partido_suma <- NULL

rm(filtros,mapa_partidos_pba,partidos_por_regiones_sanitarias,establecimientos_por_partido_region)



# Funciones ---------------------------------------------------------------



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

# Outputs -----------------------------------------------------------------

# ambos outputs tienen sus corresponidentes objetos dentro del reactive

### Tabla GT --------------------------------------------------------------



# # Dentro del reactive
# 
# datos_rendimiento <- rendimientos %>% 
#   filter(anio == "2018") %>% 
#   filter(region_sanitaria %in% c("I","II")) %>% 
#   filter(establ_tipo_financiamiento %in% c("Municipal", "Provincial"))
# 
# datos_poblacion <- poblacion %>% 
#   filter(anio == "2018") %>% 
#   filter(region_sanitaria %in% c("I","II")) %>% 
#   group_by(partido_nombre, sexo_label) %>% 
#   summarise(poblacion = sum(poblacion)) %>% 
#   pivot_wider(names_from = "sexo_label",
#               values_from = "poblacion") %>% 
#   as.data.frame()
# 
# # output tabla_resumen
# tabla_resumen <- datos_rendimiento %>%
#   group_by(partido_nombre) %>%
#   summarise(establecimientos = n_distinct(establecimiento_id),
#             cons_medicas_media = round(mean(consultas_medicas,na.rm = TRUE)),
#             camas_disp_media = round(mean(promedio_camas_disponibles,na.rm = TRUE)),
#             pacientes_dias_media = round(mean(pacientes_dias,na.rm = TRUE))) %>%
#   left_join(datos_poblacion, by = "partido_nombre") %>% 
#   gt() %>%
#   tab_header(paste("Región Sanitaria N°")) %>% # input$selectSpecie
#   cols_label(partido_nombre = "Partido",
#              establecimientos = "Establecimientos",
#              cons_medicas_media = "Consultas médicas promedio",
#              camas_disp_media = "Camas disponibles promnedio",
#              pacientes_dias_media = "Pacientes promedio por día",
#              'Ambos sexos' = "Población",
#              'Femenino' = "Mujeres",
#              'Masculino' = "Varones")
# 
# 
# tabla_resumen
# 
# rm(datos_rendimiento,datos_poblacion,tabla_resumen)

# Mapa  -------------------------------------------------------------------


# # Dentro del reactive ponemos
# 
# establecimientos_filtrados <- establecimientos %>%
#   filter(region_sanitaria %in% c("I","II"))
# 
# geometrias_filtradas <- geometrias_region_sanitaria %>%
#   filter(region_sanitaria %in% c("I", "II"))
# 
# 
# etiqueta <- sprintf(
#   "<strong>%s</strong><br/>%s",
#   establecimientos_filtrados$establ_nombre_original,
#   establecimientos_filtrados$establ_domicilio
# ) %>%
#   lapply(htmltools::HTML)
# 
# unique(establecimientos$establ_tipo_financiamiento)
# rm(paleta_colores)
# 
# #paleta de colores para los tipos de financiamiento
# getColor <- function(establecimientos_filtrados) {
#   sapply(establecimientos$establ_tipo_financiamiento, function(establ_tipo_financiamiento) {
#     if(establ_tipo_financiamiento %in% c("Municipal")) {
#       "orange"
#     } else if(establ_tipo_financiamiento %in% c("Provincial")) {
#       "#22a954"
#     } else {
#       "#0072bb"
#     } })
# }
# 
# 
# icons <- awesomeIcons(
#   icon = 'medkit',
#   iconColor = 'white',
#   library = 'ion',
#   markerColor = getColor(establecimientos_filtrados)
# 
# )
# 
# 
# mapa <- leaflet() %>%
#   addTiles() %>%
#   addAwesomeMarkers(
#     data = establecimientos_filtrados,
#     icon = icons,
#     label = etiqueta,
#     clusterOptions = markerClusterOptions()
#   ) %>%
#   addPolygons(
#     data = geometrias_filtradas,
#     fillColor = "blue",
#     fillOpacity = 0.2,
#     weight = 1,
#     color = "white",
#     dashArray = "2",
#     highlightOptions = highlightOptions(
#       weight = 5,
#       color = "#666",
#       dashArray = "",
#       fillOpacity = 0.7,
#       bringToFront = TRUE
#     )
#   )
# 
# mapa
# 
# 
# rm(geometrias_filtradas,establecimientos_filtrados,mapa)
   


# # THEME -------------------------------------------------------------------
# 
# tema_untref = bs_theme(bootswatch = "journal") #genera una lista con todo 
# # actualizamos el tema con lo que nos dio el ayudante de tema
# tema_untref = bs_theme_update(tema_untref, 
#                               bg = "#fff", 
#                               base_font = font_google("Montserrat"), 
#                               fg = "#000")

# APP ---------------------------------------------------------------------


# Interfaz de usuario -----------------------------------------------------
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
                   multiple = TRUE,
                   selected = unique(rendimientos$region_sanitaria)),
    selectizeInput(inputId = "select_financiamiento", 
                   label = "Seleccione el tipos de financiamiento:",
                   choices = unique(rendimientos$establ_tipo_financiamiento),
                   multiple = TRUE,
                   selected = unique(rendimientos$establ_tipo_financiamiento))
  ),
  mainPanel(
    fluidRow(
      column(6, tableOutput("tabla_resumen")),
      column(6, leafletOutput("mapa_leaflet"))
    )
  )
)

# Server ----------------------------------------------------------------
server <- function(input, output) {
  
  # Filtrar los datos según las selecciones del usuario
  
  datos_rendimiento_filtrados <- reactive({
  # Primer insumo rendimientos  
    datos_rendimiento <- rendimientos   %>% 
      filter(anio == input$select_anio)%>%
      filter(region_sanitaria %in% input$select_region)%>%
      filter(establ_tipo_financiamiento %in% input$select_financiamiento)
    return(datos_rendimiento) # SIEMPRE EN EL RETURN VA EL NOMBRE DE LA VARIABLE QUE QUERES MOSTRAR, NO EL NOMBRE DEL REACTIVE
  })
  
  datos_poblacion_filtrados <- reactive({
    
      datos_poblacion <- poblacion %>% 
      filter(anio == input$select_anio)%>%
      filter(region_sanitaria %in% input$select_region)%>%
      group_by(partido_nombre, sexo_label) %>% 
      summarise(poblacion = sum(poblacion)) %>% # ACA ESTABA MAL EN NOMBRE DE LA COLUMNA QUE SUMAS 
      pivot_wider(names_from = "sexo_label",
                  values_from = "poblacion") %>% 
      as.data.frame()
    return(datos_poblacion)
  
   })
  
  # Crear la tabla resumen
  output$tabla_resumen <- render_gt({
    
    
    datos_rend = datos_rendimiento_filtrados() # ASIGNAS EL REACTIVE A UNA VARIABLE PARA FACILITAR EL PROCESAMIENTO POSTERIOR (SIEMPRE EL REACTIVE SE LLAMA COMO A UNA FUNCION SIN PARAMETROS: )
    datos_pob = datos_poblacion_filtrados() # ASIGNAS EL REACTIVE A UNA VARIABLE PARA FACILITAR EL PROCESAMIENTO POSTERIOR
    
    browser()
    tabla_resumen <- datos_rend  %>%
      group_by(partido_nombre) %>%
      summarise(
        consultas_medicas_promedio = round(mean(consultas_medicas, na.rm = TRUE)),
        camas_disponibles_promedio = round(mean(promedio_camas_disponibles, na.rm = TRUE)),
        pacientes_dias_media = round(mean(pacientes_dias, na.rm = TRUE))
      ) %>%
      left_join(datos_pob, by = "partido_nombre") %>%
      gt() %>%
      tab_header(paste("Región Sanitaria N°"), input$select_financiamiento) %>%
      cols_label(partido_nombre = "Partido",
                 consultas_medicas_promedio = "Consultas médicas promedio",
                 camas_disponibles_promedio = "Camas disponibles promedio",
                 pacientes_dias_media = "Pacientes promedio por día",
                 `Ambos sexos` = "Población") # ACA CAMBIE VALUE POR AMBOS SEXOS 
    return(tabla_resumen)
  })
  
  # Input reactivo para mapa
  
  establecimientos_filtrados <- reactive({
    establecimientos %>%
    filter(region_sanitaria %in% input$select_region)
    })
  
  geometrias_filtradas <- reactive({
    geometrias_region_sanitaria %>%
      filter(region_sanitaria %in% input$select_region)
  })  
  
  
  etiqueta <- reactive({
    sprintf(
      "<strong>%s</strong><br/>%s",
      establecimientos_filtrados$establ_nombre_original,
      establecimientos_filtrados$establ_domicilio
    ) %>%
      lapply(htmltools::HTML)
  }) 
  
  
  icons <- awesomeIcons(
    icon = 'medkit',
    iconColor = 'white',
    library = 'ion',
    markerColor = getColor(establecimientos_filtrados)
    
  )
  
  

  # Crear el mapa Leaflet
  output$mapa_leaflet <- renderLeaflet({
    mapa <- leaflet() %>%
      addTiles() %>%
      addAwesomeMarkers(
        data = establecimientos_filtrados,
        icon = icons,
        label = etiqueta,
        clusterOptions = markerClusterOptions()
      ) %>%
      addPolygons(
        data = geometrias_filtradas,
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
    
    return(mapa) # FALTABA EL RETURNO
    
  })
}

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)

