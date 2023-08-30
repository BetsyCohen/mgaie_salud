# SETUP -------------------------------------------------------------------

library(shiny)
library(tidyverse)
library(tidyr)
library(DT)
library(lubridate)
library(shinyWidgets)
library(highcharter)


# DATA --------------------------------------------------------------------

options(timeout=1000000) # incrementamos el timeout debido a que la descarga es lenta

url = "http://datos.salud.gob.ar/dataset/2eff770c-1c2b-4a22-9281-c3b5e9412086/resource/c1253897-d507-41f7-a3e1-6ed756e7243b/download/tasa-mortalidad-infantil-deis-1990-2021.csv"

download.file(url, destfile = "TMI.csv")

data = read.csv("TMI.csv")
unlink("TMI.csv")

# PROCESAMIENTO -----------------------------------------------------------

###proceso los datos para graficar
bd <- data %>% pivot_longer(
  cols = !indice_tiempo,
  names_to = "prov",
  values_to = "TMI"
) %>%
  mutate(
    prov = str_sub(prov, 21, nchar(prov)),
    ano = year(ymd(indice_tiempo)),
    indice_tiempo = ymd(indice_tiempo),
    prov=case_when(prov == "cordoba" ~ str_to_title(prov),
                   prov == "caba"  ~ "CABA",
                   prov == "argentina" ~ str_to_title(prov),
                   prov == "corientes" ~ str_to_title(prov),
                   prov == "chaco" ~ str_to_title(prov),
                   prov == "chubut" ~ str_to_title(prov),
                   prov == "neuquen" ~ str_to_title(prov),
                   prov == "misiones" ~ str_to_title(prov),
                   prov == "jujuy" ~ str_to_title(prov),
                   prov == "catamarca" ~ str_to_title(prov),
                   prov == "corrientes" ~ str_to_title(prov),
                   prov == "formosa" ~ str_to_title(prov),
                   prov == "salta" ~ str_to_title(prov),
                   prov == "buenosaires" ~ "Buenos Aires",
                   prov == "santiagodelestero" ~ "Santiago del Estero",
                   prov == "santafe" ~ "Santa Fe",
                   prov == "tierradelfuego" ~ "Tierra del Fuego",
                   prov == "santacruz" ~ "Santa Cruz",
                   prov == "sanjuan" ~ "San Juan",
                   prov == "sanluis" ~ "San Luis",
                   prov == "lapampa" ~ "La Pampa",
                   prov == "larioja" ~ "La Rioja",
                   prov == "entrerios" ~ "Entre Rios",
                   prov == "rionegro" ~ "Rio Negro",
                   TRUE ~ prov  # Mantén el valor original para otros casos
    )
  ) %>% 
  select(-indice_tiempo)


# SHINYAPP ---------------------------------------------------------------

# UI ----------------------------------------------------------------------
ui <- fluidPage(
  
  # Titulo de la app -----
  titlePanel("Mortalidad infantil en Argentina"),
  
  ## imagen
  img(src = "https://icon-library.com/images/mortality-icon/mortality-icon-28.jpg", height = 72, width = 72),
  
  # Sidebar layout
  sidebarLayout(
    sidebarPanel(
      h2('Filtros'),
      selectizeInput(
        inputId = 'selectProv',
        label = 'Seleccioná la provincia',
        choices = unique(bd$prov),
        multiple = TRUE,
        selected = 'Argentina'
      ),
    ),
    
    # Grafico --------------
    mainPanel(
      h2('Gráfico'),
      highcharter::highchartOutput('hc')
    )
  )
)

# Server -----------------------------------------------------------------

server <- function(input, output) {
  
  ## Output del gráfico
  output$hc <- renderHighchart({
    
    #### --- definición y armado del output 1
    
    provinciasSeleccionadas <- input$selectProv # Define el input
    
    # Filtra y combinar los datos para las provincias seleccionadas
    grafico <- bd %>% 
      filter(prov %in% provinciasSeleccionadas)
    
    # Define el objeto hc que es el gráfico de highchart
    hc <- highchart() %>%
      hc_chart(type = "line") %>%
      hc_title(text = "Serie de tiempo de TMI por 1000 nacidos vivos, Argentina, periodo 1990-2021") %>%
      hc_xAxis(title = list(text = "Año")) %>%
      hc_yAxis(title = list(text = "TMI ")) %>%
      hc_exporting(enabled = TRUE) # Habilitar la opción de exportación
    
    # Agrega series en función de las provincias seleccionadas
    for (provincia in provinciasSeleccionadas) {
      data_serie <- grafico[grafico$prov == provincia,]
      hc <- hc %>%
        hc_add_series(
          data_serie,
          "line",
          hcaes(x = ano, y = TMI),
          name = provincia,
          marker = list(radius = 4)
        )
    }
    
    ### --- el output en cuestion
    
    hc
  })
  
}

# Proceda ---------------------------------------------------------------
shinyApp(ui = ui, server = server)


### Proximos pasos
# agregar tabla de GT y botón de descarga

