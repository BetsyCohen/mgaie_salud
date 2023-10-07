library(shiny)
library(tidyverse)
library(shinythemes)
library(bslib) 
library(shinyjs) 
library(leaflet)
library(gt)
library(sf)
library(plotly)


# Insumos


rendimientos <- read_rds("insumos_app/rendimientos.rds")


# dato reactivo

evolucion_filtrada <- rendimientos |>
  filter(partido_nombre %in% c("Marcos Paz","Roque Pérez", "Moreno")) |>
  filter(establ_tipo_financiamiento %in% c("Municipal","Provincial")) |>
  group_by(anio,establ_tipo_financiamiento,partido_nombre) |>
  summarise(consultas_medicas = sum(consultas_medicas,na.rm = T),
            camas_promedio = mean(promedio_camas_disponibles,na.rm = T)) |>
  as.data.frame()



# Crear una lista para almacenar los gráficos de cada tipo de financiamiento (esto son datos reactivos también?)
graficos_consultas <- list()


# Para cada financiamiento
for (financiamiento in unique(evolucion_filtrada$establ_tipo_financiamiento)) {
  datos_financiamiento <- evolucion_filtrada %>%
    filter(establ_tipo_financiamiento == financiamiento)

  # Agregar títulos personalizados a cada subplot utilizando annotations
  titulo <- sprintf("Financiamiento %s", financiamiento)
  annotations <- list(
    list(x = 0.5,
         y = 1.05,
         xref = "paper",
         yref = "paper",
         text = titulo,
         showarrow = FALSE)
  )

  grafico <- plot_ly(data = evolucion_filtrada,
                     x = ~anio,
                     y = ~consultas_medicas,
                     color = ~partido_nombre,
                     type = 'scatter',
                     mode = 'lines+markers',
                     text = ~partido_nombre) |>
    layout(annotations = annotations,
           title = "<b>Evolución de consultas médicas<b>")

  graficos_consultas[[financiamiento]] <- grafico
}

# Crear subplots de graficos de consultas para cada tipo de financiamiento
subplot(graficos_consultas, nrows = length(graficos_consultas), margin = 0.1)







graficos_camas <- list()

# Crear un gráficos de consultas para cada tipo de financiamiento
for (financiamiento in unique(evolucion_filtrada$establ_tipo_financiamiento)) {
  datos_financiamiento <- evolucion_filtrada %>%
    filter(establ_tipo_financiamiento == financiamiento)

  # Agregar títulos personalizados a cada subplot utilizando annotations
  titulo <- sprintf("Financiamiento %s", financiamiento)
  annotations <- list(
    list(x = 0.5,
         y = 1.05,
         xref = "paper",
         yref = "paper",
         text = titulo,
         showarrow = FALSE)
  )

  grafico <- plot_ly(data = datos_financiamiento,
                     x = ~anio,
                     y = ~camas_promedio,
                     color = ~partido_nombre,
                     type = 'scatter',
                     mode = 'lines+markers',
                     text = ~partido_nombre) |>
    layout(annotations = annotations,
           title = "<b>Evolución suma de camas promedio<b>")

  graficos_camas[[financiamiento]] <- grafico
}

# Crear subplots para cada tipo de financiamiento
subplot(graficos_camas, nrows = length(graficos_camas), margin = 0.1)






