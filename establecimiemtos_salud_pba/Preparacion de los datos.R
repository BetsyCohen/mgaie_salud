#### Shinny app establecimientos de salud PBA ####

# ETL para app ------------------------------------------------------------

# SETUP -------------------------------------------------------------------

# Librerías

library(shiny)
library(tidyverse)
library(shinythemes)
library(bslib) 
library(leaflet)
library(gt)
library(sf)




# Importación de las fuentes


establecimientos<- read.csv("https://catalogo.datos.gba.gob.ar/dataset/91743f68-bc82-4475-baca-7d5d6908eee8/resource/c2d51824-c61d-4374-a014-cca5b76b082a/download/establecimientos-salud-publicos.csv", sep = ";") 
rendimientos <- read.csv("https://catalogo.datos.gba.gob.ar/dataset/219d6b32-2e34-40cb-913d-b05c47cab75f/resource/8c3130cb-61ad-4014-b829-503b214ba3c0/download/rendimientos-hospitalarios_2005-2022.csv")
mapa_partidos_pba <- read_sf("departamento.shp") 
# tabla trabajada previamente fuente INDEC
poblacion <- read.csv("proy_1025_depto_buenos_aires.csv",sep = ";", encoding = "latin1")


# ETL ---------------------------------------------------------------------

# seleccionar y renombrar tablas establecimiento
establecimientos <- establecimientos |> 
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
         establ_domicilio = dom) |> 
  mutate(establecimiento_id = as.character(establecimiento_id))




# Tydeo de rendimientos
rendimientos <- rendimientos |> 
  rename(partido_nombre = municipio_nombre,
         partido_id = municipio_id)|> 
  mutate(anio = as.character(anio),
         establecimiento_id = as.character(establecimiento_id), 
         partido_id = sub('.', '', partido_id)) |> 
  # Nos quedamos con el rendimientos posteriores al 2010 de los establecimientos de los cuales tenemos su ubicación en la tabla establecimientos
  filter(!is.na(establecimiento_id %in% establecimientos$establecimiento_id) ) |> # eliminar establecimientos sin georreferencia
  filter((as.numeric(anio) >2017))  # 2018 en adelante


# Agregar establ_tipo_financiamiento a la tabla de rendimientos

establ_tipo_financiamiento <- establecimientos |> select(establecimiento_id, establ_tipo_financiamiento)

rendimientos <- rendimientos |> left_join(establ_tipo_financiamiento, by = "establecimiento_id" )

rm(establ_tipo_financiamiento)

# Tidear proyecciones de población y pegarle las variables de filtro
poblacion <- poblacion |> 
  pivot_longer(cols = -c(1:3),
               names_to = ("anio")) |> 
  mutate(anio= as.numeric((str_remove(anio,"X")))) |> 
  filter(anio >2017) |> # 2018 en adelante
  mutate(anio = as.character(anio)) |> 
  rename(partido_nombre = partido_label,
         poblacion = value)


# Armar una tabla con las regiones partidos y cantidad de establecimientos
establecimientos_por_partido_region <- rendimientos |> 
  group_by(region_sanitaria,partido_id,partido_nombre) |> 
  summarise(region_sanitaria = region_sanitaria,
            partido_id = partido_id,
            partido_nombre = partido_nombre,
            establecimientos_partido_suma = sum(n_distinct(establecimiento_id))) |> 
  distinct(partido_id,.keep_all = T)  |> 
  ungroup()


poblacion <- poblacion |> 
  left_join(establecimientos_por_partido_region, by =  "partido_nombre") |> 
  relocate(partido_id, .after =partido_nombre ) |> 
  filter(!(is.na(partido_id)))


# Tydeo de mapa_partidos_pba
mapa_partidos_pba <- mapa_partidos_pba |> 
  mutate(provincia = substr(as.character(in1), 1, 2), #Obtener segmento cod. prov
         partido_id = substring(as.character(in1), 3) #Obtener segmento cod. partido
  ) |> 
  filter(provincia == "06") # nos quedamos con Buenos Aires


# Pegamos las regiones en el mapa de partidos
mapa_partidos_pba <- mapa_partidos_pba |> 
  left_join(establecimientos_por_partido_region, by = "partido_id")

# Construimos las geometrías de las 12 regiones a partir de los partidos que las componen
geometrias_region_sanitaria <- mapa_partidos_pba %>%
  mutate(region_sanitaria = ifelse(nam == "Lezama", "XI", region_sanitaria)) %>%
  group_by(region_sanitaria) %>%
  summarize(
    geometry = st_union(geometry),
    region_sanitaria = region_sanitaria,
    establecimientos_partido_suma = establecimientos_partido_suma) |> # Agregar la población total
  distinct(geometry, .keep_all = TRUE)

geometrias_region_sanitaria$establecimientos_partido_suma <- NULL

rm(filtros,mapa_partidos_pba,partidos_por_regiones_sanitarias,establecimientos_por_partido_region)


# Salvar todos los archivos del enviroment en un solo rds

guardar_objetos_como_rds <- function() {
  # Obtener la lista de objetos en el entorno global
  objetos <- ls(envir = .GlobalEnv)
  
  # Carpeta de destino
  carpeta_destino <- "insumos_app"
  
  # Crear la carpeta si no existe
  if (!file.exists(carpeta_destino)) {
    dir.create(carpeta_destino)
  }
  
  # Guardar cada objeto como RDS en la carpeta
  for (objeto in objetos) {
    archivo_rds <- file.path(carpeta_destino, paste0(objeto, ".rds"))
    saveRDS(get(objeto, envir = .GlobalEnv), archivo_rds)
    cat("Guardado:", archivo_rds, "\n")
  }
}

# Llama a la función para guardar los objetos
guardar_objetos_como_rds()




