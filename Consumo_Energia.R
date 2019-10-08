#Instalar librerias
install.packages("sf")
install.packages("tidyverse")
#glue es para agregar directorios y acortar la direccion, no es necesario poner las rutas todo el tiempo
install.packages("glue")

# Cargar librerias
library(sf); library(tidyverse); library(glue)
# Definir rutas de trabajo
home_dir <- "e:/HELEN PC/CEDEUS/7. Información solicitada/R ladies/Modulo 1/"
input_dir <- glue("{home_dir}Input")
output <- glue("{home_dir}Output")
# Nombre del archivo del censo
censo_file <- "Censo2017_Manzanas.csv"
# Nombre del archivo con los codigos de comunas
codes_file <- "Microdato_Censo2017-Comunas.csv"
# Nombre del archivo con consumo electrico
energia_file <- "Consumo_Electrico.csv"
# Leer archivo codigos
codes <- read_csv2(glue("{input_dir}/{codes_file}"))
# Leer archivo consumo electrico y:
#   Cambiar los nombres a nombres más sencillos
#   Filtrar para el año 2017 y clientes residenciales
#   Modificar los nombres de las comunas para que queden en 
#     mayusculas
#   Seleccionar sólo 3 columnas
# Esto nos deja con el consumo electrico por comuna
#el ingreso de la tabla csv sera en utf8, es decir, reconoce los tildes y otros caracteres
energia <- read.csv(glue("{input_dir}/Consumo_Electrico.csv"), encoding = "UTF-8")
energia <- read_csv(glue("{input_dir}/{energia_file}")) %>%
   rename(Energia = `Energía (kWh)`,
         Cliente = `Tipo de Cliente`,
         Region = Región) %>%
  filter(Año == 2017, Cliente == "Residencial") %>%
  mutate(NOM_COMUNA = toupper(Comuna)) %>%
  right_join(codes, by = "NOM_COMUNA") %>%
  select(NOM_COMUNA, Region, Energia)
# Leer el archivo del censo y:
#   Seleccionar Comuna y total de viviendas
#   Unir con tabla de códigos por "Comuna" 
#     (para tener nombres de comunas)
#   Agrupar por el nombre de la comuna
#   Contar viviendas por comuna
#   Unir con tabla de consumo por nombre de comuna
#   Seleccionar las columnas de Region, Nombre de Comuna,
#     Consumo (Energia) y Viviendas
#   Calcular Consumo por vivienda
#   Eliminar casos sin datos
consumo <- read_csv2(glue("{input_dir}/{censo_file}")) %>%
  select(COMUNA, TOTAL_VIV) %>%
  left_join(codes, by = "COMUNA") %>%
  group_by(NOM_COMUNA) %>%
  summarise(Viviendas = sum(TOTAL_VIV)) %>%
  left_join(energia, by = "NOM_COMUNA") %>%
  select(Region, NOM_COMUNA, Energia, Viviendas) %>%
  mutate(Energia_vivienda = Energia/Viviendas) %>%
  filter(!is.na(Region))
# Crear un boxplot con los datos
grafico <- ggplot(consumo, aes(x=Region, y=Energia_vivienda, 
                           group = Region, color=Region)) +
  geom_boxplot()
grafico
# Filtrar outliers y cambiar nombres de ejes
grafico + 
  scale_y_continuous(name = "Kw/h por vivienda",
                     limits = c(0, 350)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none")
### Unir con shapefile
# Definir nombre de shapefile
comunas_shp <- "Comunas_Chile"
# Leer shapefile
comunas_shp <- read_sf(dsn = glue("{input_dir}"), 
                       layer = glue("{comunas_shp}"))
# Unir shapefile con datos de consumo segun nombre de comuna
consumo_sp <- left_join(comunas_shp, consumo, by = "NOM_COMUNA")
# Escribir resultados a un shapefile
write_sf(consumo_sp, 
         dsn = output, 
         layer = "Consumo_electrico", 
         driver = "ESRI Shapefile")
