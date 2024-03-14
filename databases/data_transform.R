
# Transformación de bases de datos

###################################
# industry_autos_mx
###################################

# Carga las librerías necesarias
library(dplyr)

# Establece el camino hacia la carpeta donde están los archivos
path <- "/Users/daviddrums180/Tec/Case_Study_Form/databases/industry_autos_mx"

# Lista todos los archivos que comienzan con "mx_venta_vehiculos"
files <- list.files(path, pattern = "^mx_venta_vehiculos", full.names = TRUE)

# Lee cada archivo y combínalos en un solo dataframe
industry_autos_df <- files %>%
  lapply(read.csv) %>%
  bind_rows()

# Antes de guardar el nuevo archivo, borra los archivos originales
if (length(files) > 0) {
  file.remove(files)
}

# Define el nombre del archivo donde guardarás el dataframe
nuevo_archivo <- paste0(path, "/mx_venta_vehiculos.csv")

# Guarda el dataframe en el nuevo archivo CSV
write.csv(industry_autos_df, nuevo_archivo, row.names = FALSE)

# Imprime un mensaje de confirmación
cat("El dataframe se ha guardado correctamente en:", nuevo_archivo, "\n")


###################################
# form bajas
###################################

# Cargar las bibliotecas necesarias
library(readxl)
library(dplyr)
library(writexl)

# Leer el archivo Excel
form_bajas <- read_excel("form/form_bajas.xlsx")

# Transformar las columnas especificadas a minusculas sin modificar el dataframe original
form_bajas <- form_bajas %>%
  mutate(
    Dpto = iconv(tolower(Dpto), "UTF-8", "ASCII//TRANSLIT"),
    Puesto = iconv(tolower(Puesto), "UTF-8", "ASCII//TRANSLIT"),
    `Estado Civil` = iconv(tolower(`Estado Civil`), "UTF-8", "ASCII//TRANSLIT"),
    Genero = iconv(tolower(Genero), "UTF-8", "ASCII//TRANSLIT"),
    Estado = iconv(tolower(Estado), "UTF-8", "ASCII//TRANSLIT"),
    Municipio = iconv(tolower(Municipio), "UTF-8", "ASCII//TRANSLIT")
  ) %>%
  mutate(
    Dpto = str_replace_all(Dpto, 'produccion carton mc', 'produccion cartón mc') %>%
      str_replace_all('produccion carton mdl', 'produccion cartón mdl') %>%
      str_replace_all("produccion cart'on mdl", 'produccion cartón mc') %>%
      str_replace_all("produccion cart'on mc", 'produccion cartón mc') %>%
      str_replace_all('paileria y pintura', 'paileria') %>%
      str_replace_all('paileria y pintura', 'paileria') %>%
      str_replace_all('costura t2', 'costura'),
    Puesto = str_replace_all(Puesto, 'ayudante de embarques', 'ayud. de embarques') %>%
      str_replace_all('inspector de calidad', 'inspector calidad') %>%
      str_replace_all('ayud. de embarques', 'ayudante de embarques') %>%
      str_replace_all('customer service inf', 'customer service') %>%
      str_replace_all('ayu. de pintor', 'ayudante de pintor') %>%
      str_replace_all('costurera', 'costurero'), # Unificando géneros cuando el puesto es el mismo
    `Estado Civil` = str_replace_all(`Estado Civil`, 'matrimonio', 'casado') %>%
      str_replace_all('solteria', 'soltero') %>%
      str_replace_all('soltera', 'soltero') %>%
      str_replace_all('casada', 'casado') %>%
      str_replace_all('union libre', 'soltero'), # Asumiendo que quieres unificar 'unión libre' con 'soltero'
    Estado = str_replace_all(Estado, 'nuevo le.on', 'nuevo leon'),
    Municipio = str_replace_all(Municipio, 'ramoz arizpe', 'ramos arizpe') %>%
      str_replace_all('ca.nada blanca', 'cañada blanca') %>%
      str_replace_all('san nicolas de los g', 'san nicolas')
  )

# Mostrar valores únicos para análisis sin modificar el dataframe
valores_unicos_dpto <- unique(form_bajas$Dpto)
valores_unicos_puesto <- unique(form_bajas$Puesto)
valores_unicos_estado_civil <- unique(form_bajas$`Estado Civil`)
valores_unicos_genero <- unique(form_bajas$Genero)
valores_unicos_estado <- unique(form_bajas$Estado)
valores_unicos_municipio <- unique(form_bajas$Municipio)

# Puedes imprimir los valores únicos para cada columna como necesites
list(
  Dpto = valores_unicos_dpto,
  Puesto = valores_unicos_puesto,
  `Estado Civil` = valores_unicos_estado_civil,
  Genero = valores_unicos_genero,
  Estado = valores_unicos_estado,
  Municipio = valores_unicos_municipio
)

# Escribir el dataframe limpio a un nuevo archivo Excel, reemplazando el original
write_xlsx(form_bajas, "form/form_bajas.xlsx")

######################################
# Nearshoring Datos Series de Tiempo
#####################################

library(readxl)
library(writexl)
library(stringi)

NS_St <- read_excel("/Users/danielnajera/Desktop/Inteligencia_Aritificial/Time_Series_Datos_NS.xlsx")
View(NS_St)
str(NS_St)

NS_St[NS_St == "-"] <- NA

NS_St$Empleo <- as.numeric(NS_St$Empleo)
NS_St$Educación <- as.numeric(NS_St$Educacion)
NS_St$Innovación <- as.numeric(NS_St$Innovacion)
NS_St$Inseguridad_Homicidio <- as.numeric(NS_St$Inseguridad_Homicidio)
NS_St$CO2_Emisiones <- as.numeric(NS_St$CO2_Emisiones)

#NS_St <- NS_St[, -which(names(NS_St) == "Empleo")]
#NS_St <- NS_St[, -which(names(NS_St) == "CO2_Emisiones")]

mediana_educacion <- median(NS_St$Educacion, na.rm = TRUE)
NS_St$Educacion[24] <- mediana_educacion

maximo_educacion <- max(NS_St$Educacion, na.rm = TRUE)
NS_St$Educacion[25] <- maximo_educacion

minimo_educacion <- min(NS_St$Educacion, na.rm = TRUE)
NS_St$Educacion[26] <- minimo_educacion

maximo_innovacion <- max(NS_St$Innovacion, na.rm = TRUE)
NS_St$Innovacion[25] <- maximo_innovacion

minimo_innovacion <- min(NS_St$Innovacion, na.rm = TRUE)
NS_St$Innovacion[26] <- minimo_innovacion

maximo_inseguridad <- max(NS_St$Inseguridad_Homicidio, na.rm = TRUE)
NS_St$Inseguridad_Homicidio[26] <- maximo_inseguridad

#names(NS_St)[names(NS_St) == "Flujo_Real"] <- "IED_Flujos_Reales"

write_xlsx(NS_St, "/Users/danielnajera/Desktop/Inteligencia_Aritificial/Time_Series_Datos_NS.xlsx")


