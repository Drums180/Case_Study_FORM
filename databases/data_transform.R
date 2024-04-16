
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

###################################
# encuestas
###################################

# Cargar las bibliotecas necesarias
library(readxl)
library(dplyr)
library(writexl)

# Leer el archivo Excel
form_encuesta <- read_excel("/Users/daviddrums180/Tec/Case_Study_Form/databases/form/Encuesta_Datos_FORM_Fall2023.xlsx")

# Modificar los nombres de las columnas
form_encuesta <- form_encuesta %>%
  rename_with(~ str_to_lower(.) %>% # Convertir a minúsculas
                str_replace_all("\\\\", "") %>% # Eliminar el caracter de escape "\"
                str_replace_all("ü", "u") %>% # Reemplazar "ü" por "u"
                iconv(to = 'ASCII//TRANSLIT') %>% # Remover acentos
                str_replace_all("[.]", "") %>% # Opcional: remover puntos si es necesario
                str_replace_all(" ", "_") %>% # Cambiar espacios por guiones bajos
                str_replace_all("-", "_") %>% # Cambiar guiones por guiones bajos
                # Reemplazo manual para "antigüedad"
                str_replace_all("antig\"uedad", "antiguedad")) 

# Mostrar los nombres de las columnas modificados
print(names(form_encuesta))

# Transformar el texto de las columnas especificadas
form_encuesta <- form_encuesta %>%
  mutate(across(c(puesto, puesto_otro, molestias_puesto, sentimiento_form),
                ~ str_to_lower(.) %>% 
                  iconv(to = 'ASCII//TRANSLIT') %>% 
                  str_replace_all("[^a-z0-9 ]", "") %>% # Conservar solo letras minúsculas, números y espacios
                  str_trim())) # Elimina espacios al inicio y al final

# Obtener valores únicos para cada una de las columnas transformadas
valores_unicos_puesto <- unique(form_encuesta$puesto)
valores_unicos_puesto_otro <- unique(form_encuesta$puesto_otro)
valores_unicos_molestias_puesto <- unique(form_encuesta$molestias_puesto)
valores_unicos_sentimiento_form <- unique(form_encuesta$sentimiento_form)

# Imprimir los valores únicos para verificar
print(valores_unicos_puesto)
print(valores_unicos_puesto_otro)
print(valores_unicos_molestias_puesto)
print(valores_unicos_sentimiento_form)

### puesto
form_encuesta$puesto_otro <- form_encuesta$puesto_otro %>%
  str_replace("maquina cnc", "operador") %>%
  str_replace("maquinas", "operador") %>%
  str_replace("director", "administrativo") %>%
  str_replace("comercial", "administrativo") %>%
  str_replace("internpracticante", "administrativo") %>%
  str_replace("coordinadora", "administrativo") %>%
  str_replace("jefe de sgc", "supervisor") %>%
  str_replace("materiales", "administrativo") %>%
  str_replace("laminado", "operador") %>%
  str_replace("desfajadora", "operador") %>%
  str_replace("ingenieria", "administrativo") %>%
  str_replace("montacarguista", "operador") %>%
  str_replace("pintura", "operador") %>%
  na_if("") # Convertir cadenas vacías a NA si es necesario

# Actualización de 'puesto' basada en 'puesto_otro' para filas con 'puesto' == "otro"
form_encuesta <- form_encuesta %>%
  mutate(puesto = if_else(puesto == "otro" & !is.na(puesto_otro), puesto_otro, puesto))

# Ahora que hemos transferido los valores de 'puesto_otro' a 'puesto', podemos eliminar 'puesto_otro'
form_encuesta <- dplyr::select(form_encuesta, -puesto_otro)

# Verificamos los cambios
print(unique(form_encuesta$puesto))

### molestias_puesto
# Definir un vector simple de stopwords
stopwords <- c("de", "la", "que", "el", "en", "y", "a", "los", "del", "se", "las", "por", "un", "para", "con", "no", "una", "su", "al", "es", "lo", "como", "más", "pero", "sus", "le", "ya", "o", "este", "sí", "porque", "esta", "entre", "cuando", "muy", "sin", "sobre", "también", "me", "hasta", "hay", "donde", "quien", "desde", "todo", "nos", "durante", "todos", "uno", "les", "ni", "contra", "otros", "ese", "eso", "ante", "ellos", "e", "esto", "mí", "antes", "algunos", "qué", "unos", "yo", "otro", "otras", "otra", "él", "tanto", "esa", "estos", "mucho", "quienes", "nada", "muchos", "cual", "poco", "ella", "estar", "estas", "algunas", "algo", "nosotros", "mi", "mis", "tú", "te", "ti", "tu", "tus", "ellas", "nosotras", "vosotros", "vosotras", "os", "mío", "mía", "míos", "mías", "tuyo", "tuya", "tuyos", "tuyas", "suyo", "suya", "suyos", "suyas", "nuestro", "nuestra", "nuestros", "nuestras", "vuestro", "vuestra", "vuestros", "vuestras", "esos", "esas", "estoy", "estás", "está", "estamos", "estáis", "están", "esté", "estés", "estemos", "estéis", "estén", "estaré", "estarás", "estará", "estaremos", "estaréis", "estarán", "estaría", "estarías", "estaríamos", "estaríais", "estarían", "estaba", "estabas", "estábamos", "estabais", "estaban", "estuve", "estuviste", "estuvo", "estuvimos", "estuvisteis", "estuvieron", "estuviera", "estuvieras", "estuviéramos", "estuvierais", "estuvieran", "estuviese", "estuvieses", "estuviésemos", "estuvieseis", "estuviesen", "estando", "estado", "estada", "estados", "estadas", "estad", "he", "has", "ha", "hemos", "habéis", "han", "haya", "hayas", "hayamos", "hayáis", "hayan", "habré", "habrás", "habrá", "habremos", "habréis", "habrán", "habría", "habrías", "habríamos", "habríais", "habrían", "había", "habías", "habíamos", "habíais", "habían", "hube", "hubiste", "hubo", "hubimos", "hubisteis", "hubieron", "hubiera", "hubieras", "hubiéramos", "hubierais", "hubieran", "hubiese", "hubieses", "hubiésemos", "hubieseis", "hubiesen", "habiendo", "habido", "habida", "habidos", "habidas", "soy", "eres", "es", "somos", "sois", "son", "sea", "seas", "seamos", "seáis", "sean", "seré", "serás", "será", "seremos", "seréis", "serán", "sería", "serías", "seríamos", "seríais", "serían", "era", "eras", "éramos", "erais", "eran", "fui", "fuiste", "fue", "fuimos", "fuisteis", "fueron", "fuera", "fueras", "fuéramos", "fuerais", "fueran", "fuese", "fueses", "fuésemos", "fueseis", "fuesen", "siendo", "sido", "sed", "tengo", "tienes", "tiene", "tenemos", "tenéis", "tienen", "tenga", "tengas", "tengamos", "tengáis", "tengan", "tendré", "tendrás", "tendrá", "tendremos", "tendréis", "tendrán", "tendría", "tendrías", "tendríamos", "tendríais", "tendrían", "tenía", "tenías", "teníamos", "teníais", "tenían", "tuve", "tuviste", "tuvo", "tuvimos", "tuvisteis", "tuvieron", "tuviera", "tuvieras", "tuviéramos", "tuvierais", "tuvieran", "tuviese", "tuvieses", "tuviésemos", "tuvieseis", "tuviesen", "teniendo", "tenido", "tenida", "tenidos", "tenidas", "tened")

# Eliminar stopwords de la columna 'molestias_puesto'
form_encuesta$molestias_puesto <- sapply(form_encuesta$molestias_puesto, function(x) {
  palabras <- str_split(x, " ")[[1]]
  palabras_filtradas <- palabras[!palabras %in% stopwords]
  paste(palabras_filtradas, collapse = " ")
})

# Reemplazar respuestas que no expresan una molestia por NA
form_encuesta$molestias_puesto <- ifelse(form_encuesta$molestias_puesto %in% c("", "nada", "no", "ninguno", "ninguna", "todo bien", "todo perfecto", "nada todo bien", "no hay", "na"), NA, form_encuesta$molestias_puesto)

# Verificar los cambios
print(unique(form_encuesta$molestias_puesto))

# sentimiento form
library(stringi)
# Diccionario de mapeo de sentimientos para simplificación
mapeo_sentimientos <- c("agusto" = "cómodo", 
                        "a gusto" = "cómodo", 
                        "feliz" = "contento",
                        "tranquila" = "cómodo",
                        "tranquilo" = "cómodo",
                        "contenta" = "contento",
                        "bien" = "cómodo",
                        "satisfecho" = "contento",
                        "satisfecha" = "contento",
                        "perfecto" = "contento",
                        "alegre" = "contento")

# Lista de stopwords en español (simplificada para el ejemplo)
stopwords <- c("y", "en", "con", "por", "el", "la", "lo", "los", "las", "un", "una", "unos", "unas", "al", "del", "me", "se", "que", "de", "no", "es")

# Ajustar la función para eliminar apóstrofos directamente
limpiar_y_simplificar_sentimientos <- function(frase) {
  # Convertir a minúsculas
  frase <- tolower(frase)
  
  # Remover acentos usando stringi
  frase <- stringi::stri_trans_general(frase, "Latin-ASCII")
  
  # Eliminar apóstrofos específicamente
  frase <- str_replace_all(frase, "'", "")
  
  # Reemplazar términos según el mapeo de sentimientos
  for (palabra in names(mapeo_sentimientos)) {
    patron <- paste0("\\b", palabra, "\\b")
    frase <- str_replace_all(frase, regex(patron, ignore_case = TRUE), mapeo_sentimientos[palabra])
  }
  
  # Eliminar stopwords
  palabras <- unlist(str_split(frase, " "))
  frase_sin_stopwords <- paste(palabras[!palabras %in% stopwords], collapse = " ")
  
  return(frase_sin_stopwords)
}

# Aplicar la limpieza y simplificación a la columna sentimiento_form de nuevo
form_encuesta$sentimiento_form <- sapply(form_encuesta$sentimiento_form, limpiar_y_simplificar_sentimientos)

# Extraer y mostrar los valores únicos después de la limpieza
valores_unicos_sentimiento_form <- unique(form_encuesta$sentimiento_form)
print(valores_unicos_sentimiento_form)

#. razon
# Ajustar el texto en la columna razon_entrada
form_encuesta$razon_entrada <- sapply(form_encuesta$razon_entrada, function(frase) {
  # Convertir a minúsculas y eliminar acentos
  frase <- tolower(stringi::stri_trans_general(frase, "Latin-ASCII"))
  
  # Eliminar apóstrofos
  frase <- str_replace_all(frase, "'", "")
  
  # Separar en palabras y eliminar stopwords
  palabras <- str_split(frase, " ")[[1]]
  palabras_filtradas <- palabras[!palabras %in% stopwords]
  
  # Reconstruir la frase
  frase_final <- paste(palabras_filtradas, collapse = " ")
  
  return(frase_final)
})

library(writexl)
write_xlsx(form_encuesta, "/Users/daviddrums180/Tec/Case_Study_Form/databases/form/Encuesta_Datos_FORM_Fall2023.xlsx")

###################################
# RH 2024
###################################

# Cargar las librerías necesarias
library(readxl)
library(dplyr)
library(stringr) 
library(readr)

# Leer el archivo Excel
# Asegúrate de cambiar la ruta del archivo a la ubicación correcta donde tienes tu archivo Excel
datos <- read_excel("/Users/daviddrums180/Tec/Case_Study_Form/databases/form/Datos_FORM_RH_FJ2024.xlsx")

datos$SD <- as.numeric(datos$SD)

# Función para limpiar texto: convertir a minúsculas, quitar puntuación, acentos y espacios extras
clean_text <- function(text) {
  text %>% 
    str_to_lower() %>% # Convertir a minúsculas
    str_remove_all("[[:punct:]]") %>% # Quitar puntuación
    # Reemplazar letras acentuadas sin quitar la ñ
    str_replace_all(c("á" = "a", "é" = "e", "í" = "i", "ó" = "o", "ú" = "u", "ü" = "u"))
}

# Aplicar la función de limpieza a las columnas de texto
datos <- datos %>%
  mutate(across(where(is.character), clean_text, .names = "{.col}")) %>%
  mutate(`Correo Electronico` = datos$`Correo Electronico`)

datos$Estado <- gsub("nuevo le'on", "nuevo leon", datos$Estado, fixed = TRUE)

datos$Municipio <- gsub("^san nicolas.*$", "san nicolas de los garza", datos$Municipio, perl = TRUE)
datos$Municipio <- gsub("ramoz arizpe", "ramos arizpe", datos$Municipio, fixed = TRUE)
datos$Municipio <- gsub("^cadereyta.*$", "cadereyta jimenez", datos$Municipio, perl = TRUE)

datos$Puesto <- ifelse(grepl("^ay", datos$Puesto, ignore.case = TRUE), "ayudante", datos$Puesto)
datos$Puesto <- gsub("innovaci[oó]n \\+ dise[nñ]o", "innovacion + diseno", datos$Puesto, ignore.case = TRUE)
datos$Puesto <- gsub("producci[oó]n", "produccion", datos$Puesto, ignore.case = TRUE)
datos$Puesto <- gsub("cordinadora", "coordinadora", datos$Puesto, ignore.case = TRUE)
datos$Puesto <- gsub("asistente de direcci[oó]n", "asistente de direccion", datos$Puesto, ignore.case = TRUE)
datos$Puesto <- gsub("contrataci[oó]n y n[oó]minas", "contratacion y nominas", datos$Puesto, ignore.case = TRUE)
datos$Puesto <- gsub("chofer gestor", "chofer", datos$Puesto, ignore.case = TRUE)
datos$Puesto <- gsub("operador de sierra", "operador sierra", datos$Puesto, ignore.case = TRUE)
datos$Puesto <- gsub("costurera", "costurero", datos$Puesto, ignore.case = TRUE) # Unificar costurera y costurero

datos$Dpto <- gsub("producci[oó]'?n cart'?on mdl", "produccion carton mdl", datos$Dpto, ignore.case = TRUE)
datos$Dpto <- gsub("producci[oó]'?n cart'?on mc", "produccion carton mc", datos$Dpto, ignore.case = TRUE)
datos$Dpto <- gsub("producci[oó]'?n retorn", "produccion retornable", datos$Dpto, ignore.case = TRUE)
datos$Dpto <- gsub("paileria y pintura", "paileria", datos$Dpto, fixed = TRUE)
datos$Dpto <- gsub("producci[oó]n", "produccion", datos$Dpto, ignore.case = TRUE)
datos$Dpto <- gsub("produccion cart[oó]n mc", "produccion carton mc", datos$Dpto, ignore.case = TRUE)
datos$Dpto <- gsub("produccion retorn.*", "produccion retornable", datos$Dpto, ignore.case = TRUE)
datos$Dpto <- ifelse(grepl("producci[oó]n", datos$Dpto, ignore.case = TRUE), "produccion", datos$Dpto)
datos$Puesto <- ifelse(grepl("^coordinadora", datos$Puesto, ignore.case = TRUE), "coordinadora", datos$Puesto)
datos$Puesto <- ifelse(grepl("^ingenieria", datos$Puesto, ignore.case = TRUE), "ingeniero", datos$Puesto)
datos$Puesto <- ifelse(grepl("^op", datos$Puesto, ignore.case = TRUE), "operador", datos$Puesto)
datos$Puesto <- ifelse(grepl("^auxiliar", datos$Puesto, ignore.case = TRUE), "auxiliar", datos$Puesto)
datos$Puesto <- ifelse(grepl("^residente", datos$Puesto, ignore.case = TRUE), "residente", datos$Puesto)
datos$Puesto <- ifelse(grepl("^inspectora", datos$Puesto, ignore.case = TRUE), "inspector", datos$Puesto) # Asumiendo que queremos normalizar el género
datos$Puesto <- ifelse(grepl("^asistente", datos$Puesto, ignore.case = TRUE), "asistente", datos$Puesto)
datos$Puesto <- ifelse(grepl("^costurero", datos$Puesto, ignore.case = TRUE), "costurero", datos$Puesto) # Incluye costurera
datos$Puesto <- ifelse(grepl("^guardia", datos$Puesto, ignore.case = TRUE), "guardia", datos$Puesto)

datos$`Estado Civil` <- ifelse(datos$`Estado Civil` == "soltera", "soltero", datos$`Estado Civil`)
datos$`Estado Civil` <- ifelse(datos$`Estado Civil` == "casada", "casado", datos$`Estado Civil`)
datos$`Estado Civil` <- ifelse(datos$`Estado Civil` == "divorciada", "divorciado", datos$`Estado Civil`)
datos$`Estado Civil` <- ifelse(datos$`Estado Civil` == "viuda", "viudo", datos$`Estado Civil`)

datos$Banco <- ifelse(datos$Banco == "santader", "santander", datos$Banco)


# Obtener valores únicos para las columnas especificadas
genero_unicos <- unique(datos$Género)
puesto_unicos <- unique(datos$Puesto)
dpto_unicos <- unique(datos$Dpto)
municipio_unicos <- unique(datos$Municipio)
estado_unicos <- unique(datos$Estado)
estado_civil_unicos <- unique(datos$`Estado Civil`)
bancos_unicos <- unique(datos$Banco)
lugar_nacimiento <- unique(datos$Lugar.de.Nacimiento)

# Imprimir los valores únicos
list(genero_unicos = genero_unicos,
     puesto_unicos = puesto_unicos,
     dpto_unicos = dpto_unicos,
     municipio_unicos = municipio_unicos,
     estado_unicos = estado_unicos,
     estado_civil_unicos = estado_civil_unicos,
     bancos_unicos = bancos_unicos)

# Utilizar summary() para obtener un resumen estadístico de los datos
summary(datos)

# Nombre del archivo original
nombre_archivo_original <- "Datos_FORM_RH_FJ2024.xlsx"

# Construir el nombre del archivo de salida reemplazando la extensión
nombre_archivo_salida <- sub("\\.xlsx$", ".csv", nombre_archivo_original)

# Exportar `datos` a CSV
write.csv(datos, nombre_archivo_salida, row.names = FALSE)

# Cargar las librerías necesarias
library(readxl)
library(dplyr)
library(stringr)
library(knitr)

# Leer el archivo Excel
# Asegúrate de cambiar la ruta del archivo a la ubicación correcta donde tienes tu archivo Excel
datos <- read_excel("/Users/daviddrums180/Tec/Case_Study_Form/databases/form/Datos_FORM_Ventas_FJ2024.xlsx")

# Función para limpiar texto: convertir a minúsculas, quitar puntuación, acentos y espacios extras
clean_text <- function(text) {
  text %>%
    str_to_lower() %>% # Convertir a minúsculas
    str_remove_all("[[:punct:]]") %>% # Quitar puntuación
    # Reemplazar letras acentuadas sin quitar la ñ
    str_replace_all(c("á" = "a", "é" = "e", "í" = "i", "ó" = "o", "ú" = "u", "ü" = "u")) %>%
    str_remove_all("s de rl de cv|sa de cv|sapi de cv|llc|gmbh") %>% # Eliminar frases comunes de nombres de empresas
    str_squish() # Eliminar espacios extras al principio y al final, y reducir espacios duplicados a uno solo
}

# Ajuste de la función de clasificación para trabajar directamente en dplyr
clasificar_producto <- function(nombre_producto) {
  case_when(
    str_detect(nombre_producto, "tarima|pallet|contenedor") ~ "Contenedores y Tarimas",
    str_detect(nombre_producto, "charola|tray") ~ "Componentes de Empaque",
    str_detect(nombre_producto, "caja|box|tote") ~ "Empaques Primarios",
    str_detect(nombre_producto, "dunnage|foam|protector|esquinero|pad") ~ "Materiales de Protección",
    str_detect(nombre_producto, "kit") ~ "Kits de Empaque",
    str_detect(nombre_producto, "tapa|celdado|separador|inserto") ~ "Accesorios de Empaque",
    str_detect(nombre_producto, "glove box|mirror|motor|lens|bezel|assy") ~ "Componentes Específicos",
    TRUE ~ "Otros"
  )
}

# Aplicar la función de limpieza a las columnas de texto
datos <- datos %>%
  mutate(across(where(is.character), clean_text, .names = "{.col}")) 

# Obtener valores únicos para las columnas especificadas
no.clientes <- unique(datos$`No. OC Cliente`)
clientes <- unique(datos$Cliente)
categoria <- unique(datos$`Categoría de producto`)
estado <- unique(datos$Estado)
producto <- unique(datos$Producto)

# Imprimir los valores únicos
print(producto)

# Aplicar la función de clasificación
datos <- datos %>%
  mutate(Tipo = clasificar_producto(Producto))

# Contar productos por categoría
conteo_categorias <- datos %>%
  group_by(Tipo) %>%
  summarise(Conteo = n())

# Imprimir el conteo por categoría
print(conteo_categorias)

# Nombre del archivo original
nombre_archivo_original <- "Datos_FORM_Ventas_FJ2024.xlsx"

# Construir el nombre del archivo de salida reemplazando la extensión
nombre_archivo_salida <- sub("\\.xlsx$", ".csv", nombre_archivo_original)

# Exportar `datos` a CSV
write.csv(datos, nombre_archivo_salida, row.names = FALSE)

######################################
library(lubridate)

# Leer el archivo Excel
# Asegúrate de cambiar la ruta del archivo a la ubicación correcta donde tienes tu archivo Excel
datos <- read_csv("/Users/daviddrums180/Tec/Case_Study_Form/databases/form/Datos_FORM_RH_FJ2024.csv")

# Primero, asegúrate de que las fechas están en formato de fecha si no es así
datos$`Fecha de Alta` <- as.Date(datos$`Fecha de Alta`, format = "%Y-%m-%d")
datos$`Primer Mes` <- as.Date(datos$`Primer Mes`, format = "%Y-%m-%d")

# Imputando los NAs en 'Primer Mes' con los valores de 'Fecha de Alta'
datos$`Primer Mes`[is.na(datos$`Primer Mes`)] <- datos$`Fecha de Alta`[is.na(datos$`Primer Mes`)]

# Asegurar que 'Cuarto Mes' está en formato de fecha
datos$`Cuarto Mes` <- as.Date(datos$`Cuarto Mes`, format = "%Y-%m-%d")

# Sumar 4 meses a 'Primer Mes' donde 'Cuarto Mes' es NA
datos$`Cuarto Mes`[is.na(datos$`Cuarto Mes`)] <- datos$`Primer Mes`[is.na(datos$`Cuarto Mes`)] + months(4)

# Utilizar summary() para obtener un resumen estadístico de los datos
summary(datos)

# Nombre del archivo original
nombre_archivo_original <- "Datos_FORM_RH_FJ2024.xlsx"

# Construir el nombre del archivo de salida reemplazando la extensión
nombre_archivo_salida <- sub("\\.xlsx$", ".csv", nombre_archivo_original)

