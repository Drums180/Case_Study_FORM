
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
  mutate(across(c(puesto, molestias_puesto, sentimiento_form, genero, estado_civil, municipio, nivel_escolar),
                ~ str_to_lower(.) %>% 
                  iconv(to = 'ASCII//TRANSLIT') %>% 
                  str_replace_all("[^a-z0-9 ]", "") %>% # Conservar solo letras minúsculas, números y espacios
                  str_trim())) # Elimina espacios al inicio y al final

# Suponiendo que tu dataframe se llama form_encuesta
form_encuesta <- form_encuesta %>%
  mutate(sufrido_situaciones_conflicto = ifelse(sufrido_situaciones_conflicto == "Si", 1, 0))

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


# Identificar las columnas que son de tipo caracter
cols_texto <- sapply(form_encuesta, is.character)

# Aplicar la función unique a cada columna de texto y almacenar los resultados en una lista
valores_unicos <- sapply(form_encuesta[, cols_texto, drop = FALSE], unique)

print(valores_unicos)

# Función para categorizar los textos basados en palabras clave
categorizar_molestias <- function(texto) {
  if (str_detect(texto, regex("salario|prestaciones|bonos|dias|sueldo", ignore_case = TRUE))) {
    return("Salario y Prestaciones")
  } else if (str_detect(texto, regex("calor|comedor|micro|espacio|condiciones|equipamiento", ignore_case = TRUE))) {
    return("Condiciones Laborales")
  } else if (str_detect(texto, regex("ambiente|compañeros|trato|comucicacion", ignore_case = TRUE))) {
    return("Ambiente Laboral")
  } else if (str_detect(texto, regex("estres|carga|sobrecarga|urgente", ignore_case = TRUE))) {
    return("Estrés y Carga de Trabajo")
  } else if (str_detect(texto, regex("horario|jornada", ignore_case = TRUE))) {
    return("Horario")
  } else if (str_detect(texto, regex("herramientas|equipos|computadora|transporte", ignore_case = TRUE))) {
    return("Equipamiento y Herramientas")
  } else if (str_detect(texto, regex("transporte", ignore_case = TRUE))) {
    return("Transporte")
  } else {
    return("Otros")
  }
}

# Aplicar la función al dataframe
form_encuesta <- form_encuesta %>%
  mutate(categoria_molestias = sapply(molestias_puesto, categorizar_molestias))

# Función para categorizar los sentimientos
categorizar_sentimientos <- function(texto) {
  if (str_detect(texto, regex("contento|comodo|agradecida|crecimiento|optimista|capaz|realizada", ignore_case = TRUE))) {
    return("Positivo")
  } else if (str_detect(texto, regex("normal|regular|equis|estable", ignore_case = TRUE))) {
    return("Neutro")
  } else if (str_detect(texto, regex("insegura|falta|complicado|pequenos desacuerdos|difícil|problema", ignore_case = TRUE))) {
    return("Negativo")
  } else {
    return("Indefinido")
  }
}

# Aplicar la función al dataframe
form_encuesta <- form_encuesta %>%
  mutate(categoria_sentimiento = sapply(sentimiento_form, categorizar_sentimientos))

# Supongamos que tu dataframe se llama form_encuesta y la columna que quieres transformar es edad
form_encuesta <- form_encuesta %>%
  mutate(edad = as.numeric(gsub("[^0-9]", "", edad)))

# Función genérica para categorizar las respuestas en texto
categorizar_respuestas_texto <- function(x) {
  case_when(
    x %in% c("Totalmente de acuerdo", "Medianamente de acuerdo") ~ "De Acuerdo",
    x == "Ni de acuerdo ni en desacuerdo" ~ "Neutro",
    x %in% c("Medianamente en desacuerdo", "Totalmente en desacuerdo") ~ "Desacuerdo"
  )
}

# Aplicar la función a todas las columnas excepto permanencia_form_futuro
form_encuesta <- form_encuesta %>%
  mutate(across(
    .cols = c(salario_bueno, prestaciones_bueno, jornada_no_excesiva, ofrecimiento_herramientas, 
              no_molestia_temperatura, estres_bajo, facilidad_transporte, zona_trabajo_comoda),
    .fns = categorizar_respuestas_texto
  ))

# Especificación especial para permanencia_form_futuro
form_encuesta$permanencia_form_futuro <- ifelse(
  form_encuesta$permanencia_form_futuro %in% c("Totalmente en desacuerdo", "Medianamente en desacuerdo"),
  1,
  0
)

# Reemplazar NA con "Neutro" en la columna facilidad_transporte
form_encuesta <- form_encuesta %>%
  mutate(facilidad_transporte = ifelse(is.na(facilidad_transporte), "Neutro", facilidad_transporte))


library(writexl)
write_csv(form_encuesta, "/Users/daviddrums180/Tec/Case_Study_Form/databases/form/Encuesta_Datos_FORM_Fall2023.csv")

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

############# PREPARACIÓN MODELADO
# Asegúrate de cambiar la ruta del archivo a la ubicación correcta donde tienes tu archivo Excel
datos <- read_csv("/Users/daviddrums180/Tec/Case_Study_Form/databases/classification/Datos_FORM_RH_FJ2024.csv")


# Convertir la columna 'Fecha de nacimiento' asumiendo que el origen es '1970-01-01'
datos <- datos %>%
  mutate(`Fecha de nacimiento` = as.Date(`Fecha de nacimiento`, origin = "1899-12-30"))

# Obtener valores únicos para las columnas especificadas
genero <- unique(datos$Género)
puestos <- unique(datos$Puesto)
dpto <- unique(datos$Dpto)
municipio <- unique(datos$Municipio)
estado_civil <- unique(datos$`Estado Civil`)

# Imprimir los valores únicos
print(estado_civil)

# Suponiendo que tu data frame se llama datos
datos <- datos %>%
  mutate(municipio = ifelse(municipio == "nuevo leon", "monterrey", municipio))

# Contar NA en las columnas específicas del data frame 'datos'
na_count <- datos %>%
  select(`Fecha de nacimiento`, Género, Puesto, Dpto, SD, Municipio, Estado, `Estado Civil`, Dirección) %>%
  summarise(across(everything(), ~ sum(is.na(.))))

# Imprimir el resultado
print(na_count)

# Lista para almacenar los data frames con registros NA para cada columna
na_records_list <- list()

# Filtrar y almacenar los registros con NA para cada columna
na_records_list$`Fecha de nacimiento` <- datos %>% filter(is.na(`Fecha de nacimiento`))
na_records_list$Género <- datos %>% filter(is.na(Género))
na_records_list$Puesto <- datos %>% filter(is.na(Puesto))
na_records_list$Dpto <- datos %>% filter(is.na(Dpto))
na_records_list$SD <- datos %>% filter(is.na(SD))
na_records_list$Municipio <- datos %>% filter(is.na(Municipio))
na_records_list$Estado <- datos %>% filter(is.na(Estado))
na_records_list$`Estado Civil` <- datos %>% filter(is.na(`Estado Civil`))
na_records_list$Dirección <- datos %>% filter(is.na(Dirección))

# Para ver los registros con NA en una columna específica, por ejemplo, 'Género'
print(na_records_list$Puesto)


# Imputar NA y cambiar valores específicos en la columna 'Puesto'
datos <- datos %>%
  mutate(Puesto = ifelse(is.na(Puesto), "servicio al cliente", Puesto), # Imputar NA con "servicio al cliente"
         Puesto = ifelse(Puesto == "especialista de servicio al cliente", "servicio al cliente", Puesto)) %>%
  mutate(Puesto = case_when(
    Puesto %in% c("inspector de calidad", "calidad") ~ "calidad", # Homologar calidad
    Puesto %in% c("almacenista", "materialista", "materiales") ~ "almacenista", # Homologar roles de almacen
    Puesto %in% c("operador", "montacarguista") ~ "operador", # Homologar operadores
    Puesto %in% c("gestor", "inspector") ~ "gestor", # Homologar gestión y supervisión
    Puesto %in% c("asistente", "auxiliar", "secretario", "mozo", "chofer") ~ "asistente", # Homologar asistentes
    Puesto %in% c("guardia", "seguridad") ~ "seguridad", # Homologar seguridad
    Puesto %in% c("enfermera", "medico") ~ "personal de salud", # Homologar salud
    TRUE ~ Puesto # Mantener los demás puestos como están
  ))

# Función para calcular la moda que maneja empates seleccionando el primero que aparece
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Suponiendo que tu data frame se llama datos
# Calcular la moda de Dpto para cada Puesto
moda_dpto_por_puesto <- datos %>%
  group_by(Puesto) %>%
  summarise(ModaDpto = {
    filtered <- na.omit(Dpto)
    if (length(filtered) > 0) {
      vals <- sort(table(filtered), decreasing = TRUE)
      as.character(names(vals[vals == max(vals)])[1])  # Devuelve el primero en caso de empate
    } else {
      NA_character_
    }
  }, .groups = 'drop')

# Imputar NA en Dpto usando la moda calculada para cada grupo de Puesto
datos <- datos %>%
  left_join(moda_dpto_por_puesto, by = "Puesto") %>%
  mutate(Dpto = ifelse(is.na(Dpto), ModaDpto, Dpto)) %>%
  select(-ModaDpto)  # Eliminar la columna de moda después de imputar

datos <- datos %>%
  mutate(Dpto = ifelse(is.na(Dpto), "calidad", Dpto))

# Contar registros que todavía tienen NA en Dpto
na_count <- sum(is.na(datos$Dpto))

# Imprimir cuántos registros todavía tienen NA en Dpto
print(na_count)

# Función para calcular la moda
get_mode <- function(x) {
  filtered <- na.omit(x)
  if (length(filtered) > 0) {
    vals <- sort(table(filtered), decreasing = TRUE)
    as.character(names(vals[vals == max(vals)])[1])  # Devuelve el primero en caso de empate
  } else {
    NA_character_
  }
}

# Suponiendo que tu data frame se llama datos
# Calcular la moda de Estado para cada Municipio
moda_estado_por_municipio <- datos %>%
  group_by(Municipio) %>%
  summarise(ModaEstado = get_mode(Estado), .groups = 'drop')

# Imputar NA en Estado usando la moda calculada para cada grupo de Municipio
datos <- datos %>%
  left_join(moda_estado_por_municipio, by = "Municipio") %>%
  mutate(Estado = ifelse(is.na(Estado), ModaEstado, Estado)) %>%
  select(-ModaEstado)  # Eliminar la columna de moda después de imputar

# Contar registros que todavía tienen NA en Dpto
na_count <- sum(is.na(datos$Estado))

datos <- datos %>%
  mutate(Municipio = ifelse(is.na(Estado), "guadalupe", Municipio),
         Estado = ifelse(is.na(Estado), "nuevo leon", Estado))

datos <- datos %>%
  mutate(SD = ifelse(is.na(SD), 
                     case_when(
                       Puesto == "servicio al cliente" ~ 8323 / 30,
                       Puesto == "coordinadora" ~ 28528 / 30,
                       Puesto == "ingeniero" ~ 27000 / 30,
                       Puesto == "innovacion + diseno" ~ 52050 / 30,
                       Puesto == "produccion" ~ 76773 / 30,
                       Puesto == "ehs" ~ 16000 / 30,
                       Puesto == "cuentas por pagar" ~ 10000 / 30,
                       Puesto == "asistente" ~ 15053 / 30,
                       Puesto == "contratacion y nominas" ~ 19387 / 30,
                       Puesto == "embarques" ~ 393.84,
                       TRUE ~ SD
                     ), SD))


# Contar NA en las columnas específicas del data frame 'datos'
na_count <- datos %>%
  select(`Fecha de nacimiento`, Género, Puesto, Dpto, SD, Municipio, Estado, `Estado Civil`, Dirección) %>%
  summarise(across(everything(), ~ sum(is.na(.))))

# Imprimir el resultado
print(na_count)

# Ruta del archivo donde se guardará el data frame
file_path <- "/Users/daviddrums180/Tec/Case_Study_Form/databases/classification/Datos_FORM_RH_FJ2024.csv"

# Guardar el data frame 'datos' en la carpeta 'classification' con el nombre especificado
write_csv(datos, file_path)

# Mensaje de confirmación
cat("El archivo ha sido guardado en:", file_path)

# Cargar el paquete readxl
library(readxl)

# Definir la ruta del archivo
file_path <- "/Users/daviddrums180/Tec/Case_Study_Form/databases/form/PREGUNTAS FRECUENTES 1.xlsx"

# Leer la única hoja del archivo Excel
data <- read_excel(file_path)

# Mostrar los primeros registros del data frame
head(data)

# Añadir la columna 'Clave' con palabras clave manualmente seleccionadas
library(dplyr)
data <- data %>%
  mutate(Clave = case_when(
    `Duda` == "¿Qué prestaciones tienen?" ~ "prestaciones, beneficios, seguro, infonavit, aguinaldo, vacaciones, bono, fondo de ahorro",
    `Duda` == "¿Qué papeleria piden?" ~ "papeleria, documentos, acta nacimiento, constancia fiscal, SAT, CURP, IFE, INE, seguro social, comprobante domicilio, comprobante estudio",
    `Duda` == "¿Dónde estan ubicados?" ~ "ubicacion, direccion, calle, Sabinal, Apodaca, referencia, Merco, colonia, Pueblo Nuevo, calle Alemania",
    `Duda` == "¿Tienen transporte? / Rutas de transporte" ~ "transporte, rutas, apoyo, semanal, 350, 527, 222, 109, 105, Transmetro, Romulo Garza",
    `Duda` == "¿Qué hacen? / ¿A que se dedica la empresa?" ~ "empresa, dedicacion, empaque, embalaje, industria automotriz, cajas, carton, plastico, coroplast, racks, almacenaje, dunnage",
    `Duda` == "¿Se necesita experiencia?" ~ "experiencia, ayudantes generales, no pedimos",
    `Duda` == "¿Hacen examen medico? / ¿Qué examenes hacen?" ~ "examen, medico, antidoping, orina, glucosa, presion arterial, prueba embarazo, mujeres, beber agua",
    `Duda` == "¿Es contratación inmediata?" ~ "contratacion, inmediata, proceso, rapido, entrevista, examen medico, respuesta inmediata, referencias",
    `Duda` == "¿Tienen semana desfasada?/ ¿Es semana al corriente?" ~ "semana, nomina, desfasada, corriente, periodo, miercoles, jueves, viernes",
    `Duda` == "¿El sueldo ya es libre de impuestos?" ~ "sueldo, impuestos, base, premios, faltas, retardos",
    `Duda` == "¿Tienen comedor subsidiado?" ~ "comedor, subsidiado, area, break, comida, refrigeradores, microondas",
    `Duda` == "¿Con que tarjeta pagan?" ~ "tarjeta, pago, AFIRME, tramite, banco",
    `Duda` == "¿Es contratación directa con la empresa?" ~ "contratacion, directa, empresa, FORM",
    `Duda` == "¿Cuándo puedo ir a entrevista?" ~ "entrevista, fecha, ir",
    TRUE ~ ""
  ), Referencia = case_when(
    `Duda` == "¿Qué prestaciones tienen?" ~ "Prestaciones",
    `Duda` == "¿Qué papeleria piden?" ~ "Documentos requeridos",
    `Duda` == "¿Dónde estan ubicados?" ~ "Ubicación",
    `Duda` == "¿Tienen transporte? / Rutas de transporte" ~ "Transporte",
    `Duda` == "¿Qué hacen? / ¿A que se dedica la empresa?" ~ "Actividad de la empresa",
    `Duda` == "¿Se necesita experiencia?" ~ "Experiencia requerida",
    `Duda` == "¿Hacen examen medico? / ¿Qué examenes hacen?" ~ "Examen médico",
    `Duda` == "¿Es contratación inmediata?" ~ "Contratación inmediata",
    `Duda` == "¿Tienen semana desfasada?/ ¿Es semana al corriente?" ~ "Semana de nómina",
    `Duda` == "¿El sueldo ya es libre de impuestos?" ~ "Sueldo e impuestos",
    `Duda` == "¿Tienen comedor subsidiado?" ~ "Comedor subsidiado",
    `Duda` == "¿Con que tarjeta pagan?" ~ "Tarjeta de pago",
    `Duda` == "¿Es contratación directa con la empresa?" ~ "Contratación directa",
    `Duda` == "¿Cuándo puedo ir a entrevista?" ~ "Entrevista",
    TRUE ~ "General"
  ))


# Mostrar los primeros registros del data frame actualizado
head(data)

library(readr)
# Ruta del archivo donde se guardará el data frame
file_path <- "/Users/daviddrums180/Tec/Case_Study_Form/databases/chatbot/q&a.csv"

# Guardar el data frame 'datos' en la carpeta 'classification' con el nombre especificado
write_csv(data, file_path)
