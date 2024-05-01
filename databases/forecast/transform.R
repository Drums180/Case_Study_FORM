
##### Librerías
library(dplyr)


#### Bases de Datos
form_ventas_detalle = read_csv("/Users/daviddrums180/Tec/Case_Study_Form/databases/forecast/Datos_FORM_Ventas_FJ2024.csv")
form_ventas_factores = read_csv("/Users/daviddrums180/Tec/Case_Study_Form/databases/forecast/FORM_Factores.csv")

#### Exploración

####### Valores Unicos de Columnas de Texto
columnas_texto <- sapply(form_ventas_detalle, is.character)
valores_unicos <- lapply(form_ventas_detalle[, columnas_texto, drop = FALSE], unique)
valores_unicos

#### Limpieza
# Definir un vector con las sustituciones manuales basadas en lo que hemos visto en tus datos
reemplazos <- c("grupo antolin saltillo" = "Grupo Antolin",
                "grupo antolin" = "Grupo Antolin",
                "yanfeng international automotive technology mexico" = "Yanfeng",
                "yanfeng seating mexico" = "Yanfeng",
                "yanfeng mexico interiors" = "Yanfeng",
                "yanfeng" = "Yanfeng",
                "stabilus" = "Stabilus",
                "iacna mexico v" = "IACNA",
                "iacna" = "IACNA",
                "johnson controls enterprises mexico srl de cv" = "Johnson Controls",
                "antolin interiors mexico" = "Grupo Antolin",
                "draexlmaier components automotive de mexico" = "Draexlmaier",
                "faurecia sistemas automotrices de mexico" = "Faurecia",
                "faurecia" = "Faurecia",
                "avanzar interior products de mexico" = "Avanzar Interior Products",
                "avanzar interior products" = "Avanzar Interior Products",
                "tesla" = "Tesla",
                "aptiv services us" = "Aptiv",
                "aptiv" = "Aptiv",
                "sanhua automotive" = "Sanhua Automotive",
                "denso mexico" = "Denso",
                "tokai rika mexico" = "Tokai Rika",
                "po lighting mexico" = "PO Lighting",
                "hella automotive mexico" = "Hella Automotive",
                "estapack sapi de cv" = "Estapack",
                "ufi filters mexico" = "UFI Filters",
                "inoac polytec de mexico" = "Inoac Polytec",
                "inoac polytec" = "Inoac Polytec",
                "meridian technologies mexico" = "Meridian Technologies",
                "meridian technologies" = "Meridian Technologies",
                "efp operations mexicana" = "EFP Operations",
                "efp operations" = "EFP Operations",
                "draexlmaier" = "Draexlmaier",
                "zkw" = "ZKW",
                "zkw mexico" = "ZKW",
                "itb packaging" = "ITB Packaging",
                "gaim regiomontana" = "Gaim Regiomontana",
                "elringklinger texas" = "ElringKlinger",
                "elringklinger" = "ElringKlinger",
                "agp worldwide operations" = "AGP Worldwide Operations",
                "aislantes y empaques" = "Aislantes y Empaques",
                "isringhausen mexico" = "Isringhausen",
                "katcon" = "Katcon",
                "grupo abc" = "Grupo ABC",
                "denso" = "Denso",
                "tokai rika" = "Tokai Rika",
                "po lighting" = "Po Lighting",
                "hella automotive" = "Hella Automotive",
                "johnson controls" = "Johnson Controls",
                "mitchell plastics" = "Mitchell Plastics",
                "isringhausen" = "Isringhausen",
                "estapack" = "Estapack",
                "ufi filters" = "Ufi Filters",
                "ufi filters" = "Ufi Filters",
                "transporte y automatización de materiales" = "Transporte y Automatización de Materiales",
                "michigan state university" = "Michigan State University",
                "grupo abc de mexico" = "Grupo ABC")

# Función de reemplazo seguro
reemplazar_cliente <- function(nombre) {
  nombre <- tolower(nombre)  # Convertir a minúsculas
  if (nombre %in% names(reemplazos)) {
    return(reemplazos[nombre])
  } else {
    return(nombre)
  }
}

# Aplicar los reemplazos a la columna Cliente
form_ventas_detalle <- form_ventas_detalle %>%
  mutate(Cliente = sapply(Cliente, reemplazar_cliente))

##### Guardar Base de Dat
file_path <- "/Users/daviddrums180/Tec/Case_Study_Form/databases/forecast/Datos_FORM_Ventas_FJ2024.csv"
write_csv(form_ventas_detalle, file_path)

