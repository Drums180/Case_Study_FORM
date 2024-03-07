
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
# form
###################################

# Establece el camino hacia la carpeta donde están los archivos
path <- "/Users/daviddrums180/Tec/Case_Study_Form/databases/form"



