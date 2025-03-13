###############################################################################
###############################################################################
####################### ENSAYO TRIMESTRE ECONÓMICO ############################
###############################################################################
###############################################################################

############################### Librerías ####################################
library(pacman)
p_load(tidyverse, ggthemes, data.table, readr, purrr, httr, fs, zip)

############################### ENOE ########################################

# Definir directorio donde están los archivos
dir_path <- "C:/Users/marco/OneDrive/Documents/GitHub/ServicioBecario/TrimestreEconómico/Datos/ENOE"

# Obtener lista de archivos COE1 y COE2
archivos_COE1 <- list.files(dir_path, pattern = "COE1.*\\.csv$", full.names = TRUE)
archivos_COE2 <- list.files(dir_path, pattern = "COE2.*\\.csv$", full.names = TRUE)

# Función para cargar cada archivo y asignarlo como un data frame con su propio nombre
cargar_archivo <- function(archivo) {
  nombre_df <- gsub(".csv", "", basename(archivo))  # Obtener el nombre sin extensión
  nombre_df <- make.names(nombre_df)  # Asegurar que sea un nombre válido en R
  assign(nombre_df, fread(archivo), envir = .GlobalEnv)  # Cargar en el entorno global
  cat("✅ Cargado:", nombre_df, "\n")  # Mensaje de confirmación
}

# Cargar cada archivo COE1  en un data frame independiente
lapply(archivos_COE1, cargar_archivo)



# Obtener los nombres de los data frames COE1 cargados
dfs_COE1 <- ls(pattern = "COE1")

# Extraer las columnas de cada COE1, ignorando `NULL`
columnas_COE1 <- map(dfs_COE1, function(df) {
  if (exists(df) && !is.null(get(df))) {
    colnames(get(df))
  } else {
    NULL
  }
})

# Eliminar elementos NULL
columnas_COE1 <- compact(columnas_COE1)

# Encontrar las columnas comunes en todas las COE1
columnas_comunes_COE1 <- reduce(columnas_COE1, intersect)

# Mostrar resultado
cat("\n🔍 Columnas comunes en todas las COE1:\n")
print(columnas_comunes_COE1)















lapply(archivos_COE2, cargar_archivo)

# Ver los nombres de los data frames creados
ls(pattern = "COE1|COE2")
