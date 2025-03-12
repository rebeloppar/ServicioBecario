###############################################################################
###############################################################################
####################### ENSAYO TRIMESTRE ECONÓMICO ############################
###############################################################################
###############################################################################

############################### Librerías ####################################
library(pacman)
p_load(tidyverse, ggthemes, googledrive, data.table)

############################### Datos ########################################

# Definir URL y destino del archivo
url <- "https://media.githubusercontent.com/media/marcomna/ServicioBecario/refs/heads/main/TrimestreEcon%C3%B3mico/Datos/SESNSP/IDM_NM_ene25.csv"
destfile <- "IDM_NM_ene25.csv"

# Descargar el archivo
download.file(url, destfile, mode = "wb")

# Cargar el archivo en un objeto "delitos"
delitos <- fread(destfile, encoding = "Latin-1")

# Asegurar que las claves sean de tipo carácter (string)
delitos[, Clave_Ent := sprintf("%02d", as.integer(Clave_Ent))]

# Modificar la clave del municipio, agregando un 0 solo si tiene un solo dígito
delitos[, Clave_Mun := sprintf("%02d", as.integer(`Cve. Municipio`))]

# Eliminar la columna original
delitos[, `Cve. Municipio` := NULL]

# Reordenar la columna `Clave_Mun` en la posición 4
setcolorder(delitos, c(names(delitos)[1:3], "Clave_Mun", names(delitos)[4:(ncol(delitos)-1)]))

