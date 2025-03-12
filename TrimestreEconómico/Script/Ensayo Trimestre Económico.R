###############################################################################
###############################################################################
####################### ENSAYO TRIMESTRE ECONÓMICO ############################
###############################################################################
###############################################################################

############################### Librerías ####################################
library(pacman)
p_load(tidyverse, ggthemes, data.table)

############################### Datos ########################################

# Definir URL y destino del archivo
url <- "https://media.githubusercontent.com/media/marcomna/ServicioBecario/refs/heads/main/TrimestreEcon%C3%B3mico/Datos/SESNSP/IDM_NM_ene25.csv"
destfile <- "IDM_NM_ene25.csv"

# Descargar el archivo
download.file(url, destfile, mode = "wb")

# Cargar el archivo en un objeto "delitos"
delitos <- fread(destfile, encoding = "Latin-1")

############################# Limpieza #######################################

# Asegurar que las claves sean de tipo carácter (string) con dos dígitos
delitos <- delitos %>%
  mutate(
    Clave_Ent = str_pad(Clave_Ent, width = 2, side = "left", pad = "0"),
    Clave_Mun = str_pad(`Cve. Municipio`, width = 5, side = "left", pad = "0")
  ) %>%
  select(-`Cve. Municipio`)  # Eliminar la columna original

# Reordenar la columna `Clave_Mun` en la posición 4
delitos <- delitos %>%
  relocate(Clave_Mun, .after = 3)

# Filtrar solo violencia familiar
violencia_familiar <- delitos %>% 
  filter(`Tipo de delito` == "Violencia familiar")

# Convertir columnas de meses a formato tidy
violencia_familiar <- violencia_familiar %>% 
  pivot_longer(cols = Enero:Diciembre,
               names_to = "Mes",
               values_to = "Delitos")

# Crear un diccionario de nombres de meses a números
meses_dict <- c(
  "Enero" = "01", "Febrero" = "02", "Marzo" = "03",
  "Abril" = "04", "Mayo" = "05", "Junio" = "06",
  "Julio" = "07", "Agosto" = "08", "Septiembre" = "09",
  "Octubre" = "10", "Noviembre" = "11", "Diciembre" = "12"
)

# Convertir los nombres de los meses a formato numérico y crear fecha
violencia_familiar <- violencia_familiar %>%
  mutate(Mes_Num = meses_dict[Mes],  # Reemplazar nombres por números
         Fecha = paste0(Mes_Num, "-", Año)) %>%  # Formato mm-yyyy
  select(-Mes_Num)  # Eliminar columna auxiliar
