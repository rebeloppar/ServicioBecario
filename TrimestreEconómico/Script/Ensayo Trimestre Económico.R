###############################################################################
###############################################################################
####################### ENSAYO TRIMESTRE ECONÓMICO ############################
###############################################################################
###############################################################################

############################### Librerías ####################################
library(pacman)
p_load(tidyverse, ggthemes, data.table)

############################### Delitos ########################################

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


############################### Censo ########################################
options(timeout = 300)  # Aumenta el tiempo de espera a 5 minutos (300 segundos)

# Definir URL y destino del archivo ZIP
url <- "https://www.inegi.org.mx/contenidos/programas/ccpv/2020/microdatos/iter/ITER_NAL_2020_csv.zip"
destfile <- "ITER_NAL_2020_csv.zip"
unzip_dir <- "iter_datos"  # Carpeta donde se extraerán los archivos

# Descargar el archivo ZIP
download.file(url, destfile, mode = "wb")

# Crear la carpeta si no existe y descomprimir
if (!dir.exists(unzip_dir)) dir.create(unzip_dir)
unzip(destfile, exdir = unzip_dir)

# Listar archivos extraídos para identificar el CSV correcto
files <- list.files(unzip_dir, full.names = TRUE, pattern = "\\.csv$")

# Verificar qué archivos están en la carpeta
print(files)

############################### Censo ########################################
options(timeout = 300)  # Aumenta el tiempo de espera a 5 minutos (300 segundos)

# Definir URL y destino del archivo ZIP
url <- "https://www.inegi.org.mx/contenidos/programas/ccpv/2020/microdatos/iter/ITER_NAL_2020_csv.zip"
destfile <- "ITER_NAL_2020_csv.zip"
unzip_dir <- "iter_datos"  # Carpeta donde se extraerán los archivos

# Descargar el archivo ZIP
download.file(url, destfile, mode = "wb")

# Crear la carpeta si no existe y descomprimir
if (!dir.exists(unzip_dir)) dir.create(unzip_dir)
unzip(destfile, exdir = unzip_dir)

# Listar archivos extraídos para identificar el CSV correcto
files <- list.files(unzip_dir, full.names = TRUE, pattern = "\\.csv$")

# Verificar qué archivos están en la carpeta
print(files)

# Leer el archivo en formato de texto crudo con ISO-8859-1
raw_lines <- readLines(files[1], encoding = "ISO-8859-1")

# Guardar el archivo en UTF-8 para que fread lo lea bien
writeLines(raw_lines, "temp_utf8.csv", useBytes = TRUE)

# Ahora leer con fread en UTF-8
censo_iter <- fread("temp_utf8.csv", encoding = "UTF-8")

# Corregir claves de entidad y municipio
censo_iter <- censo_iter %>%
  mutate(
    ENTIDAD = str_pad(ENTIDAD, width = 2, side = "left", pad = "0"),
    MUN = str_pad(MUN, width = 3, side = "left", pad = "0"))

# Mostrar los valores únicos que comienzan con "Total"
total_municipios <- censo_iter %>%
  mutate(NOM_MUN = str_trim(NOM_MUN)) %>%  # Eliminar espacios innecesarios
  filter(str_detect(NOM_MUN, "^Total")) %>%  # Buscar "Total" al inicio con regex
  distinct(NOM_MUN)

# Filtrar eliminando registros con nombres que inician con "Total"
censo_iter <- censo_iter %>%
  mutate(NOM_MUN = str_trim(str_squish(NOM_MUN))) %>%  # Quita espacios extra
  filter(!str_detect(NOM_MUN, regex("^Total", ignore_case = TRUE)))  # Buscar sin importar mayúsculas

# Eliminar "Total del Municipio" asegurando limpieza de caracteres ocultos
censo_iter <- censo_iter %>%
  mutate(NOM_LOC = str_trim(str_squish(NOM_LOC))) %>%  # Elimina espacios extra
  filter(NOM_LOC != "Total del Municipio")  # Elimina la categoría específica

# Sumar toda la población total (POBTOT)
censo_iter %>%
  summarise(Total_Poblacion = sum(POBTOT, na.rm = TRUE))

# Reemplazar "*" por NA solo en columnas de tipo character
censo_iter <- censo_iter %>%
  mutate(across(where(is.character), ~ na_if(.x, "*")))

# Colapsar sumando POBTOT por MUN y mantener columnas clave
censo_municipal <- censo_iter %>%
  group_by(ENTIDAD, NOM_ENT, MUN, NOM_MUN) %>%  # Agrupar por estas columnas
  summarise(POBTOT = sum(POBTOT, na.rm = TRUE), .groups = "drop")  # Sumar POBTOT

# Nueva columna
censo_municipal <- censo_municipal %>%
  mutate(Clave_Mun = paste0(ENTIDAD, MUN))  # Concatenar ENTIDAD y MUN

# Unir bases por "Clave_Mun"
violencia_familiar <- violencia_familiar %>%
  left_join(censo_municipal %>% select(Clave_Mun, POBTOT), by = "Clave_Mun")

# Incidencia delictiva
violencia_familiar <- violencia_familiar %>%
  mutate(Incidencia = (Delitos / POBTOT) * 100000)

# Reordenar y seleccionar columnas
violencia_familiar <- violencia_familiar %>%
  select(Fecha, Clave_Ent, Entidad, Clave_Mun, Municipio, Delitos, Incidencia, 
         `Bien jurídico afectado`, `Tipo de delito`, `Subtipo de delito`, Modalidad)

# Filtrar fechas
violencia_familiar <- violencia_familiar %>%
  filter(as.Date(paste0("01-", Fecha), format = "%d-%m-%Y") < as.Date("2025-02-01"))
