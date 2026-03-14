
# ESTRUCTURA --------------------------------------------------------------
# Script que extrae [signo_vital] a partir de evoluciones
# y genera una tabla resultante que servirá como insumo para construir la tabla bt_signos_vitales_via_evolucion

# Levantar evoluciones
# Ejecutar proceso de extracción de valor/es de [signo_vital]
# Transformar para adaptar a la tabla del DW
# Escribir csv



# SETUP -------------------------------------------------------------------
options(java.parameters = "-Xmx8048m") #Seteamos memoria de Java

library(tidyverse)
library(lubridate)
library(agiseR)


# ELIMINO INSTANCIA PREVIA DEL ARCHIVO SI EXISTE --------------------------
path_exporte <- paste0(entorno_agiserver(), "agise/recursos_compartidos/data_warehouse/bt_signos_vitales_via_evolucion/")
path_archivo_talla_actual <- (paste0(path_exporte, "signo_vital_talla_actual.csv"))

# Si el archivo ya existe, eliminarlo
if (file.exists(path_archivo_talla_actual)) {
  file.remove(path_archivo_talla_actual)
}




# EVOLUCIONES ------------------------------------------------------------------
# Levanto evoluciones con dato de
# id_consulta
# id_bt_consulta
# edad_anios
# sk_id_evolucion
# id_evolucion
# evolucion
# id_paciente
# sk_id_paciente
# fecha_hora_consulta

# PENTAHO genera un csv con el set a evaluar
base_evoluciones <- read_delim("/compartida/agise/recursos_compartidos/data_warehouse/bt_signos_vitales_via_evolucion/base_evoluciones.csv",
                               delim=";",
                               show_col_types = FALSE)

names(base_evoluciones) <- tolower(names(base_evoluciones))

# Provisoriamente, consulto directo al DW.
# con <- agiseR::connect_dw_prd()
# fecha_hora_fin_sql <- sql(paste0("TO_DATE('", Sys.Date(), " 00:00:00', 'YYYY-MM-DD HH24:MI:SS')"))
# fecha_hora_inicio_sql <- sql(paste0("TO_DATE('", rollback(Sys.Date()), " 00:00:00', 'YYYY-MM-DD HH24:MI:SS')"))
# base_evoluciones <- consultar_dw("bt_evoluciones") %>% 
#   select(id_bt_consulta, id_evolucion, evolucion) %>% 
#   # Sólo evoluciones cargadas en sesiones del periodo de interés
#   # Me traigo el dato de edad
#   inner_join(consultar_dw("bt_consultas") %>% 
#                filter(hora_consulta > fecha_hora_inicio_sql) %>% #agrego piso para acortar periodo
#                filter(hora_consulta < fecha_hora_fin_sql) %>% 
#                select(id_bt_consulta, 
#                       id_consulta,
#                       hora_consulta,
#                       sk_id_paciente,
#                       id_paciente,
#                       edad_anios)) %>% 
#   collect() %>% 
#   filter(str_detect(evolucion, regex('[0-9]')))


# EXTRACCION DE VALORES -----------------------------------------------------------
# Ejecutar proceso de extracción de valor/es de talla_actual
source("~/agise/proyectos/entidad_peso_talla/proceso_extraccion_talla_actual.R")


# TRANSFORMACION ----------------------------------------------------------
## Transformar para adaptar a la tabla del DW
# Columnas necesarias:
# id_bt_consulta
# id_consulta
# hora_consulta
# sk_id_paciente
# id_paciente
# sk_id_evolucion
# id_evolucion
# descripcion_signo
# valor (número)
# unidad (ej: kg, cm)
# medida_nro (si no hay múltiples valores, ingresar 1)
# id_signo_vital, para talla 1

talla_actual_via_evolucion <- evoluciones_con_valor_talla_actual %>% 
  rename(valor = talla_actual) %>% 
  mutate(descripcion_signo = "talla_actual",
         unidad = "cm",
         medida_nro = 1,
         id_signo_vital = 1) %>% 
  # Sumo variables de sesiones que están en base_evoluciones
  left_join(base_evoluciones %>% 
              select(id_bt_consulta,
                     id_consulta,
                     hora_consulta,
                     sk_id_evolucion,
                     id_evolucion,
                     sk_id_paciente,
                     id_paciente
              ) %>% 
              distinct(),
              by = c("id_bt_consulta", "id_evolucion")) %>% 
  select(id_bt_consulta,
         id_consulta,
         hora_consulta,
         sk_id_paciente,
         id_paciente,
         sk_id_evolucion,
         id_evolucion, 
         descripcion_signo,
         valor,
         unidad,
         medida_nro,
         id_signo_vital
         )
rm(evoluciones_con_valor_talla_actual)


# EXPORTE -----------------------------------------------------------------

options(scipen = 999) # desactivo notacion cientifica

readr::write_delim(talla_actual_via_evolucion, path_archivo_talla_actual, delim = ";")

encolar_archivo_productivo(path_archivo_talla_actual)
enviar_archivo_productivo()

rm(path_exporte, path_archivo_talla_actual)
rm(talla_actual_via_evolucion)


