# DESCRIPCION ----------------------------------------------------------------
# Versión más actual del proceso de extracción de peso
# Para detalle sobre cómo se confeccionó y se evaluó, ver contenido de anotacion_peso/

# Toma como INPUT un objeto llamado 'base_evoluciones' que debe contener
# al menos las columnas: 'id_bt_consulta', 'id_evolucion' y 'evolucion'

# Devuelve como OUTPUT un objeto llamado 'evoluciones_con_valor_peso' que contiene
# un subset de 'base_evoluciones', sólo aquellas con algún valor válido de talla
# con una columna adicional llamada 'peso'
# Puede que haya 2 o más id_evolucion para una misma id_bt_consulta,
# incluso con el mismo texto 'evolucion' y por ende el mismo valor de talla extraido


# SETUP -------------------------------------------------------------------
options(java.parameters = "-Xmx8048m") #Seteamos memoria de Java

library(tidyverse)
library(lubridate)
library(agiseR)



# PROCESO -----------------------------------------------------------------

# INPUT: base_evoluciones con id_consulta, id_evolucion y evolucion

# Filtro evoluciones que tengan mención al peso
paso_1_evoluciones_con_peso <- base_evoluciones %>% 
  # filter(str_detect(evolucion, regex('[0-9]'))) %>%  %>% # en el marco de la ETL ágil, este filtro lo va a realizar previamente PENTAHO
  filter(str_detect(evolucion, regex("kilos|kg|gramos|(\\b|[0-9])gr\\b|pes[aoó]|\\sp\\b|(\\b|[0-9])g\\b|paeg|\\bnacer|\\bnaci.|(P|\\b)PN", TRUE))) 


# Extraigo todos los valores de una evolución que refieran a la talla
paso_2_valores_peso <-   
  as_tibble(str_extract_all(paso_1_evoluciones_con_peso$evolucion,
                            # NO PRECEDIDO POR: "lab" 1-300 caracteres antes
                            # NO PRECEDIDO POR: referencias a cambios de peso y pesos al momento del nacimiento: pn|rnt|control|ganancia|incremento|aument.|baj.|subi.|progreso
                            # PRECEDIDO POR: peso actual, peso
                            # EXTRAER: 1 dígito entre 0 y 9, seguido de 1-5 dígitos, seguido de una coma o un punto, seguido 1-3 dígitos entre 0 y 9
                            # SEGUIDO POR: kg, gr y variantes
                            # NO SEGUIDO POR: do, ero, to
                            regex(ignore_case = T, '(?<!(\\blab).{1,300})(?<!(pn|peso al nacer|rnt|control|ganancia|incremento|aument.|baj.|p[ée]rdida|subi.|progreso|g.{0,3}[0-9]).{1,15})(?<=(peso actual|peso|\\sp)(\\s)?(:|-|\\.)?(\\s){0,3})[0-9]{1,5}([,\\.][0-9]{1,3})?(\\s?(kilos|kilogramos|kg|gs|gr|g|k))?(?!.{0,2}(do|ero|to|-|\\)|\\/\\s?[0-9]))'),
                            simplify = T))

# Uno los valores extraidos a las evoluciones que tenían mención de talla
paso_3_evoluciones_valores_peso <- paso_1_evoluciones_con_peso %>% 
  cbind(paso_2_valores_peso)
rm(paso_1_evoluciones_con_peso, paso_2_valores_peso)


# Extraigo valores atípicos y me quedo con 1 valor por evolución
# Acá voy a tener igual o menos registros que en el paso 1
evoluciones_con_valor_peso_actual <- paso_3_evoluciones_valores_peso %>%  
  # Paso valores a filas
  pivot_longer(cols = starts_with("V"),
               names_to = "peso_regex",
               values_to = "valor",
               values_drop_na = T) %>% 
  # Normalizo unidades
  mutate(unidad = str_extract(valor,  regex(ignore_case=T, 'kilos|kilogramos|kg|gs|gr|g|k')),  #Extraer y Estandarizar unidades
         unidad = str_replace(unidad, regex(ignore_case=T, 'kilos|kilogramos|kg|k'), 'kg'),
         unidad = str_replace(unidad, regex(ignore_case=T, 'gs|gr|\\bg'), 'gr'),
         valor = str_replace(valor, regex(ignore_case=T, 'kilos|kilogramos|kg|gs|gr|g|k'), '')) %>% 
  mutate(unidad = ifelse(is.na(unidad), 'otro', unidad),
         valor_kg = ifelse(unidad == 'gr', str_replace(valor, ',|\\.', ''), valor)) %>% 
  mutate(valor_kg = str_replace(valor_kg, ',', '\\.'),
         valor_kg = as.numeric(valor_kg),
         valor_kg = ifelse(str_detect(valor_kg, '[0-9]{4,6}'), valor_kg/1000, valor_kg),
         valor_kg = ifelse(str_detect(valor_kg, regex('[0-9]{3}')) & unidad == 'kg' & valor_kg > 250, valor_kg/100, valor_kg),
         valor_kg = ifelse(str_detect(valor_kg, regex('\\b[0-9]{3}\\b')) & unidad %in% c('gr', 'otro') & valor_kg > 200, valor_kg/1000, valor_kg)) %>% 
  # Saco valores atípicos y referencias a percentilos y asociados a ecografias de embarazo
  filter(!(valor_kg %in% c(3,10,25,50,75,90,97) &
             str_detect(evolucion, regex("(p|perc(entilo)?.)(3|10|25|50|75|90|97)", TRUE)))) %>%
  filter(valor_kg > 2.5) %>% 
  # Excluir valores bajos para mayores de 2 años
  mutate(valor_kg = ifelse(edad_anios > 2 & valor_kg < 6, NA, valor_kg)) %>% 
  # 1 valor por consulta
  group_by(id_bt_consulta) %>%
  arrange(valor_kg) %>%
  mutate(rank = rank(valor_kg, ties.method = 'average')) %>% #Mantener el valor mas alto que generalmente denota progresion de peso
  filter(rank == max(rank)) %>%
  select(-c(valor, unidad, rank)) %>%
  filter(!is.na(valor_kg)) %>%
  ungroup() %>% 
  distinct() %>% 
  select(id_bt_consulta, edad_anios, id_evolucion, peso_actual = valor_kg) %>% 
  distinct()
# Puede haber más de 1 id_evolucion para una misma id_bt_consulta
# porque tienen el mismo texto de evolucion y, por ende, el mismo valor (empatan en valor priorizado)

rm(paso_3_evoluciones_valores_peso)

# OUTPUT
# Subset de evolucion con valor de peso válido extraído
# id_consulta, id_evolucion, evolucion y peso

