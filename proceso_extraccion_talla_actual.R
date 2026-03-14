# DESCRIPCION ----------------------------------------------------------------
# Versión más actual del proceso de extracción de talla actual (al momento de la consulta)
# Para detalle sobre cómo se confeccionó y se evaluó, ver contenido de anotacion_talla/

# Toma como IMPUT un objeto llamado 'base_evoluciones' que debe contener
# al menos las columnas: 'id_bt_consulta', 'id_evolucion' y 'evolucion'

# Devuelve como OUTPUT un objeto llamado 'evoluciones_con_valor_talla' que contiene
# un subset de 'base_evoluciones', sólo aquellas con algún valor válido de talla
# con una columna adicional llamada 'talla_actual'
# Puede que haya 2 o más id_evolucion para una misma id_bt_consulta,
# incluso con el mismo texto 'evolucion' y por ende el mismo valor de talla extraido


# SETUP -------------------------------------------------------------------
options(java.parameters = "-Xmx8048m") #Seteamos memoria de Java

library(tidyverse)
library(lubridate)
library(agiseR)



# PROCESO -----------------------------------------------------------------

# INPUT: base_evoluciones con id_consulta, id_evolucion y evolucion

# Filtro evoluciones que tengan mención a la talla
paso_1_evoluciones_con_talla <- base_evoluciones %>% 
  # filter(str_detect(evolucion, regex('[0-9]'))) %>% # en el marco de la ETL ágil, este filtro lo va a realizar previamente PENTAHO 
  filter(str_detect(evolucion, regex(ignore_case = T, 'talla|altura|\\bt\\b|mide|cm|\\bm\\b|\\mts|metros'))) 


# Extraigo todos los valores de una evolución que refieran a la talla
paso_2_valores_talla <-   
  as_tibble(str_extract_all(paso_1_evoluciones_con_talla$evolucion,
                            # PRECEDIDO POR: talla/altura/etc, seguido de 0-2 espacios, seguido de 0-3 espacios
                            # NO PRECEDIDO POR: palabras varias (cadera, cintura, prot, nac, PN, mama, papa),
                            # NO PRECEDIDO POR: el signo "-" o "troponina t" o "dm T" o "onda T" o "testosterona t", seguido de 0-1 espacios
                            # EXTRAER: 1 dígito entre 0 y 2, seguido de 1-2 dígitos, seguido de 1 espacios o no, seguido de signos o un espacio, word boundary,seguido de 1-2 dígitos o no
                            # NO SEGUIDO POR: grados/%
                            regex(ignore_case = T, '(?<=(\\btalla|altura|\\bt\\b)(\\s){0,2}(:|-|\\.)?(\\s){0,3})(?<!(\\/|\\bco|ca.da|\\bpsa|\\b(bi|bb|b)|cintura|prot|nac|PN|paeg|rnt|\\bpapa\\b|\\bmama\\b).{0,20})(?<!(\\-|troponina t\\b|dm t\\b|onda t\\b|testosterona t\\b).{0,1})\\b[0-2]?[0-9]{1,2}\\b(\\s)?([,\\.;](\\s)?[0-9]{1,3})?(?!.?(grados|%|\\bc\\b))'),
                            simplify = T))

# Uno los valores extraidos a las evoluciones que tenían mención de talla
paso_3_evoluciones_valores_talla <- paso_1_evoluciones_con_talla %>% 
  cbind(paso_2_valores_talla)
rm(paso_1_evoluciones_con_talla, paso_2_valores_talla)


# Extraigo valores atípicos y me quedo con 1 valor por evolución
# Acá voy a tener igual o menos registros que en el paso 1
evoluciones_con_valor_talla_actual <- paso_3_evoluciones_valores_talla %>%  
  # Paso valores a filas
  pivot_longer(cols = starts_with("V"),
               names_to = "peso_regex",
               values_to = "valor",
               values_drop_na = T) %>% 
  # Elimino valores ausentes y extremos, normalizo unidades
  mutate(valor = str_replace_all(valor, '\\s', ''), #elimino espacios en blanco
         valor = as.numeric(str_replace(valor, ',|;', '\\.'))) %>% #transformo comas a puntos
  filter(valor > 44.9 | valor < 2.1) %>% #excluyo valores extremos
  mutate(valor = ifelse(valor < 2.1, valor*100, valor)) %>% #paso m a cm
  filter(valor < 210 & valor > 44.9) %>% #excluyo valores extremos post normalizacion
  # Me quedo con el mayor valor de cada evolución
  group_by(id_bt_consulta) %>%
  arrange(valor) %>%
  mutate(rank = rank(valor, ties.method = 'average')) %>%
  filter(rank == max(rank)) %>%
  select(-c(peso_regex, rank)) %>%
  ungroup() %>% 
  distinct() %>% 
  select(id_bt_consulta, id_evolucion, talla_actual = valor) %>% 
  distinct()
# Puede haber más de 1 id_evolucion para una misma id_bt_consulta
# porque tienen el mismo texto de evolucion y, por ende, el mismo valor (empatan en valor priorizado)

rm(paso_3_evoluciones_valores_talla)

# OUTPUT
# Subset de evolucion con valor de talla válido extraído
# id_consulta, id_evolucion y talla_actual

