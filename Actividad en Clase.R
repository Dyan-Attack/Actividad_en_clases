# Actividad en Clases

## Se cargan las bases de datos que se ocuparán
library(tidyverse) # Para ocupar las funciones select; filter; group_by; summarize; mutate
library(janitor)# Para ocupar las funciones remove_empty
library(lubridate) # Para ocupar la función parse_date_time

# 0. Crear un objeto a partir de la lectura de las 2 bbdd proporcionadas
# para la actividad
# Fijarse en la ruta, en este caso ocupé la de mi pc, 
#   donde tengo una carpeta donde está el script (clase 8)
# Dentro de esta carpeta tengo una subcarpeta llamada data
#   donde se encuentran ambas bases de datos (Sea_projects y detalle_tipologia)
SEA <- read.csv("data/SEA_projects.csv") 
tipologia <- read.csv("data/detalle_tipologia.csv")

#1. Remover las columnas que tienen solo argumentos NA (no hay) con remove_empty
# De la librería "janitor"
# Borrar la columna x1 del data frame "tipologia" ocupando select

SEA <- SEA %>% 
  remove_empty(which = c("rows", "cols"))
tipologia <- tipologia %>% 
  select(-X)


#2. Renombrar las variables con la función select de dplyr del paquete tidyverse

SEA <- SEA %>%
 select(
    proyecto = name,
    tipo = type,
    region = region,
    empresa = owner,
    cod_tipologia = typology,
    inversion = investment,
    fecha_date = entry_date,
    estado = state,
    fecha_calificacion = qualification_date,
  )

#3. Confirmar que las variables están en el formato correcto

glimpse(SEA)

# La columna fecha_date no está en formato fecha
# Corregir fechas con la función mutate del paquete tidyverse y parse_date_time
#   de la librería "lubridate"

SEA <- SEA %>%
  mutate(fecha_date = parse_date_time(fecha_date, c("dmy", "ymd"))) %>% 
  mutate(fecha_calificacion = parse_date_time(fecha_calificacion, c("dmy", "ymd")))

#Revisar nuevamente a través de la función glimpse
glimpse(SEA)

# Revisar consistencia en la variable región

unique(SEA$region)

# Cambiar el 5 por Quinta y dejar las otras regiones tal cual a través de TRUE
# Si no se pone TRUE, solo cambia 5 y borra todo lo demás
# Al parecer TRUE significa todo lo demás (porque existe y eso sería TRUE)

SEA <- SEA %>% 
  mutate(region = case_when(region == "5" ~ "Quinta", TRUE ~ region))

#4.  Crear una nueva columna llamada "estado_cat"
# Se hace mutate creando una columna estado_cat del dataframe SEA
# Se ocupa el parámetro case_when para que aplique solamenteº cambios en
# El estado_cat == "En Admisión" | #En Calificación se llaman ~"En Proceso" 
#   (se ocupa | para unirlos dentro del mismo argumento)
# El estado_cat == "Aprobado" se llamará "Aprobado"
# El estado_cat "Rechazado se llamará "Rechazado"
# Los demás estados_cat (TRUE) se llamarán "Otro"
SEA <- SEA %>% 
  mutate(estado_cat = case_when(estado == "En Admisión" | estado == "En Calificación" ~ "En proceso",
                                estado == "Aprobado" ~ "Aprobado",
                                estado == "Rechazado" ~ "Rechazado",
                                TRUE ~ "Otro"))

# 5. Filtrar en qué estado se encuentran los proyectos ingresados el 
#   Se escoge el dataframe
#   Se filtra el año primero con filter
# Luego se agrupa el estado_cat que indica el estado del proyecto con group_by
# Luego se cuenta con count

SEA %>% 
  filter(year(fecha_date) == 2015) %>% 
  group_by(estado_cat) %>% 
  count()

# 6. Realizar un full join entre las bases SEA y Tipología
# Como no tienen una columna en común, se ocupa el argumento by
# Se le indica que "typology" = a "cod_tipo"

base <- full_join(SEA, tipologia, by = c("cod_tipologia" = "cod_tipo"))

# 7. Realizar un resumen de la nueva base e indicar en promedio cuántos millones
# de dólares se invierten por proyecto (Mean)

summary(base)

# 8. Realizar un análisis de los NA de la variable fecha_calificación
# Segregar por distintos estados de los proyectos estado_cat
# Hacer un resumen de la columna para ver cuántos NA hay

base %>% 
  select(fecha_calificacion) %>% 
  summary()

# Luego de eso, hacer un filtro para ver los NA de la columna fecha_calificacion
# Agruparlas por estado de categorías
# Contar con la función count

base %>% 
  filter(is.na(fecha_calificacion)) %>% 
  group_by(estado_cat) %>% 
  count()


# 9. Calcular el número total de proyectos de cada empresa con group_by(empresa)
# y el monto total invertido a través de la función summarise
#   Hacer una columna llamada Mto_inversion que es la suma de 
#   las columnas inversión por cada empresa agrupada (total empresa)
# Ordenar por monto invertido a través de la función arrange (-Mto_Inversión)
# Se ocupa "-" para que sea descendente (De mayor a menor)

base %>% 
  group_by(empresa) %>% 
  summarise(Mto_inversion = sum(inversion),
            Proyectos = length(proyecto)) %>% 
  arrange(-Mto_inversion)

# 10. Filtrar los proyectos desde 2015 en adelante y hacer un gráfico
#   Ocupar ggplot y el argumento aes para definir el eje X a partir de la columna
#   Inversión
# Hacer un gráfico de cajas con la función geom_boxplot


base %>% 
  filter(fecha_date > "2015-01-01") %>% 
  ggplot(aes(x = inversion)) +
  geom_boxplot()


# Hacer el mismo procedimiento pero agregando un filtro al principio
# para filtrar los proyectos cuya inversión sea menor a 30 millones de dólares
base %>% 
  filter(fecha_date > "2015-01-01",
         inversion < 30) %>% 
  ggplot(aes(x = inversion)) +
  geom_boxplot()
  
