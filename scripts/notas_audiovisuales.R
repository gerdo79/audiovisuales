
# Intento de análisis de notas audiovisuales ------------------------------


# # Objetivos -------------------------------------------------------------

# Conocer la cantidad de noticieros que se monitorean
# Conocer la cantidad total de notas
# COnocer la cantidad de notas por horario
# Conocer la cantidad de notas por medio de comunicación
# Conocer la cantidad de notas por temática
# Conocer el tiempo que se dedica a cada temática
# Conocer la cantidad de notas por tendencia
# Conocer el tiempo que se dedica a cada tendencia
# Conocer la cantidad de notas electorales
# Conocer el tiempo que se dedica a las notas electorales


# Librerías ---------------------------------------------------------------

library(tidyverse) # Para todo!
library(stringr) # Transformación y manejo de texto
library(lubridate) # para trabajar con fechas
library(tidytext) # Para el análisis del texto
library(DataEditR) # Para editar los datos de la tabla
library(kableExtra) # Para tablas bonitas
library(ggthemes) # Para gráficas bonitas
library(gganimate) # Para gráficos animados
library(treemap) # Para el gráfico de cuadritos
library(ggalluvial)




# # Carga y adecuación de la data ------------------------------------------------------

library(readr)
audiovisuales <- 
  read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTjyGBaMjcbumwObs8PqncIQdHYIQ4EBuBcxqo1MtNDv9Yz62j-nt1Mwz14WAxwrezfiK1IxbVLkVXy/pub?output=csv", 
           col_types = cols(
             `Marca temporal` = col_datetime(format = "%d/%m/%Y %H:%M:%S"), 
             Fecha = col_date(format = "%d/%m/%Y"), 
             Horario = col_factor(levels = c("Matutino", "Vespertino", "Nocturno")), 
             Duración = col_time(format = "%H:%M:%S"),  
             Analista = col_factor(levels = c("Alejandro", "Elisa", "Gael",
                                              "Luisa", "Monserrat")), 
             Tipo_noticiero = col_factor(levels = c("Internet", "Radio", 
                                                    "Televisión")), 
             Nivel_gobierno = col_character()))
View(audiovisuales)

library(readr)
audiovisuales <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTjyGBaMjcbumwObs8PqncIQdHYIQ4EBuBcxqo1MtNDv9Yz62j-nt1Mwz14WAxwrezfiK1IxbVLkVXy/pub?output=csv")
View(audiovisuales)

glimpse(audiovisuales)

# Procesamiento de la data ------------------------------------------------

audiovisuales <- data_edit(x= audiovisuales)

# Contar número de nulos por columna
summarise_all(audiovisuales, funs(sum(is.na(.))))

#Elimino las filas que tienen valores nulos
audiovisuales <- audiovisuales [!is.na(audiovisuales$`Marca temporal`),]
audiovisuales <- audiovisuales [!is.na(audiovisuales$Fecha),]
audiovisuales <- audiovisuales [!is.na(audiovisuales$Titular),]
audiovisuales <- audiovisuales [!is.na(audiovisuales$Enlace),]
audiovisuales <- audiovisuales [!is.na(audiovisuales$Horario),]
audiovisuales <- audiovisuales [!is.na(audiovisuales$Duración),]
audiovisuales <- audiovisuales [!is.na(audiovisuales$Tipo_noticiero),]
audiovisuales <- audiovisuales [!is.na(audiovisuales$Noticiero),]
audiovisuales <- audiovisuales [!is.na(audiovisuales$Nivel_gobierno),]
audiovisuales <- audiovisuales [!is.na(audiovisuales$Organización),]

#separo la info que se captura en una sola columna
audiovisuales <- audiovisuales %>% 
  unnest_tokens(Nivel_gobierno, Nivel_gobierno, token = "regex", pattern = ",") %>% 
  unnest_tokens(Organización, Organización, token = "regex", pattern = ",") %>% 
  unnest_tokens(Tendencia, Tendencia, token = "regex", pattern = ",")

# dependiendo de las fechas que quiera analizar, se hace el filtro correspondiente
audio_ayer <- audiovisuales %>% 
  filter(Fecha >= "2021-07-23" & Fecha <= "2021-07-24")

# La limpieza en esta caso la voy a limitar a remover acentos y pasar a
# minúsculas todas las letras 

limpiar_texto <- function(texto){
  # El orden de la limpieza no es arbitrario
  # Se convierte todo el texto a minúsculas
  nuevo_texto <- str_to_lower(texto)
  # Eliminación de signos de puntuación
  #nuevo_texto <- str_replace_all(nuevo_texto,"[[:punct:]]", " ")
  # Eliminación de acentos
  nuevo_texto <- chartr("áéíóú", "aeiou", nuevo_texto)
  #nuevo_texto <- str_replace_all(nuevo_texto,"[áéíóú]", "[aeiou]")
  # Eliminación de espacios en blanco múltiples
  nuevo_texto <- str_replace_all(nuevo_texto,"[\\s]+", " ")
  return(nuevo_texto)
}

audio_ayer<- audio_ayer %>% 
  mutate(Nivel_gobierno, Nivel_gobierno = limpiar_texto(texto = Nivel_gobierno),
         Organización, Organización = limpiar_texto(texto = Organización),
         Tendencia, Tendencia = limpiar_texto(texto = Tendencia))


# Selección de noticias de la organización (Ayudó pero ya no es necesaria-----

# definimos el conjunto de palabras que se detectarán - en este caso de turismo
sectur <- c("SECTUR", "sectur", "Sectur", "Turismo", "turismo",
            "secretaría de turismo", "Secretaría de turismo")
# junta las variaciones de la organización en un nuevo data set 
# separados por una  barra vertical
coincidencia_sectur <- str_c(sectur, collapse = "|")
coincidencia_sectur

# Filtramos las frases  que tengan una determinada variación de la organización
tiene_sectur <- str_subset(audio_ayer$Organización, coincidencia_sectur)
tiene_sectur <- as_tibble(tiene_sectur)

# Extraemos las frases  que tengan una determinada variación de la organización
tiene_sectur <- str_extract(audio_ayer$Organización, coincidencia_sectur)
tiene_sectur <- as.character(tiene_sectur)
tiene_sectur <- as_tibble(tiene_sectur)
head(tiene_sectur)

# Unimos los data frames en uno
audio_ayer_sectur <- merge(audio_ayer, tiene_sectur,
                        by = 0, all = TRUE)

# renombramos la columna 
audio_ayer_sectur <- rename(audio_ayer_sectur, sectur = value)


# Exploración de la data --------------------------------------------------

# Me quedo solo con los registros que son de sectur
audio_ayer_sectur <- audio_ayer_sectur %>% 
  filter(sectur == "sectur")

# Hago una tabla bonita
audio_ayer_sectur %>% 
  select(c(Fecha, Titular, Enlace, Horario, Duración, Tipo_noticiero,
           Noticiero)) %>% 
  kable(escape = F, align = "l") %>% 
  kable_minimal(bootstrap_options = "striped", 
                full_width = T, position = "left",font_size = 14) %>%
  collapse_rows(columns = c(1), valign = "middle")

# hago una gráfica 







