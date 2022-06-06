# Análisis de notas audiovisuales -----------------------------------------


### Objetivos -------------------------------------------------------------

# Conocer la cantidad total de notas de radio y tv publicadas durante el día


### Librerías

library(googlesheets4) # para lleer la data de googlesheet
library(tidyverse) # Para todo!
library(janitor) # para limpiar los nombres de las columnas
library(tidytext) # Para el análisis del texto
library(wordcloud2) # Nubes de palabras 
library(gt) # Probamos tablas bonitas con esta otra librería
library(gtExtras) # Complemento de librería gt
library(ggthemes) # Para gráficas bonitas
library(gganimate) # Para gráficos animados
library(treemap) # Para el gráfico de cuadritos
library(DT) # para tablas con grandes volumenes de datos
library(xlsx)


# Constantes --------------------------------------------------------------

#para filtrar los datos de acuerdo al periodo de tiempo que quiero analizar
fecha_hoy <- Sys.Date() -1

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



# Carga de los datos ------------------------------------------------------

library(readr)
audiovisuales <- 
  read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTjyGBaMjcbumwObs8PqncIQdHYIQ4EBuBcxqo1MtNDv9Yz62j-nt1Mwz14WAxwrezfiK1IxbVLkVXy/pub?output=csv", 
           col_types = cols(
             'Marca temporal' = col_datetime(format = "%d/%m/%Y %H:%M:%S"), 
             Fecha = col_date(format = "%d/%m/%Y"), 
             Horario = col_factor(levels = c("Matutino", "Vespertino", "Nocturno")), 
             Duración = col_time(format = "%H:%M:%S"),  
             Analista = col_factor(levels = c("Alejandro", "Elisa", "Gael",
                                              "Luisa", "Monserrat", "Braulio",
                                              "Azucena", "Cynthia")), 
             Tipo_noticiero = col_character(), 
             Nivel_gobierno = col_character()))
View(audiovisuales)

# Procesamiento de la data ------------------------------------------------

# normalizamos los nombres de las columnas
audiovisuales <- audiovisuales %>% 
  clean_names()

colnames(audiovisuales)

# dependiendo de las fechas que quiera analizar, se hace el filtro correspondiente
audio_hoy <- audiovisuales %>% 
  filter(fecha == fecha_hoy) %>%
  select("marca_temporal", "titular", "enlace", "horario", "duracion", 
         "tipo_noticiero", "noticiero", "nivel_gobierno", "organizacion",
         "tendencia")
# guardamos los datos en un documento de excel
write.xlsx(audio_hoy, "exports/radio-tv_analisis_totales.xlsx")

## limpieza
audio_hoy<- audio_hoy %>% 
  mutate(nivel_gobierno, nivel_gobierno = limpiar_texto(texto = nivel_gobierno),
         organizacion, organizacion = limpiar_texto(texto = organizacion),
         tendencia, tendencia = limpiar_texto(texto = tendencia))

# Analizando nuestros datos -----------------------------------------------

# Contamos las notas totales de los medios
audio_hoy %>% 
  count(noticiero, sort = TRUE)

# Seleccionamos la info
total_audio <- audio_hoy %>% 
  group_by(noticiero) %>%
  count(noticiero, total = n(), sort = TRUE) %>% 
  arrange(desc(n)) %>% 
  ungroup()

# Análisis por bigramas ----------------------------------------------------

# Toekenizamos los titulares en palabras y generamos un id
audio_bigramas <- audio_hoy %>% 
  mutate(id = row_number()) %>% 
  unnest_tokens(bigram, titular, token = "ngrams", n = 2)
audio_bigramas

# Contamos los bigramas
audio_bigramas %>% 
  count(bigram, sort = TRUE)

# Necesitamos limpiar las stopword, para ello, separamos los bigramas
audio_bigramas_separados <- audio_bigramas %>% 
  separate(bigram, c("palabra1", "palabra2"), sep = " ")
head(audio_bigramas_separados)

# Filtrado de los bigramas que contienen alguna stopword
audio_bigramas_separados <- audio_bigramas_separados  %>%
  filter(!palabra1 %in% tm::stopwords(kind="es")) %>%
  filter(!palabra2 %in% tm::stopwords(kind="es"))

# Unión de las palabras para formar de nuevo los bigramas
audio_bigramas <- audio_bigramas_separados %>%
  unite(bigram, palabra1, palabra2, sep = " ")

# Contamos nuevamente los bigramas
audio_bigramas %>% 
  count(bigram, sort = TRUE)

# Frecuencia de términos (tf-idf)
audio_bigramas_tf_idf <- audio_bigramas %>% 
  count(noticiero, id, bigram) %>% 
  bind_tf_idf(bigram, id, n) %>% 
  arrange(-tf_idf)
audio_bigramas_tf_idf

# Graficamos los bigramas
audio_bigramas_tf_idf %>%
  filter(!near(tf, 1)) %>%
  filter(noticiero %in% noticiero) %>%
  group_by(noticiero) %>%
  slice_max(tf_idf, n = 5, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(bigram = factor(bigram, levels = rev(unique(bigram)))) %>%
  ggplot(aes(tf_idf, bigram, fill =noticiero)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ noticiero, ncol = 2, scales = "free")

# nube de palabras
audio_bigramas %>% 
  select(bigram) %>%
  count(bigram, sort = TRUE) %>%
  wordcloud2(audio_bigramas, size = 1.5)

# tabla
datatable(audio_hoy,
          extensions = 'FixedColumns',
          rownames= TRUE,
          filter = 'top',
          options = list(
            pageLength = 10,
            language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
            autoWidth = TRUE),
          
)











### Totales

## Resumen 
# Durante el día de **hoy y hasta las 3 pm** se encontró que en los distintos 
# medios de comunicación monitoreados por Punto Preciso **se publicaron un total 
# de `r nrow(audio_hoy)` noticias**. A continuación el desglose.


### 1. Noticias totales con relación al turismo en el estado.
# total turismo

# tablas con DT --------------------------------------------------

# tabla tendencias
# filtro la informacion sobre las tendencias
tendencia_hoy <- audio_hoy %>% 
  filter(!is.na(tendencia)) %>%
  select(tendencia, duracion) %>% 
  group_by(tendencia) %>%
  count() %>% 
  ungroup()


# la grafica
tendencia_hoy %>% 
  ggplot(aes(x = duracion, y = reorder(tendencia, duracion)), label = duracion)+
  geom_col()+
  geom_label(aes(label = duracion))+
  labs(title = "tendencias en radio y tv",
       subtitle = fecha_hoy)

  
# la tabla con gt
datatable(tendencia_hoy)




# Tabla general

datatable(audio_hoy,
          extensions = 'FixedColumns',
          rownames= TRUE,
          filter = 'top',
          options = list(
            pageLength = 10,
            language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
            autoWidth = TRUE),
          
)




