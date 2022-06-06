
# Turismo ------------------------------


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

library(tidyverse)
library(tidytext)
library(lubridate)




# # Carga y adecuación de la data ------------------------------------------------------

audiovisuales_pp <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSz-kEQVFYwz0My5w1CbSnUm91f_LyF8bXIFll8M1VHo3HWlUfHXaowp82OsbzUxhJsM6U3UPejVa4D/pub?output=csv", 
                             col_types = cols(`Dependencias federales` = col_character(), 
                                              Fecha = col_date(format = "%d/%m/%Y"), 
                                              Horario = col_factor(levels = c("Matutino", 
                                                                              "Vespertino", "Nocturno")), `Marca temporal` = col_datetime(format = "%d/%m/%Y %H:%M:%S"), 
                                              `Nombre del legislador GPMC` = col_character(), 
                                              `Nombre del legislador GPMORENA` = col_character(), 
                                              `Nombre del legislador GPPANAL` = col_character(), 
                                              `Nombre del legislador GPPRD` = col_character(), 
                                              `Nombre del legislador GPPRI` = col_character(), 
                                              `Nombre del legislador GPPT` = col_character(), 
                                              `Nombre del legislador PGPVEM` = col_character(), 
                                              `Nombre del legislador SIN PARTIDO` = col_character(), 
                                              `Nombre del noticiero de internet` = col_character(), 
                                              `Tiempo de duración` = col_time(format = "%H:%M:%S"), 
                                              `Valoración de la nota` = col_factor(levels = c("Positiva", 
                                                                                              "Negativa"))))
View(audiovisuales_pp)


# Procesamiento de la data ------------------------------------------------

audiovisuales_ayer <- audiovisuales_pp %>% 
  filter(Fecha == "2021-06-07") %>% 
  select("Título de la nota", "Fecha", "Horario", "Tiempo de duración", 
         "Tipo de noticiero", "Nombre del noticiero de internet",
         "Nombre del noticiero de radio", "Nombre del noticiero de televisión",
         "Dependencias estatales") %>% 
  rename(dependencias = 'Dependencias estatales')

separate(audiovisuales_ayer, dependencias,
         into = c("dependencia1", "dependencia2", "dependencia3", "dependencia4",
                  "dependencia5", "dependencia6", "dependencia7", "dependencia8",
                  "dependencia9", "dependencia10", "dependencia11", "dependencia12"),
         convert = TRUE)



audiovisuales_turismo <- audiovisuales_pp %>% 
  filter(Fecha == "2021-06-07", 
         `Dependencias estatales` == "SDES") %>% 
  select("Título de la nota", "Fecha", "Horario", "Tiempo de duración", 
         "Tipo de noticiero", "Nombre del noticiero de internet",
         "Nombre del noticiero de radio", "Nombre del noticiero de televisión",
         "Dependencias estatales")


