
# audiovisuales desde telegram --------------------------------------------

library(tidyverse)
library(jsonlite)


# data --------------------------------------------------------------------

#cargamos y convertimos a tibble
df_audiovisuales <-
  fromJSON("data/result.json", flatten = TRUE) %>%
  as_tibble()
glimpse(df_audiovisuales) #visualiza resultados
glimpse(df_audiovisuales$messages)

# convertimos a tibble los mensajes
df_audiovisuales <- as_tibble(df_audiovisuales$messages) 
glimpse(df_audiovisuales) #visualiza resultados


dat <- fromJSON("data/result.json", flatten = TRUE)

as_tibble(dat$messages$text)

dat <- ldply(dat$messages$text, as.data.frame)


