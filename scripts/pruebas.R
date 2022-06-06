

# Librerías ---------------------------------------------------------------

library(tidyverse) # Para todo!
library(stringr) # Transformación y manejo de texto
library(tidytext) # Para el análisis del texto
library(kableExtra) # Para tablas bonitas
library(gt)
library(gtExtras)
library(flextable)
library(flexpivot)
library(ggthemes) # Para gráficas bonitas
library(gganimate) # Para gráficos animados
library(treemap) # Para el gráfico de cuadritos
library(ggalluvial) # Para el gráfico alluvial
library(circlize) # Para gráficas de cuerda
library(chorddiag)


# Separar organizaciones --------------------------------------------------

#ejemplo
my_df = data.frame(id=1:2, urls=c("url.a, url.b, url.c",
                                  "url.d, url.e, url.f"))
tidytext::unnest_tokens(my_df, out, urls, token = "regex", pattern=",")
my_df

#mis datos
audio_semanal <- audiovisuales

audio_semanal <- audio_semanal %>% 
  unnest_tokens(Nivel_gobierno, Nivel_gobierno, token = "regex", pattern = ",") %>% 
  unnest_tokens(Organización, Organización, token = "regex", pattern = ",") %>% 
  unnest_tokens(Tendecnia, Tendencia, token = "regex", pattern = ",")


#creación de id
coca_df <- mutate(coca_df, id = c(1:nrow(coca_df)))

# buscar y encontrar palabras específicas ---------------------------------

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

# peeeeeeero... tenemos una forma más fácil
tiene_sectur <- audio_ayer %>% 
  filter(Organización == "sectur")
# y ya



# paste0 ------------------------------------------------------------------

audiovisuales2 <- audiovisuales %>%
  na.omit(Enlace) %>% 
  mutate(vinculo = paste0(c("[enlace(]", Enlace, ")", sep = "")))

# Transición gganimate ----------------------------------------------------

#Filtramos la fecha de las notas que quiero mostrar
audio_ds <- audiovisuales %>% 
  filter(Fecha >= "2021-07-12" & Fecha <= "2021-10-25")

total_notas_horario <- audio_ds %>%
  group_by(Analista, Fecha, Horario) %>%
  summarise(n = n()) %>%
  ggplot(aes(Horario, Analista, fill = n))+
  geom_tile(aes(group = Horario))+
  facet_grid(~Fecha) + 
  scale_fill_gradient(low = "white", high = "steelblue")+
  geom_text(aes(label = n))+
  labs(title = "Total de notas revisadas por los analistas divididas por horario",
       subtitle = paste0(nrow(audio_ds)," notas totales"),
       x = "Fecha",
       y = "Total de notas",
       caption = "\n@Data recolectada a través del formulario de captura de Punto Preciso")+
  theme_hc()+
  theme(axis.text.x=element_text(angle=90, hjust=1))
total_notas_horario


audio_semanal %>% 
  select(c(Fecha, Titular, Enlace, Horario, Duración, Tipo_noticiero,
           Noticiero)) %>%
  group_by(Tipo_noticiero) %>% 
  count(sort = TRUE) %>% 
  ungroup() %>% 
  kable() %>% 
  kable_styling(bootstrap_options = "responsive", full_width = 320, 
                font_size = 9)

total_notas_noticiero <- audio_semanal %>%
  group_by(Noticiero, Fecha) %>%
  summarise(n = n()) %>%
  ggplot(aes(Fecha, reorder(Noticiero, n), fill = n))+
  geom_tile(aes(group = Noticiero))+
  #facet_grid(~Fecha) + 
  scale_fill_gradient(low = "white", high = "steelblue")+
  geom_text(aes(label = n))+
  labs(title = "Distribución de noticias por noticiero",
       subtitle = paste0(nrow(audio_semanal)," notas totales"),
       x = "Fecha",
       y = "Total de notas",
       caption = "\n@Data recolectada a través del formulario de captura de Punto Preciso")+
  theme_classic()+
  theme(axis.text.x=element_text(angle=90, hjust=1))
total_notas_noticiero




# treemap -----------------------------------------------------------------

total_notas_tipo_noticiero <- audio_semanal %>%
  group_by(Tipo_noticiero) %>%
  summarise(n = n())

treemap(total_notas_tipo_noticiero,
        index = c("Tipo_noticiero", "n"),
        vSize = "n",
        type = "index",
        title = "Total de noticias revisadas durante la semana",
        palette = "Dark2",
        border.col = "white",
        border.lwds = 2,
        fontsize.labels = c(15, 15),
        fontcolor.labels = "white",
        fontface.labels = c("bold", "italic"),
        bg.labels = 0,
        lowerbound.cex.labels = 0,
        inflate.labels = F,
        force.print.labels = T,
        overlap.labels = 1,
        align.labels = list(c("center", "center"),
                            c("right", "top"))
        )



# ¿hacemos un diagrama alluvial? hacemos un diagrama alluvial -------------

total_notas_fecha <- audio_semanal %>%
  group_by(Fecha, Horario) %>%
  summarise(n = n()) %>% 
  ggplot(aes(axis1 = Fecha, axis2 = Horario, y = n)) +
  geom_alluvium(aes(fill = Horario)) +
  geom_stratum(aes(fill = Horario, color = "white")) +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Fecha", "Horario"),
                   expand = c(0.15, 0.05)) +
  theme_void()+
  theme(legend.position = "none")
total_notas_fecha



# gráfica de cuerdas ------------------------------------------------------

# librería chorddiag

audio_semanal %>% 
  select(c(Noticiero, Nivel_gobierno)) %>%
  group_by(Nivel_gobierno) %>% 
  count(sort = TRUE) %>% 
  ungroup() %>% 
  kable() %>% 
  kable_styling(bootstrap_options = "responsive", font_size = 9)

por_nivel_gobierno <- audio_semanal %>% 
  select(c(Noticiero, Nivel_gobierno)) %>%
  group_by(Noticiero, Nivel_gobierno) %>% 
  summarise(n = n()) %>% 
  ungroup() %>%
  mutate_if(is.character, str_to_title)

datos_wider <- por_nivel_gobierno %>% 
  spread(value = n, key= Nivel_gobierno, drop = F, fill = 0) %>% 
  column_to_rownames("Noticiero") %>% 
  as.matrix()



datos_wider <- matrix(data = por_nivel_gobierno$n, 
                      nrow = length(por_nivel_gobierno$Noticiero),
                      ncol = length(por_nivel_gobierno$Nivel_gobierno))
datos_wider

rownames(datos_wider) <- por_nivel_gobierno$Noticiero
colnames(datos_wider) <- por_nivel_gobierno$Nivel_gobierno

chorddiag(datos_wider)





name <- "Hola"
chorddiag(
  datos_wider,
  groupnameFontsize = 8,
  ticklabelFontsize = 6,
  tickInterval = 500,
  chordedgeColor = NA,
  categoryNames = c("Hola", name),categorynameFontsize = 10
)



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
audiovisuales<- audiovisuales %>% 
  mutate(Nivel_gobierno, Nivel_gobierno = limpiar_texto(texto = Nivel_gobierno),
         Organización, Organización = limpiar_texto(texto = Organización),
         Tendencia, Tendencia = limpiar_texto(texto = Tendencia))
 
coca_df$palabra <- str_trim(coca_df$palabra, side = "both")
coca_df

audiovisuales$Nivel_gobierno <- str_trim(audiovisuales$Nivel_gobierno, 
                                         side = "both")
audiovisuales



# tablas con gt -----------------------------------------------------------

# creo un tema con el formato general de la tabla para ahorrar tiempo
my_theme <- function(audio_ayer_sectur) {
  tab_options(
    data = audio_ayer_sectur,
    heading.title.font.size = 24,
    heading.title.font.weight = "bold",
    heading.subtitle.font.size = 12,
    heading.align = "center",
    table.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width= px(3),
  )
}

# aquí la tabla
tiene_sectur %>%
  select(Fecha, Titular, Enlace, Horario, Duración, Noticiero, Tipo_noticiero) %>% 
  head() %>%
  group_by(Noticiero) %>%
  gt(groupname_col = "Noticero",
     rowname_col = "Noticiero") %>%
  tab_header(title = "Turismo. Información publicada en radio y televisión") %>%
  fmt_date(columns = c(Fecha), date_style = 2) %>%
    tab_source_note(source_note = md("**Datos obtenidos** del monitoreo de medios
               audiovisuales hecho **por Punto Preciso**")) %>%
  gt_theme_nytimes()


# probamos una tabla con el paquete DT ------------------------------------

audio_ayer_sectur %>%
  select(Fecha, Titular, Enlace, Horario, Duración, Noticiero, Tipo_noticiero) %>% 
  datatable()


# # ahora con reactable ---------------------------------------------------

audio_ayer_sectur %>% 
  select(Fecha, Titular, Enlace, Horario, Duración, Noticiero, Tipo_noticiero) %>%
  reactable(groupBy = c("Horario", "Tipo_noticiero", "Noticiero"), 
            searchable = TRUE, showPagination = TRUE, defaultExpanded = TRUE,
            striped = TRUE, compact = TRUE, 
            style = list(fontFamily ="Segoe UI", max_width = 600))


# tablas con kablextra ---------------------------------------------------
# Me quedo solo con los registros que son de sectur
audio_ayer_sectur <- audio_ayer_sectur %>% 
  filter(sectur == "sectur")

# Hago una tabla bonita con kablextra
audio_ayer_sectur %>% 
  select(c(Fecha, Titular, Enlace, Horario, Duración, Tipo_noticiero,
           Noticiero)) %>%
  arrange(desc(Duración, Horario, Noticiero)) %>%
  kable(caption = "Información publicada en radio y tv sobre turismo") %>%
  kable_styling(bootstrap_options = c("responsive", "striped", "hover", 
                                      "condensed"),
                fixed_thead = T, latex_options = "scale_down") %>%
  column_spec(2, background = "#80AAEC", color = "white") %>% 
  column_spec(3, width = "1cm") %>% 
  column_spec(5, color = "white",
              background = spec_color(as.numeric(audio_ayer_sectur$Duración[1:3]), 
                                      end = 0.3, option = "A")) %>% 
  footnote(general = "*Datos obtenidos mediante el monitoreo de radio y tv hecho por Punto Preciso")

audio_ayer_sectur %>% 
  select(c(Fecha, Titular, Enlace, Horario, Duración, Tipo_noticiero,
           Noticiero)) %>%
  arrange(desc(Duración, Horario, Noticiero)) %>%
  gt() %>% 
  gt_theme_nytimes()



# tabla con flextable -----------------------------------------------------

audio_ayer_sectur %>%
  select(c(Fecha, Titular, Enlace, Horario, Duración, Tipo_noticiero,
           Noticiero)) %>%
  flextable() %>%
  set_caption(caption = "Información publicada en radio y tv sobre turismo",
              ) %>%
  add_footer_lines("Datos obtenidos mediante el monitoreo de radio y tv hecho por Punto Preciso") %>% 
  theme_zebra()

audio_ayer_sectur %>%
  select(c(Fecha, Titular, Enlace, Horario, Duración, Tipo_noticiero,
           Noticiero)) %>%
  pivot_table("Horario", "Tipo_noticiero") %>% 
  pivot_format()

