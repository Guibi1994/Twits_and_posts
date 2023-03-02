# Muejres en la policia

# Contenido: Este script analiza la información
  # disponible en la páginas históricas del 
  # Observatorio del Direccionamiento del 
  # Talento Humano de la Policia Nacional 
  # de Colombia. De igual modo, se producen
  # algunos gráficos orientados a evaluar
  # la evolución de la participación de las
  # mujeres al interior de esta intitución.

# Acrónimos:
  # PNC: Policia Nacional de Colombia

setwd("2023_02_25_Mujeres_policia")

# 0. librerias ----
library(dplyr)
library(ggplot2)
library(stringr)
library(waffle)




# 1. Cargar datos ----
a0_raw_personal <- read.csv("00_data/mc01_planta_personal.csv") %>% 
  # cambiar formato de fecha
  mutate(fecha = as.Date(fecha))


# 2. Producción de gráficos ----
## 2.2. Participación hoy
waffle(c(Mujeres=16,Hombres = 84),
       colors = c("purple","grey70"), size = 2) +
  coord_flip()+
  labs(title = "Participación de la mujer en la Policia Nacional para\nel año 2023",
       subtitle = "Fuente: Observatorio del Direccionamiento del Talento Humano de la\nPolicia Nacional de Colombia\n@GuiborCamargo",
       y = "",x = "", caption = "@GuiborCamargo",color = "")+
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        legend.position = "bottom",
        plot.subtitle = element_text(face = "italic",size = 7, color ="grey40"),
        axis.text = element_blank(),
        plot.background = element_rect(fill = "white", color = "white"))

ggsave("03_plots/01_distribución_acutal_de_generos.png",h = 5,w=4.2)

## 2.2. Evolución del número de miembros en la PNC segun sexo ----
a0_raw_personal %>% 
  ### 2.2.1. Transforamción ----
  group_by(fecha) %>% 
  summarise(across(hombres:mujeres,~sum(.,na.rm = T))) %>% 
  as.data.frame() %>% 
  reshape2::melt(id.vars = "fecha",
                 variable.name = "sexo",
                 value.name = "miembros") %>% 
  ### 2.2.2. Gráfica ----
  ggplot(aes(fecha, miembros,color = sexo, group = sexo))+
  geom_point()+
  geom_path()+
  scale_color_manual(values = c("cyan3","purple3"))+
  scale_y_continuous(labels = scales::comma,
                     breaks = scales::pretty_breaks(n=6))+
  scale_x_date(breaks = scales::pretty_breaks(n = 4))+
  labs(title = "¿Cómo ha evolucionado la cantidad de hombres y mujeres al\ninterior de la Policia Nacional de Colombia?",
       subtitle = "Fuente: Observatorio del Direccionamiento del Talento Humano-Policia Nacional de Colombia\n@GuiborCamargo",
       y = "Número de personas",x = "", caption = "@GuiborCamargo",color = "")+
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        legend.position = "bottom",
        plot.subtitle = element_text(face = "italic",size = 7, color ="grey40"),
        plot.caption = element_text(color = "grey45"),
        plot.background = element_rect(fill = "white", color = "white"))+
facet_wrap(.~sexo, scales = "free")


ggsave("03_plots/02_evolucion_neta_de_policias_por_genero.png",h=4,w=6)





## 2.3. Participación histórica de la mujer
a0_raw_personal %>% 
  group_by(fecha) %>% 
  summarise(across(hombres:mujeres,~sum(.,na.rm = T))) %>% 
  as.data.frame() %>% 
  reshape2::melt(id.vars = "fecha",
                 variable.name = "sexo",
                 value.name = "miembros") %>% 
  group_by(fecha) %>% 
  mutate(total = sum(miembros)) %>% 
  ungroup() %>% as.data.frame() %>% 
  mutate(miembros = miembros/total) %>%
  filter(sexo == "mujeres") %>% 
  ggplot(aes(fecha, miembros))+
  geom_area(fill = "purple4",alpha=.4)+
  geom_point()+
  geom_text(aes(fecha, miembros,
                label = paste0(round(100*miembros,1),"%")), 
            nudge_y = .01, family = "serif",size = 3)+
  scale_y_continuous(labels = scales::percent_format())+
  labs(title = "Participación de la mujer en la Policia\nNacional de Colombia",
       subtitle = "Fuente: Observatorio del Direccionamiento del Talento Humano-Policia Nacional de Colombia\n@GuiborCamargo",
       y = "% de muejres",x = "", caption = "@GuiborCamargo",color = "")+
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        legend.position = "bottom",
        plot.subtitle = element_text(face = "italic",size = 7, color ="grey40"),
        plot.background = element_rect(fill = "white", color = "white"))

ggsave("03_plots/03_evolucion_participación_muejres.png",h=5,w=5)


###########
a0_raw_personal %>% 
  group_by(fecha, grupo) %>% 
  summarise(across(hombres:mujeres,~sum(.,na.rm = T))) %>% 
  as.data.frame() %>% 
  reshape2::melt(id.vars = c("fecha","grupo"),
                 variable.name = "sexo",
                 value.name = "miembros") %>% 
  group_by(fecha,grupo) %>% 
  mutate(total = sum(miembros)) %>% 
  ungroup() %>% as.data.frame() %>% 
  mutate(miembros = miembros/total) %>%
  filter(sexo == "mujeres", grupo != "Agentes") %>%
  mutate(grupo = factor(grupo, levels = c
                        ("Oficiales","Suboficiales","Nivel ejecutivo",
                          "En formacion","Servicio militar",
                          "Personal no uniformado"))) %>% 
  ggplot(aes(fecha, miembros))+
  geom_area(fill = "purple4",alpha=.4)+
  geom_point()+
  # geom_text(aes(fecha, miembros,
  #               label = paste0(round(100*miembros,1),"%")), 
  #           nudge_y = .01, family = "serif",size = 2)+
  scale_y_continuous(labels = scales::percent_format())+
  scale_x_date(breaks = scales::pretty_breaks(n = 5))+
  labs(title = "¿Cómo se ha trasnforamdo la composición de genero\nen las categorias de la Policia Nacional?",
       subtitle = "Fuente: Observatorio del Direccionamiento del Talento Humano-Policia Nacional de Colombia\n@GuiborCamargo",
       y = "% mujeres",x = "", caption = "@GuiborCamargo",color = "")+
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        legend.position = "bottom",
        plot.subtitle = element_text(face = "italic",size = 7, color ="grey40"),
        plot.background = element_rect(fill = "white", color = "white"))+
  facet_wrap(.~grupo, scales = "free")

ggsave("03_plots/04_evolucion_participación_muejres_por_categoria.png",h=5,w=6)


##
a0_raw_personal %>% 
  group_by(fecha, grupo) %>% 
  summarise(across(hombres:mujeres,~sum(.,na.rm = T))) %>% 
  as.data.frame() %>% 
  reshape2::melt(id.vars = c("fecha","grupo"),
                 variable.name = "sexo",
                 value.name = "miembros") %>% 
  group_by(fecha,grupo) %>% 
  mutate(total = sum(miembros)) %>% 
  ungroup() %>% as.data.frame() %>% 
  filter(sexo == "mujeres", grupo != "Agentes") %>%
  mutate(grupo = factor(grupo, levels = c
                        ("Personal no uniformado","Servicio militar","En formacion",
                          "Nivel ejecutivo","Suboficiales","Oficiales"))) %>% 
  arrange(grupo,fecha) %>%
  group_by(grupo) %>% 
  mutate(m2020 = first(miembros)) %>% 
  filter(fecha > as.Date("2023-01-01")) %>% 
  mutate(dif = miembros-m2020,
         dif.pop = (miembros-m2020)/m2020,
         cat = ifelse(dif.pop <0,F,T),
         labels = ifelse(dif.pop >0,
                        paste0("+",dif),as.character(dif))) %>% 
 
  ggplot(aes(x = grupo, y = dif.pop, color = cat))+
  geom_segment(aes(xend =grupo,yend = 0))+
  geom_point()+
  coord_flip()+
  geom_hline(yintercept = 0, color= "grey30",lty=2)+
  scale_y_continuous(labels = scales::percent_format())+
  geom_text(aes(x = grupo, y = dif.pop/2,label = labels),nudge_x = .15,
            family = "serif")+
  geom_text(aes(x = grupo, y = dif.pop/2,
                label = paste0("(",round(dif.pop*100,1),"%)")),nudge_x = -.15,
            family = "serif",size = 3)+
  labs(title = "¿Cuánto ha aumentado el número de mujeres\nen cada categoria de la Policia Nacional?",
      subtitle = "Fuente: Observatorio del Direccionamiento del Talento Humano-Policia Nacional de Colombia\n@GuiborCamargo",
      y = "Cambio del 2020 al 2023 (%)",x = "", caption = "@GuiborCamargo",color = "")+
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        legend.position = "none",
        plot.subtitle = element_text(face = "italic",size = 7, color ="grey40"),
        plot.background = element_rect(fill = "white", color = "white"))

ggsave("03_plots/05_cambio_neto_muejeres_por_categoria.png",h=5,w=7)
