# Hacinamiento carcelario en Colobmaia ---
# @GUiborCamargo

library(RSocrata)
library(dplyr)
library(ggplot2)
library(waffle)
setwd("2023_03_02_Hacinamiento_carcelario_colombia")


# 1. Cargar base ----
a0_carceles_RAW <- read.socrata("https://www.datos.gov.co/resource/ux6v-gpit.json")


# 2. Curar base ----
a1_carceles_cooked <- a0_carceles_RAW %>%
  mutate(fecha = as.Date(paste(anno,mes,"01",sep="-"))) %>%  
  mutate(across(capacidad:hacinamiento,~as.numeric(.)),
         hacinamiento = hacinamiento/100) %>% 
  merge(
    (.[] %>% 
      group_by(nombre_regional, fecha) %>% 
      summarise(avg = mean(hacinamiento, na.rm = T),
                max = max(hacinamiento, na.rm = T)) %>%
      as.data.frame() %>% 
      mutate(etiqueta = paste(nombre_regional,"\n promedio: ",
                              round(avg*100,1),"%\nmáximo: ",
                              round(max*100,1),"%"))),
    by  = c("nombre_regional","fecha"))

# 3. Gráficas ----

## 3.1. Histograma de la sobrepoblación y capacidad ----
a1_carceles_cooked %>% 
  filter(fecha == max(a1_carceles_cooked$fecha)) %>% 
  ggplot(aes(hacinamiento, reorder(etiqueta,hacinamiento, mean,na.rm = T))) +
  geom_boxplot(outlier.shape = NULL)+
  stat_summary(fun = mean, color = "red3",shape = 18,
               size = 1,alpha =.6)+
  geom_point(position = position_dodge2(width = .2), alpha = .5,
             color = "grey40")+
  scale_x_continuous(labels = scales::percent_format())+
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        legend.position = "bottom",
        plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5,color = "grey25"),
        plot.background = element_rect(fill = "white", color = "white"))+
  labs(title = "Hacinamiento carcelario en Colombia al 2023",
       subtitle = "@GuiborCamargo\nFuente: INPEC 2023",
       x = "Hacinamiento (%)",y = "Regionales del INPEC")
ggsave("02. Plots/01_Hacinamiento_carcelario_por_regiones.png",
       h=6,w=7)
  

## 3.2. Waffle chart ----
waffle(c("Capacidad actual" = 81,
         "Capacidad faltante" = 19),
       colors = c("grey60","red4"))+
  labs(title = "Hoy la capacidad carcelaria de Colombia ha\nsido superada por más de 19mil presos.",
            subtitle = "@GuiborCamargo\nFuente: INPEC 2023", 
       caption = "*Nota: Cada cuadro representa mil privados de la libertad")+
  theme(text = element_text(family = "serif"),
        legend.position = "bottom",
        plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5,color = "grey25"),
        plot.background = element_rect(fill = "white", color = "white"))

ggsave("02. Plots/02_Hacinamiento_carcelario_general.png",
       h=6,w=7)

## 3.2. Serie de tiempo ----
a1_carceles_cooked %>% 
  group_by(nombre_regional, fecha) %>%
  summarise(across(c(poblacion,sobrepoblacion),
                   ~sum(.,na.rm = T))) %>% 
  as.data.frame() %>% 
  mutate(hacinamiento = sobrepoblacion/poblacion) %>%
  
  ### a. Union con total nacional  ----
  rbind(a1_carceles_cooked %>% 
          group_by(fecha) %>%
          summarise(across(c(poblacion,sobrepoblacion),
                           ~sum(.,na.rm = T))) %>% 
          as.data.frame() %>% 
          mutate(hacinamiento = sobrepoblacion/poblacion,
                 nombre_regional = "TOTAL NACIONAL")) %>%
  
  mutate(nombre_regional = 
           factor(nombre_regional, 
                  levels = 
                    c("TOTAL NACIONAL",
                      "NOROESTE","NORTE","OCCIDENTE","ORIENTE",
                      "CENTRAL","VIEJO CALDAS"))) %>% 
  ### b. Gráfica ----
  ggplot(aes(as.Date(fecha),hacinamiento, 
             group = nombre_regional,
             color = nombre_regional))+
  geom_point()+
  geom_path(aes(lty =nombre_regional))+
  scale_y_continuous(labels = scales::percent_format())+
  scale_linetype_manual(values = c(1,rep(2,6)))+
  labs(title = "Evolución 2022-2023 del hacinamiento carcerlario\nen Colombia",
       subtitle = "@GuiborCamargo\nFuente: INPEC 2023",
       color = "Regional del\nINPEC",
       y = "Hacinamiento (%)",x = "Fecha")+
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        legend.position = "bottom",
        plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5,color = "grey25"),
        plot.background = element_rect(fill = "white", color = "white"))+
  guides(lty="none")

ggsave("02. Plots/03_Hacinamiento_carcelario_historico.png",
       h=6,w=8)
  

# 4. Resumenes adicionales ----

## 4.1. Historia nacional ----
a1_carceles_cooked %>% 
  group_by(fecha) %>%
  summarise(across(c(poblacion,sobrepoblacion),
                   ~sum(.,na.rm = T))) %>% 
  as.data.frame() %>% 
  mutate(hacinamiento = sobrepoblacion/poblacion,
         nombre_regional = "TOTAL NACIONAL")

## 4.1. Al último corte ----
a1_carceles_cooked %>% 
  filter(fecha == max(a1_carceles_cooked$fecha))  %>% 
  group_by() %>% 
  summarise(sum.cap = sum(capacidad),
            sum.pob = sum(poblacion),
            max.hac = max(hacinamiento),
            sum.sobrepob = sum(sobrepoblacion))

(19366-18743)/18743    
