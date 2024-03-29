# Inclsuion Financiera, FinTechs, reflexiones y retos



library(dplyr)
library(ggplot2)
library(googlesheets4)
library(stringr)
setwd("2023_06_01_Inclsuion Financiera_FinTechs_reflexiones_y_retos/")

a0_macro <- read_sheet("https://docs.google.com/spreadsheets/d/15yO4Fn3y3MKIapMKQKpwBZfGLxDDkIjFHBFYZZ7H1u4/edit?usp=sharing")
a1_calidad <- read_sheet("https://docs.google.com/spreadsheets/d/15yO4Fn3y3MKIapMKQKpwBZfGLxDDkIjFHBFYZZ7H1u4/edit?usp=sharing",
                        sheet = 2)
a2_trenging <- read_sheet("https://docs.google.com/spreadsheets/d/15yO4Fn3y3MKIapMKQKpwBZfGLxDDkIjFHBFYZZ7H1u4/edit?usp=sharing",
                          sheet = 4)


# 1. Indicadores Macro ----

a0_macro %>% 
  ## 1.1. Data preparation ----
  reshape2::melt(id.vars = "Date") %>% 
  mutate(Date = as.Date(Date),
         variable = str_replace_all(variable, "\\."," "),
         BanRep = 
           ifelse(str_detect(variable,"Rep")==T, "Tasa de Intervención del\nBanco de la República","Otros Indicadores")) %>% 
  
  ## 1.2. Plot ----
  ggplot(aes(Date, value, color = variable))+
  geom_point() +
  geom_line() +
  geom_vline(xintercept = as.Date("2021-09-30"), lty = 2, color = "grey45")+
  theme_minimal() + 
  theme(legend.position = "bottom")+
  facet_wrap(BanRep~., scales = "free")+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))+
  labs(x = "Año", y = "%", color = "")

ggsave("01_plots/01_indicadores_macro.png",
       h = 4, w = 8)

# 2. Calidad de las carteras de créditos ----
a1_calidad %>% 
  ## 2.1. Plot ----
  mutate(mes = as.Date(mes)) %>% 
  ggplot(aes(mes, calidad))+
  geom_point(color = "red4")+
  geom_path(color = "red4")+
  theme_minimal()+
  scale_y_continuous(labels = scales::percent_format())+
  scale_x_date(breaks = scales::pretty_breaks(n = 10))+
  geom_vline(xintercept = as.Date("2021-12-01"),
             lty = 2, color = "grey45")+
  labs(y = "Cartera vencida / Cartera bruta",
       x = "")

ggsave("01_plots/02_evalucion_calidad_creditos_consumo.png",
       h = 4, w = 8)

# 3. Tending info google ----
a2_trenging %>% 
  ## 3.1. Arreglar fecha
  mutate(Month = paste0(
    ### Año
    "20",substr(Month,9,10),"-",
    ### Mes y dia
    substr(Month, 6,7),"-","01"),
    Month = as.Date(Month)) %>%
  reshape2::melt(id.vars = "Month") %>% 
  mutate(variable = str_replace_all(variable,"_"," ")) %>% 
  # 3.1. Plot
  ggplot(aes(Month, value, color = variable)) +
  geom_point()+
  geom_path()+
  theme_minimal()+
  scale_x_date(breaks = scales::pretty_breaks(n = 10))+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = .5))+
  labs(y = "Índice de interes en búsquedas\n(Google)",
       x = "Año", color = "",
       title = "Evolución de las búsquedas de algunas de las\nFinTech más relevantes en Colombia",
       caption = "Fuente: Google Trends.")
ggsave("01_plots/03_trending_search_google.png",
       h = 4, w = 8)



