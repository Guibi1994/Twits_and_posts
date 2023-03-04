# CUltivos de Coca en Colombia


library(dplyr)
library(ggplot2)
library(stringr)

a0_coca_raw <- RSocrata::read.socrata("https://www.datos.gov.co/resource/acs4-3wgp.json")



# 2. Curación de datos 
a1_coca <- a0_coca_raw %>% 
  reshape2::melt(id.vars = c(names(a0_coca_raw)[1:4]),
                 variable.name = "year") %>% 
  mutate(year = as.integer(substr(year,3,10)),
         value = stringr::str_remove_all(value,",|\\s|-") %>% 
           as.numeric()) %>% 
  arrange(departamento,municipio,year)



a1_coca %>% 
  group_by(year,departamento) %>% 
  summarise(value = sum(value)) %>% 
  ggplot(aes(year, value, group = departamento,
             color = departamento)) +
  geom_point()+
  geom_path()+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  geom_vline(xintercept = c(2002,2010,2018), lty = 2, 
             color = "grey24")+
  labs(title = "Hectáreas de coca en Colombia (2000-2020)",
       subtitle = "@GuiborCamargo\nFuente: Observatorio de Drogas de Colombia-MinJusticia",
       x = "Año",y ="Hectáreas de coca (Ha)")+
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        legend.position = "bottom",
        plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5,color = "grey25"),
        plot.background = element_rect(fill = "white", color = "white"))+
  guides(lty="none")



a1_coca %>% 
  group_by(year) %>% 
  summarise(value = sum(value)) %>% 
  ggplot(aes(year, value)) +
  geom_point()+
  geom_path()+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  geom_vline(xintercept = c(2002,2010,2018), lty = 2, 
             color = "grey24")+
  labs(title = "Hectáreas de coca en Colombia (2000-2020)",
       subtitle = "@GuiborCamargo\nFuente: Observatorio de Drogas de Colombia-MinJusticia",
       x = "Año",y ="Hectáreas de coca (Ha)")+
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        legend.position = "bottom",
        plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5,color = "grey25"),
        plot.background = element_rect(fill = "white", color = "white"))+
  guides(lty="none")







