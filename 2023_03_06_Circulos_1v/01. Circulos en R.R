# Circulos en R

# @GuiborCamargo

# 0. Librerias

library(dplyr)
library(ggplot2)
library(ggfx)
setwd("2023_03_06_Circulos_1v/")


# 1. Ploting function ----
circle <- function(df) {
  df %>% 
    ggplot(aes(x,y,color = x))+
    with_outer_glow(
      geom_path(lwd = .2),
      colour = "grey90",
      sigma = 20,
      expand = 1)+
    scale_color_gradient(low = "red",high = "cyan")+
    theme_minimal()+
    theme(
      legend.position = "none",
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.title = element_blank(),
      plot.background = element_rect(fill = "grey10",
                                     colour = "grey10"),
      panel.grid = element_blank(),
      plot.title = element_text(
        hjust = .5,color = "white",size = 14))
}



# 2. Creamos un data frame ----
df <- data.frame(
  x = -100:100/10) %>% 
  mutate(
    y = sqrt(100-x^2)) %>% 
  # Agregamos la matriz de resultados negativos
  rbind(.[] %>% mutate(y=y*-1))


# 3. Plots ----

## 3.1. General Plot ----
ggpubr::ggarrange(
  (df %>% arrange(x) %>% circle() +
     labs(title = expression(    (x)    ))),
  
  (df %>% arrange(y) %>% circle() +
     labs(title = expression(    (y)    ))),
  
  (df %>% arrange(x+y) %>% circle() +
     labs(title =  expression(    (x+y)  ))),
  
  (df %>% arrange(x/y) %>% circle() +
    labs(title = expression(    (x/y)  ))) ,
  
  (df %>% arrange(x^y) %>% circle() +
      labs(title = expression(    (x^{y})    ))),
  
  (df %>% arrange( (x*y)+x) %>% circle() +
    labs(title = expression(    (x*y)+x  )))) 


ggsave("01_plots/01_final_results.png",
       h = 5.3*2,w=15)


## 3.2. Medio circulo ----
data.frame(
  x = -100:100/10) %>% 
  mutate(
    y = sqrt(100-x^2)) %>% 
  #rbind(.[] %>% mutate(y=y*-1)) %>% 
  ggplot(aes(x,y, color = x)) +
  geom_point(size = 3,alpha = .4)+
  geom_line(color = "grey40",lwd =.2)+
  labs(title = expression(   paste("y = ", sqrt(100-x^2))),
       caption = "@GuiborCamargo")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = .5),
        legend.position = "none",
        plot.background = element_rect(fill = "white", color = "white"))


ggsave("01_plots/02_ploting_functions.png",
       h = 4,w=6)

## 3.3. Circulo completo ----
data.frame(
  x = -100:100/10) %>% 
  mutate(
    y = sqrt(100-x^2)) %>% 
  rbind(.[] %>% mutate(y=y*-1)) %>% 
  ggplot(aes(x,y, color = x)) +
  geom_point(size = 3,alpha = .4)+
  geom_path(color = "grey40",lwd =.2)+
  labs(title = expression(   paste("y = ", sqrt(100-x^2))),
       caption = "@GuiborCamargo")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = .5),
        legend.position = "none",
        plot.background = element_rect(fill = "white", color = "white"))

ggsave("01_plots/03_ploting_equations.png",
       h = 6.3,w=6)


## 3.4. Extra 1 ----
df <- data.frame(
  x = -1000:1000/100) %>% 
  mutate(
    y = sqrt(100-x^2)) %>% 
  rbind(.[] %>% mutate(y=y*-1))

df %>% arrange(x^y) %>% circle() +
    labs(title = expression(    (x^{y})),
         caption = "@GuiborCamargo")+
  theme(plot.title = element_text(hjust = .5),
        plot.caption = element_text(colour = "grey"),
        legend.position = "none")


ggsave("01_plots/04_prety_plot1.png",
       h = 6.3,w=6)


## 3.5. Extra 2 ----
df %>% arrange((x*y)+x) %>% circle() +
  labs(title = expression(    (x*y)+x ),
       caption = "@GuiborCamargo")+
  theme(plot.title = element_text(hjust = .5),
        plot.caption = element_text(colour = "grey"),
        legend.position = "none")

ggsave("01_plots/04_prety_plot2.png",
       h = 6.3,w=6)

