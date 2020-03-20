# Library

rm(list = ls())

library(readxl)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(data.table)
library(ggpubr)

source("../coord_radar.R")

#setwd("~/Escritorio/history_cris_art/bin/")

df<-read_excel("../data/BdD Ene 29 2020.xlsx", sheet = "Clasificación de parámetros", col_names = T)
names(df)

df$`Categoria 1`

df$`Categoria 1`[df$`Categoria 1` == "1"] <- "Hábitat"
df$`Categoria 1`[df$`Categoria 1` == "2"] <- "Demografía"
df$`Categoria 1`[df$`Categoria 1` == "3"] <- "Interacciones"
df$`Categoria 1`[df$`Categoria 1` == "4"] <- "Biología \nEvolutiva"

df$`Procesos evolutivos`[df$`Procesos evolutivos` == "1"] <- "Variación"
df$`Procesos evolutivos`[df$`Procesos evolutivos` == "2"] <- "Reproducción"
df$`Procesos evolutivos`[df$`Procesos evolutivos` == "3"] <- "Selección"
df$`Procesos evolutivos`[df$`Procesos evolutivos` == "4"] <- "Migración \nDispersión"

#View(df)

df1 <- df %>% mutate(Presencia= rep(1,nrow(df)))%>% 
  filter(`Indicador de cambio`== "1") %>% 
  reshape2::dcast(`Procesos evolutivos` ~ Herramienta, value.var = "Presencia", fun= sum)%>%
  gather(Herramientas, Total, 2:7) %>% filter(`Procesos evolutivos` != "NA")

ggplot(df1,
       aes(x = `Procesos evolutivos`,
           y = Total,
           color = factor(Herramientas),
           fill= Herramientas,
           group = Herramientas)) +
  geom_polygon(alpha = 0.9) + 
  coord_radar() +
  facet_wrap(~Herramientas)+ theme_bw()+
  labs(x="", y="Total parameters")+ 
  scale_color_manual(values=c(
                              "#1f78b4",
                              "#33a02c",
                              "#e31a1c",
                              "#ff7f00",
                              "#6a3d9a",
                              "#b15928"))+
  theme(legend.position = "none")+ 
  theme(axis.text.x = element_text(face = "bold",
                                   size = 9, color = "grey40"))+
  scale_fill_manual(values = c("#a6cee3",
                               "#b2df8a",
                               "#fb9a99",
                               "#fdbf6f",
                               "#cab2d6",
                               "#ffff99"))

ggsave("../figures/radar_Procesos_total.png", dpi= 300, width = 8.5, height = 6)


### POR PORCENTAJE DE CATEGORIA # 

df1<- df1 %>% group_by(Herramientas)%>%
  mutate(Percentage=Total/sum(Total)*100)

ggplot(df1,
       aes(x = `Procesos evolutivos`,
           y = Percentage,
           color = factor(Herramientas),
           fill= Herramientas,
           group = Herramientas)) +
  geom_polygon(alpha = 0.9) + 
  coord_radar() +
  facet_wrap(~Herramientas)+ theme_bw()+
  labs(x="", y="Percentage of parameters")+ 
  scale_color_manual(values=c(
    "#1f78b4",
    "#33a02c",
    "#e31a1c",
    "#ff7f00",
    "#6a3d9a",
    "#b15928"))+
  theme(legend.position = "none")+ 
  theme(axis.text.x = element_text(face = "bold",
                                   size = 9, color = "grey40"))+
  scale_fill_manual(values = c("#a6cee3",
                               "#b2df8a",
                               "#fb9a99",
                               "#fdbf6f",
                               "#cab2d6",
                               "#ffff99"))

ggsave("../figures/radar_Procesos_porcentajes.png", dpi = 300,
       width = 8.5, height = 6)

# END # 

########################## caategoria 1 ######

names(df)

df1 <- df %>% mutate(Presencia= rep(1,nrow(df)))%>% 
  filter(`Indicador de cambio`== "1") %>% 
  reshape2::dcast(`Categoria 1` ~ Herramienta, value.var = "Presencia", fun= sum)%>%
  gather(Herramientas, Total, 2:7) %>% filter(`Categoria 1` != "????")

df1
names(df1)

ggplot(df1,
       aes(x = `Categoria 1`,
           y = Total,
           color = factor(Herramientas),
           fill= Herramientas,
           group = Herramientas)) +
  geom_polygon(alpha = 0.9) + 
  coord_radar() +
  facet_wrap(~Herramientas)+ theme_bw()+
  labs(x="", y="Total parameters")+ 
  scale_color_manual(values=c(
    "#1f78b4",
    "#33a02c",
    "#e31a1c",
    "#ff7f00",
    "#6a3d9a",
    "#b15928"))+
  theme(legend.position = "none")+ 
  theme(axis.text.x = element_text(face = "bold",
                                   size = 9, color = "grey40"))+
  scale_fill_manual(values = c("#a6cee3",
                               "#b2df8a",
                               "#fb9a99",
                               "#fdbf6f",
                               "#cab2d6",
                               "#ffff99"))

ggsave("../figures/radar_categoria_1_total.png", dpi= 300, width = 8.5, height = 6)


### POR PORCENTAJE DE CATEGORIA # 

df1<- df1 %>% group_by(Herramientas)%>%
  mutate(Percentage=Total/sum(Total)*100)

names(df1)


ggplot(df1,
       aes(x = `Categoria 1`,
           y = Percentage,
           color = factor(Herramientas),
           fill= Herramientas,
           group = Herramientas)) +
  geom_polygon(alpha = 0.9) + 
  coord_radar() +
  facet_wrap(~Herramientas)+ theme_bw()+
  labs(x="", y="Percentage of parameters")+ 
  scale_color_manual(values=c(
    "#1f78b4",
    "#33a02c",
    "#e31a1c",
    "#ff7f00",
    "#6a3d9a",
    "#b15928"))+
  theme(legend.position = "none")+ 
  theme(axis.text.x = element_text(face = "bold",
                                   size = 9, color = "grey40"))+
  scale_fill_manual(values = c("#a6cee3",
                               "#b2df8a",
                               "#fb9a99",
                               "#fdbf6f",
                               "#cab2d6",
                               "#ffff99"))

ggsave("../figures/radar_categoria_1_porcentajes.png", dpi = 300,
       width = 8.5, height = 6)






