###  GLOBAL ###

rm(list = ls())

library(extrafont)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(readxl)
library(tidyverse)

#font_import()
loadfonts()
fonts()   

df<-read_excel("../data/BdD Ene 29 2020.xlsx", sheet = "Clasificación de parámetros", col_names = T)
names(df)

df$`Categoria 1`[df$`Categoria 1` == "1.0"] <- "Hábitat"
df$`Categoria 1`[df$`Categoria 1` == "2.0"] <- "Demografía"
df$`Categoria 1`[df$`Categoria 1` == "3.0"] <- "Interacciones"
df$`Categoria 1`[df$`Categoria 1` == "4.0"] <- "Biol_Evo"

df$`Procesos evolutivos`[df$`Procesos evolutivos` == "1.0"] <- "Variación"
df$`Procesos evolutivos`[df$`Procesos evolutivos` == "2.0"] <- "Reproducción"
df$`Procesos evolutivos`[df$`Procesos evolutivos` == "3.0"] <- "Selección"
df$`Procesos evolutivos`[df$`Procesos evolutivos` == "4.0"] <- "Migración Dispersión"


df1<-df%>% mutate(Presencia= rep(1,nrow(df)))%>% 
  filter(`Indicador de cambio`== "1.0")%>%
  reshape2::dcast(`Categoria 1`~ Herramienta, value.var = "Presencia", fun= sum)%>%
  rename(Categoria_1=`Categoria 1`)%>% # rename para hacer el match con la base
  filter(Categoria_1 != "????")

df2<-df1 %>% gather(Herramientas, valor, 2:7)%>% 
  group_by(Herramientas, Categoria_1)%>% summarise(total= sum(valor))

df2



## grafica global para categori 1 # 

library(moonBook)
library(webr)

PieDonut(df2,aes(Herramientas,Categoria_1,count=total),r1=0.9,
         explodeDonut=TRUE,title="Distribution of carburetors by gears",
         star=3*pi/2,labelposition=0)

names(df2)


tiff("../figures/PieDonut_Categoria_1.tif", res = 300,
     height = 1900, width = 2000)

PieDonut(df2,aes(Herramientas,Categoria_1, count= total), 
         explodeDonut=T,
         ratioByGroup=F,labelposition=1,title="Categoría 1", 
         pieLabelSize = 4, donutLabelSize = 3,
         showRatioThreshold = 0, color = "white", 
         donutAlpha = 1,
         start=2*pi/2.8, titlesize = 6, r0=.4,
         showPieName=FALSE)

dev.off()

# hacer uno identico para los procesos # 

names(df)

df1<-df%>% mutate(Presencia= rep(1,nrow(df)))%>% 
  filter(`Indicador de cambio`== "1.0")%>%
  reshape2::dcast(`Procesos evolutivos`~ Herramienta, value.var = "Presencia", fun= sum)%>%
  rename(Categoria_1=`Procesos evolutivos`)%>% # rename para hacer el match con la base
  filter(Categoria_1 != "????")

df3<-df1 %>% gather(Herramientas, valor, 2:7)%>% 
  group_by(Herramientas, Categoria_1)%>% summarise(total= sum(valor))


names(df3)

tiff("../figures/PieDonut_Procesos.tif", res = 300,
     height = 1900, width = 2000)

PieDonut(df3,aes(Herramientas,Categoria_1, count= total), 
         explodeDonut=T,
         ratioByGroup=T,labelposition=2,title="Procesos", 
         pieLabelSize = 4, donutLabelSize = 3,
         showRatioThreshold = 0, color = "white", 
         donutAlpha = 1,
         start=2*pi/2.8,
         titlesize = 6, r0=.4,
         showPieName=FALSE)

dev.off()


###
### END 
###


# extraigo las palabras de cada set

df<-read_excel("../data/BdD Ene 29 2020.xlsx", sheet = "Clasificación de parámetros", col_names = T)
names(df)

df$`Categoria 1`[df$`Categoria 1` == "1.0"] <- "Hábitat"
df$`Categoria 1`[df$`Categoria 1` == "2.0"] <- "Demografía"
df$`Categoria 1`[df$`Categoria 1` == "3.0"] <- "Interacciones"
df$`Categoria 1`[df$`Categoria 1` == "4.0"] <- "Biol_Evo"

df$`Procesos evolutivos`[df$`Procesos evolutivos` == "1.0"] <- "Variación"
df$`Procesos evolutivos`[df$`Procesos evolutivos` == "2.0"] <- "Reproducción"
df$`Procesos evolutivos`[df$`Procesos evolutivos` == "3.0"] <- "Selección"
df$`Procesos evolutivos`[df$`Procesos evolutivos` == "4.0"] <- "Migración Dispersión"

View(df)


df1<-df%>% mutate(Presencia= rep(1,nrow(df)))%>% 
  filter(`Indicador de cambio`== "1.0")%>% 
  group_by(Herramienta, `Categoria 1`,`Procesos evolutivos`, Parámetro) %>% 
  summarise(presencia= sum(Presencia))

View(df1)

write_csv(df1, "../data/parametros_en_categorias.csv")



