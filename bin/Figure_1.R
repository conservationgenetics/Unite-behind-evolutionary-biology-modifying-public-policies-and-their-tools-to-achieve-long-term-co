#### Scripts used in the article: ####
# Unite behind evolutionary biology: modifying public policies and their tools to achieve long term conservation

# Authors: Cristina I. Guzmán-González1*; Javier Pérez-López1*; Ana Wegier1**

# 1 Laboratorio de Genética de la Conservación, Jardín Botánico, Instituto de Biología, Universidad Nacional Autónoma de México, Ciudad de México, México.
# * Posgrado en Ciencias Biológicas, Instituto de Biología, Universidad Nacional Autónoma de México, Ciudad de México, México.
# ** Correspondence: awegier@ib.unam.mx

rm(list = ls())

library(readxl)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(data.table)
library(ggpubr)
library(grid)
library(egg)

####### Figure 1 #####

df<-read_excel("../data/tabla_resumen.xlsx", sheet = 1, col_names = T)%>%
  gather(Category, Value, 2:10)%>%
  filter(Category != "Parameters")%>%
  separate(Category, c("Category","Indicador"), sep = "_")%>%
  group_by(Tools,Category)%>%
  mutate(Percentage=Value/sum(Value)*100)%>% arrange(Tools)

df$Category[df$Category == "Character"] <- "B) Evolutionary character"
df$Category[df$Category == "EcologicalLevel"] <- "A) Ecological level"
df$Category[df$Category == "Time"] <- "C) Change factor"

df$Indicador<- factor(df$Indicador, levels=c("Population", "Community", "Ecosystem",
                                                  "Direct", "Indirect", "Non-evolutionary",
                                                  "Change", "Non-Change"))

ggplot(df, aes(fill=Indicador, y=Value, x=Tools)) + 
  geom_bar(stat="identity") +
  xlab("")+theme_classic()+
  scale_fill_manual(values = c("#993404","#fe9929", "#fed98e",
                              "#00441b", "#66c2a4", "#99d8c9",
                               "#649ed5", "#d1415e"),name = "")+
  facet_grid(vars(Category), scales = "free")+
  theme(strip.text.x = element_text(size=8, angle=75),
        strip.background = element_rect(colour="#e6b7bf", fill="#e6b7bf", linetype="solid"),
        panel.grid.major.y = element_line(colour = "grey70", linetype="dashed",size=0.5))+
  labs(y= "Number of parameters", x = "Tools")

ggsave("../figures/Figure_1.png", height = 6, width = 6, dpi = 300)

####### Chi-squared test #####

df<-read_excel("../data/tabla_resumen.xlsx", sheet = 1, col_names = T)

# trait evolutivos 
Xsq<-chisq.test(df[,c(3:5)], simulate.p.value = TRUE) # Traits evolutivos
Xsq # df = 10
Xsq$observed
Xsq$stdres

# factor de cambio #
Xsq<-chisq.test(df[,c(6:7)]) # Factor de cambio
Xsq
Xsq$observed
Xsq$stdres

#factor ecologico 
Xsq<-chisq.test(df[,c(8:10)], simulate.p.value = TRUE) # Factor ecologico
Xsq # df = 10
Xsq$observed
Xsq$stdres

# END #
