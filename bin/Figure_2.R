#### Scripts used in the article: ####
# Unite behind evolutionary biology: modifying public policies and their tools to achieve long term conservation

# Authors: Cristina I. Guzmán-González1*; Javier Pérez-López1*; Ana Wegier1**

# 1 Laboratorio de Genética de la Conservación, Jardín Botánico, Instituto de Biología, Universidad Nacional Autónoma de México, Ciudad de México, México.
# * Posgrado en Ciencias Biológicas, Instituto de Biología, Universidad Nacional Autónoma de México, Ciudad de México, México.
# ** Correspondence: awegier@ib.unam.mx

rm(list = ls())

library(extrafont)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(readxl)
library(tidyverse)
library(data.table)

#font_import()
loadfonts()
fonts()   

####### Figure 2 #####
df<-read_excel("../data/database_parameters.xlsx",
               sheet = "Clasificación de parámetros", col_names = T)%>% 
  filter(Change_factor== "1")

df$Procesos[df$Procesos == "1"] <- "Habitat"
df$Procesos[df$Procesos == "2"] <- "Demography"
df$Procesos[df$Procesos == "3"] <- "Interactions"
df$Procesos[df$Procesos == "4"] <- "Evolutionary biology"

df$Biologia_Evolutiva[df$Biologia_Evolutiva == "1"] <- "Variation"
df$Biologia_Evolutiva[df$Biologia_Evolutiva == "2"] <- "Reproduction"
df$Biologia_Evolutiva[df$Biologia_Evolutiva == "3"] <- "Selection"
df$Biologia_Evolutiva[df$Biologia_Evolutiva == "4"] <- "Dispersion/Migration"

df1<-df%>% mutate(Presencia= rep(1,nrow(df)))%>% 
  filter(Change_factor== "1")%>%
  reshape2::dcast(Procesos~ Tools, value.var = "Presencia", fun= sum)%>%
  rename(Procesos=Procesos)%>% # rename para hacer el match con la base
  filter(Procesos != "????")

df2<-df %>% mutate(Presencia= rep(1,nrow(df)))%>% 
  filter(Change_factor== "1")%>%
  reshape2::dcast(Biologia_Evolutiva~ Tools, value.var = "Presencia", fun= sum)%>%
  rename(Procesos=Biologia_Evolutiva)%>% # rename para hacer el match con la base
  filter(Procesos != "????")

#### Dataframe ####
df3<-df%>% mutate(Presencia= rep(1,nrow(df)))%>%
  reshape2::dcast(Biologia_Evolutiva~ Tools, value.var = "Presencia", fun= sum)%>%
  rename(Procesos=Biologia_Evolutiva)%>% 
    filter(Procesos != "NA") %>%
    bind_rows(df1)%>%
  gather(Tools, Total, 2:7) %>% filter(Procesos != "X")

# Biologia Evolutiva #  
df_cat1<-df3%>% filter(Procesos %in% c("Habitat", 
                                          "Demography", 
                                          "Interactions",
                                          "Evolutionary biology"))

# Procesos evolutovos #  
df_PE<-df3%>% filter(Procesos %in% c("Variation", 
                                  "Reproduction", 
                                  "Selection",
                                  "Dispersion/Migration"))

#### Theme and colors ####

theme <- theme(panel.grid.minor = element_blank(),
              panel.grid.major = element_blank(),
              axis.text.y = element_blank(),
              axis.title.y = element_blank(),
              plot.title = element_text(size = 10, hjust = 0.5))

mycolors<-c("#cb9135",
            "#7075d9",
            "#84ac46",
            "#5d3786")

positions1<-c("PSM", "RAM I", 
              "IUCN", "RAM II", 
              "MSC", "CITES")

mycolors1<-c("#468c5f",
            "#d45732",
            "#c3ab44",
            "#b44956")

#### Plot ####
Evolutivos<-ggplot(df_cat1, aes(fill=factor(Procesos, levels=c("Evolutionary biology",
                                                                  "Interactions",
                                                                  "Demography",
                                                                  "Habitat")), y=Total, x=Tools)) + 
  geom_bar( stat="identity")+coord_flip()+theme+
  theme(text=element_text(size=10,  family="Times New Roman", hjust = .5))+
  guides(fill=guide_legend(title=""))+ xlab("")+ 
  scale_fill_manual(values=mycolors)+
  theme_pander(base_family = "Times New Roman", base_size = 16)+
  labs(title = "B) Evolutionary context") + scale_x_discrete(limits = positions1)+
  theme(plot.title = element_text(hjust=0.5))+ 
  theme(axis.text.y = element_text(angle = 0, hjust = -.04))+ 
  theme(legend.position = c(.8, 0.3))
Evolutivos

Ecologicos<-ggplot(df_PE,aes(fill= factor(Procesos, levels=c("Dispersion/Migration",
                                                            "Reproduction",
                                                            "Variation",
                                                            "Selection")),
                             y=Total, x=Tools)) + 
  geom_bar( stat="identity")+coord_flip()+
  scale_x_discrete(limits = positions1)+theme+
  theme(text=element_text(size=10,  family="Times New Roman"))+
  guides(fill=guide_legend(title=""))+theme_pander(base_family = "Times New Roman", base_size = 16) + 
  scale_fill_manual(values = mycolors1) +
  scale_y_continuous(trans = 'reverse')+xlab("") + 
  theme(legend.position = c(0.3, 0.3),axis.text.y = element_blank(), axis.title.y = element_blank())+ 
  labs(title = "A) Evolutionary potential", y = "Total")+ 
  theme(plot.title = element_text(hjust=0.5))

Ecologicos

gtM <- ggplotGrob(Evolutivos)
gtF <- ggplotGrob(Ecologicos)
gt = cbind(gtF, gtM, size = NULL)

# save figure a-b
png("../../history_cris_art/figures/Figure_2.png",width = 3200, height = 1600, res=300)

# 2. Create the plot
plot(gt)
# 3. Close the file
dev.off()

#END#

