library(dplyr)
library(ggplot2)
setwd("/")
setwd("C:Users/Pabel/Documents/Angela/paper")

survey <- read.csv("tesis_paper.csv",header = T,sep = ",",encoding = "UTF-8")


# VARIABLES CAPTURADAS


survey<- read.csv("tesis_paper.csv",header = T,sep = ";",encoding = "UTF-8",na.strings = c(""))

# Convertir a factores

for (mynames in names(survey)) {
  
  if(mynames!="Horas_.diarias_.estudio"){
    survey[,mynames] <- as.factor(survey[,mynames])
  }
}

# variables

str(survey)

# observaciones
nrow(survey)
  
#columnas
length(names(survey)) 

#resumen de cada categoría:

summary(survey)


# TRATAMIENTO DE VALORES ATÍPICOS
  
# Valor atípico en la columna edad
  
 
df_info  <- as.data.frame(prop.table(table(survey$Edad))) %>% arrange(-Freq)
boxplot(df_info$Freq)
qqnorm(df_info$Freq)


#frecuencia
df_info

#transformacion

df_info[df_info$Var1 %in% c(">38","33-37"), "categoria"] <- ">33"
df_info[df_info$Var1 %in% c("23-27"), "categoria"] <- "23-27"
df_info[df_info$Var1 %in% c("18-22"), "categoria"] <- "18-22"
df_info[df_info$Var1 %in% c("28-32"), "categoria"] <- "28-32"

df_info

#añadimos esta transformación a la encuesta original:

df_info <- df_info %>% select(Var1, categoria)
survey$Edad <- as.factor(survey$Edad)

survey<-left_join(survey,df_info,by=c("Edad"="Var1"))
survey<- survey[,!(names(survey) %in% c("Edad"))] 
names(survey)[length(names(survey))] <-"Edad"
survey$Edad <- as.factor(survey$Edad)
summary(survey$Edad)

#comprobacion de valores

df_trans <- as.data.frame(prop.table(table(survey$Edad))) %>% arrange(-Freq)
boxplot(df_trans$Freq)
qqnorm(df_trans$Freq)

### Valor atípico en la columna lms_utiliza

df_lms  <- as.data.frame(prop.table(table(survey$lms_.utiliza))) %>% arrange(-Freq)
boxplot(df_lms$Freq)
qqnorm(df_lms$Freq)

#los datos no presentan una distribución normal pues se encuentran muy dispersos y al examinar la frecuencia de la información se obtuvo la siguiente información:

df_lms


#Con respecto a la información recolectada se decidió transformar las respuestas de la columna lms_utiliza a expresiones referentes a si conoce moodle o no cambiando el nombre de la columna de lms_utiliza a **conoce_moodle**.


df_lms[df_lms$Var1 %in% c("Moodle","Moodle;Otras","Moodle;Wordpress","Moodle;Wordpress;Otras","Moodle;Canvas LMS;Wordpress","Moodle;Canvas LMS","Moodle;Canvas LMS;Otras") , "categoria" ] <- "Si"
df_lms[df_lms$Var1 %in% c("Otras","Wordpress","Canvas LMS","Canvas LMS;Wordpress","Canvas LMS;Wordpress;Otras"), "categoria"  ] <- "No"

df_lms<-df_lms%>%select(Var1,categoria)

survey<-left_join(survey,df_lms,by=c("lms_.utiliza"="Var1"))

survey<- survey[,!(names(survey) %in% c("lms_.utiliza"))] 

names(survey)[length(names(survey))] <-"conoce_moodle"

survey$conoce_moodle <-  as.factor(survey$conoce_moodle) 

summary(survey$conoce_moodle)

#según los gráficos no se muestran datos atípicos.
df_trans_lms <- as.data.frame(prop.table(table(survey$conoce_moodle))) %>% arrange(-Freq)
boxplot(df_trans_lms$Freq)
qqnorm(df_trans_lms$Freq)
