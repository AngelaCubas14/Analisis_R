library(dplyr)
library(ggplot2)
setwd("/")
setwd("C:Users/Pabel/Documents/Angela/paper")


survey<- read.csv("tesis_paper.csv",header = T,sep = ";",encoding = "UTF-8",na.strings = c(""))

# Edad

df_edad  <- as.data.frame(prop.table(table(survey$Edad))) %>% arrange(-Freq)

df_edad

#podemos observar que el 60% de los encuestados se encuentran en un rango de edad entre los 23-27 años.

# Genero

df_genero  <- as.data.frame(prop.table(table(survey$Genero))) %>% arrange(-Freq)

df_genero


#vemos que el 68% de los encuestados son hombres, mostrando el predominio de los hombres en la cerrera de ingeniería en sistemas.

## Reprobación 

df_reprobado <- as.data.frame(prop.table(table(survey$Reprobaci.n))) %>% arrange(-Freq)
df_reprobado


#vemos que aproximadamente el 7% de los estudiantes admiten que han reprobado en al menos una ocasión presentado una estadística alarmante contra el 23% que no han reprobado en ninguna ocasión.

boxplot(df_reprobado$Freq)


#Observamos que no se presentan valores atípicos en la información.


#Excelencia académica

df_exce <- as.data.frame(prop.table(table(survey$Exelencia_academica))) %>% arrange(-Freq)
df_exce

#observamos que el aproximadamente el 68% de los estudiantes han sido excelencia academia al menos una vez en el transcurso de su carrera

# Rango académico 


df_academico<- as.data.frame(prop.table(table(survey$Rango_.acad.mico))) %>% arrange(-Freq)
df_academico


#La mayoría de los encuestados presentan un índice académico en un rango de 70 a 79 %

#el grafico boxplot no muestra valores atipicos

boxplot(df_academico$Freq)

# Lista de espera

df_espera <- as.data.frame(prop.table(table(survey$Lista_.espera))) %>% arrange(-Freq)

df_espera

# el 83% de los encuestados han estado en lista de espera en al menos una ocasión en el transcurso de su carrera, 
# el 97% de los encuestados argumentan que se necesitan la apertura de más cupos

df_cupos<-as.data.frame(prop.table(table(survey$Mas_.cupos))) %>% arrange(-Freq)
df_cupos
