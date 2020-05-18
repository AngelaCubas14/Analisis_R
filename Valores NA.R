library(dplyr)
library(ggplot2)
setwd("/")
setwd("C:Users/Pabel/Documents/Angela/paper")

survey <- read.csv("tesis_paper.csv",header = T,sep = ",",encoding = "UTF-8",na.strings = c(""))

# TRATAMIENTO DE VALORES NA


na.survey <- c()	 #creamos el vector para los valores NA

for (myname in names(survey)){
  en<-as.data.frame(prop.table(table(is.na(survey[,myname]))))
  operacion <- en %>% filter(Var1==TRUE) %>% select(Freq)
  length(operacion$Freq)
  df_temp <- data.frame(
    column.name=c(myname),
    na.percentage=ifelse(length(operacion$Freq)==0,0,operacion$Freq[1])
  )
  na.survey <- rbind(na.survey,df_temp)
}
#valores NA
na.survey %>% arrange(-na.percentage) %>% filter(na.percentage > 0)


#Realizaremos el tratamiento correspondiente para los valores NA obtenidos de las variables:
  

### Imputación calidad conexión


summary(survey$Conexi.n_.permanente)
summary(survey$Calidad_.conexi.n)


#Respecto a la variable Calidad_.conexi.n imputamos los valores NA con valores NO 

survey[is.na(survey$Calidad_.conexi.n), "Calidad_.conexi.n"] <-"No"

summary(survey$Calidad_.conexi.n)


# Imputacion conoce Moodle

summary(survey$conoce_moodle)


#Reemplazaremos las incidencias NA de la variable conoce_moodle por NO 

survey[is.na(survey$conoce_moodle), "conoce_moodle"] <- "No" ## cambiando apariciones NA por NO

summary(survey$conoce_moodle)

# Tratamiento de estimación de horas libres

summary(survey$Horas_.libres)
summary(survey$Estimaci.n_.horas_.libres)


#El tratamiento que se realizó con esta variable fue reemplazar las ocurrencias NA por Ninguna 


survey$Estimaci.n_.horas_.libres<- as.character(survey$Estimaci.n_.horas_.libres)
survey[is.na(survey$Estimaci.n_.horas_.libres),"Estimaci.n_.horas_.libres"] <- "Ninguna"
survey$Estimaci.n_.horas_.libres<- as.factor(survey$Estimaci.n_.horas_.libres)

summary(survey$Estimaci.n_.horas_.libres)



# Tratamiento estimacion del uso de LMS


summary(survey$Uso_.lms)

survey$Estimaci.n_.lms<- as.factor(survey$Estimaci.n_.lms)
summary(survey$Estimaci.n_.lms)


#En relación a la variable Estimaci.n_.lms esta dependía de la variable Uso_.lms, 
#si la respuesta a la variable era NO seguía con las demás preguntas, 
#el tratamiento que realizamos fue reemplazar las ocurrencias NA por No utilizó LMS 

survey$Estimaci.n_.lms<- as.character(survey$Estimaci.n_.lms)
survey[is.na(survey$Estimaci.n_.lms), "Estimaci.n_.lms"] <- "No utilizó LMS"
survey$Estimaci.n_.lms<- as.factor(survey$Estimaci.n_.lms)

summary(survey$Estimaci.n_.lms)





