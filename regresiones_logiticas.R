library(dplyr)
library(ggplot2)
library(caret)
setwd("/")
setwd("C:Users/Pabel/Documents/Angela/paper")


survey<- read.csv("tesis_paper.csv",header = T,sep = ";",encoding = "UTF-8",na.strings = c(""))


## REGRESIONES LOGÍSTICAS

#Uso_LMS utilizaremos para hacer una regresión logística
#con el objetivo de identificar las variables influyentes en este aspecto, 
#puesto que la propuesta de solución tecnológica es utilizar un LMS para contribuir en el rendimiento del estudiante

# el 24% de los encuestados afirma no haber utilizado ningun LMS en sus clases

prop.table(table(survey$Uso_.lms)) 

#Cargamos las variables de acompañamiento junto con la variable de peso para la  regresión logística 
  

features <- c(
  "Computadora_.permanente",
  "Conexión_.permanente",
  "Rango_.promedio_.clases",
  "Horas_.diarias_.estudio",
  "Disciplina",
  "Autodidacta",
  "Plataforma",
  "Frecuencia_.de_.uso", 
  "Recursos",
  "Uso_.lms"
)
#creamos un nuevo vector set para cargar las variables que solo vamos usar
set <- survey[, names(survey) %in% features ]

set$Uso_.lms <- as.factor(set$Uso_.lms)

#Aplicamos la regresión logística y cargamos el resultado en vector model
model <- glm(Uso_.lms ~ ., data = set, family = "binomial")

#imprimimos el vector model para ver los resultados 
model   

#Podemos apreciar el orden del coeficiente de importancia de cada variable de acompañamiento.


Importancia <- varImp(model)

Importancia$col <- row.names(Importancia)

Importancia <- Importancia %>% arrange(-Overall)  

Importancia

## Correlación entre disciplina  y uso de LMS
  


ggplot(survey)+
  ggtitle ("Correlación: Disciplina  y Uso de LMS") + 
  theme (plot.title = element_text(
    vjust=2, #Justificación vertical, para separarlo del gráfico
    face="bold",
    lineheight=2
  ),
  axis.text.x = element_text(angle = 45)
  
  ) +
  aes(x= Disciplina, fill= Uso_.lms)+
  geom_bar(position = "fill")+
  labs(x = "Disciplina",y = "Uso de LMS") +
  scale_fill_manual(values = c("#CD6155","#64BA8D"))

#Como lo indica el grafico aquellos estudiantes que mencionaron que no son disciplinados son los 
#que tienen más riesgo que a la hora de implementar una plataforma de LMS no incida en su rendimiento académico 
#puesto que no tienen una buena práctica para cumplir con sus deberes como estudiante.

# Correlación entre computadora permanente  y uso del LMS


ggplot(survey)+
  ggtitle ("Correlación: Computadora Permanente  y Uso de LMS") + 
  theme (plot.title = element_text(
    vjust=2, #Justificación vertical, para separarlo del gráfico
    face="bold",
    lineheight=2
  ),
  axis.text.x = element_text(angle = 45)
  
  ) +
  aes(x=Computadora_.permanente, fill= Uso_.lms)+
  geom_bar(position = "fill")+
  labs(x = "Computadora Permanente",y = "Uso de LMS") +
  scale_fill_manual(values = c("#CD6155","#64BA8D"))

#La grafica nos indica que tenemos que poner más atención a aquellos estudiantes que señalan 
#que no tiene computadora permanente pues ellos no tendrían las herramientas necesarias para el uso de una plataforma LMS.

# Correlación entre rango promedio de clases por periodo y uso de LMS 


ggplot(survey)+
  ggtitle ("Correlación: Rango de Clases por Periodo y Uso de LMS") + 
  theme (plot.title = element_text(
    vjust=2, #Justificación vertical, para separarlo del gráfico
    face="bold",
    lineheight=2
  ),
  axis.text.x = element_text(angle = 45)
  
  ) +
  aes(x= Rango_.promedio_.clases, fill= Uso_.lms)+
  geom_bar(position = "fill")+
  labs(x = "Rango de Clases por Periodo",y = "Uso de LMS") +
  scale_fill_manual(values = c("#CD6155","#64BA8D"))

#Como se puede apreciar en la gráfica a medida que van aumentando el número de clases matriculadas 
#por periodo aumenta el riesgo de que se logre mejorar el rendimiento del estudiante por medio de 
#una plataforma LMS debido a que estaría sobre cargado. 

# Correlación entre ser autodidacta y uso de LMS


ggplot(survey)+
  ggtitle ("Correlación: Autodidacta y Uso de LMS") + 
  theme (plot.title = element_text(
    vjust=2, #Justificación vertical, para separarlo del gráfico
    face="bold",
    lineheight=2
  ),
  axis.text.x = element_text(angle = 45)
  
  ) +
  aes(x= Autodidacta, fill= Uso_.lms)+
  geom_bar(position = "fill")+
  labs(x = "Autodidacta",y = "Uso de LMS") +
  scale_fill_manual(values = c("#CD6155","#64BA8D"))

#se puede apreciar en la gráfica que aquellos estudiantes que no son autodidactas corren más riesgo
#de no mejorar su rendimiento mediante el uso de plataforma LMS

# Correlación entre horas diarias de estudio y uso de LMS


ggplot(survey)+
  ggtitle ("Correlación: Horas de Estudio y Uso del LMS") + 
  theme (plot.title = element_text(
    vjust=2, #Justificación vertical, para separarlo del gráfico
    face="bold",
    lineheight=2
  ),
  axis.text.x = element_text(angle = 45)
  
  ) +
  aes(x=Horas_.diarias_.estudio , fill= Uso_.lms)+
  geom_bar(position = "fill")+
  labs(x = "Horas de Estudio",y = "Uso de LMS") +
  scale_fill_manual(values = c("#CD6155","#64BA8D"))

#Se puede notar en la gráfica que esta variable no produce una buena interpretación debido a que 
#nos indica un comportamiento desigual entre el número de horas que dedica a estudiar y el uso de la plataforma LMS.


# Correlación aumento de recursos y uso de LMS 


ggplot(survey)+
  ggtitle ("Correlación: Aumento de Recursos y Uso del LMS") + 
  theme (plot.title = element_text(
                                  vjust=2, #Justificación vertical, para separarlo del gráfico
                                  face="bold",
                                  lineheight=2
                                  ),
         axis.text.x = element_text(angle = 45)
         
         ) +
  aes(x= Recursos, fill= Uso_.lms)+
  geom_bar(position = "fill")+
  labs(x = "Aumento de Recursos",y = "Uso de LMS") +
  scale_fill_manual(values = c("#CD6155","#64BA8D"))

#La grafica nos indica que aquellos que consideran que un aumento de los recursos 
#tanto de infraestructura como de personal no ayudaría a mejorar el rendimiento académico 
#son los que tienen más riesgo de no poder mejorar su rendimiento mediante el uso de una plataforma LMS. 





