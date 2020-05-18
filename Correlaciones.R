library(dplyr)
library(ggplot2)
setwd("/")
setwd("C:Users/Pabel/Documents/Angela/paper")


survey<- read.csv("tesis_paper.csv",header = T,sep = ";",encoding = "UTF-8",na.strings = c(""))


#H_O las categorías relacionadas son independientes 
#H_A las categorías relacionadas son dependientes 

#p-value < 0.05 aceptar hipotesis nula


# Sobrecarga laboral en relación con la reprobación estudiantil


#H_O = las categorías de sobrecarga laboral y reprobación son INDEPENDIENTES
#H_A = las categorías de sobrecarga laboral y reprobación son DEPENDIENTES
  
prop.table(table(survey$Sobrecarga_.laboral,survey$Reprobaci.n),1)

ggplot(survey)+
  ggtitle ("Sobrecarga Docente y Reprobación Estudiantil") + 
  theme (plot.title = element_text(
    vjust=2, 
    face="bold",
    lineheight=2
  ),
  axis.text.x = element_text(angle = 45)
  
  ) +
  aes(x= Sobrecarga_.laboral, fill= Reprobaci.n)+
  labs(x = "Sobrecarga Docente",y = "Reprobación Estudiantil") +
  geom_bar(position = "fill")+
  scale_fill_manual(values = c("#64BA8D","#CD6155"))


chisq.test(table(survey$Sobrecarga_.laboral,survey$Reprobaci.n))

#p-value es mayor a 0.05 por lo tanto, rechazamos la hipótesis nula
#las categorías sobrecarga laboral y reprobación son DEPENDIENTES


# Horas libres en relación con demora en los estudios

# H_O = las categorías de horas libres y demora son INDEPENDIENTES
# H_A= las categorías de horas libres y demora son DEPENDIENTES
  
prop.table(table(survey$Horas_.libres,survey$Demora),1)


ggplot(survey)+
  ggtitle ("Horas Libres y Demora Estudiantil") + 
  theme (plot.title = element_text(
    vjust=2, 
    face="bold",
    lineheight=2
  ),
  axis.text.x = element_text(angle = 45)
  
  ) +
  aes(x= Horas_.libres, fill= Demora)+
  labs(x = "Horas Libres",y = "Demora Estudiantil") +
  geom_bar(position = "fill")+
  scale_fill_manual(values = c("#64BA8D","#CD6155"))


chisq.test(table(survey$Horas_.libres,survey$Demora))

#p-value > 0.05 por lo tanto, rechazamos la hipótesis nula
#las categorías horas libres y demora son DEPENDIENTES


# Reprobación en relación a la sobrepoblación

#H_O = las categorías de reprobación y sobrepoblacion son INDEPENDIENTES
#H_A= las categorías de reprobación y sobrepoblacion son DEPENDIENTES

prop.table(table(survey$Reprobaci.n,survey$Sobrepoblaci.n),1)


ggplot(survey)+
  ggtitle ("Reprobación y Sobrepoblación") + 
  theme (plot.title = element_text(
    vjust=2, 
    face="bold",
    lineheight=2
  ),
  axis.text.x = element_text(angle = 45)
  
  ) +
  aes(x= Reprobaci.n, fill= Sobrepoblaci.n)+
  labs(x = "Reprobación",y = "Sobrepoblación") +
  geom_bar(position = "fill")+
  scale_fill_manual(values = c("#64BA8D","#CD6155"))


chisq.test(table(survey$Reprobaci.n,survey$Sobrepoblaci.n))

#p-value > 0.05 por lo tanto, rechazamos la hipótesis nula 
#las categorías reprobación y sobrepoblacion son DEPENDIENTES

### Estudiar una carrera orientada a la informática en relación con el rango académico

#H_O = las categorías de estudio secundaria y rango académico son INDEPENDIENTES
#H_A= las categorías de estudio secundaria y rango académico son DEPENDIENTES
  

prop.table(table(survey$Estudio_.secundaria,survey$Rango_.acad.mico),1)




ggplot(survey)+
  ggtitle ("Estudios de Secundaria y Rango Académico") + 
  theme (plot.title = element_text(
    vjust=2, #Justificación vertical, para separarlo del gráfico
    face="bold",
    lineheight=2
  ),
  axis.text.x = element_text(angle = 45)
  
  ) +
  aes(x= Estudio_.secundaria, fill= Rango_.acad.mico)+
  labs(x = "Estudios de Secundaria",y = "Rango Académico") +
  geom_bar(position = "fill")


chisq.test(table(survey$Estudio_.secundaria,survey$Rango_.acad.mico))

#p-value=0.06342> 0.05 por lo tanto, rechazamos la hipótesis nula
#las categorías estudio de secundaria y rango académico son DEPENDIENTES


# Incremento de la población de estudiantes y rango académico

#H_O = las categorías de aumento de la población  y rango académico son INDEPENDIENTES
#H_A= las categorías de aumento de la población  y rango académico son DEPENDIENTES
  

prop.table(table(survey$Aumento._de._la.poblaci.n,survey$Rango_.acad.mico),1)



ggplot(survey)+
  ggtitle ("Incremento de la Población y Rango Académico") + 
  theme (plot.title = element_text(
    vjust=2, #Justificación vertical, para separarlo del gráfico
    face="bold",
    lineheight=2
  ),
  axis.text.x = element_text(angle = 45)
  
  ) +
  aes(x= Aumento._de._la.poblaci.n, fill= Rango_.acad.mico)+
  labs(x = "Incremento de la Población",y = "Rango Académico") +
  geom_bar(position = "fill")


chisq.test(table(survey$Aumento._de._la.poblaci.n,survey$Rango_.acad.mico))

#p-value> 0.05 por lo tanto, rechazamos la hipótesis nula
#las categorías aumento de la población  y rango académico son DEPENDIENTES