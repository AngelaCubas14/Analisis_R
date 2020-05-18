library(dplyr)
library(ggplot2)
setwd("/")
setwd("C:Users/Pabel/Documents/Angela/paper")


survey<- read.csv("tesis_paper.csv",header = T,sep = ";",encoding = "UTF-8",na.strings = c(""))


#H_O las categor�as relacionadas son independientes 
#H_A las categor�as relacionadas son dependientes 

#p-value < 0.05 aceptar hipotesis nula


# Sobrecarga laboral en relaci�n con la reprobaci�n estudiantil


#H_O = las categor�as de sobrecarga laboral y reprobaci�n son INDEPENDIENTES
#H_A = las categor�as de sobrecarga laboral y reprobaci�n son DEPENDIENTES
  
prop.table(table(survey$Sobrecarga_.laboral,survey$Reprobaci.n),1)

ggplot(survey)+
  ggtitle ("Sobrecarga Docente y Reprobaci�n Estudiantil") + 
  theme (plot.title = element_text(
    vjust=2, 
    face="bold",
    lineheight=2
  ),
  axis.text.x = element_text(angle = 45)
  
  ) +
  aes(x= Sobrecarga_.laboral, fill= Reprobaci.n)+
  labs(x = "Sobrecarga Docente",y = "Reprobaci�n Estudiantil") +
  geom_bar(position = "fill")+
  scale_fill_manual(values = c("#64BA8D","#CD6155"))


chisq.test(table(survey$Sobrecarga_.laboral,survey$Reprobaci.n))

#p-value es mayor a 0.05 por lo tanto, rechazamos la hip�tesis nula
#las categor�as sobrecarga laboral y reprobaci�n son DEPENDIENTES


# Horas libres en relaci�n con demora en los estudios

# H_O = las categor�as de horas libres y demora son INDEPENDIENTES
# H_A= las categor�as de horas libres y demora son DEPENDIENTES
  
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

#p-value > 0.05 por lo tanto, rechazamos la hip�tesis nula
#las categor�as horas libres y demora son DEPENDIENTES


# Reprobaci�n en relaci�n a la sobrepoblaci�n

#H_O = las categor�as de reprobaci�n y sobrepoblacion son INDEPENDIENTES
#H_A= las categor�as de reprobaci�n y sobrepoblacion son DEPENDIENTES

prop.table(table(survey$Reprobaci.n,survey$Sobrepoblaci.n),1)


ggplot(survey)+
  ggtitle ("Reprobaci�n y Sobrepoblaci�n") + 
  theme (plot.title = element_text(
    vjust=2, 
    face="bold",
    lineheight=2
  ),
  axis.text.x = element_text(angle = 45)
  
  ) +
  aes(x= Reprobaci.n, fill= Sobrepoblaci.n)+
  labs(x = "Reprobaci�n",y = "Sobrepoblaci�n") +
  geom_bar(position = "fill")+
  scale_fill_manual(values = c("#64BA8D","#CD6155"))


chisq.test(table(survey$Reprobaci.n,survey$Sobrepoblaci.n))

#p-value > 0.05 por lo tanto, rechazamos la hip�tesis nula 
#las categor�as reprobaci�n y sobrepoblacion son DEPENDIENTES

### Estudiar una carrera orientada a la inform�tica en relaci�n con el rango acad�mico

#H_O = las categor�as de estudio secundaria y rango acad�mico son INDEPENDIENTES
#H_A= las categor�as de estudio secundaria y rango acad�mico son DEPENDIENTES
  

prop.table(table(survey$Estudio_.secundaria,survey$Rango_.acad.mico),1)




ggplot(survey)+
  ggtitle ("Estudios de Secundaria y Rango Acad�mico") + 
  theme (plot.title = element_text(
    vjust=2, #Justificaci�n vertical, para separarlo del gr�fico
    face="bold",
    lineheight=2
  ),
  axis.text.x = element_text(angle = 45)
  
  ) +
  aes(x= Estudio_.secundaria, fill= Rango_.acad.mico)+
  labs(x = "Estudios de Secundaria",y = "Rango Acad�mico") +
  geom_bar(position = "fill")


chisq.test(table(survey$Estudio_.secundaria,survey$Rango_.acad.mico))

#p-value=0.06342> 0.05 por lo tanto, rechazamos la hip�tesis nula
#las categor�as estudio de secundaria y rango acad�mico son DEPENDIENTES


# Incremento de la poblaci�n de estudiantes y rango acad�mico

#H_O = las categor�as de aumento de la poblaci�n  y rango acad�mico son INDEPENDIENTES
#H_A= las categor�as de aumento de la poblaci�n  y rango acad�mico son DEPENDIENTES
  

prop.table(table(survey$Aumento._de._la.poblaci.n,survey$Rango_.acad.mico),1)



ggplot(survey)+
  ggtitle ("Incremento de la Poblaci�n y Rango Acad�mico") + 
  theme (plot.title = element_text(
    vjust=2, #Justificaci�n vertical, para separarlo del gr�fico
    face="bold",
    lineheight=2
  ),
  axis.text.x = element_text(angle = 45)
  
  ) +
  aes(x= Aumento._de._la.poblaci.n, fill= Rango_.acad.mico)+
  labs(x = "Incremento de la Poblaci�n",y = "Rango Acad�mico") +
  geom_bar(position = "fill")


chisq.test(table(survey$Aumento._de._la.poblaci.n,survey$Rango_.acad.mico))

#p-value> 0.05 por lo tanto, rechazamos la hip�tesis nula
#las categor�as aumento de la poblaci�n  y rango acad�mico son DEPENDIENTES