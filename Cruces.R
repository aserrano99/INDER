# Librerias ---------------------------------------------

library(readxl)
library(ggplot2)
library(dplyr)

# Datos -------------------------------------------------

#Lectura de datos
datos <- read.csv("datos depurados.csv")

#Eliminar variables
datos <- datos[,-c(1)]

#Etnia vs INDER -----------------------------------------

datos$Pregunta.1.[datos$Pregunta.1.=="No usuario del INDER"]<- "No es usuario del INDER"
datos$Pregunta.1.[datos$Pregunta.1.=="Formador"]<- "Formador del INDER"
datos$Pregunta.1.[datos$Pregunta.1.=="Usuario"]<- "Usuario del INDER"
datos$Pregunta.1.[datos$Pregunta.1.=="No es usuario del INDER"]<- "No usuario INDER"
datos$Pregunta.1.[datos$Pregunta.1.=="Formador del INDER"]<- "Formador INDER"
datos$Pregunta.1.[datos$Pregunta.1.=="Usuario del INDER"]<- "Usuario INDER"

attach(datos)

win.graph(height = 8, width = 12)
ggplot(datos, aes(x = Pregunta.6, fill = Pregunta.44.)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("green3", "steelblue3", "darkorange", "azure4"))+
  facet_grid(~"Etnia vs Participa en la oferta del INDER")+
  theme(strip.text.x = element_text(size = 15))+
  theme(axis.text = element_text(size=10))+
  theme(legend.title = element_text(size=14))+
  theme(legend.text = element_text(size=14))+
  theme(axis.title  = element_text(size=15))+
  ylab("Porcentaje de encuestados")+xlab("Etnia")+
  guides(fill = guide_legend(title = "Participa en la oferta del INDER"))

#Deporte vs Usuario -----------------------------------------

win.graph(height = 8, width = 12)
ggplot(datos, aes(x = Pregunta.1., fill = Pregunta.19.)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("green3", "steelblue3"))+
  facet_grid(~"Practica deporte vs Tipo de usuario")+
  theme(strip.text.x = element_text(size = 15))+
  theme(axis.text = element_text(size=12))+
  theme(legend.title = element_text(size=14))+
  theme(legend.text = element_text(size=14))+
  theme(axis.title  = element_text(size=15))+
  ylab("Porcentaje de encuestados")+xlab("Tipo de usuario")+
  guides(fill = guide_legend(title = "Practica deporte"))

#Consideracion vs IMC ---------------------------------------

attach(datos)

win.graph()
ggplot(datos, aes(x = as.character(Pregunta.38), 
                  y = as.numeric(IMC), fill = Pregunta.38)) +
  geom_boxplot() + ylab("IMC") + ylim(0, 75)+
  xlab("Consideración de peso") + 
  facet_grid(~"Consideración de peso vs IMC") +
  theme(strip.text.x = element_text(size = 15))+
  theme(axis.title  = element_text(size=15))+
  guides(fill = guide_legend(title = "Consideración de peso"))

#Agua vs IMC ---------------------------------------

attach(datos)

win.graph()
ggplot(datos, aes(x = as.character(Pregunta.32), 
                  y = as.numeric(IMC), fill = Pregunta.32)) +
  geom_boxplot() + ylab("IMC") + ylim(0, 75)+
  xlab("Consideración de peso") + 
  facet_grid(~"Consideración de peso vs IMC") +
  theme(strip.text.x = element_text(size = 15))+
  theme(axis.title  = element_text(size=15))+
  guides(fill = guide_legend(title = "Consideración de peso"))
