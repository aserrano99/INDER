library(readxl)
library(dplyr)
library(ggplot2)
library(FactoClass)

datos <- read.csv("datos depurados.csv",sep = ",")
datos <- datos[,-1]

#Pregunta 1 ¿Quién diligencia esta encuesta?-----
datos$Pregunta.1.[datos$Pregunta.1.=="No usuario del INDER"]<- "No es usuario del INDER"
datos$Pregunta.1.[datos$Pregunta.1.=="Formador"]<- "Formador del INDER"
datos$Pregunta.1.[datos$Pregunta.1.=="Usuario"]<- "Usuario del INDER"
datos$Pregunta.1.[datos$Pregunta.1.=="No es usuario del INDER"]<- "No usuario INDER"
datos$Pregunta.1.[datos$Pregunta.1.=="Formador del INDER"]<- "Formador INDER"
datos$Pregunta.1.[datos$Pregunta.1.=="Usuario del INDER"]<- "Usuario INDER"

encuestados= data.frame(table(datos$Pregunta.1.))
totaledad <- sum(table(datos$Pregunta.1.))
f1 <- data.frame(
  cat = c("Acudiente", "Formador INDER", "No usuario INDER" ,"Usuario INDER"),
  encuestados= as.numeric(encuestados$Freq),
  porcentajes = round(100*c(as.numeric(encuestados$Freq) /totaledad),2)
)
attach(f1)
win.graph(height = 8, width = 12)
p1= ggplot(f1, aes(x=cat, y=encuestados)) + geom_bar(stat = "identity")
p1 + geom_col(aes(fill =cat)) +
  scale_fill_manual(values = c("green3", "steelblue3", "darkorange", "magenta3", "azure4", "brown"))+
  geom_text(aes(label=paste0(encuestados," ", "", "(", porcentajes, "%",")")),
            vjust = -0.3, color = "black", size=5)+
  theme(plot.title = element_text(family = "serif", # Familia de fuente del título
                                  face = "bold", # Estilo de fuente
                                  color = 1, # Color de la fuente
                                  size = 15, # Tamaño de la fuente
                                  hjust = 0.5, # Ajuste horizontal
                                  vjust = 1, # Ajuste vertical
                                  lineheight = 1, # Espacio entre líneas
                                  margin = margin(20, 0, 0, 0)), # Márgenes (t, r, b, l)
        plot.tag = element_text(face = "italic"), # Personalizar el tag
        plot.title.position = "panel", # Posición del título y del subtítulo ("plot" o "panel")
        plot.tag.position = "bottomleft") +
  facet_grid(~"¿Quién diligencia esta encuesta?")+
  theme(strip.text.x = element_text(size = 16))+theme(axis.text = element_text(size=14))+
  theme(legend.title = element_text(size=15))+guides(fill=guide_legend(title="¿Quién Diligenció? (Total=5890)"))+
  ylab("Encuestados")+xlab("")+theme(legend.text = element_text(size=15))+
  scale_x_discrete(limits = c("Acudiente",
                              "Formador INDER",
                              "No usuario INDER" ,
                              "Usuario INDER"),
                   labels = c("Acudiente" = "Acudiente",
                              "Formador INDER" = "Formador",
                              "No usuario INDER" = "No usuario",
                              "Usuario INDER" = "Usuario"))

#Pregunta 2 Indique su rango de edad (en años)------

encuestados= data.frame(table(datos$Pregunta.2.))
encuestados <- encuestados[c(1,5,2,3,4,6),]
totaledad <- sum(encuestados$Freq)


dataD <- data.frame(
  edad = c("Primera Infancia (0 - 5 Años)", "Segunda Infancia (6 - 12 Años)", 
           "Adolescencia (13 - 17 Años)"  ,"Juventud (18 - 28 Años)",
           "Adultos (29 - 54 Años)","Adulto Mayor (55 años en adelante)"),
  encuestados= as.numeric(encuestados$Freq),
  porcentajes = round(100*c(as.numeric(encuestados$Freq) /totaledad),2)
)

dataD$edad2 <- factor(dataD$edad,levels =c("Primera Infancia (0 - 5 Años)", "Segunda Infancia (6 - 12 Años)", 
                                           "Adolescencia (13 - 17 Años)"  ,"Juventud (18 - 28 Años)",
                                           "Adultos (29 - 54 Años)","Adulto Mayor (55 años en adelante)"))

win.graph(height = 8, width = 12)
gd= ggplot(dataD, aes(x=edad, y=encuestados)) + geom_bar(stat = "identity")
gd + geom_col(aes(fill = edad2)) +
  scale_fill_manual(values = c("brown", "#9932CC", "darkcyan","gold",
                               "chocolate", "#FF1C00"))+
  geom_text(aes(label=paste0(encuestados," ", "", "(", porcentajes, "%",")")),
            vjust = -0.3, color = "black", size=5)+
  theme(plot.title = element_text(family = "serif", # Familia de fuente del título
                                  face = "bold", # Estilo de fuente
                                  color = 1, # Color de la fuente
                                  size = 15, # Tamaño de la fuente
                                  hjust = 0.5, # Ajuste horizontal
                                  vjust = 1, # Ajuste vertical
                                  lineheight = 1, # Espacio entre líneas
                                  margin = margin(20, 0, 0, 0)), # Márgenes (t, r, b, l)
        plot.tag = element_text(face = "italic"), # Personalizar el tag
        plot.title.position = "panel", # Posición del título y del subtítulo ("plot" o "panel")
        plot.tag.position = "bottomleft") +
  facet_grid(~"Total de encuestados por Rango de Edad")+
  scale_x_discrete(limits = c("Primera Infancia (0 - 5 Años)",
                              "Segunda Infancia (6 - 12 Años)",
                              "Adolescencia (13 - 17 Años)" ,"Juventud (18 - 28 Años)",
                              "Adultos (29 - 54 Años)","Adulto Mayor (55 años en adelante)"),
                   labels = c("Primera Infancia (0 - 5 Años)" = "0 a 5",
                              "Segunda Infancia (6 - 12 Años)" = "6 a 12",
                              "Adolescencia (13 - 17 Años)" = "13 a 17",
                              "Juventud (18 - 28 Años)" = "18 a 28",
                              "Adultos (29 - 54 Años)" = "29 a 54",
                              "Adulto Mayor (55 años en adelante)" = "55 o más"))+
  theme(strip.text.x = element_text(size = 16))+theme(axis.text = element_text(size=12))+
  theme(legend.title = element_text(size=15))+guides(fill=guide_legend(title="Rango de edad"))+
  ylab("Encuestados")+xlab("Edad")+theme(legend.text = element_text(size=15))


#Pregunta 3 Género-----

encuestados= data.frame(table(datos$Pregunta.3))
totaledad <- sum(encuestados$Freq)


dataD <- data.frame(
  edad = c("Hombre","Mujer"),
  encuestados= as.numeric(encuestados$Freq),
  porcentajes = round(100*c(as.numeric(encuestados$Freq) /totaledad),2)
)

win.graph(height = 8, width = 9)
gd= ggplot(dataD, aes(x=edad, y=encuestados)) + geom_bar(stat = "identity")
gd + geom_col(aes(fill = edad)) +
  scale_fill_manual(values = c("brown", "#9932CC", "darkcyan","gold",
                               "chocolate", "#FF1C00"))+
  geom_text(aes(label=paste0(encuestados," ", "", "(", porcentajes, "%",")")),
            vjust = -0.3, color = "black", size=5)+
  theme(plot.title = element_text(family = "serif", # Familia de fuente del título
                                  face = "bold", # Estilo de fuente
                                  color = 1, # Color de la fuente
                                  size = 15, # Tamaño de la fuente
                                  hjust = 0.5, # Ajuste horizontal
                                  vjust = 1, # Ajuste vertical
                                  lineheight = 1, # Espacio entre líneas
                                  margin = margin(20, 0, 0, 0)), # Márgenes (t, r, b, l)
        plot.tag = element_text(face = "italic"), # Personalizar el tag
        plot.title.position = "panel", # Posición del título y del subtítulo ("plot" o "panel")
        plot.tag.position = "bottomleft") +
  facet_grid(~"Genéro Menores de Edad")+
  theme(strip.text.x = element_text(size = 16))+theme(axis.text = element_text(size=12))+
  theme(legend.title = element_text(size=15))+guides(fill=guide_legend(title="Género (Total=895)"))+
  ylab("Encuestados")+xlab("Género")+theme(legend.text = element_text(size=15))

#Pregunta 3.1 Identidad de género----

encuestados= data.frame(table(datos$Pregunta.32))
totaledad <- sum(encuestados$Freq)


dataD <- data.frame(
  edad = c("Hombre","Hombre trans","Mujer","Mujer trans","No binario","No deseo responder"),
  encuestados= as.numeric(encuestados$Freq),
  porcentajes = round(100*c(as.numeric(encuestados$Freq) /totaledad),2)
)

win.graph(height = 8, width = 12)
gd= ggplot(dataD, aes(x=edad, y=encuestados)) + geom_bar(stat = "identity")
gd + geom_col(aes(fill = edad)) +
  scale_fill_manual(values = c("brown", "#9932CC", "darkcyan","gold",
                               "chocolate", "#FF1C00"))+
  geom_text(aes(label=paste0(encuestados," ", "", "(", porcentajes, "%",")")),
            vjust = -0.3, color = "black", size=5)+
  theme(plot.title = element_text(family = "serif", # Familia de fuente del título
                                  face = "bold", # Estilo de fuente
                                  color = 1, # Color de la fuente
                                  size = 15, # Tamaño de la fuente
                                  hjust = 0.5, # Ajuste horizontal
                                  vjust = 1, # Ajuste vertical
                                  lineheight = 1, # Espacio entre líneas
                                  margin = margin(20, 0, 0, 0)), # Márgenes (t, r, b, l)
        plot.tag = element_text(face = "italic"), # Personalizar el tag
        plot.title.position = "panel", # Posición del título y del subtítulo ("plot" o "panel")
        plot.tag.position = "bottomleft") +
  facet_grid(~"Genéro Mayores de 18 años")+
  theme(strip.text.x = element_text(size = 16))+theme(axis.text = element_text(size=12))+
  theme(legend.title = element_text(size=15))+guides(fill=guide_legend(title="Género (Total=4995)"))+
  ylab("Encuestados")+xlab("Género")+theme(legend.text = element_text(size=15))

#Pregunta 4 ¿Cuál de las siguientes alternativas define mejor su orientación sexual?----

encuestados= data.frame(table(datos$Pregunta.4))
totaledad <- sum(encuestados$Freq)


dataD <- data.frame(
  edad = c("Bisexual (Atracción hacia ambos sexos)",
           "Gay/Lesbiana (Atracción hacia el mismo sexo)",
           "Heterosexual (Atracción hacia el sexo opuesto)",
           "No deseo responder",
           "Otra"),
  encuestados= as.numeric(encuestados$Freq),
  porcentajes = round(100*c(as.numeric(encuestados$Freq) /totaledad),2)
)

win.graph(height = 8, width = 12)
gd= ggplot(dataD, aes(x=edad, y=encuestados)) + geom_bar(stat = "identity")
gd + geom_col(aes(fill = edad)) +
  scale_fill_manual(values = c("brown", "#9932CC", "darkcyan","gold",
                               "chocolate", "#FF1C00"))+
  geom_text(aes(label=paste0(encuestados," ", "", "(", porcentajes, "%",")")),
            vjust = -0.3, color = "black", size=5)+
  theme(plot.title = element_text(family = "serif", # Familia de fuente del título
                                  face = "bold", # Estilo de fuente
                                  color = 1, # Color de la fuente
                                  size = 15, # Tamaño de la fuente
                                  hjust = 0.5, # Ajuste horizontal
                                  vjust = 1, # Ajuste vertical
                                  lineheight = 1, # Espacio entre líneas
                                  margin = margin(20, 0, 0, 0)), # Márgenes (t, r, b, l)
        plot.tag = element_text(face = "italic"), # Personalizar el tag
        plot.title.position = "panel", # Posición del título y del subtítulo ("plot" o "panel")
        plot.tag.position = "bottomleft") +
  facet_grid(~"¿Cuál de las siguientes alternativas define mejor su orientación sexual?")+
  theme(strip.text.x = element_text(size = 16))+theme(axis.text = element_text(size=12))+
  theme(legend.title = element_text(size=15))+guides(fill=guide_legend(title="Orientación Sexual (Total=4995)"))+
  ylab("Encuestados")+xlab("")+theme(legend.text = element_text(size=15))+
  scale_x_discrete(limits = c("Bisexual (Atracción hacia ambos sexos)",
                              "Gay/Lesbiana (Atracción hacia el mismo sexo)",
                              "Heterosexual (Atracción hacia el sexo opuesto)",
                              "No deseo responder","Otra"),
                   labels = c("Bisexual (Atracción hacia ambos sexos)" = "Bisexual",
                              "Gay/Lesbiana (Atracción hacia el mismo sexo)" = "Gay/Lesbiana",
                              "Heterosexual (Atracción hacia el sexo opuesto)" = "Heterosexual",
                              "No deseo responder" = "No deseo responder",
                              "Otra" = "Otra"))

#Pregunta 5 Actualmente su estado civil es:----

encuestados= data.frame(table(datos$Pregunta.5))
totaledad <- sum(encuestados$Freq)


dataD <- data.frame(
  edad = c("Casado(a)","Soltero(a)"),
  encuestados= as.numeric(encuestados$Freq),
  porcentajes = round(100*c(as.numeric(encuestados$Freq) /totaledad),2)
)

win.graph(height = 8, width = 9)
gd= ggplot(dataD, aes(x=edad, y=encuestados)) + geom_bar(stat = "identity")
gd + geom_col(aes(fill = edad)) +
  scale_fill_manual(values = c("brown", "#9932CC", "darkcyan","gold",
                               "chocolate", "#FF1C00"))+
  geom_text(aes(label=paste0(encuestados," ", "", "(", porcentajes, "%",")")),
            vjust = -0.3, color = "black", size=5)+
  theme(plot.title = element_text(family = "serif", # Familia de fuente del título
                                  face = "bold", # Estilo de fuente
                                  color = 1, # Color de la fuente
                                  size = 15, # Tamaño de la fuente
                                  hjust = 0.5, # Ajuste horizontal
                                  vjust = 1, # Ajuste vertical
                                  lineheight = 1, # Espacio entre líneas
                                  margin = margin(20, 0, 0, 0)), # Márgenes (t, r, b, l)
        plot.tag = element_text(face = "italic"), # Personalizar el tag
        plot.title.position = "panel", # Posición del título y del subtítulo ("plot" o "panel")
        plot.tag.position = "bottomleft") +
  facet_grid(~"Actualmente su estado civil es:")+
  theme(strip.text.x = element_text(size = 16))+theme(axis.text = element_text(size=12))+
  theme(legend.title = element_text(size=15))+guides(fill=guide_legend(title="Estado Civil (Total=4995)"))+
  ylab("Encuestados")+xlab("")+theme(legend.text = element_text(size=15))

#Pregunta 6 ¿De acuerdo con su cultura pueblo o rasgos, es o se reconoce como?----
encuestados= data.frame(table(datos$Pregunta.6))
totaledad <- sum(encuestados$Freq)


dataD <- data.frame(
  edad = c("Afrodescendiente, Mulato o Negro","Blanco","Indígena",
           "Mestizo","Ninguna","Otra","Palenquero","Raizal","ROM / Gitano"),
  encuestados= as.numeric(encuestados$Freq),
  porcentajes = round(100*c(as.numeric(encuestados$Freq) /totaledad),2)
)

win.graph()
gd= ggplot(dataD, aes(x=reorder(edad, desc(encuestados)) , y=encuestados)) + geom_bar(stat = "identity")
gd + geom_col(aes(fill = edad)) +
  scale_fill_manual(values = c("green3", "steelblue3", "darkorange", "magenta3", 
                               "azure4", "brown","orange","pink","red"))+
  geom_text(aes(label=paste0(encuestados," ", "", "(", porcentajes, "%",")")),
            vjust = 0.5,hjust = 0, color = "black", size=4)+
  theme(plot.title = element_text(family = "serif",              # Familia de fuente del título
                                  face = "bold",                 # Estilo de fuente
                                  color = 1,                     # Color de la fuente
                                  size = 15,                     # Tamaño de la fuente
                                  hjust = 0.5,                     # Ajuste horizontal
                                  vjust = 1,                     # Ajuste vertical
                                  lineheight = 1,                # Espacio entre líneas
                                  margin = margin(20, 0, 0, 0)), # Márgenes (t, r, b, l)
        plot.tag = element_text(face = "italic"), # Personalizar el tag
        plot.title.position = "panel", # Posición del título y del subtítulo ("plot" o "panel")
        plot.tag.position = "bottomleft") +
  facet_grid(~"Actualmente su estado civil es:")+
  theme(strip.text.x = element_text(size = 16))+theme(axis.text = element_text(size=12))+
  theme(legend.title = element_text(size=15))+guides(fill=guide_legend(title="Estado Civil (Total=5890)"))+
  ylab("Encuestados")+xlab("")+theme(legend.text = element_text(size=15))+
  coord_flip() + scale_y_continuous(expand = expansion(mult = c(0.0, 0.18)))

#Pregunta 7 Nivel de escolaridad----
encuestados= data.frame(table(datos$Pregunta.7))
totaledad <- sum(encuestados$Freq)


dataD <- data.frame(
  edad = c("Doctorado","Especialización","Maestría",
           "Pregrado","Primaria","Secundaria","Sin estudio","Técnico","Tecnológico"),
  encuestados= as.numeric(encuestados$Freq),
  porcentajes = round(100*c(as.numeric(encuestados$Freq) /totaledad),2)
)

win.graph()
gd= ggplot(dataD, aes(x=reorder(edad, desc(encuestados)) , y=encuestados)) + geom_bar(stat = "identity")
gd + geom_col(aes(fill = edad)) +
  scale_fill_manual(values = c("green3", "steelblue3", "darkorange", "magenta3", 
                               "azure4", "brown","orange","pink","red"))+
  geom_text(aes(label=paste0(encuestados," ", "", "(", porcentajes, "%",")")),
            vjust = 0.5,hjust = 0, color = "black", size=4)+
  theme(plot.title = element_text(family = "serif",              # Familia de fuente del título
                                  face = "bold",                 # Estilo de fuente
                                  color = 1,                     # Color de la fuente
                                  size = 15,                     # Tamaño de la fuente
                                  hjust = 0.5,                     # Ajuste horizontal
                                  vjust = 1,                     # Ajuste vertical
                                  lineheight = 1,                # Espacio entre líneas
                                  margin = margin(20, 0, 0, 0)), # Márgenes (t, r, b, l)
        plot.tag = element_text(face = "italic"), # Personalizar el tag
        plot.title.position = "panel", # Posición del título y del subtítulo ("plot" o "panel")
        plot.tag.position = "bottomleft") +
  facet_grid(~"Nivel de escolaridad")+
  theme(strip.text.x = element_text(size = 16))+theme(axis.text = element_text(size=12))+
  theme(legend.title = element_text(size=15))+guides(fill=guide_legend(title="Escolaridad (Total=5890)"))+
  ylab("Encuestados")+xlab("")+theme(legend.text = element_text(size=15))+
  coord_flip() + scale_y_continuous(expand = expansion(mult = c(0.0, 0.18)))

#-Pregunta 8 Ocupación-----

encuestados= data.frame(table(datos$Pregunta.8))
totaledad <- sum(encuestados$Freq)


dataD <- data.frame(
  edad = c("Ama de casa / hombre de casa","Desempleado (a)","Empleado (a)",
           "Estudiante","No sabe/No responde","Otra","Pensionado (a)/Jubilado (a)"),
  encuestados= as.numeric(encuestados$Freq),
  porcentajes = round(100*c(as.numeric(encuestados$Freq) /totaledad),2)
)

win.graph()
gd= ggplot(dataD, aes(x=reorder(edad, desc(encuestados)) , y=encuestados)) + geom_bar(stat = "identity")
gd + geom_col(aes(fill = edad)) +
  scale_fill_manual(values = c("green3", "steelblue3", "blue", "magenta3", 
                               "azure4", "brown","orange","pink","red"))+
  geom_text(aes(label=paste0(encuestados," ", "", "(", porcentajes, "%",")")),
            vjust = 0.5,hjust = 0, color = "black", size=4)+
  theme(plot.title = element_text(family = "serif",              # Familia de fuente del título
                                  face = "bold",                 # Estilo de fuente
                                  color = 1,                     # Color de la fuente
                                  size = 15,                     # Tamaño de la fuente
                                  hjust = 0.5,                     # Ajuste horizontal
                                  vjust = 1,                     # Ajuste vertical
                                  lineheight = 1,                # Espacio entre líneas
                                  margin = margin(20, 0, 0, 0)), # Márgenes (t, r, b, l)
        plot.tag = element_text(face = "italic"), # Personalizar el tag
        plot.title.position = "panel", # Posición del título y del subtítulo ("plot" o "panel")
        plot.tag.position = "bottomleft") +
  facet_grid(~"Ocupación")+
  theme(strip.text.x = element_text(size = 16))+theme(axis.text = element_text(size=12))+
  theme(legend.title = element_text(size=15))+guides(fill=guide_legend(title="Ocupación (Total=5890)"))+
  ylab("Encuestados")+xlab("")+theme(legend.text = element_text(size=15))+
  coord_flip() + scale_y_continuous(expand = expansion(mult = c(0.0, 0.18)))

#Pregunta 9 Número de hijos (as)----

encuestados= data.frame(table(datos$Pregunta.9))
totaledad <- sum(encuestados$Freq)


dataD <- data.frame(
  edad = c("1","2","3","4 ó más","Ninguno (a)"),
  encuestados= as.numeric(encuestados$Freq),
  porcentajes = round(100*c(as.numeric(encuestados$Freq) /totaledad),2)
)

win.graph(height = 8, width = 12)
gd= ggplot(dataD, aes(x=edad, y=encuestados)) + geom_bar(stat = "identity")
gd + geom_col(aes(fill = edad)) +
  scale_fill_manual(values = c("brown", "#9932CC", "darkcyan","gold",
                               "chocolate", "#FF1C00"))+
  geom_text(aes(label=paste0(encuestados," ", "", "(", porcentajes, "%",")")),
            vjust = -0.3, color = "black", size=5)+
  theme(plot.title = element_text(family = "serif", # Familia de fuente del título
                                  face = "bold", # Estilo de fuente
                                  color = 1, # Color de la fuente
                                  size = 15, # Tamaño de la fuente
                                  hjust = 0.5, # Ajuste horizontal
                                  vjust = 1, # Ajuste vertical
                                  lineheight = 1, # Espacio entre líneas
                                  margin = margin(20, 0, 0, 0)), # Márgenes (t, r, b, l)
        plot.tag = element_text(face = "italic"), # Personalizar el tag
        plot.title.position = "panel", # Posición del título y del subtítulo ("plot" o "panel")
        plot.tag.position = "bottomleft") +
  facet_grid(~"Número de hijos (as)")+
  theme(strip.text.x = element_text(size = 16))+theme(axis.text = element_text(size=12))+
  theme(legend.title = element_text(size=15))+guides(fill=guide_legend(title="Número de hijos (as) (Total=5890)"))+
  ylab("Encuestados")+xlab("")+theme(legend.text = element_text(size=15))

#Pregunta 10 Número de personas con quien vive-----

encuestados= data.frame(table(datos$Pregunta.10))
totaledad <- sum(encuestados$Freq)


dataD <- data.frame(
  edad = c("1","2","3","4 ó más","Solo (a)"),
  encuestados= as.numeric(encuestados$Freq),
  porcentajes = round(100*c(as.numeric(encuestados$Freq) /totaledad),2)
)

win.graph(height = 8, width = 12)
gd= ggplot(dataD, aes(x=edad, y=encuestados)) + geom_bar(stat = "identity")
gd + geom_col(aes(fill = edad)) +
  scale_fill_manual(values = c("brown", "#9932CC", "darkcyan","gold",
                               "chocolate", "#FF1C00"))+
  geom_text(aes(label=paste0(encuestados," ", "", "(", porcentajes, "%",")")),
            vjust = -0.3, color = "black", size=5)+
  theme(plot.title = element_text(family = "serif", # Familia de fuente del título
                                  face = "bold", # Estilo de fuente
                                  color = 1, # Color de la fuente
                                  size = 15, # Tamaño de la fuente
                                  hjust = 0.5, # Ajuste horizontal
                                  vjust = 1, # Ajuste vertical
                                  lineheight = 1, # Espacio entre líneas
                                  margin = margin(20, 0, 0, 0)), # Márgenes (t, r, b, l)
        plot.tag = element_text(face = "italic"), # Personalizar el tag
        plot.title.position = "panel", # Posición del título y del subtítulo ("plot" o "panel")
        plot.tag.position = "bottomleft") +
  facet_grid(~"Número de personas con quien vive")+
  theme(strip.text.x = element_text(size = 16))+theme(axis.text = element_text(size=12))+
  theme(legend.title = element_text(size=15))+guides(fill=guide_legend(title="Número de personas con quien vive (Total=5890)"))+
  ylab("Encuestados")+xlab("")+theme(legend.text = element_text(size=15))


#Pregunta 11 ¿En qué municipio/distrito vive?----

encuestados= data.frame(table(datos$Pregunta.11))
porcp11 = round(100*((encuestados$Freq)/sum(encuestados$Freq)),2)
p11  <- cbind(encuestados,porcp11)
attach(p11)
win.graph()
gd= ggplot(p11, aes(x=reorder(Var1, desc(Freq)) , y=Freq)) + geom_bar(stat = "identity")
gd + geom_col(aes(fill = Var1)) +
  scale_fill_manual(values = c("green3", "steelblue3", "darkorange", "magenta3", 
                               "azure4", "brown","orange","pink","red","cyan"))+
  geom_text(aes(label=paste0(Freq," ", "", "(", porcp11, "%",")")),
            vjust = 0.5,hjust = 0, color = "black", size=4)+
  theme(plot.title = element_text(family = "serif",              # Familia de fuente del título
                                  face = "bold",                 # Estilo de fuente
                                  color = 1,                     # Color de la fuente
                                  size = 15,                     # Tamaño de la fuente
                                  hjust = 0.5,                     # Ajuste horizontal
                                  vjust = 1,                     # Ajuste vertical
                                  lineheight = 1,                # Espacio entre líneas
                                  margin = margin(20, 0, 0, 0)), # Márgenes (t, r, b, l)
        plot.tag = element_text(face = "italic"), # Personalizar el tag
        plot.title.position = "panel", # Posición del título y del subtítulo ("plot" o "panel")
        plot.tag.position = "bottomleft") +
  facet_grid(~"Nivel de escolaridad")+
  theme(strip.text.x = element_text(size = 16))+theme(axis.text = element_text(size=12))+
  theme(legend.title = element_text(size=15))+guides(fill=guide_legend(title="Escolaridad (Total=5890)"))+
  ylab("Encuestados")+xlab("")+theme(legend.text = element_text(size=15))+
  coord_flip() + scale_y_continuous(expand = expansion(mult = c(0.0, 0.18)))

#Pregunta 12 ¿En qué comuna o corregimiento vive?----
encuestados= data.frame(table(datos$Pregunta.12))
porcp12 = round(100*((encuestados$Freq)/sum(encuestados$Freq)),2)
p12  <- cbind(encuestados,porcp12)
attach(p12)
win.graph()
gd= ggplot(p12, aes(x=reorder(Var1, desc(Freq)) , y=Freq)) + geom_bar(stat = "identity")
gd + geom_col(aes(fill = Var1)) +
  scale_fill_manual(values = 1:21)+
  geom_text(aes(label=paste0(Freq," ", "", "(", porcp12, "%",")")),
            vjust = 0.5,hjust = 0, color = "black", size=4)+
  theme(plot.title = element_text(family = "serif",              # Familia de fuente del título
                                  face = "bold",                 # Estilo de fuente
                                  color = 1,                     # Color de la fuente
                                  size = 15,                     # Tamaño de la fuente
                                  hjust = 0.5,                     # Ajuste horizontal
                                  vjust = 1,                     # Ajuste vertical
                                  lineheight = 1,                # Espacio entre líneas
                                  margin = margin(20, 0, 0, 0)), # Márgenes (t, r, b, l)
        plot.tag = element_text(face = "italic"), # Personalizar el tag
        plot.title.position = "panel", # Posición del título y del subtítulo ("plot" o "panel")
        plot.tag.position = "bottomleft") +
  facet_grid(~"¿En qué comuna o corregimiento vive?")+
  theme(strip.text.x = element_text(size = 16))+theme(axis.text = element_text(size=12))+
  theme(legend.title = element_text(size=15))+guides(fill=guide_legend(title="Escolaridad (Total=5890)"))+
  ylab("Encuestados")+xlab("")+theme(legend.text = element_text(size=15))+
  coord_flip() + scale_y_continuous(expand = expansion(mult = c(0.0, 0.18)))+
  theme(legend.position='none')

#Pregunta 13 ¿Cuál es su estrato socio económico?----

encuestados= data.frame(table(datos$Pregunta.13.))
totaledad <- sum(encuestados$Freq)


dataD <- data.frame(
  edad = c("1","2","3","4","5","6","No Aplica"),
  encuestados= as.numeric(encuestados$Freq),
  porcentajes = round(100*c(as.numeric(encuestados$Freq) /totaledad),2)
)

win.graph(height = 8, width = 12)
gd= ggplot(dataD, aes(x=edad, y=encuestados)) + geom_bar(stat = "identity")
gd + geom_col(aes(fill = edad)) +
  scale_fill_manual(values = c("brown", "#9932CC", "darkcyan","gold",
                               "chocolate", "#FF1C00","blue"))+
  geom_text(aes(label=paste0(encuestados," ", "", "(", porcentajes, "%",")")),
            vjust = -0.3, color = "black", size=5)+
  theme(plot.title = element_text(family = "serif", # Familia de fuente del título
                                  face = "bold", # Estilo de fuente
                                  color = 1, # Color de la fuente
                                  size = 15, # Tamaño de la fuente
                                  hjust = 0.5, # Ajuste horizontal
                                  vjust = 1, # Ajuste vertical
                                  lineheight = 1, # Espacio entre líneas
                                  margin = margin(20, 0, 0, 0)), # Márgenes (t, r, b, l)
        plot.tag = element_text(face = "italic"), # Personalizar el tag
        plot.title.position = "panel", # Posición del título y del subtítulo ("plot" o "panel")
        plot.tag.position = "bottomleft") +
  facet_grid(~"¿Cuál es su estrato socio económico?")+
  theme(strip.text.x = element_text(size = 16))+theme(axis.text = element_text(size=12))+
  theme(legend.title = element_text(size=15))+guides(fill=guide_legend(title="Estrato (Total=5890)"))+
  ylab("Encuestados")+xlab("")+theme(legend.text = element_text(size=15))




#Pregunta 15 ¿Padece usted alguna discapacidad?-----

dis <- funcioncita(datos, 19)

attach(dis)


win.graph()
gi= ggplot(dis, aes(x=reorder(Discapacidad, desc(Frecuencia)) , y=Frecuencia)) + 
  geom_bar(stat = "identity")

gi + geom_col(aes(fill = Discapacidad)) +
  scale_fill_manual(values = c("brown", "#9932CC", "darkcyan","gold",
                               "chocolate", "#FF1C00","blue","green"))+
  geom_text(aes(label=paste0(Frecuencia," ", "", "(", porcentajes, "%",")")), 
            vjust= 0.5, hjust = 0, color = "black", size=4)+
  theme(plot.title = element_text(family = "serif",              # Familia de fuente del título
                                  face = "bold",                 # Estilo de fuente
                                  color = 1,                     # Color de la fuente
                                  size = 10,                     # Tamaño de la fuente
                                  hjust = 0.5,                   # Ajuste horizontal
                                  vjust = 1,                     # Ajuste vertical
                                  lineheight = 1,                # Espacio entre líneas
                                  margin = margin(20, 0, 0, 0)), # Márgenes (t, r, b, l)
        plot.tag = element_text(face = "italic"),   # Personalizar el tag
        plot.title.position = "panel",               # Posición del título y del subtítulo ("plot" o "panel")
        plot.tag.position = "bottomleft") + 
  coord_flip() +facet_grid(~"¿Padece usted alguna discapacidad?")+
  xlab("Discapacidades")+ ylab("")+
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.13)))+
  theme(axis.text = element_text(size=12))+labs(color='Discapacidad')+ theme(legend.position = "none")+
  scale_x_discrete(limits = c("1","2","3","4","5","6","7","8"),
                   labels = c("1" = "Discapacidad física",
                              "2" = "Discapacidad visual",
                              "3" = "Discapacidad múltiple",
                              "4" = "Discapacidad auditiva",
                              "5" = "Discapacidad intelectual",
                              "6" = "Discapacidad psicosocial (mental)",
                              "7"= "Sordo ceguera",
                              "8"= "Ninguna"))+
  theme(strip.text.x = element_text(size = 15))
  

#Pregunta 16 ¿Se considera víctima del conflicto armado?----
encuestados= data.frame(table(datos$Pregunta.16))
totaledad <- sum(encuestados$Freq)


dataD <- data.frame(
  edad = c("NO","SI"),
  encuestados= as.numeric(encuestados$Freq),
  porcentajes = round(100*c(as.numeric(encuestados$Freq) /totaledad),2)
)

win.graph(height = 8, width = 9)
gd= ggplot(dataD, aes(x=edad, y=encuestados)) + geom_bar(stat = "identity")
gd + geom_col(aes(fill = edad)) +
  scale_fill_manual(values = c("brown", "#9932CC", "darkcyan","gold",
                               "chocolate", "#FF1C00"))+
  geom_text(aes(label=paste0(encuestados," ", "", "(", porcentajes, "%",")")),
            vjust = -0.3, color = "black", size=5)+
  theme(plot.title = element_text(family = "serif", # Familia de fuente del título
                                  face = "bold", # Estilo de fuente
                                  color = 1, # Color de la fuente
                                  size = 15, # Tamaño de la fuente
                                  hjust = 0.5, # Ajuste horizontal
                                  vjust = 1, # Ajuste vertical
                                  lineheight = 1, # Espacio entre líneas
                                  margin = margin(20, 0, 0, 0)), # Márgenes (t, r, b, l)
        plot.tag = element_text(face = "italic"), # Personalizar el tag
        plot.title.position = "panel", # Posición del título y del subtítulo ("plot" o "panel")
        plot.tag.position = "bottomleft") +
  facet_grid(~"¿Se considera víctima del conflicto armado?")+
  theme(strip.text.x = element_text(size = 16))+theme(axis.text = element_text(size=12))+
  theme(legend.title = element_text(size=15))+guides(fill=guide_legend(title="Victima del Conflicto (Total=5890)"))+
  ylab("Encuestados")+xlab("")+theme(legend.text = element_text(size=15))

#INGRESO----
summary(Ingreso)

#Pregunta 43 ¿Conoce al INDER de Medellín?----
encuestados= data.frame(table(datos$Pregunta.43.))
totaledad <- sum(encuestados$Freq)


dataD <- data.frame(
  edad = c("NO","SI"),
  encuestados= as.numeric(encuestados$Freq),
  porcentajes = round(100*c(as.numeric(encuestados$Freq) /totaledad),2)
)

win.graph(height = 8, width = 9)
gd= ggplot(dataD, aes(x=edad, y=encuestados)) + geom_bar(stat = "identity")
gd + geom_col(aes(fill = edad)) +
  scale_fill_manual(values = c("brown", "#9932CC", "darkcyan","gold",
                               "chocolate", "#FF1C00"))+
  geom_text(aes(label=paste0(encuestados," ", "", "(", porcentajes, "%",")")),
            vjust = -0.3, color = "black", size=5)+
  theme(plot.title = element_text(family = "serif", # Familia de fuente del título
                                  face = "bold", # Estilo de fuente
                                  color = 1, # Color de la fuente
                                  size = 15, # Tamaño de la fuente
                                  hjust = 0.5, # Ajuste horizontal
                                  vjust = 1, # Ajuste vertical
                                  lineheight = 1, # Espacio entre líneas
                                  margin = margin(20, 0, 0, 0)), # Márgenes (t, r, b, l)
        plot.tag = element_text(face = "italic"), # Personalizar el tag
        plot.title.position = "panel", # Posición del título y del subtítulo ("plot" o "panel")
        plot.tag.position = "bottomleft") +
  facet_grid(~"¿Conoce al INDER de Medellín?")+
  theme(strip.text.x = element_text(size = 16))+theme(axis.text = element_text(size=12))+
  theme(legend.title = element_text(size=15))+guides(fill=guide_legend(title="Conoce (Total=5890)"))+
  ylab("Encuestados")+xlab("")+theme(legend.text = element_text(size=15))









#Pregunta 44 ¿Participa en alguno de los programas de la oferta del INDER de Medellín?----

encuestados= data.frame(table(datos$Pregunta.44.))
totaledad <- sum(encuestados$Freq)


dataD <- data.frame(
  edad = c("NO","SI"),
  encuestados= as.numeric(encuestados$Freq),
  porcentajes = round(100*c(as.numeric(encuestados$Freq) /totaledad),2)
)

win.graph(height = 8, width = 9)
gd= ggplot(dataD, aes(x=edad, y=encuestados)) + geom_bar(stat = "identity")
gd + geom_col(aes(fill = edad)) +
  scale_fill_manual(values = c("brown", "#9932CC", "darkcyan","gold",
                               "chocolate", "#FF1C00"))+
  geom_text(aes(label=paste0(encuestados," ", "", "(", porcentajes, "%",")")),
            vjust = -0.3, color = "black", size=5)+
  theme(plot.title = element_text(family = "serif", # Familia de fuente del título
                                  face = "bold", # Estilo de fuente
                                  color = 1, # Color de la fuente
                                  size = 15, # Tamaño de la fuente
                                  hjust = 0.5, # Ajuste horizontal
                                  vjust = 1, # Ajuste vertical
                                  lineheight = 1, # Espacio entre líneas
                                  margin = margin(20, 0, 0, 0)), # Márgenes (t, r, b, l)
        plot.tag = element_text(face = "italic"), # Personalizar el tag
        plot.title.position = "panel", # Posición del título y del subtítulo ("plot" o "panel")
        plot.tag.position = "bottomleft") +
  facet_grid(~"¿Participa en alguno de los programas 
  de la oferta del INDER de Medellín?")+
  theme(strip.text.x = element_text(size = 16))+theme(axis.text = element_text(size=12))+
  theme(legend.title = element_text(size=15))+guides(fill=guide_legend(title="Participa (Total=5603)"))+
  ylab("Encuestados")+xlab("")+theme(legend.text = element_text(size=15))

#Pregunta 45 ¿Cuál es la razón principal por la que no participa de la oferta del INDER- Medellín?----
encuestados= data.frame(table(datos$Pregunta.45.))
porcp45 = round(100*((encuestados$Freq)/sum(encuestados$Freq)),2)
p45  <- cbind(encuestados,porcp45)
attach(p45)
win.graph()
gd= ggplot(p45, aes(x=reorder(Var1, desc(Freq)) , y=Freq)) + geom_bar(stat = "identity")
gd + geom_col(aes(fill = Var1)) +
  scale_fill_manual(values = c("green3", "steelblue3", "darkorange", "magenta3", 
                               "azure4", "brown","orange","pink","red","cyan"))+
  geom_text(aes(label=paste0(Freq," ", "", "(", porcp45, "%",")")),
            vjust = 0.5,hjust = 0, color = "black", size=4)+
  theme(plot.title = element_text(family = "serif",              # Familia de fuente del título
                                  face = "bold",                 # Estilo de fuente
                                  color = 1,                     # Color de la fuente
                                  size = 15,                     # Tamaño de la fuente
                                  hjust = 0.5,                     # Ajuste horizontal
                                  vjust = 1,                     # Ajuste vertical
                                  lineheight = 1,                # Espacio entre líneas
                                  margin = margin(20, 0, 0, 0)), # Márgenes (t, r, b, l)
        plot.tag = element_text(face = "italic"), # Personalizar el tag
        plot.title.position = "panel", # Posición del título y del subtítulo ("plot" o "panel")
        plot.tag.position = "bottomleft") +
  facet_grid(~"¿Cuál es la razón principal por la que no 
  participa de la oferta del INDER- Medellín?")+
  theme(strip.text.x = element_text(size = 16))+theme(axis.text = element_text(size=12))+
  theme(legend.title = element_text(size=15))+guides(fill=guide_legend(title="Escolaridad (Total=5890)"))+
  ylab("Encuestados")+xlab("")+theme(legend.text = element_text(size=15))+
  coord_flip() + scale_y_continuous(expand = expansion(mult = c(0.0, 0.18)))+
  theme(legend.position='none')


#Pregunta 46 ¿A través de qué medio se enteró del programa en el que participa en el INDER de Medellín?----
encuestados= data.frame(table(datos$Pregunta.46.))
porcp46 = round(100*((encuestados$Freq)/sum(encuestados$Freq)),2)
p46  <- cbind(encuestados,porcp46)
p46 <- p46 %>%
  arrange(Freq) %>%
  mutate(Var1=factor(Var1, Var1))

p46$Var1 <- as.character(p46$Var1)
p46$Var1[p46$Var1=="1"]<- "Radio y/o televisión"
p46$Var1[p46$Var1=="2"]<- "Página web del INDER"
p46$Var1[p46$Var1=="3"]<- "Redes sociales"
p46$Var1[p46$Var1=="4"]<- "Amigo o conocido"
p46$Var1[p46$Var1=="5"]<- "Campañas o eventos del INDER"
p46$Var1[p46$Var1=="6"]<- "Personal del INDER"
p46$Var1[p46$Var1=="7"]<- "Escenarios del INDER"
p46$Var1[p46$Var1=="8"]<- "Por otra organización"
p46$Var1[p46$Var1=="9"]<- "Recomendación médica"
p46$Var1[p46$Var1=="10"]<- "Otra"


attach(p46)
win.graph()
gd= ggplot(p46, aes(x= reorder(Var1, desc(Freq)),  y=Freq)) + geom_bar(stat = "identity")
gd + geom_col(aes(fill = Var1)) +
  scale_fill_manual(values = c("green3", "steelblue3", "darkorange", "magenta3", 
                               "azure4", "brown","orange","pink","red","cyan"))+
  geom_text(aes(label=paste0(Freq," ", "", "(", porcp46, "%",")")),
            vjust = 0.5,hjust = 0, color = "black", size=4)+
  theme(plot.title = element_text(family = "serif",              # Familia de fuente del título
                                  face = "bold",                 # Estilo de fuente
                                  color = 1,                     # Color de la fuente
                                  size = 15,                     # Tamaño de la fuente
                                  hjust = 0.5,                     # Ajuste horizontal
                                  vjust = 1,                     # Ajuste vertical
                                  lineheight = 1,                # Espacio entre líneas
                                  margin = margin(20, 0, 0, 0)), # Márgenes (t, r, b, l)
        plot.tag = element_text(face = "italic"), # Personalizar el tag
        plot.title.position = "panel", # Posición del título y del subtítulo ("plot" o "panel")
        plot.tag.position = "bottomleft") +
  facet_grid(~" ¿A través de qué medio se enteró del programa
  en el que participa en el INDER de Medellín?")+
  theme(strip.text.x = element_text(size = 16))+theme(axis.text = element_text(size=12))+
  theme(legend.title = element_text(size=15))+guides(fill=guide_legend(title="Medio (Total=4788)"))+
  ylab("Encuestados")+xlab("")+theme(legend.text = element_text(size=15))+
  coord_flip() + scale_y_continuous(expand = expansion(mult = c(0.0, 0.18)))+
  theme(legend.position='none')
  
#Pregunta 47¿Cuál es su interés al inscribirse en un programa de actividad física, recreativa y/o deportiva del INDER Medellín?-----
int <- funcioncita(datos, 52)
int <- int %>%
  arrange(Frecuencia) %>%
  mutate(Interes=factor(Interes, Interes))

int$Interes <- as.character(int$Interes)
int$Interes[int$Interes=="1"]<- "Me gusta competir en campeonatos"
int$Interes[int$Interes=="2"]<- "Quiero ser deportista profesional"
int$Interes[int$Interes=="3"]<- "Me quiero divertir"
int$Interes[int$Interes=="4"]<- "Para mejorar mi salud física y metal"
int$Interes[int$Interes=="5"]<- "Para socializar"
int$Interes[int$Interes=="6"]<- "Para tener un hobby"
int$Interes[int$Interes=="7"]<- "Para mejorar mi apariencia física"
int$Interes[int$Interes=="8"]<- "Para aprovechar el tiempo libre"


attach(int)


win.graph()
gi= ggplot(int, aes(x=reorder(Interes, desc(Frecuencia)) , y=Frecuencia)) + 
  geom_bar(stat = "identity")

gi + geom_col(aes(fill = Interes)) +
  scale_fill_manual(values = c("brown", "#9932CC", "darkcyan","gold",
                               "chocolate", "#FF1C00","blue","green"))+
  geom_text(aes(label=paste0(Frecuencia," ", "", "(", porcentajes, "%",")")), 
            vjust= 0.5, hjust = 0, color = "black", size=4)+
  theme(plot.title = element_text(family = "serif",              # Familia de fuente del título
                                  face = "bold",                 # Estilo de fuente
                                  color = 1,                     # Color de la fuente
                                  size = 10,                     # Tamaño de la fuente
                                  hjust = 0.5,                   # Ajuste horizontal
                                  vjust = 1,                     # Ajuste vertical
                                  lineheight = 1,                # Espacio entre líneas
                                  margin = margin(20, 0, 0, 0)), # Márgenes (t, r, b, l)
        plot.tag = element_text(face = "italic"),   # Personalizar el tag
        plot.title.position = "panel",               # Posición del título y del subtítulo ("plot" o "panel")
        plot.tag.position = "bottomleft") + 
  coord_flip() +facet_grid(~"¿Cuál es su interés al inscribirse en un programa de actividad física, recreativa y/o deportiva del INDER Medellín?")+
  xlab("Discapacidades")+ ylab("")+
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.13)))+
  theme(axis.text = element_text(size=12))+labs(color='Interes')+ theme(legend.position = "none")+
  theme(strip.text.x = element_text(size = 13)) #lETRA GRANDE TITULO DE LA GRÁFICA 



#Pregunta 48 Durante su presencia en un escenario para el deporte, recreación y actividad física, ¿usted ha observado alguna(s) de la(s) siguiente(s) situación(es)?-----

pre <- funcioncita(datos, 53)

attach(pre)


win.graph()
gi= ggplot(pre, aes(x=reorder(Presencia, desc(Frecuencia)) , y=Frecuencia)) + 
  geom_bar(stat = "identity")

gi + geom_col(aes(fill = Presencia)) +
  scale_fill_manual(values = c("brown", "#9932CC", "darkcyan","gold",
                               "chocolate", "#FF1C00","blue","green","red"))+
  geom_text(aes(label=paste0(Frecuencia," ", "", "(", porcentajes, "%",")")), 
            vjust= 0.5, hjust = 0, color = "black", size=4)+
  theme(plot.title = element_text(family = "serif",              # Familia de fuente del título
                                  face = "bold",                 # Estilo de fuente
                                  color = 1,                     # Color de la fuente
                                  size = 10,                     # Tamaño de la fuente
                                  hjust = 0.5,                   # Ajuste horizontal
                                  vjust = 1,                     # Ajuste vertical
                                  lineheight = 1,                # Espacio entre líneas
                                  margin = margin(20, 0, 0, 0)), # Márgenes (t, r, b, l)
        plot.tag = element_text(face = "italic"),   # Personalizar el tag
        plot.title.position = "panel",               # Posición del título y del subtítulo ("plot" o "panel")
        plot.tag.position = "bottomleft") + 
  coord_flip() +facet_grid(~"Durante su presencia en un escenario para el deporte, recreación y actividad física, ¿usted ha observado alguna(s) de la(s) siguiente(s) situación(es)?")+
  xlab("Discapacidades")+ ylab("")+
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.13)))+
  theme(axis.text = element_text(size=12))+labs(color='Interes')+ theme(legend.position = "none")+
  theme(strip.text.x = element_text(size = 13)) #lETRA GRANDE TITULO DE LA GRÁFICA 









#Pregunta 49-----
dataP49 <- data.frame("frecuencia" = c(datos$Horario,
                                       datos$Escenarios.o.espacios.deportivos,
                                       datos$Implementos,
                                       datos$Participación.comunitaria,
                                       datos$Capacitación.sobre.deporte..recreación.o.salud,
                                       datos$Ampliación.de.oferta,
                                       datos$Formador.o.profesor), 
                      "comp" = c(rep("Horario", 5890), 
                                      rep("Escenarios", 5890),
                                      rep("Implementos", 5890),
                                      rep("Participación", 5890),
                                      rep("Capacitación", 5890),
                                      rep("Ampliación", 5890),
                                      rep("Formador o Profesor", 5890)))

dataP49$frecuencia <- as.factor(dataP49$frecuencia)
attach(dataP49)

win.graph(height = 8, width = 12)
ggplot(dataP49, aes(x = comp, fill = frecuencia)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("brown", "#9932CC", "darkcyan","gold",
                               "chocolate", "#FF1C00","blue","green"))+
  facet_grid(~" ¿Qué tan satisfecho estás frente a los siguientes componentes del deporte, 
  la recreación y la actividad física que realiza en el INDER- Medellín?")+
  theme(strip.text.x = element_text(size = 15))+
  theme(axis.text = element_text(size=10))+
  theme(legend.title = element_text(size=14))+
  theme(legend.text = element_text(size=14))+
  theme(axis.title  = element_text(size=15))+
  ylab("Encuestados")+xlab("")+
  guides(fill=guide_legend(title="Escala de Calificación"))





#Pregunta 50 En general su percepción sobre los escenarios INDER Medellín, es:----
encuestados= data.frame(table(datos$Pregunta.50.))
totaledad <- sum(encuestados$Freq)


dataD <- data.frame(
  edad = c("Buena","Excelente", "Por mejorar","Regular"),
  encuestados= as.numeric(encuestados$Freq),
  porcentajes = round(100*c(as.numeric(encuestados$Freq) /totaledad),2)
)

win.graph(height = 8, width = 12)
gd= ggplot(dataD, aes(x=edad, y=encuestados)) + geom_bar(stat = "identity")
gd + geom_col(aes(fill = edad)) +
  scale_fill_manual(values = c("brown", "#9932CC", "darkcyan","gold",
                               "chocolate", "#FF1C00"))+
  geom_text(aes(label=paste0(encuestados," ", "", "(", porcentajes, "%",")")),
            vjust = -0.3, color = "black", size=5)+
  theme(plot.title = element_text(family = "serif", # Familia de fuente del título
                                  face = "bold", # Estilo de fuente
                                  color = 1, # Color de la fuente
                                  size = 15, # Tamaño de la fuente
                                  hjust = 0.5, # Ajuste horizontal
                                  vjust = 1, # Ajuste vertical
                                  lineheight = 1, # Espacio entre líneas
                                  margin = margin(20, 0, 0, 0)), # Márgenes (t, r, b, l)
        plot.tag = element_text(face = "italic"), # Personalizar el tag
        plot.title.position = "panel", # Posición del título y del subtítulo ("plot" o "panel")
        plot.tag.position = "bottomleft") +
  facet_grid(~"En general su percepción sobre los escenarios INDER Medellín, es:")+
  theme(strip.text.x = element_text(size = 16))+theme(axis.text = element_text(size=12))+
  theme(legend.title = element_text(size=15))+guides(fill=guide_legend(title="Percepción. (Total=4788)"))+
  ylab("Encuestados")+xlab("")+theme(legend.text = element_text(size=15))


#Pregunta 51 ¿Conoce algunas metodologías para la apropiación social de la Política Pública del deporte, la recreación y la actividad física- DRAF?-----
encuestados= data.frame(table(datos$Pregunta.51))
totaledad <- sum(encuestados$Freq)


dataD <- data.frame(
  edad = c("NO","SI"),
  encuestados= as.numeric(encuestados$Freq),
  porcentajes = round(100*c(as.numeric(encuestados$Freq) /totaledad),2)
)

win.graph(height = 8, width = 9)
gd= ggplot(dataD, aes(x=edad, y=encuestados)) + geom_bar(stat = "identity")
gd + geom_col(aes(fill = edad)) +
  scale_fill_manual(values = c("brown", "#9932CC", "darkcyan","gold",
                               "chocolate", "#FF1C00"))+
  geom_text(aes(label=paste0(encuestados," ", "", "(", porcentajes, "%",")")),
            vjust = -0.3, color = "black", size=5)+
  theme(plot.title = element_text(family = "serif", # Familia de fuente del título
                                  face = "bold", # Estilo de fuente
                                  color = 1, # Color de la fuente
                                  size = 15, # Tamaño de la fuente
                                  hjust = 0.5, # Ajuste horizontal
                                  vjust = 1, # Ajuste vertical
                                  lineheight = 1, # Espacio entre líneas
                                  margin = margin(20, 0, 0, 0)), # Márgenes (t, r, b, l)
        plot.tag = element_text(face = "italic"), # Personalizar el tag
        plot.title.position = "panel", # Posición del título y del subtítulo ("plot" o "panel")
        plot.tag.position = "bottomleft") +
  facet_grid(~"¿Conoce algunas metodologías para la apropiación 
  social de la Política Pública del deporte, 
  la recreación y la actividad física- DRAF?")+
  theme(strip.text.x = element_text(size = 16))+theme(axis.text = element_text(size=12))+
  theme(legend.title = element_text(size=15))+guides(fill=guide_legend(title="Conoce (Total=5890)"))+
  ylab("Encuestados")+xlab("")+theme(legend.text = element_text(size=15))

#Pregunta 52 ¿Las metodologías para que la comunidad se apropia del Deporte, la Recreación y la Actividad Física- DRAF, son las adecuadas?-----
encuestados= data.frame(table(datos$Pregunta.52))
totaledad <- sum(encuestados$Freq)


dataD <- data.frame(
  edad = c("NO","NO SÉ","SI"),
  encuestados= as.numeric(encuestados$Freq),
  porcentajes = round(100*c(as.numeric(encuestados$Freq) /totaledad),2)
)

win.graph(height = 8, width = 9)
gd= ggplot(dataD, aes(x=edad, y=encuestados)) + geom_bar(stat = "identity")
gd + geom_col(aes(fill = edad)) +
  scale_fill_manual(values = c("brown", "#9932CC", "darkcyan","gold",
                               "chocolate", "#FF1C00"))+
  geom_text(aes(label=paste0(encuestados," ", "", "(", porcentajes, "%",")")),
            vjust = -0.3, color = "black", size=5)+
  theme(plot.title = element_text(family = "serif", # Familia de fuente del título
                                  face = "bold", # Estilo de fuente
                                  color = 1, # Color de la fuente
                                  size = 15, # Tamaño de la fuente
                                  hjust = 0.5, # Ajuste horizontal
                                  vjust = 1, # Ajuste vertical
                                  lineheight = 1, # Espacio entre líneas
                                  margin = margin(20, 0, 0, 0)), # Márgenes (t, r, b, l)
        plot.tag = element_text(face = "italic"), # Personalizar el tag
        plot.title.position = "panel", # Posición del título y del subtítulo ("plot" o "panel")
        plot.tag.position = "bottomleft") +
  facet_grid(~"¿Las metodologías para que la comunidad se apropia del 
  Deporte, la Recreación y la Actividad Física, son las 
  adecuadas?")+
  theme(strip.text.x = element_text(size = 16))+theme(axis.text = element_text(size=12))+
  theme(legend.title = element_text(size=15))+guides(fill=guide_legend(title="Son adecuadas (Total=5890)"))+
  ylab("Encuestados")+xlab("")+theme(legend.text = element_text(size=15))


#Pregunta 53 ¿Conoce usted las transformaciones sociales propuestas desde el programa Presupuesto Participativo y Planeación Local - PLYPP?-----

encuestados= data.frame(table(datos$Pregunta.53))
totaledad <- sum(encuestados$Freq)


dataD <- data.frame(
  edad = c("NO","SI"),
  encuestados= as.numeric(encuestados$Freq),
  porcentajes = round(100*c(as.numeric(encuestados$Freq) /totaledad),2)
)

win.graph(height = 8, width = 9)
gd= ggplot(dataD, aes(x=edad, y=encuestados)) + geom_bar(stat = "identity")
gd + geom_col(aes(fill = edad)) +
  scale_fill_manual(values = c("brown", "#9932CC", "darkcyan","gold",
                               "chocolate", "#FF1C00"))+
  geom_text(aes(label=paste0(encuestados," ", "", "(", porcentajes, "%",")")),
            vjust = -0.3, color = "black", size=5)+
  theme(plot.title = element_text(family = "serif", # Familia de fuente del título
                                  face = "bold", # Estilo de fuente
                                  color = 1, # Color de la fuente
                                  size = 15, # Tamaño de la fuente
                                  hjust = 0.5, # Ajuste horizontal
                                  vjust = 1, # Ajuste vertical
                                  lineheight = 1, # Espacio entre líneas
                                  margin = margin(20, 0, 0, 0)), # Márgenes (t, r, b, l)
        plot.tag = element_text(face = "italic"), # Personalizar el tag
        plot.title.position = "panel", # Posición del título y del subtítulo ("plot" o "panel")
        plot.tag.position = "bottomleft") +
  facet_grid(~"¿Conoce usted las transformaciones sociales 
  propuestas desde el programa Presupuesto 
  Participativo y Planeación Local - PLYPP?")+
  theme(strip.text.x = element_text(size = 16))+theme(axis.text = element_text(size=12))+
  theme(legend.title = element_text(size=15))+guides(fill=guide_legend(title="Conoce (Total=5890)"))+
  ylab("Encuestados")+xlab("")+theme(legend.text = element_text(size=15))




#Pregunta 54 ¿Estas transformaciones sociales son pertinentes para la comunidad?----

encuestados= data.frame(table(datos$Pregunta.54))
totaledad <- sum(encuestados$Freq)


dataD <- data.frame(
  edad = c("NO","NO SÉ","SI"),
  encuestados= as.numeric(encuestados$Freq),
  porcentajes = round(100*c(as.numeric(encuestados$Freq) /totaledad),2)
)

win.graph(height = 8, width = 9)
gd= ggplot(dataD, aes(x=edad, y=encuestados)) + geom_bar(stat = "identity")
gd + geom_col(aes(fill = edad)) +
  scale_fill_manual(values = c("brown", "#9932CC", "darkcyan","gold",
                               "chocolate", "#FF1C00"))+
  geom_text(aes(label=paste0(encuestados," ", "", "(", porcentajes, "%",")")),
            vjust = -0.3, color = "black", size=5)+
  theme(plot.title = element_text(family = "serif", # Familia de fuente del título
                                  face = "bold", # Estilo de fuente
                                  color = 1, # Color de la fuente
                                  size = 15, # Tamaño de la fuente
                                  hjust = 0.5, # Ajuste horizontal
                                  vjust = 1, # Ajuste vertical
                                  lineheight = 1, # Espacio entre líneas
                                  margin = margin(20, 0, 0, 0)), # Márgenes (t, r, b, l)
        plot.tag = element_text(face = "italic"), # Personalizar el tag
        plot.title.position = "panel", # Posición del título y del subtítulo ("plot" o "panel")
        plot.tag.position = "bottomleft") +
  facet_grid(~"¿Estas transformaciones sociales 
  son pertinentes para la comunidad?")+
  theme(strip.text.x = element_text(size = 16))+theme(axis.text = element_text(size=12))+
  theme(legend.title = element_text(size=15))+guides(fill=guide_legend(title="Son pertinentes (Total=5890)"))+
  ylab("Encuestados")+xlab("")+theme(legend.text = element_text(size=15))




#Cruce
#P2 vs P19----
attach(datos)

win.graph()
ggplot(datos, 
       aes(x = reorder(Pregunta.2., desc(Pregunta.19.)), 
           fill = Pregunta.19.)) + 
  geom_bar(position = "fill") +
  labs(y = "Proporción", x= "Rango de Edad")+facet_grid(~"Realiza alguna actividad física, recreativa y/o deportiva vs Rango de Edad")+
  theme(strip.text.x = element_text(size = 15))+
  guides(fill=guide_legend(title="Realiza Ejercicio"))+theme(legend.text = element_text(size=12))+
  theme(legend.title = element_text(size=15)) +theme(axis.text = element_text(size=15))
  
#P2 VS P44----
attach(datos)

win.graph()
ggplot(datos, 
       aes(x = reorder(Pregunta.2., desc(Pregunta.44.)), 
           fill = Pregunta.44.)) + 
  geom_bar(position = "fill") +
  labs(y = "Proporción", x= "Rango de Edad")+facet_grid(~"¿Participa en alguno de los programas de la oferta del INDER de Medellín?  vs Rango de Edad")+
  theme(strip.text.x = element_text(size = 15))+
  guides(fill=guide_legend(title="Participa"))+theme(legend.text = element_text(size=12))+
  theme(legend.title = element_text(size=15)) +theme(axis.text = element_text(size=15))


#P4 vs p17-----

attach(datos)

win.graph()
ggplot(datos, 
       aes(x = reorder(Pregunta.4, desc(Pregunta.17)), 
           fill = Pregunta.17)) + 
  geom_bar(position = "fill") +
  labs(y = "Proporción", x= "")+facet_grid(~"¿Participa en alguno de los programas de la oferta del INDER de Medellín?  vs Orientación Sexual")+
  theme(strip.text.x = element_text(size = 15))+
  guides(fill=guide_legend(title="Participa"))+theme(legend.text = element_text(size=12))+
  theme(legend.title = element_text(size=15)) +theme(axis.text = element_text(size=15))+
  scale_x_discrete(limits = c("Bisexual (Atracción hacia ambos sexos)",
                              "Gay/Lesbiana (Atracción hacia el mismo sexo)",
                              "Heterosexual (Atracción hacia el sexo opuesto)",
                              "No deseo responder","Otra"),
                   labels = c("Bisexual (Atracción hacia ambos sexos)" = "Bisexual",
                              "Gay/Lesbiana (Atracción hacia el mismo sexo)" = "Gay/Lesbiana",
                              "Heterosexual (Atracción hacia el sexo opuesto)" = "Heterosexual",
                              "No deseo responder" = "No deseo responder",
                              "Otra" = "Otra"))







#P13 vs P17-----
attach(datos)

win.graph()
ggplot(datos, 
       aes(x = Pregunta.13., 
           fill = Pregunta.17)) + 
  geom_bar(position = "fill") +
  labs(y = "Proporción", x= "Estrato")+facet_grid(~"¿Participa en alguno de los programas de la oferta del INDER de Medellín?  vs Estrato Socio Economico")+
  theme(strip.text.x = element_text(size = 15))+
  guides(fill=guide_legend(title="Participa"))+theme(legend.text = element_text(size=12))+
  theme(legend.title = element_text(size=15)) +theme(axis.text = element_text(size=15))



#boxplot-----

win.graph()
ggplot(datos, aes(x = as.character(Pregunta.13.), 
                  y = as.numeric(Ingreso),fill=Pregunta.13.)) +
  geom_boxplot() + ylab("Ingreso") + 
  xlab("Estrato") + facet_grid(~"Estrato Socio Economico vs Ingreso") +
  theme(strip.text.x = element_text(size = 15))+ylim(0,10000000)+
  guides(fill=guide_legend(title="Estrato"))+
  theme(axis.text = element_text(size=13))+
  theme(legend.title = element_text(size=15))+
  theme(legend.text = element_text(size=15))+
  theme(axis.title = element_text(size=15)) 
  
