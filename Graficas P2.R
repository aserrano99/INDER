# Librerias ----------------------------------------------

library(readxl)
library(ggplot2)
library(dplyr)

# Datos -------------------------------------------------

#Lectura de datos
datosD <- read.csv("datos depurados.csv")
#Eliminar variables
datosD <- datosD[,-c(1)]

#Lectura de datos de la comuna 1
com1 <- read.csv("Comuna 1-Popular.csv")
#Eliminar variables
com1 <- com1[,-c(1)]

#Lectura de datos de la comuna 9
com9 <- read.csv("Comuna 9-Buenos Aires.csv")
#Eliminar variables
com9 <- com9[,-c(1)]

# Funciones para sacar porcentaje -----------------------

#Funcion mejorada
fun4 <- function(vector){
  total <- sum(vector)
  round((vector/total)*100, 2)
}

# Graficas ----------------------------------------------

# Segunda seccion ---------------------------------------

# P17 ---------------------------------------------------

tab <- as.data.frame(table(datos[,22]))

dataP17 <- data.frame(
  bienestar = tab$Var1,
  encuestados = tab$Freq,
  porcentajes = fun4(tab$Freq)
)

win.graph(height = 8, width = 12)
g17= ggplot(dataP17, aes(x=bienestar, y=encuestados)) + 
  geom_bar(stat = "identity")

g17 + geom_col(aes(fill = bienestar)) +
  scale_fill_manual(values = c("green3", "steelblue3", "darkorange"))+
  geom_text(aes(label=paste0(encuestados," ", "", "(", porcentajes, "%",")")), 
            vjust = -0.3, color = "black", size=5)+
  theme(plot.title = element_text(family = "serif",              # Familia de fuente del título
                                  face = "bold",                 # Estilo de fuente
                                  color = 1,                     # Color de la fuente
                                  size = 15,                     # Tamaño de la fuente
                                  hjust = 0.5,                   # Ajuste horizontal
                                  vjust = 1,                     # Ajuste vertical
                                  lineheight = 1,                # Espacio entre líneas
                                  margin = margin(20, 0, 0, 0)), # Márgenes (t, r, b, l)
        plot.tag = element_text(face = "italic"),   # Personalizar el tag
        plot.title.position = "panel",              # Posición del título y del subtítulo ("plot" o "panel")
        plot.tag.position = "bottomleft") + 
  facet_grid(~"Al participar en los programas del INDER, \n ¿percibe aumento en su bienestar?")+
  theme(strip.text.x = element_text(size = 15))+
  theme(axis.text = element_text(size=12))+
  theme(legend.title = element_text(size=14))+
  theme(legend.text = element_text(size=14))+
  ylab("Encuestados")+xlab("Percibe aumento en su bienestar")+
  guides(fill = guide_legend(title = "Bienestar (Total=125)"))

# P18 ---------------------------------------------------

dataP18 <- funcioncita(datos, 23)

win.graph(height = 8, width = 12)
g18= ggplot(dataP18, aes(x=reorder(Bienestar, desc(Frecuencia)), y=Frecuencia)) + 
  geom_bar(stat = "identity")

g18 + geom_col(aes(fill = Bienestar)) +
  scale_fill_manual(values = c("green3", "steelblue3", "darkorange"))+
  geom_text(aes(label=paste0(Frecuencia," ", "", "(", porcentajes, "%",")")), 
            vjust = -0.3, color = "black", size=5)+ 
  theme(plot.title = element_text(family = "serif",              # Familia de fuente del título
                                  face = "bold",                 # Estilo de fuente
                                  color = 1,                     # Color de la fuente
                                  size = 15,                     # Tamaño de la fuente
                                  hjust = 0.5,                   # Ajuste horizontal
                                  vjust = 1,                     # Ajuste vertical
                                  lineheight = 1,                # Espacio entre líneas
                                  margin = margin(20, 0, 0, 0)), # Márgenes (t, r, b, l)
        plot.tag = element_text(face = "italic"),   # Personalizar el tag
        plot.title.position = "panel",              # Posición del título y del subtítulo ("plot" o "panel")
        plot.tag.position = "bottomleft") + 
  facet_grid(~"¿En que ámbitos percibe aumento en su bienestar?")+
  theme(strip.text.x = element_text(size = 15))+
  theme(axis.text = element_text(size=12))+
  theme(legend.title = element_text(size=14))+
  theme(legend.text = element_text(size=14))+
  ylab("Encuestados")+xlab("Bienestar")+
  guides(fill = guide_legend(title = "Bienestar (Total=150)"))

# P19 ----------------------------------------------------

tab <- as.data.frame(table(datos[,24]))

dataP19 <- data.frame(
  deporte = tab$Var1,
  encuestados = tab$Freq,
  porcentajes = fun4(tab$Freq)
)

win.graph(height = 8, width = 12)
g19= ggplot(dataP19, aes(x=deporte, y=encuestados)) + 
  geom_bar(stat = "identity")

g19 + geom_col(aes(fill = deporte)) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))+
  geom_text(aes(label=paste0(encuestados," ", "", "(", porcentajes, "%",")")), 
            vjust = -0.3, color = "black", size=5)+
  theme(plot.title = element_text(family = "serif",              # Familia de fuente del título
                                  face = "bold",                 # Estilo de fuente
                                  color = 1,                     # Color de la fuente
                                  size = 15,                     # Tamaño de la fuente
                                  hjust = 0.5,                   # Ajuste horizontal
                                  vjust = 1,                     # Ajuste vertical
                                  lineheight = 1,                # Espacio entre líneas
                                  margin = margin(20, 0, 0, 0)), # Márgenes (t, r, b, l)
        plot.tag = element_text(face = "italic"),   # Personalizar el tag
        plot.title.position = "panel",              # Posición del título y del subtítulo ("plot" o "panel")
        plot.tag.position = "bottomleft") + 
  facet_grid(~"¿Realiza alguna actividad deportiva en su tiempo libre?")+
  theme(strip.text.x = element_text(size = 15))+
  theme(axis.text = element_text(size=12))+
  theme(legend.title = element_text(size=14))+
  theme(legend.text = element_text(size=14))+
  ylab("Encuestados")+xlab("Realiza deporte")+
  guides(fill = guide_legend(title = "Realiza deporte (Total=125)"))

# P20 ----------------------------------------------------

tab <- as.data.frame(table(datos[,25]))

dataP20 <- data.frame(
  manera = tab$Var1,
  encuestados = tab$Freq,
  porcentajes = fun4(tab$Freq)
)

win.graph(height = 8, width = 12)
g20= ggplot(dataP20, aes(x=manera, y=encuestados)) + 
  geom_bar(stat = "identity")

g20 + geom_col(aes(fill = manera)) +
  scale_fill_manual(values = c("green3", "steelblue3", "darkorange"))+
  geom_text(aes(label=paste0(encuestados," ", "", "(", porcentajes, "%",")")), 
            vjust = -0.3, color = "black", size=5)+
  theme(plot.title = element_text(family = "serif",              # Familia de fuente del título
                                  face = "bold",                 # Estilo de fuente
                                  color = 1,                     # Color de la fuente
                                  size = 15,                     # Tamaño de la fuente
                                  hjust = 0.5,                   # Ajuste horizontal
                                  vjust = 1,                     # Ajuste vertical
                                  lineheight = 1,                # Espacio entre líneas
                                  margin = margin(20, 0, 0, 0)), # Márgenes (t, r, b, l)
        plot.tag = element_text(face = "italic"),   # Personalizar el tag
        plot.title.position = "panel",              # Posición del título y del subtítulo ("plot" o "panel")
        plot.tag.position = "bottomleft") + 
  facet_grid(~"¿De qué manera prefiere realizar actividad física, deportiva y/o recreativa?")+
  theme(strip.text.x = element_text(size = 15))+
  theme(axis.text = element_text(size=12))+
  theme(legend.title = element_text(size=14))+
  theme(legend.text = element_text(size=14))+
  ylab("Encuestados")+xlab("Manera")+
  guides(fill = guide_legend(title = "Manera (Total=67)"))

# P21 ----------------------------------------------------

tab <- as.data.frame(table(datos[,26]))

dataP21 <- data.frame(
  veces = tab$Var1,
  encuestados = tab$Freq,
  porcentajes = fun4(tab$Freq)
)

win.graph(height = 8, width = 12)
g21= ggplot(dataP21, aes(x=reorder(veces, -desc(encuestados)), y=encuestados)) + 
  geom_bar(stat = "identity")

g21 + geom_col(aes(fill = veces)) +
  scale_fill_manual(values = c("green3", "steelblue3", "darkorange"))+
  geom_text(aes(label=paste0(encuestados," ", "", "(", porcentajes, "%",")")), 
            vjust = -0.3, color = "black", size=5)+
  theme(plot.title = element_text(family = "serif",              # Familia de fuente del título
                                  face = "bold",                 # Estilo de fuente
                                  color = 1,                     # Color de la fuente
                                  size = 15,                     # Tamaño de la fuente
                                  hjust = 0.5,                   # Ajuste horizontal
                                  vjust = 1,                     # Ajuste vertical
                                  lineheight = 1,                # Espacio entre líneas
                                  margin = margin(20, 0, 0, 0)), # Márgenes (t, r, b, l)
        plot.tag = element_text(face = "italic"),   # Personalizar el tag
        plot.title.position = "panel",              # Posición del título y del subtítulo ("plot" o "panel")
        plot.tag.position = "bottomleft") + 
  facet_grid(~"¿Cuántas veces por semana realiza \n actividad física, recreativa y/o deportiva?")+
  theme(strip.text.x = element_text(size = 15))+
  theme(axis.text = element_text(size=12))+
  theme(legend.title = element_text(size=14))+
  theme(legend.text = element_text(size=14))+
  ylab("Encuestados")+xlab("Frecuencia")+
  guides(fill = guide_legend(title = "Frecuencia (Total=67)"))

# P22 ----------------------------------------------------

dataP22 <- funcioncita(datos, 27)

win.graph(height = 8, width = 12)
g21= ggplot(dataP22, aes(x=reorder(Espacio, desc(Frecuencia)), y=Frecuencia)) + 
  geom_bar(stat = "identity")

g21 + geom_col(aes(fill = Espacio)) +
  scale_fill_manual(values = c("#FF6EB4","#FF9900", "chocolate", "green", 
                               "#FF3800", "aquamarine4", "cyan", "darkblue",
                               "pink", "royalblue"))+
  geom_text(aes(label=paste0(Frecuencia," ", "", "(", porcentajes, "%",")")), 
            vjust= 0.5, hjust = 0, color = "black", size=4)+
  theme(plot.title = element_text(family = "serif",              # Familia de fuente del título
                                  face = "bold",                 # Estilo de fuente
                                  color = 1,                     # Color de la fuente
                                  size = 15,                     # Tamaño de la fuente
                                  hjust = 0.5,                   # Ajuste horizontal
                                  vjust = 1,                     # Ajuste vertical
                                  lineheight = 1,                # Espacio entre líneas
                                  margin = margin(20, 0, 0, 0)), # Márgenes (t, r, b, l)
        plot.tag = element_text(face = "italic"),   # Personalizar el tag
        plot.title.position = "panel",              # Posición del título y del subtítulo ("plot" o "panel")
        plot.tag.position = "bottomleft") + coord_flip() +
  facet_grid(~"¿Qué espacio prefiere usar?")+
  theme(strip.text.x = element_text(size = 15))+
  theme(axis.text = element_text(size=12))+
  theme(legend.title = element_text(size=14))+
  theme(legend.text = element_text(size=14))+
  ylab("Encuestados")+xlab("Espacio")+
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.14)))+
  guides(fill = guide_legend(title = "Espacio (Total=107)"))

# P23 ----------------------------------------------------

dataP23 <- funcioncita(datos, 28)

win.graph(height = 8, width = 12)
g21= ggplot(dataP23, aes(x=reorder(Motivo, desc(Frecuencia)), y=Frecuencia)) + 
  geom_bar(stat = "identity")

g21 + geom_col(aes(fill = Motivo)) +
  scale_fill_manual(values = c("#FF6EB4","#FF9900", "chocolate", "green", 
                               "#FF3800", "aquamarine4", "cyan", "darkblue",
                               "pink", "royalblue"))+
  geom_text(aes(label=paste0(Frecuencia," ", "", "(", porcentajes, "%",")")), 
            vjust= 0.5, hjust = 0, color = "black", size=4)+
  theme(plot.title = element_text(family = "serif",              # Familia de fuente del título
                                  face = "bold",                 # Estilo de fuente
                                  color = 1,                     # Color de la fuente
                                  size = 15,                     # Tamaño de la fuente
                                  hjust = 0.5,                   # Ajuste horizontal
                                  vjust = 1,                     # Ajuste vertical
                                  lineheight = 1,                # Espacio entre líneas
                                  margin = margin(20, 0, 0, 0)), # Márgenes (t, r, b, l)
        plot.tag = element_text(face = "italic"),   # Personalizar el tag
        plot.title.position = "panel",              # Posición del título y del subtítulo ("plot" o "panel")
        plot.tag.position = "bottomleft") + coord_flip() +
  facet_grid(~"Motivo por el cual usa este espacio")+
  theme(strip.text.x = element_text(size = 15))+
  theme(axis.text = element_text(size=12))+
  theme(legend.title = element_text(size=14))+
  theme(legend.text = element_text(size=14))+
  ylab("Encuestados")+xlab("Motivo")+
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.14)))+
  guides(fill = guide_legend(title = "Motivo (Total=120)"))

# P24 ----------------------------------------------------

tab <- as.data.frame(table(datos[,29]))

dataP24 <- data.frame(
  dinero = tab$Var1,
  encuestados = tab$Freq,
  porcentajes = fun4(tab$Freq)
)

win.graph(height = 8, width = 12)
g24= ggplot(dataP24, aes(x=dinero, y=encuestados)) + 
  geom_bar(stat = "identity")

g24 + geom_col(aes(fill = dinero)) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))+
  geom_text(aes(label=paste0(encuestados," ", "", "(", porcentajes, "%",")")), 
            vjust = -0.3, color = "black", size=5)+
  theme(plot.title = element_text(family = "serif",              # Familia de fuente del título
                                  face = "bold",                 # Estilo de fuente
                                  color = 1,                     # Color de la fuente
                                  size = 15,                     # Tamaño de la fuente
                                  hjust = 0.5,                   # Ajuste horizontal
                                  vjust = 1,                     # Ajuste vertical
                                  lineheight = 1,                # Espacio entre líneas
                                  margin = margin(20, 0, 0, 0)), # Márgenes (t, r, b, l)
        plot.tag = element_text(face = "italic"),   # Personalizar el tag
        plot.title.position = "panel",              # Posición del título y del subtítulo ("plot" o "panel")
        plot.tag.position = "bottomleft") + 
  facet_grid(~"¿Suele invertir dinero cuando realiza actividad física?")+
  theme(strip.text.x = element_text(size = 15))+
  theme(axis.text = element_text(size=12))+
  theme(legend.title = element_text(size=14))+
  theme(legend.text = element_text(size=14))+
  ylab("Encuestados")+xlab("Invierte dinero")+
  guides(fill = guide_legend(title = "Invierte dinero (Total=67)"))

# P25 ----------------------------------------------------

tab <- as.data.frame(table(datos[,30]))

dataP25 <- data.frame(
  compañia = tab$Var1,
  encuestados = tab$Freq,
  porcentajes = fun4(tab$Freq)
)

win.graph(height = 8, width = 12)
g25= ggplot(dataP25, aes(x=reorder(compañia, desc(encuestados)), y=encuestados)) + 
  geom_bar(stat = "identity")

g25 + geom_col(aes(fill = compañia)) +
  scale_fill_manual(values = c("green3", "steelblue3", "darkorange", 
                               "magenta3", "azure4", "brown"))+
  geom_text(aes(label=paste0(encuestados," ", "", "(", porcentajes, "%",")")), 
            vjust= 0.5, hjust = 0, color = "black", size=4)+
  theme(plot.title = element_text(family = "serif",              # Familia de fuente del título
                                  face = "bold",                 # Estilo de fuente
                                  color = 1,                     # Color de la fuente
                                  size = 15,                     # Tamaño de la fuente
                                  hjust = 0.5,                   # Ajuste horizontal
                                  vjust = 1,                     # Ajuste vertical
                                  lineheight = 1,                # Espacio entre líneas
                                  margin = margin(20, 0, 0, 0)), # Márgenes (t, r, b, l)
        plot.tag = element_text(face = "italic"),   # Personalizar el tag
        plot.title.position = "panel",              # Posición del título y del subtítulo ("plot" o "panel")
        plot.tag.position = "bottomleft") + coord_flip() +
  facet_grid(~"¿En compañía de quién realiza actividad física?")+
  theme(strip.text.x = element_text(size = 15))+
  theme(axis.text = element_text(size=12))+
  theme(legend.title = element_text(size=14))+
  theme(legend.text = element_text(size=14))+
  ylab("Encuestados")+xlab("Compañia")+
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.18)))+
  guides(fill = guide_legend(title = "Compañia (Total=67)"))

# P26 ----------------------------------------------------

dataP26 <- funcioncita(datos, 31)

win.graph(height = 8, width = 12)
g26= ggplot(dataP26, aes(x=reorder(Organizacion, desc(Frecuencia)), y=Frecuencia)) + 
  geom_bar(stat = "identity")

g26 + geom_col(aes(fill = Organizacion)) +
  scale_fill_manual(values = c(1:17))+
  geom_text(aes(label=paste0(Frecuencia," ", "", "(", porcentajes, "%",")")), 
            vjust= 0.5, hjust = 0, color = "black", size=4)+
  theme(plot.title = element_text(family = "serif",              # Familia de fuente del título
                                  face = "bold",                 # Estilo de fuente
                                  color = 1,                     # Color de la fuente
                                  size = 15,                     # Tamaño de la fuente
                                  hjust = 0.5,                   # Ajuste horizontal
                                  vjust = 1,                     # Ajuste vertical
                                  lineheight = 1,                # Espacio entre líneas
                                  margin = margin(20, 0, 0, 0)), # Márgenes (t, r, b, l)
        plot.tag = element_text(face = "italic"),   # Personalizar el tag
        plot.title.position = "panel",              # Posición del título y del subtítulo ("plot" o "panel")
        plot.tag.position = "bottomleft") + coord_flip() +
  facet_grid(~"¿Cuál de las siguientes organizaciones es la que más frecuenta?")+
  theme(strip.text.x = element_text(size = 15))+
  theme(axis.text = element_text(size=12))+
  theme(legend.title = element_text(size=14))+
  theme(legend.text = element_text(size=14))+
  ylab("Encuestados")+xlab("Organización")+
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.18)))+
  guides(fill = guide_legend(title = "Organización (Total=91)"))

# P27 ----------------------------------------------------

dataP27 <- funcioncita(datos, 32)

win.graph(height = 8, width = 12)
g27= ggplot(dataP27, aes(x=reorder(Motivo, desc(Frecuencia)), y=Frecuencia)) + 
  geom_bar(stat = "identity")

g27 + geom_col(aes(fill = Motivo)) +
  scale_fill_manual(values = c(1:17))+
  geom_text(aes(label=paste0(Frecuencia," ", "", "(", porcentajes, "%",")")), 
            vjust= 0.5, hjust = 0, color = "black", size=4)+
  theme(plot.title = element_text(family = "serif",              # Familia de fuente del título
                                  face = "bold",                 # Estilo de fuente
                                  color = 1,                     # Color de la fuente
                                  size = 15,                     # Tamaño de la fuente
                                  hjust = 0.5,                   # Ajuste horizontal
                                  vjust = 1,                     # Ajuste vertical
                                  lineheight = 1,                # Espacio entre líneas
                                  margin = margin(20, 0, 0, 0)), # Márgenes (t, r, b, l)
        plot.tag = element_text(face = "italic"),   # Personalizar el tag
        plot.title.position = "panel",              # Posición del título y del subtítulo ("plot" o "panel")
        plot.tag.position = "bottomleft") + coord_flip() +
  facet_grid(~"¿Cuál es el motivo por el que no realiza actividad física en su tiempo libre?")+
  theme(strip.text.x = element_text(size = 15))+
  theme(axis.text = element_text(size=12))+
  theme(legend.title = element_text(size=14))+
  theme(legend.text = element_text(size=14))+
  ylab("Encuestados")+xlab("Motivo")+
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.18)))+
  guides(fill = guide_legend(title = "Motivo (Total=64)"))

# P28 ----------------------------------------------------

dataP28 <- funcioncita(datos, 33)

win.graph(height = 8, width = 12)
g28= ggplot(dataP28, aes(x=reorder(Accion, desc(Frecuencia)), y=Frecuencia)) + 
  geom_bar(stat = "identity")

g28 + geom_col(aes(fill = Accion)) +
  scale_fill_manual(values = c("green3", "steelblue3", "darkorange", 
                               "magenta3", "azure4"))+
  geom_text(aes(label=paste0(Frecuencia," ", "", "(", porcentajes, "%",")")), 
            vjust= 0.5, hjust = 0, color = "black", size=4)+
  theme(plot.title = element_text(family = "serif",              # Familia de fuente del título
                                  face = "bold",                 # Estilo de fuente
                                  color = 1,                     # Color de la fuente
                                  size = 15,                     # Tamaño de la fuente
                                  hjust = 0.5,                   # Ajuste horizontal
                                  vjust = 1,                     # Ajuste vertical
                                  lineheight = 1,                # Espacio entre líneas
                                  margin = margin(20, 0, 0, 0)), # Márgenes (t, r, b, l)
        plot.tag = element_text(face = "italic"),   # Personalizar el tag
        plot.title.position = "panel",              # Posición del título y del subtítulo ("plot" o "panel")
        plot.tag.position = "bottomleft") + coord_flip() +
  facet_grid(~"¿Cuál de las siguientes acciones podría ser útil \n para resolver el problema de inseguridad?")+
  theme(strip.text.x = element_text(size = 15))+
  theme(axis.text = element_text(size=12))+
  theme(legend.title = element_text(size=14))+
  theme(legend.text = element_text(size=14))+
  ylab("Encuestados")+xlab("Acción")+
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.18)))+
  guides(fill = guide_legend(title = "Acción (Total=36)"))

# P29 ----------------------------------------------------

tab <- as.data.frame(table(datos[,34]))

dataP29 <- data.frame(
  actividad = tab$Var1,
  encuestados = tab$Freq,
  porcentajes = fun4(tab$Freq)
)

win.graph(height = 8, width = 12)
g29= ggplot(dataP29, aes(x=reorder(actividad, desc(encuestados)), y=encuestados)) + 
  geom_bar(stat = "identity")

g29 + geom_col(aes(fill = actividad)) +
  scale_fill_manual(values = c(1:17))+
  geom_text(aes(label=paste0(encuestados," ", "", "(", porcentajes, "%",")")), 
            vjust= 0.5, hjust = 0, color = "black", size=4)+
  theme(plot.title = element_text(family = "serif",              # Familia de fuente del título
                                  face = "bold",                 # Estilo de fuente
                                  color = 1,                     # Color de la fuente
                                  size = 15,                     # Tamaño de la fuente
                                  hjust = 0.5,                   # Ajuste horizontal
                                  vjust = 1,                     # Ajuste vertical
                                  lineheight = 1,                # Espacio entre líneas
                                  margin = margin(20, 0, 0, 0)), # Márgenes (t, r, b, l)
        plot.tag = element_text(face = "italic"),   # Personalizar el tag
        plot.title.position = "panel",              # Posición del título y del subtítulo ("plot" o "panel")
        plot.tag.position = "bottomleft") + coord_flip() +
  facet_grid(~"¿Qué actividad realiza con mayor frecuencia en su tiempo libre?")+
  theme(strip.text.x = element_text(size = 15))+
  theme(axis.text = element_text(size=12))+
  theme(legend.title = element_text(size=14))+
  theme(legend.text = element_text(size=14))+
  ylab("Encuestados")+xlab("Actividad")+
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.18)))+
  guides(fill = guide_legend(title = "Actividad (Total=125)"))

















