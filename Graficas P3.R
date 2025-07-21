# Librerias ---------------------------------------------

library(readxl)
library(ggplot2)
library(dplyr)

# Datos -------------------------------------------------

#Lectura de datos de todas las comunas
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

# P30 ---------------------------------------------------

tab <- as.data.frame(table(datos[,35]))

dataP30 <- data.frame(
  comidas = tab$Var1,
  encuestados = tab$Freq,
  porcentajes = fun4(tab$Freq)
)

nom <- c("1", "2", "3", "4 o más")

win.graph(height = 8, width = 12)
g30= ggplot(dataP30, aes(x=reorder(comidas, desc(encuestados)), y=encuestados)) + 
  geom_bar(stat = "identity")+scale_x_discrete(limits = nom, labels = nom)

g30 + geom_col(aes(fill = comidas)) +
  scale_fill_manual(values = c("green3", "steelblue3", "darkorange", 
                               "magenta3"))+
  geom_text(aes(label=paste0(encuestados," ", "", "(", porcentajes, "%",")")), 
            vjust= -0.3, color = "black", size=5)+
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
  facet_grid(~"¿Cuántas comidas consume al día?")+
  theme(strip.text.x = element_text(size = 15))+
  theme(axis.text = element_text(size=12))+
  theme(legend.title = element_text(size=14))+
  theme(legend.text = element_text(size=14))+
  ylab("Encuestados")+xlab("Comidas diarias")+
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.18)))+
  guides(fill = guide_legend(title = "Comidas (Total=244)"))

# P31 --------------------------------------------------------

#Lectura de datos
alimentos <- read.csv("alimentos.csv")

dataP31 <- data.frame("frecuencia" = c(alimentos$Lácteos.y.derivados.,
                                       alimentos$Carnes..res..cerdo..pollo..pescado..,
                                       alimentos$Cereales..arroz..pasta..avena..quinoa..,
                                       alimentos$Productos.cárnicos..chorizos..salchichas..jamón..morcilla..etc...,
                                       alimentos$Aceites.y.grasas..aceite.de.canola..aceite.de.oliva..aceite.de.ajonjolí..aceite.de.aguacate..aguacate..aceitunas..,
                                       alimentos$Leguminosas..frijol..garbanzo..lenteja..etc..,
                                       alimentos$Raíces..tubérculos.y.plátanos..Arracacha..batata..ñame..yuca..papa..,
                                       alimentos$Frutas..Banano..borojo..chontaduro..manzana..mango.,
                                       alimentos$Verduras,
                                       alimentos$Comidas.rápidas.perro..pizza..hamburguesa.etc..,
                                       alimentos$Snacks.o.productos.de.paquete.,
                                       alimentos$Dulces..postres.y.golosinas,
                                       alimentos$Gaseosas), 
                      "alimentos" = c(rep("Lacteos", 244), 
                                      rep("Carnes", 244),
                                      rep("Cereales", 244),
                                      rep("Productos cárnicos", 244),
                                      rep("Aceites", 244),
                                      rep("Luguminosas", 244),
                                      rep("Raíces", 244),
                                      rep("Frutas", 244),
                                      rep("Verduras", 244),
                                      rep("Comidas rápidas", 244),
                                      rep("Snacks", 244),
                                      rep("Dulces", 244),
                                      rep("Gaseosas", 244)))

attach(dataP31)

win.graph(height = 8, width = 12)
ggplot(dataP31, aes(x = alimentos, fill = frecuencia)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("green3", "steelblue3", "darkorange", 
                               "magenta3", "azure4"))+
  facet_grid(~"Frecuencia de consumo de los siguientes grupos de alimentos")+
  theme(strip.text.x = element_text(size = 15))+
  theme(axis.text = element_text(size=10))+
  theme(legend.title = element_text(size=14))+
  theme(legend.text = element_text(size=14))+
  theme(axis.title  = element_text(size=15))+
  ylab("Encuestados")+xlab("Alimentos")

# P32 --------------------------------------------------------

tab <- as.data.frame(table(datos[,36]))

dataP32 <- data.frame(
  sal = tab$Var1,
  encuestados = tab$Freq,
  porcentajes = fun4(tab$Freq)
)

win.graph(height = 8, width = 12)
g32= ggplot(dataP32, aes(x=sal, y=encuestados)) + 
  geom_bar(stat = "identity")

g32 + geom_col(aes(fill = sal)) +
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
  facet_grid(~"¿Adiciona sal a los alimentos listos para consumir? ")+
  theme(strip.text.x = element_text(size = 15))+
  theme(axis.text = element_text(size=12))+
  theme(legend.title = element_text(size=14))+
  theme(legend.text = element_text(size=14))+
  ylab("Encuestados")+xlab("Adiciona sal")+
  guides(fill = guide_legend(title = "Sal (Total=244)"))

# P33 --------------------------------------------------------

tab <- as.data.frame(table(datos[,37]))

dataP33 <- data.frame(
  azucar = tab$Var1,
  encuestados = tab$Freq,
  porcentajes = fun4(tab$Freq)
)

win.graph(height = 8, width = 12)
g33= ggplot(dataP33, aes(x=azucar, y=encuestados)) + 
  geom_bar(stat = "identity")

g33 + geom_col(aes(fill = azucar)) +
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
  facet_grid(~"¿Adiciona azúcar a los alimentos listos para consumir? ")+
  theme(strip.text.x = element_text(size = 15))+
  theme(axis.text = element_text(size=12))+
  theme(legend.title = element_text(size=14))+
  theme(legend.text = element_text(size=14))+
  ylab("Encuestados")+xlab("Adiciona azúcar")+
  guides(fill = guide_legend(title = "Azúcar (Total=244)"))

# P34 --------------------------------------------------------

tab <- as.data.frame(table(datos[,38]))

dataP34 <- data.frame(
  agua = tab$Var1,
  encuestados = tab$Freq,
  porcentajes = fun4(tab$Freq)
)

win.graph(height = 8, width = 12)
g34= ggplot(dataP34, aes(x=agua, y=encuestados)) + 
  geom_bar(stat = "identity")

g34 + geom_col(aes(fill = agua)) +
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
  facet_grid(~"¿Toma diariamente agua?")+
  theme(strip.text.x = element_text(size = 15))+
  theme(axis.text = element_text(size=12))+
  theme(legend.title = element_text(size=14))+
  theme(legend.text = element_text(size=14))+
  ylab("Encuestados")+xlab("Toma agua")+
  guides(fill = guide_legend(title = "Agua (Total=244)"))

# P35 ---------------------------------------------------

dataP35 <- funcioncita(datos, 39)

nom <- c("Antes", "Durante", "Después")

win.graph(height = 8, width = 12)
g35= ggplot(dataP35, aes(x=reorder(Momento, desc(Frecuencia)), y=Frecuencia)) + 
  geom_bar(stat = "identity")+scale_x_discrete(limits = nom, labels = nom)

g35 + geom_col(aes(fill = Momento)) +
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
  facet_grid(~"El momento de hidratación al realizar actividad física es")+
  theme(strip.text.x = element_text(size = 15))+
  theme(axis.text = element_text(size=12))+
  theme(legend.title = element_text(size=14))+
  theme(legend.text = element_text(size=14))+
  ylab("Encuestados")+xlab("Momento de hidratación")+
  guides(fill = guide_legend(title = "Momento (Total=351)"))

# P36 ---------------------------------------------------

dataP36 <- funcioncita(datos, 40)

win.graph(height = 8, width = 12)
g36= ggplot(dataP36, aes(x=reorder(Liquido, desc(Frecuencia)), y=Frecuencia)) + 
  geom_bar(stat = "identity")

g36 + geom_col(aes(fill = Liquido)) +
  scale_fill_manual(values = c("green3", "steelblue3", "darkorange", 
                               "magenta3", "azure4"))+
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
  facet_grid(~"¿Qué tipo de líquidos consume durante la actividad física?")+
  theme(strip.text.x = element_text(size = 15))+
  theme(axis.text = element_text(size=12))+
  theme(legend.title = element_text(size=14))+
  theme(legend.text = element_text(size=14))+
  ylab("Encuestados")+xlab("Tipo de líquidos")+
  guides(fill = guide_legend(title = "Tipo de líquidos (Total=271)"))

# P37 --------------------------------------------------------

tab <- as.data.frame(table(datos[,41]))

dataP37 <- data.frame(
  suplemento = tab$Var1,
  encuestados = tab$Freq,
  porcentajes = fun4(tab$Freq)
)

win.graph(height = 8, width = 12)
g37= ggplot(dataP37, aes(x=suplemento, y=encuestados)) + 
  geom_bar(stat = "identity")

g37 + geom_col(aes(fill = suplemento)) +
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
  facet_grid(~"¿Consume algún tipo de suplemento nutricional?")+
  theme(strip.text.x = element_text(size = 15))+
  theme(axis.text = element_text(size=12))+
  theme(legend.title = element_text(size=14))+
  theme(legend.text = element_text(size=14))+
  ylab("Encuestados")+xlab("Suplemento nutricional")+
  guides(fill = guide_legend(title = "Suplemento (Total=244)"))

# P38 --------------------------------------------------------

tab <- as.data.frame(table(datos[,42]))

dataP38 <- data.frame(
  estado = tab$Var1,
  encuestados = tab$Freq,
  porcentajes = fun4(tab$Freq)
)

win.graph(height = 8, width = 12)
g38= ggplot(dataP38, aes(x=reorder(estado, desc(encuestados)), y=encuestados)) + 
  geom_bar(stat = "identity")

g38 + geom_col(aes(fill = estado)) +
  scale_fill_manual(values = c("green3", "steelblue3", "darkorange", 
                               "magenta3", "azure4"))+
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
  facet_grid(~"Con respecto a su estado nutricional, usted se considera")+
  theme(strip.text.x = element_text(size = 15))+
  theme(axis.text = element_text(size=12))+
  theme(legend.title = element_text(size=14))+
  theme(legend.text = element_text(size=14))+
  ylab("Encuestados")+xlab("Estado nutricional")+
  guides(fill = guide_legend(title = "Estado (Total=244)"))

# P39 y P40 ------------------------------------------------

attach(datos)

win.graph(height = 8, width = 12)
ggplot(datos, aes(x = as.numeric(Pregunta.40), y = as.numeric(Pregunta.39))) +
  geom_point() + xlim(100, 200)+ ylim(30, 110) + ylab("Peso (kgs)") + 
  xlab("Estatura (cms)") + facet_grid(~"Relación estatura vs peso") +
  theme(strip.text.x = element_text(size = 15))+
  theme(axis.title  = element_text(size=20))

# P41 --------------------------------------------------------

tab <- as.data.frame(table(datos[,45]))

dataP41 <- data.frame(
  habitos = tab$Var1,
  encuestados = tab$Freq,
  porcentajes = fun4(tab$Freq)
)

nom <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")

win.graph(height = 8, width = 12)
g41= ggplot(dataP41, aes(x=reorder(habitos, desc(encuestados)), y=encuestados)) + 
  geom_bar(stat = "identity")+scale_x_discrete(limits = nom, labels = nom)

g41 + geom_col(aes(fill = habitos)) +
  scale_fill_manual(values = c("#FF6EB4","#FF9900", "chocolate", "green", 
                               "#FF3800", "aquamarine4", "cyan", "darkblue",
                               "pink", "royalblue"))+
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
  facet_grid(~"¿Qué tan saludables considera que son sus hábitos alimentarios?")+
  theme(strip.text.x = element_text(size = 15))+
  theme(axis.text = element_text(size=12))+
  theme(legend.title = element_text(size=14))+
  theme(legend.text = element_text(size=14))+
  ylab("Encuestados")+xlab("Escala")+
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.14)))+
  guides(fill = guide_legend(title = "Escala (Total=244)"))

# P42 -----------------------------------------------------

dataP42 <- data.frame("frecuencia" = c(datos$Alcohol,
                                       datos$Cafeína,
                                       datos$Hongos,
                                       datos$Metanfetamina,
                                       datos$Nicotina,
                                       datos$Ayahuasca,
                                       datos$Cocaína,
                                       datos$Marihuana,
                                       datos$Esteroides,
                                       datos$Heroína,
                                       datos$Popper,
                                       datos$Opiodes,
                                       datos$Extasis,
                                       datos$LSD,
                                       datos$Tussi,
                                       datos$Otras), 
                      "sustancias" = c(rep("Alcohol", 244), 
                                       rep("Cafeína", 244),
                                       rep("Hongos", 244),
                                       rep("Meta", 244),
                                       rep("Nicotina", 244),
                                       rep("Ayahuasca", 244),
                                       rep("Cocaína", 244),
                                       rep("Marihuana", 244),
                                       rep("Esteroides", 244),
                                       rep("Heroína", 244),
                                       rep("Popper", 244),
                                       rep("Opiodies", 244),
                                       rep("Extasis", 244),
                                       rep("LSD", 244),
                                       rep("Tussi", 244),
                                       rep("Otras", 244)))

attach(dataP42)

win.graph(height = 8, width = 12)
ggplot(dataP42, aes(x = sustancias, fill = frecuencia)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("green3", "steelblue3", "darkorange", 
                               "magenta3", "azure4"))+
  facet_grid(~"Frecuencia de consumo de las siguientes sustancias")+
  theme(strip.text.x = element_text(size = 15))+
  theme(axis.text = element_text(size=10))+
  theme(legend.title = element_text(size=14))+
  theme(legend.text = element_text(size=14))+
  theme(axis.title  = element_text(size=15))+
  ylab("Encuestados")+xlab("Sustancias")





















