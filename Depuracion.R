# Librerias ----------------------------------------------

library(readxl)
library(ggplot2)
library(dplyr)

# Datos -------------------------------------------------

#Lectura de datos
datosO <- read_excel("ENCUESTA DE PERCEPCIÓN, HÁBITOS Y PREFERENCIAS INDER - MEDELLÍN 1.xlsx")

#Cambiar nombres de variables
names(datosO)[6] <- "Aviso"

#Eliminar los NO
datos <- datosO[datosO$Aviso != "NO", ]

#Eliminar variables
datos <- datos[,-c(2:6)]

#Cambiar nombres de variables
names(datos)[44] <- "Pregunta 38"

#Convertir de caracter a numerico
datos$Ingreso <- as.numeric(datos$Ingreso)
datos$`Pregunta 39` <- as.numeric(datos$`Pregunta 39`)
datos$`Pregunta 40` <- as.numeric(datos$`Pregunta 40`)

#Depuracion del peso -----------------------------------

#Eliminar el que pesa 0
datos <- datos[-which(datos$`Pregunta 39`==0),]

#Datos mayores a 100000
datos <- mutate(datos, `Pregunta 39` = ifelse(datos$`Pregunta 39`>=100000,
                                              datos$`Pregunta 39`/10000,
                                              datos$`Pregunta 39`))

#Datos mayores a 10000
datos <- mutate(datos, `Pregunta 39` = ifelse(datos$`Pregunta 39`>=10000,
                                              datos$`Pregunta 39`/1000,
                                              datos$`Pregunta 39`))

#Datos mayores a 1000
datos <- mutate(datos, `Pregunta 39` = ifelse(datos$`Pregunta 39`>=1000,
                                              datos$`Pregunta 39`/10,
                                              datos$`Pregunta 39`))
#Datos mayores a 400
datos <- mutate(datos, `Pregunta 39` = ifelse(datos$`Pregunta 39`>=400,
                                              datos$`Pregunta 39`/10,
                                              datos$`Pregunta 39`))

#Datos menores a 2
datos <- mutate(datos, `Pregunta 39` = ifelse(datos$`Pregunta 39`<2,
                                              datos$`Pregunta 39`*50,
                                              datos$`Pregunta 39`))

#Datos menores a 11
datos <- mutate(datos, `Pregunta 39` = ifelse(datos$`Pregunta 39`<=11,
                                              datos$`Pregunta 39`*10,
                                              datos$`Pregunta 39`))

#Filtrar los que se consideran normales y delgados
datos <- mutate(datos, `Pregunta 39` = ifelse(datos$`Pregunta 39`>100 &
                                                (datos$`Pregunta 38`=="Delgado"|
                                                   datos$`Pregunta 38`=="Normal"),
                                              datos$`Pregunta 39`-100,
                                              datos$`Pregunta 39`))

#Depurar los datos de la estatura ------------------------------------

#Eliminar el que mide 0
datos <- datos[-which(datos$`Pregunta 40`==0),]

#Datos menores a 2
datos <- mutate(datos, `Pregunta 40` = ifelse(datos$`Pregunta 40`<=2,
                                              datos$`Pregunta 40`*100,
                                              datos$`Pregunta 40`))

#Datos menores a 10
datos <- mutate(datos, `Pregunta 40` = ifelse(datos$`Pregunta 40`<10,
                                              datos$`Pregunta 40`*10+100,
                                              datos$`Pregunta 40`))

#Datos menores a 20
datos <- mutate(datos, `Pregunta 40` = ifelse(datos$`Pregunta 40`<20,
                                              datos$`Pregunta 40`*10,
                                              datos$`Pregunta 40`))

#Datos menores a 80
datos <- mutate(datos, `Pregunta 40` = ifelse(datos$`Pregunta 40`<80,
                                              datos$`Pregunta 40`+100,
                                              datos$`Pregunta 40`))

#Mayores a 5000
datos <- mutate(datos, `Pregunta 40` = ifelse(datos$`Pregunta 40`>5000,
                                              datos$`Pregunta 40`/50,
                                              datos$`Pregunta 40`))

#Mayores a 1000
datos <- mutate(datos, `Pregunta 40` = ifelse(datos$`Pregunta 40`>1000,
                                              datos$`Pregunta 40`/10,
                                              datos$`Pregunta 40`))
#Mayores a 600
datos <- mutate(datos, `Pregunta 40` = ifelse(datos$`Pregunta 40`>600,
                                              datos$`Pregunta 40`/10+100,
                                              datos$`Pregunta 40`))

#Mayores a 210
datos <- mutate(datos, `Pregunta 40` = ifelse(datos$`Pregunta 40`>210,
                                              datos$`Pregunta 40`-100,
                                              datos$`Pregunta 40`))

#IMC---------------------------------------------------------

datos <- mutate(datos, IMC = `Pregunta 39`/(`Pregunta 40`/100)^2)
datos$IMC <- round(datos$IMC, 2)

#Depurar los datos del ingreso ------------------------------

#Datos mayores a 1000000000
datos <- mutate(datos, Ingreso = ifelse(datos$Ingreso>=1000000000,
                                        datos$Ingreso/1000,
                                        datos$Ingreso))

#Datos mayores a 100000000
datos <- mutate(datos, Ingreso = ifelse(datos$Ingreso>=100000000,
                                        datos$Ingreso/10,
                                        datos$Ingreso))

#------------------------------------------------------------

#Eliminar mas variables
alimentos <- datos[,c(36:37)]
datos <- datos[,-c(36:37)]
alimentos <- cbind (alimentos, datos[,c(57:60)])
datos <- datos[,-c(57:60)]
alimentos <- cbind (alimentos, datos[,c(62)])
datos <- datos[,-c(62)]
alimentos <- cbind (alimentos, datos[,c(64:65)])
datos <- datos[,-c(64:65)]
alimentos <- cbind (alimentos, datos[,c(65)])
datos <- datos[,-c(65)]
alimentos <- cbind (alimentos, datos[,c(70:71)])
datos <- datos[,-c(70:71)]
alimentos <- cbind (alimentos, datos[,c(74)])
datos <- datos[,-c(74)]

#Separar los datos por cada comuna
com1 <- datos[datos$`Pregunta 12` == "1-Popular ", ]
com1 <- com1 %>% drop_na(`Pregunta 12`)
com9 <- datos[datos$`Pregunta 12` == "9-Buenos Aires ", ]
com9 <- com9 %>% drop_na(`Pregunta 12`)

#Generar bases de datos en excel -----------------------------

write.csv(datos,"datos depurados.csv")
write.csv(alimentos,"alimentos.csv")
write.csv(com1,"Comuna 1-Popular.csv")
write.csv(com9,"Comuna 9-Buenos Aires.csv")
























