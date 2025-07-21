## Librerias -----------------------------------------------
library(tidyverse)


# Funcion para sacar porcentaje ------------------------------

fun3 <- function(frec){
  total <- sum(frec)
  round((frec/total)*100, 2)
}


## Devuelve un vector con todos las enfermedades que se repiten --------

# Datos: El data.frame
# Columna: La n-esima columna del data frame a ser analizada

funcioncita <- function(datos, columna){

  #Hacer una lista con todas las palabras
  x <- c()
  n <- nrow(datos)
  for (i in 1:n){
    #Separa todas las palabras
    palabras <- str_split(datos[i, columna], ";")
    #Hacer una lista con todas las palabras
    m <- length(palabras[[1]])
    for (j in 1:m){
      p <- palabras[[1]][j]
      x <- c(x, p)  
    }
  }
  
  #Tabla con la frecuencia de cada palabra
  dep <- as.data.frame(table(x))
  # Sacar "" del data frame 
  dep <- dep[-1,]         
  
  #Cambiamos los nombre a las variables
  names(dep) <- c("Liquido", "Frecuencia")
  
  #Sacar los porcentajes
  dep <- cbind(dep, porcentajes = fun3(dep$Frecuencia))
  
  return(dep)
}





