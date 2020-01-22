##### Andres Felipe Ordoñez Casadiego 2019
##### Curso de programación en R nivel intermedio-avanzado
##### Universidad Industrial de Santander - MTR COLEVOL Santander

# S4 ------------------------------------------------------------------- 

#Cargar paquetes

library(maptools)
library(rgeos)
library(nnclust)
library(shapefiles)


# Ejercicios 1. ¿Que hace el objeto graph_mst? comente cada componente de la función

graph_mst <- function (l) {
  distrib<-l@lines[[1]]@Lines[1][[1]]@coords
  mst<-mst(l@lines[[1]]@Lines[1][[1]]@coords)
  plot(l, col="white");title("MST")
  segments(distrib[mst$from,1], distrib[mst$from,2], distrib[mst$to,1],distrib[mst$to,2],col="red")
}


# Ejercicios 2. Crear un objeto s4 en donde incluya 30 letras y 30 observaciones aleatorias de una distribucion normal. 

setClass("myS4", slots = c(letras = "character", 
                           datos = "numeric"))

objeto1 <- new("myS4", letras = sample(x = LETTERS, size = 30, replace = T), datos = rnorm(30))

# Ejercicios 3. Usando setGeneric() y setMethod(), escribir dos funciones para extraer los datos del objeto creado en el punto 2 y graficarlos.

#la tabla
setGeneric("TheTable", function(x) standardGeneric("TheTable"))

fun_table <- function(x){
  
  tmp_datos <- x@datos
  tmp_letras <- x@letras
  tabla <- cbind(tmp_letras, as.numeric(tmp_datos))
  return(tabla)
}

setMethod("TheTable", "myS4", fun_table)

Tabla_final <- TheTable(objeto1)

#ahora el plot
setGeneric("graph", function(x) standardGeneric("graph"))

fun_graph <- function(x){
  
  tmp_datos <- x@datos
  
  return(plot(tmp_datos))
}

setMethod("graph", "myS4", fun_graph)

graph(objeto1)

# Ejercicios 4. Sin ejecutar que pasa en cada una de laa asignaciones y ¿por qué? comente errores de código y la correción que le realizó

rm(list=ls()) #limpiar el entorno


setClass("angelito",
         representation(nombre = "character",
                        apellido = "character",
                        altura = "numeric") #error doble asignación, borre el "peso="
) #crear una clase llamada angelito con 3 slots



setClass("angelito",
         representation(nombre = "character",
                        apellido = "character",
                        peso  <- "numeric", #no sé si sea un error pero crea un objeto y no un slot, cambiar el " <- " por " = "
                        altura = "numeric")
)

new(Class = "angelito", nombre = "w", apellido = "s", peso = 23, altura = 98) #no reconoce al slot peso

setClass("angelito",
         representation(nombre = "character",
                        apellido = "character",
                        peso = "numeric",
                        altura = "numeric")
) # todo bien

yo  <- new("angelito",
           nombre  = "Daniel", # " <- " no es la asignación correcta, debe ser " = "
           apellido  = "Miranda" )# " <- " no es la asignación correcta, debe ser " = "


yo  <- new("angelito",
           nombre  = "Daniel",
           apellido  = "Miranda")


yo

setClass("estudiante",
         representation(
           identidad="angelito",
           semestre="numeric",
           grado="logical")
)


yoReal  <- new("estudiante")

yoReal

yoReal$angelito
yoReal@identidad #en objetos S4 se debe utilizar "@", y el slot se llama "identidad" no "angelito"

yoReal@estudiante # no hay ningun slot llamado "estudiante"

yo@nombre

yoReal@identidad <- yo # se copian los datos de "yo" en el slot identidad del objeto "yoReal"

yoReal

yo@nombre

yoReal@nombre #es incorrecto el slot, debe ir a slot identidad y luego si al slot nombre

yoReal@identidad@nombre #este sí es correcto

ls()