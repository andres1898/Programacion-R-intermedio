##### Andres Felipe Ordoñez Casadiego 2019
##### Curso de programación en R nivel intermedio-avanzado
##### Universidad Industrial de Santander - MTR COLEVOL Santander

# REPASO -------------------------------------------------------------------

#Ejercicio 1: Repetir el ejercicio usando un "for" sin "while" pero con la condicion
c <- 0

while(c <= 10) {
  print(c)
  c <- c + 1
}

#solución 4 min
for (i in 1:11){
  print(c)
  c <- c + i
  if (c >= 10) break
}

#Ejercicio 2: Extraer los números pares del 1 al 100 

#solución 4 min
num <- c(1:100)
pares <- vector()
for (i in 1:length(num)) { if (num[i] %% 2 == 0) { pares[i] <- num[i] } }
pares <- pares[!is.na(pares)]


#Ejercicio 3: Modifique el siguiente código para que el loop rellene un vector con todos los resultados

for (i in 1:10){
  
  cat("Numero", i, "")
  
}

#solución 2 min

llenito <- vector()

for (i in 1:10){
  
  llenito[i] <- paste("Numero", i, "")
  
}

#Ejercicio 4: cargue el data "cars" y cree una nueva columna que rellene con "muy rapido" si la velocidad del modelo es mayor 15 o rellene con "muy lento" si es menor a 15

#solución 4 min
cars <- cars

rapidez <- vector(length = nrow(cars))

for (i in 1:nrow(cars)){
  if (cars$speed[i] >= 15) { 
    rapidez[i] <- "muy rapido"
  } else {rapidez[i] <- "muy lento"}
}

#Ejercicio 5: Cree un dataframe con 1000 filas y 2 columnas, reste la primer columna a la segunda y cree una nueva columna con el resultado. Haga este ejercicio usando "for" y apply.

LaMatriz <- matrix(data = rnorm(2000), ncol = 2, nrow = 1000)

laResta <- vector()

for (i in 1:nrow(LaMatriz)){
  
  laResta[i] <- LaMatriz[i, 1] - LaMatriz[i, 2]
  
}

LaTareaLoop <- cbind(LaMatriz, laResta)
##
restar <- function(x, y) {
  x - y
  return(x - y)
}

LaTareaApply <- restar(LaMatriz[,1], LaMatriz[,2])

LaTareaApply == LaTareaLoop[,3]
