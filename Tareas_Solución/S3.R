##### Andres Felipe Ordoñez Casadiego 2019
##### Curso de programación en R nivel intermedio-avanzado
##### Universidad Industrial de Santander - MTR COLEVOL Santander

##########
# S3 -------------------------------------------------------------------

# ejercicio 1.1 Cree un objeto de clase "Ropa" cuyos componentes sean precio y prenda.

Ropa <- list(precio = vector(), prenda = vector())

class(Ropa) <- "ropa"

Ropa$precio <- rnorm(6, mean = 8)

Ropa$prenda <- sample(LETTERS, 6)

# ejercicio 1.2 cree una función generica para la clase "Ropa" y un método que imprima la prenda y su precio.

print(Ropa)

Mostrar <- function(x) {
  UseMethod("Mostrar")
}

Mostrar.ropa <- function(ropa) {
  print(ropa$precio)
  print(ropa$prenda)
}

Mostrar(Ropa)

# ejercicio 1.3 cree un print para la clase "Ropa" que muestre la suma del valor de todas las prendas.

print(Ropa)

print.ropa <- function(ropa){
  suma <- sum(ropa$precio)
  paste("Hay", length(ropa$prenda), "prendas con un precio total de:", suma)
}

print(Ropa)

# ejercicio 2.1 Cree un objeto de clase "Baraja" cuyos componentes sean número y simbolo. Según la baraja española de 40 cartas https://es.wikipedia.org/wiki/Baraja_espa%C3%B1ola#/media/Archivo:Baraja_de_40_cartas.png

numero <- rep(c(1:7, 10:12), 4)

simbolo <- c(rep("oros", 10), rep("copas", 10), rep("espadas", 10), rep("bastos", 10))
  
Baraja_española <- list(numero = numero,
                        simbolo = simbolo)

class(Baraja_española) <- "Baraja"

# ejercicio 2.2 Cree un print que para la clase "Baraja" donde se muestre una descripción breve del objeto y 3 reglas de juego de cualquier variante de la baraja española, puede guiarse de http://www.juntadeandalucia.es/averroes/centros-tic/18700441/myscrapbook/bookcontents.php?page=8&section=7&viewis=&username=

library(stringr)

print(Baraja_española)

length(levels(as.factor(Baraja_española$simbolo)))

print.Baraja <- function(baraja){
  
cat("Este objeto tiene", length(Baraja_española$simbolo), "cartas de", length(levels(as.factor(Baraja_española$numero))), "simbolos (", levels(as.factor(Baraja_española$simbolo)), ")", "y con", 10, "número cada uno", "\n",
    "El juego consiste en: Baraja Española (Escoba)", "\n",
    "\t1) \tLos simbolos no tiene valor, solamente el número que representan", "\n",
    "\t2) \tTus cartas deben sumar exactamente 15", "\n",
    "\t3) \tAlguin empieza a repartir las cartas y cada jugador decide cuando llenarse")
  
}

print(Baraja_española)

# ejercicio 2.3 cree una función generica para la clase "Baraja" que sirva de manera al azar (uniforme sin reemplazo) 9 cartas y se las asigne a un objeto jugador.

Servir <- function(baraja){
  UseMethod("Servir", baraja)
}

Servir.Baraja <- function(baraja){
  
  numero <- sample(baraja$numero, 9, replace = F)
  simbolo <- sample(baraja$simbolo, 9, replace = F)
  
  mazo <- cbind(numero, simbolo)
  
  return(mazo)
}

jugador1 <- Servir(baraja = Baraja_española)
jugador2 <- Servir(baraja = Baraja_española)

# ejercicio 3. Dado lo siguiente, explique y argumente (con evidencia resultante de diferentes comandos) por qué modificar el objeto "bubba" afecta a "louise" y viceversa? Recuerde comentar el código que use como argumento.

NordAmericain <- function(eatsBreakfast = TRUE, myFavorite = "cereal") {
  thisEnv <- environment()
  hasBreakfast <- eatsBreakfast
  favoriteBreakfast <- myFavorite
  me <- list(
    thisEnv = thisEnv,
    getEnv = function() {
      return(get("thisEnv", thisEnv))
    }
  )
  assign('this', me, envir = thisEnv)
  class(me) <- append(class(me), "NordAmericain")
  return(me)
}


bubba <- NordAmericain()
bubba
bubba <- NordAmericain(myFavorite="oatmeal")
bubba
get("favoriteBreakfast", bubba$getEnv())

louise <- bubba
assign("favoriteBreakfast", "toast", louise$getEnv())
get("favoriteBreakfast", louise$getEnv())
get("favoriteBreakfast", bubba$getEnv())

#evidencia, son el mismo entorno

environment(louise$getEnv); environment(bubba$getEnv)