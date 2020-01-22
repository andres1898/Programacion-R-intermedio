##### Andres Felipe Ordoñez Casadiego 2019
##### Curso de programación en R nivel intermedio-avanzado
##### Universidad Industrial de Santander - MTR COLEVOL Santander

# ENTORNOS -------------------------------------------------------------------

# Responda las siguientes preguntas y comente las operaciones y valores de cada objeto en cada función
# ejerc 1 ---------------------------------------

func_1 <- function(u){
  
  u <<- 2*u   # es un reasignación local o global? REASIGNACIÓN GLOBAL DE LA VARIABLE u
  
  return(u)   # REPORTA EL VALOR DE u
  
}

u <- 1        # ASIGNAR EL VALOR DE u COMO VARIABLE GLOBAL

func_1(u) # es un reasignación local o global? GLOBAL


# ejerc 2 ---------------------------------------

func_1 <- function(u){
  
  u <- 2*u    #AGINACIÓN LOCAL DE LA VARIABLE u
  
  func_2 <- function(uu){
    u <<- 3*uu    # es un reasignación local o global? LOCAL HACIA EL ENTORNO ANTERIOR
  }
  
  return(u)   # RETORNA EL VALOR DE u, DEBERIA SER LA u DE func_2
  return(func_2(u))   #DEBERIA SER LA MISMA u DE ARRIBA
  
}

u <- 1

func_1(u) # es un reasignación local o global?


# ejerc 3 ---------------------------------------

fun_1 <- function(d, j){
  d <- 8               # VARIABLE LOCAL REASIGNADA
  j <- "esto es local y por defecto"   # VARIABLE LOCAL REASIGNADA
  paste(d,j, sep = " ")   # MUESTRA LAS VARIABLES REASIGNADAS
}

d <- 1   # VARIABLE GLOBAL ASIGNADA
j <- "esto es global y un argumento"   # VARIABLE GLOBAL ASIGNADA

fun_1()      #porque ignora las variables locales?  IGNORA LAS GLOBALES NO LAS LOCALES


# ejerc 4 ---------------------------------------

fun_1 <- function(d = 8, j = "esto es local y por defecto"){  
  paste(d,j, sep = " ")
}

fun_1()      #que valores muestra, explique su respuesta LA FUNCION TIENE VARIABLES CON VALORES POR DEFECTO

fun_1(d = v1, j = v2)   #que valores muestra, explique su respuesta AUNQUE LA FUNCION TENGA VALORES POR DEFECTO, SI SE LE ESPECIFICAN LOS TOMA COMO ARGUMENTO E IGNORA LOS POR DEFECTO
