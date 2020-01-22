##### Andres Felipe Ordoñez Casadiego 2019
##### Curso de programación en R nivel intermedio-avanzado
##### Universidad Industrial de Santander - MTR COLEVOL Santander

##########
# RegEx   -------------------------------------------------

vectorCars <- c("juan1@hotmail.com",
                "2juanes@hotmail.com",
                "juaMes@hatmail.com",
                "pedro99@gmail.com",
                "pedro@hotmail.com",
                "pedro9@gmail.com",
                "peter11@xmail.com",
                "otroJujuan99@hotmail.com",
                "otrojuan99@gmail.com",
                "juanchito99@gmail.com",
                "superJuancho99@xmail.com",
                "mary01@gmail.com",
                "sistermarria@gmail.com",
                "marryMe@gmail.com",
                "maria01@gmail.com",
                "maRia01@gmail.com",
                "jhonBravo@gmail.xom",
                "johnBravo@gmail.com",
                "jaunMalEscritoBravin@gmiial.com",
                "jaunGravo@gmail.com",
                "JHONNY@gmail.com",
                "pedrito611@gmail.com",
                "marian@hotmail.com",
                "PedroMaria@gmail.com",
                "juanMaria@xmail.com",
                "JuanPedro@hotmaail.com",
                "pedroJuan@hotmail.com",
                "juanPedrito@hotmail.com")


### crear un regex para extraer del vectorCars
library(stringr)

## Ejercicio 1. el mismo usario (explícito) en dos proveedores de correo

tmp_usuario <- matrix(nrow = length(vectorCars), ncol = 2)

for (i in 1:length(vectorCars)){
  tmp_usuario[i, 1] <- str_split(vectorCars[i], "@")[[1]][1]
  tmp_usuario[i, 2] <- str_split(vectorCars[i], "@")[[1]][2]
}

for (i in 1:nrow(tmp_usuario)) {
  tmp_usuario[i,1] == tmp_usuario[i,1]
}

length(unique(tmp_usuario[,1])) == length(levels(as.factor(tmp_usuario[,1])))

#no hay usuarios explicitos iguales

## Ejercicio 2. potencialmente el mismo usuario tanto en el mismo proveedor como en distintos proveedores. Para los dos casos suponga que solo existen tres nombres: Maria / Juan / Pedro, en distintos idiomas

#juan
tmp_juan <- str_detect("j", string = tmp_usuario[,1])

juan_correo <- tmp_usuario[tmp_juan,]

#pedro
tmp_pedro <- grep("^p", tmp_usuario[,1], ignore.case = T)

pedro_correo <- tmp_usuario[tmp_pedro,]

#maria
tmp_maria <- grep("mar", tmp_usuario[,1], ignore.case = T)

maria_correo <- tmp_usuario[tmp_maria,]

## Ejercicio 3. corrija de manera automatica los usarios o los dominios con nombres errados; preferiblemente escrito como una función.

ElLimpiador <- function(vector_correos) {
  
  require(stringr)
  
  #separar los usuarios de los correos
  tmp_usuario <- matrix(nrow = length(vector_correos), ncol = 2) #crear un objeto en blanco
  for (i in 1:length(vector_correos)){
    tmp_usuario[i, 1] <- str_split(vector_correos[i], "@")[[1]][1] #escribir
    tmp_usuario[i, 2] <- str_split(vector_correos[i], "@")[[1]][2] #escribir
  }
  
  #arreglar los correos, asumo que solo existe gmail y hotmail
  tmp_correo <- str_detect(pattern = "^.m", tmp_usuario[,2])
  tmp_usuario[tmp_correo,2] <- "gmail.com"
  
  tmp_correo <- str_detect(pattern = "^.m", tmp_usuario[,2], negate = T)
  tmp_usuario[tmp_correo,2] <- "hotmail.com"
  
  # para Maria
  tmp_maria <- grep("mar", tmp_usuario[,1], ignore.case = T) #1er filtro
  tmp_maria_correo <- tmp_usuario[tmp_maria,]
  tmp_maria <- str_detect(pattern = "[pP]e|ju", tmp_maria_correo[,1], negate = T) #2do filtro
  maria_correo <- tmp_maria_correo[tmp_maria,] 
  maria_correo <- paste(maria_correo[,1], maria_correo[,2], sep = "@")
  
  # para Pedro
  tmp_pedro <- grep("pe[dt]", tmp_usuario[,1], ignore.case = T) #1er filtro
  tmp_pedro_correo <- tmp_usuario[tmp_pedro,]
  tmp_pedro <- str_detect(pattern = "[Jj]|[Mm]a", tmp_pedro_correo[,1], negate = T) #2do filtro
  pedro_correo <- tmp_pedro_correo[tmp_pedro,] 
  pedro_correo <- paste(pedro_correo[,1], pedro_correo[,2], sep = "@")
  
  # para Juan
  tmp_juan <- grep("j", tmp_usuario[,1], ignore.case = T) #1er filtro
  tmp_juan_correo <- tmp_usuario[tmp_juan,]
  tmp_juan <- str_detect(pattern = "[Pp]ed|[Mm]ar", tmp_juan_correo[,1], negate = T) #2do filtro
  juan_correo <- tmp_juan_correo[tmp_juan,] 
  juan_correo <- paste(juan_correo[,1], juan_correo[,2], sep = "@")
  
  # ahora los que no se saben que son
  tmp_juan_ind <- str_detect(pattern = "[Pp]ed|[Mm]ar", tmp_juan_correo[,1]) 
  juan_correo_ind <- tmp_juan_correo[tmp_juan_ind,] 
  juan_correo_ind <- paste(juan_correo_ind[,1], juan_correo_ind[,2], sep = "@")
  
  tmp_pedro_ind <- str_detect(pattern = "[Jj]|[Mm]a", tmp_pedro_correo[,1])
  pedro_correo_ind <- tmp_pedro_correo[tmp_pedro_ind,]
  pedro_correo_ind <- paste(pedro_correo_ind[,1], pedro_correo_ind[,2], sep = "@")
  
  tmp_maria_ind <- str_detect(pattern = "[pP]e|ju", tmp_maria_correo[,1])
  maria_correo_ind <- tmp_maria_correo[tmp_maria_ind,] 
  maria_correo_ind <- paste(maria_correo_ind[,1], maria_correo_ind[,2], sep = "@")
  
  correos_indeterminados <- unique(c(juan_correo_ind, pedro_correo_ind, maria_correo_ind))
  
  correos_limpios <- list(Maria = maria_correo, Pedro = pedro_correo, Juan = juan_correo, Correos_indeterminados = correos_indeterminados)

}

correos_limpios <- ElLimpiador(vectorCars)

### Utilizando el archivo fasta
#library(ape)
library(stringr)

adn <- read.csv("tarea.fasta", header = F, sep = "")
tmp_secuencia <- as.vector(adn$V2[1])


## Ejercicio 4.1 cuantificar el número de veces que se repiten tripletas de la misma base AANA/CCC/TTNT/GGG  
  
Contar_tripletas_N <- function(adn) {
  
  tmp_secuencia <- adn
  AAA <- str_count(tmp_secuencia, "A{3}")
  CCC <- str_count(tmp_secuencia, "C{3}")
  TTT <- str_count(tmp_secuencia, "T{3}")
  GGG <- str_count(tmp_secuencia, "G{3}")
  
  Tripleta_sec <- list(AAA = AAA, 
                       CCC = CCC,
                       TTT = TTT,
                       GGG = GGG)
  
  return(Tripleta_sec)

}

Conteo_tripletas_N <- apply(adn, 1, Contar_tripletas_N)


## Ejercicio 4.2 repita el conteo anterior pero considere que N puede ser la base de la izquierda o de la derecha

Contar_tripletas_NN <- function(adn) {
  
  tmp_secuencia <- adn
  AAA <- str_count(tmp_secuencia, "AAAA")
  CCC <- str_count(tmp_secuencia, "CCC")
  TTT <- str_count(tmp_secuencia, "TTTT")
  GGG <- str_count(tmp_secuencia, "GGG")
  
  Tripleta_sec <- list(AAAA = AAA, 
                       CCC = CCC,
                       TTTT = TTT,
                       GGG = GGG)
  
  return(Tripleta_sec)
  
}

Conteo_tripletas_NN <- apply(adn, 1, Contar_tripletas_NN)


## Ejercicio 5.1 Suponga que los datos contienen 0 / 1 o 2 genes, el gen 1 inicia con SSSN y termina en WWWW y el gen 2 inicia en BWW y termina en AANA, indique si cada entrada contiene o no los genes 1 y/o 2. [Pista, inicio sucede ANTES que termina]

Encontrar_genes <- function(adn) {
  
  if (!is.na(str_locate(adn, "[CGS][CGS][CGS][ACGT]")[1]) && !is.na(str_locate(tmp_secuencia, "[ATW][ATW][ATW][ATW]")[1])) {
    gen1 <- paste("El gen 1 está presente")
  } else {gen1 <- paste("El gen 1 no está presente")}
  
  if (!is.na(str_locate(adn, "[BCGT][ATW][ATW]")[1]) && !is.na(str_locate(adn, "AA[ACGT]A")[1])) {
    gen2 <- paste("El gen 2 está presente")
  } else {gen2 <- paste("El gen 2 no está presente")}
  
  genes <- list(Gen1 = gen1,
                Gen2 = gen2)
  
  return(genes)
}

Genes_identificados <- sapply(adn[,2], Encontrar_genes)

str(Genes_identificados)

## Ejercicio 5.2 cuantificar el número de veces que se repiten tripletas de la misma base AAA/CCC/TTT/GGG

Contar_tripletas <- function(adn) {
  
  tmp_secuencia <- adn
  AAA <- str_count(tmp_secuencia, "AAA")
  CCC <- str_count(tmp_secuencia, "CCC")
  TTT <- str_count(tmp_secuencia, "TTT")
  GGG <- str_count(tmp_secuencia, "GGG")
  
  Tripleta_sec <- list(AAA = AAA, 
                       CCC = CCC,
                       TTT = TTT,
                       GGG = GGG)
  
  return(Tripleta_sec)
  
}

Conteo_tripletas <- apply(adn, 1, Contar_tripletas)


## Ejercicio 5.3 repita el conteo anterior pero considere que N (el codigo IUPAC) puede ser cualquier base

Contar_tripletas <- function(adn) {
  
  tmp_secuencia <- adn
  AAA <- str_count(tmp_secuencia, "[AN]{3}")
  CCC <- str_count(tmp_secuencia, "[CN]{3}")
  TTT <- str_count(tmp_secuencia, "[TN]{3}")
  GGG <- str_count(tmp_secuencia, "[GN]{3}")
  
  Tripleta_sec <- list(AAA = AAA, 
                       CCC = CCC,
                       TTT = TTT,
                       GGG = GGG)
  
  return(Tripleta_sec)
  
}

Conteo_tripletas <- apply(adn, 1, Contar_tripletas)