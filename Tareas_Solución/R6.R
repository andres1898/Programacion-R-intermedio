##### Andres Felipe Ordoñez Casadiego 2019
##### Curso de programación en R nivel intermedio-avanzado
##### Universidad Industrial de Santander - MTR COLEVOL Santander

# R6 ------------------------------------------------------------------- 
library(R6)

# Ejercicio 1. Cree una clase de cuenta bancaria R6 que almacene un saldo y le permita depositar y retirar dinero. Cree una subclase que arroje un error si intenta entrar en sobregiro. Cree otra subclase que le permita entrar en sobregiro, pero le cobra una tarifa.

# La Cuenta
LaCuenta <- R6::R6Class(classname = "CuentaBancaria",
                        public = list(
                          saldo = NA,
                          initialize = function(saldo){
                            self$saldo <- saldo
                          },
                          print = function(...){
                          cat("Mire mano, su saldo es de:", self$saldo)},
                          retirar = function(x){
                            invisible(self)
                            self$saldo <- self$saldo - x
                            cat("Tu nuevo saldo es de:", self$saldo)
                            },
                          depositar = function(x){
                            self$saldo <- self$saldo + x
                            cat("Tu nuevo saldo es de:", self$saldo)
                          }
                        )
                        )
                         
cuenta1 <- LaCuenta$new(5000)

cuenta1

cuenta1$retirar(8000)
cuenta1$depositar(8000)

# Permite sobregiros
Sobregiro <- R6Class("CuentaBancaria",
                     inherit = LaCuenta,
                     public = list(
                       retirar = function(x){
                         if (x < self$saldo){
                           self$saldo <- self$saldo - x
                           cat("Tu nuevo saldo es de:", self$saldo)
                         } else {cat("No hay suficiente dinero, entrarás en sobre giro \n")
                           cat("Tu saldo es solo de:", self$saldo, "\n")
                           cat("Se generó un cargo de:", self$saldo*0.1)
                           self$saldo <- self$saldo - x}
                       }
                     )
                    )

cuenta2 <- Sobregiro$new(2000)
cuenta2$retirar(3000)

# Prohibido los sobregiros
No_Sobregiro <- R6Class("CuentaBancaria",
                        inherit = LaCuenta,
                        public = list(
                          retirar = function(x){
                            if (x < self$saldo){
                              self$saldo <- self$saldo - x
                              cat("Tu nuevo saldo es de:", self$saldo)
                            } else {cat("No hay suficiente dinero, Sorry \n")
                              cat("Tu saldo es solo de:", self$saldo)}
                        }))

cuenta3 <- No_Sobregiro$new(5000)
cuenta3$retirar(10000)


# Ejercicio 2. Cree una clase (parquedero) R6 que almacene diferentes vehiculos y le permita  diferencar motocicletas y automoviles. Cree una subclase para diferenciar el cobro por tipo de vehiculo y otra por la cantidad de tiempo. Adicione otra clase "dia" y finalemente presente los ingresos mensueales.

# El parqueadero

ElParquedero <- R6Class("parquedero",
                        public = list(
                          motos = NA,
                          automovil = NA,
                          initialize = function(automovil, motos){
                            self$motos <- motos
                            self$automovil <- automovil
                          },  
                          print = function(...){
                            cat("Hay", self$automovil, "carros parqueados\n")
                            cat("Hay", self$motos, "motos parqueadas\n")
                          }
                        ))

MiNegocio <- ElParquedero$new(3, 10)
MiNegocio


# La subclase para el cobro por tipo
Factura_tipo <- R6Class("parquedero",
                   inherit = ElParquedero,
                   public = list(
                     A_pagar = function(Cobro_autos = 3000,
                                        Cobro_motos = 1000){
                       cat("Los carros deben pagar", Cobro_autos, "\n")
                       cat("Las motos deben pagar", Cobro_motos, "\n")
                     }
                   )
                   )

por_tipo <- Factura_tipo$new(10,9)
por_tipo$A_pagar()

# La subclase para el cobro por tiempo
Factura_tiempo <- R6Class("parquedero",
                          inherit = ElParquedero,
                          public = list(
                            A_pagar = function(tiempo){
                              pago <- tiempo * 20
                              cat("El valor a pagar por sus", tiempo, "minutos es de:", pago)
                            }
                          )
                          )

por_tiempo <- Factura_tiempo$new(10,9)
por_tiempo$A_pagar(3000)

# Ganancias al mes
Ganancias <- R6Class("parquedero",
                     inherit = ElParquedero,
                     public = list(
                       Ganancia = function(tiempo, precio){
                         Las_motos <- self$motos * tiempo * precio
                         Los_carros <- self$automovil * tiempo * precio
                         
                         cat("Las motos generaron", Las_motos, "ganancias \n")
                         cat("Los carros generaron", Los_carros, "ganancias \n")
                       }
                      )
                     )

el_mes <- Ganancias$new(10, 9)
el_mes$Ganancia(tiempo = 10, precio = 200)
