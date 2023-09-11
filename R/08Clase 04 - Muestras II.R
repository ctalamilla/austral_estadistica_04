##########################################################
#                   ESTADISTICA                          #
#               Prof. DEL ROSSO - LEVINIS                #
#	            MAESTRIA EN CIENCIA DE DATOS               #
#                FACULTAD DE INGENIERIA                  #
#                 UNIVERSIDAD AUSTRAL                    #
##########################################################

## Asignatura: ESTADÍSTICA
## Docentes: Rodrigo Del Rosso - Gustavo Levinis

rm(list = ls())

## CON POISSON ##

muestra1 = NULL
muestra2 = NULL
n = 1000000
for(i in 1:n){
  muestra1[i] = runif(1)
  muestra2[i] = runif(1)
}

muestra = muestra1 + muestra2

## Gráfico

x11()
layout(matrix(1:4,2,2,byrow = T))

hist(muestra1,
     breaks = 10,
     main = "Poisson con Lambda 1.5",
     freq = F)

hist(muestra2,
     breaks = 10,
     main = "Poisson con Lambda 2",
     freq = F)

hist(muestra,
     breaks = 30,
     main = "Suma de 2 Uniformes",
     freq = F)

## CON BINOMIAL ##

muestra1 = NULL
muestra2 = NULL
n = 1000000
for(i in 1:n){
  muestra1[i] = rbinom(1,1,0.50)
  muestra2[i] = rbinom(1,1,0.50)
}
muestra = muestra1 + muestra2

## Gráfico

x11()
layout(matrix(1:4,2,2,byrow = T))

hist(muestra1,
     breaks = 10,
     main = "Poisson con Lambda 1.5",
     freq = F)

hist(muestra2,
     breaks = 10,
     main = "Poisson con Lambda 2",
     freq = F)

hist(muestra,
     breaks = 30,
     main = "Suma de 2 Poisson",
     freq = F)