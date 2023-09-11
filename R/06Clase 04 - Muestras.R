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

## Simular la suma de dos variables normales mediante la función rnorm(x,mu,sigma) 
## en R con media igual a 0 y desvío igual a 1. 
## Probar con el tamaño de muestra n = 10 y n = 100. 
## ¿Qué observa si grafica ambos objetos?

## Simulación con Variables Aleatorias ##

## Suma de 2 Normales con Media 0 y Desvio 1 ##

rnorm(10)
rnorm(100)

n = 100000
muestra1 = rep(NA,n)
muestra2 = rep(NA,n)
for(i in 1:n){
  muestra1[i] = rnorm(1)
  muestra2[i] = rnorm(1)
}

combinado_2 = muestra1 + muestra2

mean(muestra1) + mean(muestra2)
mean(combinado_2)

sd(combinado_2)

var(muestra1) + var(muestra2)
var(combinado_2)

## Gráfico

x11()

hist(combinado_2, 
     breaks = 30, 
     main = "Suma de 2 Normales", 
     freq = F,
     xlab = "Valores de S")

curve(expr = dnorm(x,mean(combinado_2),sd(combinado_2)),
      add=TRUE, 
      col = "red",
      lty = 2)

## Si se utiliza un solo vector donde se suman ambas variables ##

n = 1000000
muestra = rep(NA,n)
for(i in 1:n){
  muestra[i] = rnorm(1) + rnorm(1) 
}

## Gráfico

x11()

hist(muestra, 
     breaks = 30, 
     main = "Suma de 2 Normales", 
     freq = F,
     xlab = "Valores de S")

## Suma de 10 Normales con Media 0 y Desvio 1 ##

## Simular la suma de diez variables normales mediante la 
## función rnorm en R con media igual a 0 y desvío igual a 1. 
## Probar con distintos valores de tamaño de muestra. 
## Podría probar con n = 100, n = 1000, n = 10.

muestra1 = NULL
muestra2 = NULL
muestra3 = NULL
muestra4 = NULL
muestra5 = NULL
muestra6 = NULL
muestra7 = NULL
muestra8 = NULL
muestra9 = NULL
muestra10 = NULL

n = 10000

for(i in 1:n){
  muestra1[i] = rnorm(1,0,1)
  muestra2[i] = rnorm(1,0,1)
  muestra3[i] = rnorm(1,0,1)
  muestra4[i] = rnorm(1,0,1)
  muestra5[i] = rnorm(1,0,1)
  muestra6[i] = rnorm(1,0,1)
  muestra7[i] = rnorm(1,0,1)
  muestra8[i] = rnorm(1,0,1)
  muestra9[i] = rnorm(1,0,1)
  muestra10[i] = rnorm(1,0,1)
}

combinado_10 = muestra1 + muestra2 + muestra3 + 
  muestra4 + muestra5 + muestra6 + 
  muestra7 + muestra8 + muestra9 +
  muestra10

mean(combinado_10)
sd(combinado_10)
var(combinado_10)

hist(combinado_10, 
     breaks = 30, 
     main = "Suma de 10 Normales", 
     freq = F,
     xlab = "Valores de S")

curve(expr = dnorm(x,mean(combinado_10),sd(combinado_10)),
      add=TRUE, 
      col = "red",
      lty = 2)

## Suma de 2 Normales con Distinta Media y Desvio ##

n = 1000000
muestra1 = rep(NA,n)
muestra2 = rep(NA,n)
for(i in 1:n){
  muestra1[i] = rnorm(1,4,2)
  muestra2[i] = rnorm(1,5,3)
}

combinado_2 = muestra1 + muestra2

hist(combinado_2, 
     breaks = 30, 
     main = "Suma de 2 Normales", 
     freq = F,
     xlab = "Valores de S")

curve(expr = dnorm(x,mean(combinado_2),sd(combinado_2)),
      add=TRUE, 
      col = "red",
      lty = 2)

mean(combinado_2)
sd(combinado_2)
var(combinado_2)

## GRÁFICO ##

## Simular la suma de dos variables normales mediante la función rnorm(x,mu,sigma) 
## en R con media igual a 0 y desvío igual a 1. 
## Probar con distintos valores de tamaño de muestra. 
## Podría probar con n = 100, n = 1000, n = 10000, n = 100000. 
## Graficar para estos distintos valores de muestra. 
## Comprobar que la media (valor esperado) es igual a la suma de sus valores esperados 
## y la varianza es igual a la suma de varianzas individuales.

options(scipen = 999)
par(mar = c(1,1,1,1))
n = c(100,1000,10000,100000)
col = c("blue","red","yellow","green")
layout(matrix(1:4,2,2))

for(j in 1:4){
  
  muestra1 = rep(NA,n[j])
  muestra2 = rep(NA,n[j])
  
  for(i in 1:n[j]){
    muestra1[i] = rnorm(1,0,1)
    muestra2[i] = rnorm(1,0,1)
  }
  
  combinado_2 = muestra1 + muestra2
  
  hist(combinado_2, 
       breaks = 30, 
       main = paste0("Suma con"," ",n[j]),
       freq = F,
       xlab = "Valores de S",
       col = col[j])
  
  curve(expr = dnorm(x,mean(combinado_2),sd(combinado_2)),
        add=TRUE, 
        col = col[j],
        lty = 2)
}

## Suma de 10 Normales con Distinta Media y Desvio ##

muestra1 = NULL
muestra2 = NULL
muestra3 = NULL
muestra4 = NULL
muestra5 = NULL
muestra6 = NULL
muestra7 = NULL
muestra8 = NULL
muestra9 = NULL
muestra10 = NULL

n = 10000

for(i in 1:n){
  muestra1[i] = rnorm(1,3,1)
  muestra2[i] = rnorm(1,4,2)
  muestra3[i] = rnorm(1,5,4)
  muestra4[i] = rnorm(1,2,2)
  muestra5[i] = rnorm(1,1,4)
  muestra6[i] = rnorm(1,6,6)
  muestra7[i] = rnorm(1,7,5)
  muestra8[i] = rnorm(1,8,7)
  muestra9[i] = rnorm(1,9,2)
  muestra10[i] = rnorm(1,10,3)
}

combinado_10 = muestra1 + muestra2 + muestra3 + 
  muestra4 + muestra5 + muestra6 + 
  muestra7 + muestra8 + muestra9 +
  muestra10

mean(combinado_10)
sd(combinado_10)
var(combinado_10)

hist(combinado_10, 
     breaks = 30, 
     main = "Suma de 10 Normales", 
     freq = F,
     xlab = "Valores de S")

curve(expr = dnorm(x,mean(combinado_10),sd(combinado_10)),
      add=TRUE, 
      col = "red",
      lty = 2)

## BONUS TRACK ##
## Mezcla (Mixtura) de Dos Normales

library(ggplot2)

## Con un tamaño "pequeño"
sampa <- rnorm(100,0,1)
sampb <- rnorm(150,3,1)
combinado = c(sampa, sampb)

hist(combinado, breaks = 30)

plt <- ggplot(data.frame(combinado), aes(x=combinado)) + 
  stat_bin(binwidth=0.25, 
           position="identity")
plt

## Con un tamaño "más grande"
sampa <- rnorm(1000000,0,1)
sampb <- rnorm(1500000,3,1)
combinado = c(sampa, sampb)

hist(combinado, breaks = 30)

plot1 <- ggplot(data.frame(combinado), aes(x=combinado)) + 
  stat_bin(binwidth=0.25, 
           position="identity")
plot1

## Mezcla de Dos Normales

pop1 <- rnorm(1000000,10,3)
pop2 <- rnorm(1000000,5,4)

combined <- c(pop1, pop2)

plot2 <- ggplot(data.frame(data=c(combined, pop1, pop2), 
                           labels=rep(c("combined", "pop1", "pop2"), 
                                      c(3e6, 2e6, 1e6))), aes(x=data)) + 
  stat_bin(aes(fill=labels), position="identity", binwidth=0.25, alpha=0.5) + 
  theme_bw()

plot2