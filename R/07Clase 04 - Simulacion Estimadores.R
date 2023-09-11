##########################################################
#                   ESTADISTICA                          #
#               Prof. DEL ROSSO - LEVINIS                #
#	            MAESTRIA EN CIENCIA DE DATOS               #
#                FACULTAD DE INGENIERIA                  #
#                 UNIVERSIDAD AUSTRAL                    #
##########################################################

## Asignatura: ESTADÍSTICA
## Docentes: Rodrigo Del Rosso - Gustavo Levinis

options(scipen = 999)

#Ejercicio Simulacion
x = ifelse(runif(100000)>0.5,1,-1)
table(x)
x0 = cumsum(x)
x0
plot(seq_along(x0),x0,col="indianred",type = "l",lwd=1,frame.plot = F)
abline(h=0,col="gray",lty="dashed")

#Distribucion normal de la media
media_de_uniforme = function(n=100){
  return(mean(runif(100)))
}

muestra_de_muestras = replicate(1000,media_de_uniforme())

qqnorm(muestra_de_muestras)
qqline(muestra_de_muestras)
hist(muestra_de_muestras)

#Estimacion de parametros
x = rnorm(100,mean=3,sd=5)
sd(x)
mean(x)

#Distribuciones locas
x = rnorm(50,mean=0.5,sd=1)
hist(x)
y = runif(50)
z = c(x,y)
hist(z)
mean(c(x,y))
