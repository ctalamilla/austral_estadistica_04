# Ejercicio 1
#En la Facultad de Ciencias Económicas, la altura de los estudiantes se distribuye
#normalmente con una media de 174 cm y desvío 20 cm. Se toma un curso de 50
#alumnos al azar:
#a. ¿Cuál es la probabilidad de que la altura promedio de la muestra sea inferior
#a 172 cm? 

media_poblacion = 174
sd_poblacion = 20
n=50
valor = 172

desvio_dela_media = sd_poblacion/sqrt(n)

#alterativa 1
z = (valor - media_poblacion) / (sd_poblacion/sqrt(n))
probabilidad = pnorm(z)

#alternativa 2
probabilidad <- pnorm(valor, mean = media_poblacion, sd = desvio_dela_media)
probabilidad

#b. ¿De qué tamaño deberá ser la muestra si se quiere que esta probabilidad
#sea de 0,20?

# Calcular el n para P( X < 172) = 0.20

media_poblacion = 174
sd_poblacion = 20
n=50

valor = 172
probabilidad_objetivo= 0.2

desvio_dela_media = sd_poblacion/sqrt(n)

z_objetivo = qnorm(probabilidad_objetivo)
z_objetivo


# z = x-X/(sd_poblacion/sqrt(n))
# z = 172-174 / (20/sqrt(n))
# z * 20 / sqrt(n) = -2
# 1 / sqrt(n) = -2 / z*20
#sqrt(n) = z*20 / -2
# n = (z*20/-2)**20

 n = (z_objetivo * 20 / -2)**2


#Ejercicio 2
#La proporción de fumadores en la Ciudad de Buenos Aires es de 0,35. Se toma una
#muestra de 50 personas al azar. ¿Cuál es la probabilidad de que la proporción de
#fumadores de la muestra sea menor a 0,30? 

# Parámetros de la distribución binomial
p <- 0.35  # Probabilidad de éxito (proporción de fumadores)
n <- 50    # Tamaño de la muestra

#P(p<30) => n = 50
pp = 0.3

z = (pp - p) / sqrt(p*(1-p)/n)
prob = pnorm(z)
prob

# Ejercicio 3

#Los precios de los artículos que vende un supermercado, se distribuyen normalmente con media US$ 4 y desvío US$ 0,75. Se toma al azar una muestra de 50 artículos.
#¿Cuál es la probabilidad de que el precio promedio de los artículos de la muestra esté entre US$ 3,9 y US$ 4,2?

# precios ~ N (4, 0.75)
media_poblacion = 4
desvio_poblacion =0.75
#muestra
n = 50
media_muestra = 4
desvio_muestra = desvio_poblacion/sqrt(n)

# P(3.9 < X < 4.2)
zi = (3.9 - media_poblacion)/desvio_muestra
pi = pnorm(zi)

zs = (4.2 - media_poblacion)/desvio_muestra
ps = pnorm(zs)

ps - pi

# Ejercicio 4
#Sabiendo que el peso de los paquetes de galletitas de una conocida empresa alimenticia se distribuye normalmente con desvío 398 gramos.

# pesp ~ N ( , sd = 398 )
sd_poblacion = 398


#a ¿Cuál es la probabilidad de que la media de una muestra de 40 paquetes difiera del peso medio en menos de 50 gramos?

n = 40
desvio_muestra = sd_poblacion / sqrt(n)

# P (x - X) < 50

diferencia  =  50
zobj = (diferencia / desvio_muestra)
zobj # zobj = valor de media mas la diferencia. 


#Alternativa 1, por simetria. Calculo el area entre la media (z=0, p=0.5) y z de la diferencia.
2 * (pnorm(zobj) - pnorm(0))

# Alternativa 2, calculo el area entre la media +- 50
pnorm(zobj) - pnorm(-zobj)

#b ¿De qué tamaño debería ser la muestra si se quiere que dicha probabilidad
# sea del 0,90?

# Parámetros del problema
desvio_poblacion <- 398  # Desvío estándar de la población
diferencia_maxima <- 50  # Diferencia máxima permitida en gramos
nivel_confianza <- 0.90  # Nivel de confianza deseado (90%)

#percentil 95
alpha = 1 - nivel_confianza

# Encontrar el valor Z crítico correspondiente al percentil 0.95
z_critico <- qnorm(1-alpha/2)

z_critico2 = qnorm(0.05)


# n = (z (alpha/2) * desvio_poblacion / error )**2 

# Calcular el tamaño de la muestra necesario
n <- (z_critico2 * (desvio_poblacion / diferencia_maxima))^2

# Redondear hacia arriba al número entero más cercano, ya que el tamaño de la muestra debe ser un número entero
n <- ceiling(n)

# Ejercicio 5

#Se sabe que el 60% de los alumnos de la UBA, trabajan. También se sabe que el
#45% de los alumnos de una determinada universidad privada, trabajan.

p_trabajar = 0.6

# a) Si se toma una muestra al azar de 80 alumnos de la UBA, ¿cuál es la
#probabilidad de que menos de la mitad de los alumnos de la muestra
#trabajen?

n = 80
pobjetivo = 0.5 # menos de la mitad trabajen

z = (pobjetivo - p_trabajar) / sqrt(p_trabajar*(1-p_trabajar)/n)
# z = -1.82574

resultado = pnorm(z)
#0.0339

# b) ¿Y si se toma una muestra del mismo tamaño en la universidad privada?

p_trabajar = 0.45
n=80
pobjetivo = 0.5

z = (pobjetivo - p_trabajar) / sqrt(p_trabajar*(1-p_trabajar)/n)
# z = -1.82574

resultado = pnorm(z)
#0.81565

# Ejercicio 6 

# La resistencia a la rotura de ciertos cables de acero producidos por una empresa
# es una variable aleatoria distribuida normalmente con media 15.800 kg/m y con
# un desvío estándar de 2.600 kg/m. ¿Cuál es la probabilidad de que una muestra
# de 16 cables de la misma longitud, proporcione una media superior a 17.360
# kg/m?

media_poblacion = 15800
sd_poblacion = 2600

n = 16
media_muestra = 17360
desvio_muestra = sd_poblacion / sqrt(n)

z = (media_muestra - media_poblacion) / desvio_muestra
# z = 2.4

resultado = 1 - pnorm(2.4) # es 1 - P(z) por que busca probabilidad superior
# 0.00819

# Ejercicio 7
# Se sabe que en un lote de 700 lápices hay un 5% que presentan defectos de
# fabricación. ¿Cuál es la probabilidad de que en una muestra de 80 lápices
# provenientes de dicho lote se encuentren a lo sumo 5 lápices con defectos?

n_poblacion = 700
p_poblacion = 0.05

n_muestra = 80
p_objetivo = 5/80

factor_ajuste = sqrt((n_poblacion-n_muestra)/(n_poblacion-1))

z = (p_objetivo - p_poblacion)/(sqrt(p_poblacion*(1-p_poblacion)/n_muestra) * factor_ajuste)
# z = 0.5129
pnorm(z)

# Ejercicio 8
# Una empresa tiene 478 clientes. En promedio, cada uno compra mensualmente
# por valor de $935.460, con un desvío estándar de $38.274. ¿Cuál es la probabilidad
# de que el promedio de una muestra de 70 clientes esté entre $930.000 y $940.000?

N = 478
media_poblacion = 935460
sd_poblacion = 32274

n = 70
sd_muestra = sd_poblacion/sqrt(n)
li = 930000
ls = 940000

factor_ajuste = sqrt((N-n)/(N-1))

zi = (li - media_poblacion) / sd_muestra * factor_ajuste
#zi = -1.4154
pzi = pnorm(zi)
#pzi = 0.078471

zs = (ls - media_poblacion) / sd_muestra * factor_ajuste
#zi = -1.4154
pzs = pnorm(zs)
#pzi = 0.88038

resultado = pzs - pzi
#0.7665

# Ejercicio 9
# El 25% de los clientes que desean renovar su crédito en cierta Entidad Financiera,
# no requiere codeudor en razón de sus antecedentes. Se sacó una muestra de 160
# solicitudes de renovación. ¿Cuál es la probabilidad de que la proporción de clientes
# que no necesitan codeudor esté entre el 20% y el 28%?

pp = 0.25

n = 160
pminf = 0.20
pmsup = 0.28

zinf = (pminf - pp) / sqrt(pp*(1-pp)/n)
# zinf = -1.4605
pzinf = pnorm(zinf)
#pzinf = 0.07206

zsup = (pmsup - pp) / sqrt(pp*(1-pp)/n)
# zinf = -1.4605
pzsup = pnorm(zsup)
#sup = 0.8763

resultado = pzsup - pzinf
#0.7375

# Ejercicio 10
# Se sabe que las ventas efectuadas por una empresa tienen distribución normal con
# media $343.200 y desvío estándar de $48.152. De las ventas realizadas en el mes,
# se saca una muestra de 16 facturas.

Mu = 343200
S = 48152

#(a) ¿Cuál es la probabilidad de que la media de la muestra difiera de la media
#poblacional en más de $20.000?

n=16
dif = 20000
s = S/ sqrt(n)


v_objetivo1 = Mu - dif
z1 = (v_objetivo1 - Mu) / s
p1 = pnorm(z1)

v_objetivo2 = Mu + dif
z2 = (v_objetivo2 - Mu) / s
p2 = 1 - pnorm(z2)

resultado = p1+p2
#0.096

# ¿Cuál es el valor de la media muestral que será superado con probabilidad
#0,05?

#Alternativa 1
resultado = qnorm(1-0.05, mean = Mu, sd=s)

#363000.7

# Alternativa 2
z = qnorm(1-0.05)
# z = 1.6448
# z = media - Mu / s
# z *s + Mu = media

resultado = z*s+Mu
#36300.7



