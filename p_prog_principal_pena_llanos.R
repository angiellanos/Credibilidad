
# Modelo de estimación y credibilidad
# CREDIBILIDAD Y MODELOS DE PÉRDIDA
# UNIVERSIDAD NACIONAL DE COLOMBIA
# Maestría en Actuaría y Finanzas
# 2023-II


# Presentado por: Angie Llanos, Camila Peña
# Presentado a: Edwin Riveros


##          DEFINIR E IMPORTAR LOS DATOS EN R

rm(list=ls())
options(scipen=999)
options(digits=8)

#Carga de librerias
# install.packages (c ("lubridate", "dplyr", "ggplot2", "readr", "janitor", "pastecs", "modeest"), character.only = TRUE)
library(lubridate)
library(dplyr)
library(ggplot2)
library(readr)
library(janitor)
library(pastecs)
library(modeest)
library(actuar)
library(nleqslv)

# Datos originales
data <- read_delim("d_datos_modelo_pena_llanos.txt", delim = "\t")

# Se buscan valores importados como cadena vacía: "", NA o ND
any(data == "")          # No hay cadenas vacías
any(is.na(data))         # No hay NA
!nzchar(data)            # Ninguna columna tiene elementos vacíos
any(grepl("ND", data))   # No hay ND
any(duplicated(data))    # No hay duplicados

# Datos del modelo
datos <- data %>% clean_names()
# Se seleccionan solo los datos del año 2016
datos <- filter (datos, filing_year == 2016)
# Verificar que todas las aseguradoras en el año 2016 son diferentes
any(duplicated(datos$naic))
# Verificar que todas las aseguradoras tienen premiums_written_in_millions diferentes
any(duplicated(datos$premiums_written_in_millions))
# Se seleccionan solo las columnas de interés en el análisis, index y premiums_written_in_millions
datos <- datos[, c(1,9)]

# Inspección incial de los datos
summary(datos$premiums_written_in_millions)
# Note que es una distribución con cola pesada a la derecha.
# Luego, como conclusión incial se tiene que ese valor máximo aparenta ser un valor atípico.
# En principio, a la variable se le pueden calcular medidas de tendencia central y de dispersión
# Por ende, sí aparenta tener datos raros.

x <- as.numeric(datos$premiums_written_in_millions)
x
n <- as.numeric(datos$index)
n


##          ANÁLISIS DESCRIPTIVO Y DISTRIBUCIÓN EMPÍRICA DE X


# Como las aseguradoras seleccionados son una muestra aleatoria,
# se deben realizar estas métricas con tratamiento muestral.
# Así, se definen las funciones:

momento     <- function(X,n,i){sum(X^i)/n}
cuartil     <- function(X,n,alfa){z <- sort(X)
if (n%%4 == 0){(z[n*alfa]+z[(n*alfa)+1])/4} else {z[ceiling(n*alfa)]}}
varianza    <- function(X,n){momento(X,n,2)-((momento(X,n,1))^2)}
asimetria   <- function(X,n){sum((X-momento(X,n,1))^3)/(n*(varianza(X,n))^(3/2))}
curtosis    <- function(X,n){sum((X-momento(X,n,1))^4)/(n*(varianza(X,n))^(2))-3}

m <- length(x)
m

##      Métricas descriptivas de x:

mean   <- momento(x,m,1)
median <- cuartil(x,m,0.5)
mode   <- mlv(x, method = "meanshift")[1]

var    <- varianza(x,m)
std    <- sqrt(var)
cv     <- std/mean

VaR_95  <- cuartil(x,m,0.95)
TVaR_95 <- momento(x[x>=VaR_95],length(x[x>=VaR_95]),1)

skew   <- asimetria(x,m)
kurt   <- curtosis(x,m)

# Estas métricas se calculan para entender si hay valores atítipos necesarios a eliminar de la muestra
min      <- min(x)
max      <- max(x)
cuartiles <- quantile(x, probs = c(0.25, 0.5, 0.75))
IQR       <- cuartiles[3] - cuartiles[1]
bigote1  <- max(min,cuartiles[1]-1.5*IQR)
bigote2  <- min(max,cuartiles[3]+1.5*IQR)

# Presenta las métricas anteriores de forma organizada
summ <- data.frame(
  "Mean" = mean, "Median" = median, "Mode" =  mode,
  "Variance" = var, "Std_deviation" = std, "Coeff_variation" =  cv,
  "Value_at_Risk_95" = VaR_95, "Tale_Value_at_Risk_95" = TVaR_95,
  "Skewness" =  skew,"Curtosis" =  kurt, row.names = "Valor",
  "min" = min, "max" = max, "Cuartil 1" = cuartiles[1],
  "Cuartil 2" = cuartiles[2], "Cuartil 3" = cuartiles[3], "IQR" = IQR,
  "Bigote 1" = bigote1, "Bigote 2" = bigote2
  )
t(summ)

# Se observa que entre el Bigote 2 y el valor máximo hay mucha distancia
# Por lo tanto, veamos cuáles y cuántos son estos datos:

z = x[x>bigote2]
z
length(z)

z = x[x>310]
z
length(z)

# Para no eliminar todos los valores atípicos, 20datos relativos al 12.66%
# se opta por eliminar solo los que están por encima de 310, notando que
# se reduce el porcentaje a 4.43 y notando que como por encima de este valor
# hay valores muy extremos, las medidas de tendencia central serán más controladas
# bajando así la varianza y por ende la desviación típica. Veamos estos cálculos
# para la nueva variable aleatoria:

x <- x[x<310]
x
m <- length(x)
m

mean   <- momento(x,m,1)
median <- cuartil(x,m,0.5)
mode   <- mlv(x, method = "meanshift")[1]

var    <- varianza(x,m)
std    <- sqrt(var)
cv     <- std/mean

VaR_95  <- cuartil(x,m,0.95)
TVaR_95 <- momento(x[x>=VaR_95],length(x[x>=VaR_95]),1)

skew   <- asimetria(x,m)
kurt   <- curtosis(x,m)

# Estas métricas se calculan para comparar con los datos iniciales
min      <- min(x)
max      <- max(x)
cuartiles <- quantile(x, probs = c(0.25, 0.5, 0.75))
IQR       <- cuartiles[3] - cuartiles[1]
bigote1  <- max(min,cuartiles[1]-1.5*IQR)
bigote2  <- min(max,cuartiles[3]+1.5*IQR)

# Presenta las métricas anteriores de forma organizada
summ_n <- data.frame(
  "Mean" = mean, "Median" = median, "Mode" =  mode,
  "Variance" = var, "Std_deviation" = std, "Coeff_variation" =  cv,
  "Value_at_Risk_95" = VaR_95, "Tale_Value_at_Risk_95" = TVaR_95,
  "Skewness" =  skew,"Curtosis" =  kurt, row.names = "Valor",
  "min" = min, "max" = max, "Cuartil 1" = cuartiles[1],
  "Cuartil 2" = cuartiles[2], "Cuartil 3" = cuartiles[3], "IQR" = IQR,
  "Bigote 1" = bigote1, "Bigote 2" = bigote2
)
t(summ)
t(summ_n)

# Dado que, las métricas como la media, la varianza, el VaR, el TVaR presentaron
# elevado cambio en su valor, se puede restringir este estudio a aseguradoras
# o productos de cobertura de riesgo que presenten niveles altos para los
# deducibles en sus pólizas de seguro, o tengas bajos límites de cobertura, o
# en su defecto, estas dos condiciones, con el fin de que la premium written
# in millions no supere o no por mucho el valor máximo tenido en cuenta aquí,
# también pueden ser incluidas aseguradoras que cubran riesgos pequeños o que
# sean nuevas en el mercado y no tengan una alta demanda y por ende alto riesgo
# así, este análisis resulta bastante descriptivo e inferencial para este tipo
# de productos en el mercado de seguros.

##      Gráficos descriptivos de x:

# Histograma
datos <- filter(datos, premiums_written_in_millions < 310)
x11();ggplot(datos, aes(x=premiums_written_in_millions)) + geom_histogram(binwidth = 5)

# Ojiva
x11();plot(cumsum(x));lines(cumsum(x), col = 'purple')
# En términos relativos
x11();plot(cumsum(x/sum(x)));lines(cumsum(x/sum(x)), col = 'purple')

# Boxplot
x11();boxplot(x, horizontal = TRUE);stripchart(x, method = "jitter", pch = 19, add = TRUE, col = "blue")

# Diagrama de secuencia de x: i vs xi
x11();plot(x, main = "Diagrama de secuencia de x")

# Estimadores empíricos de la f.d.d (fn(x)) y f.d.D (Fn(x)) de x.
Fn <- function(z){
  sum( sort(x) <= z )/m
}

fdd_fdD <- function(x,n,titulo_d,titulo_D){
  h <- 1
  z_01 <- seq(0,310-h,h)
  z <- seq(0+h,310,h)
  f <- (sapply(z, Fn)-sapply(z_01, Fn))/h   #Derivada de la Distribución Acumulada
  x11();plot( z, f, type = "h", lwd=5, xlab = "x", ylab = "fn(x)", main = titulo_d)
  x11();plot( z, sapply(z, Fn), type = "s", xlab = "x", ylab = "Fn(x)", main = titulo_D)
  f
}
f  = fdd_fdD(x,m,"Densidad Empírica de x, fn(x)","Distribución Empírica de x, Fn(x)")

# Estimando la varianza de Fx en x1=7.094 y x2=101.955.
x1 <- x[45]
x2 <- x[95]

z <- seq(5,2300,5)
media_Fx <- momento(sapply(z, Fn),length(sapply(z, Fn)),1)
var_x1   <- (Fn(x1) - media_Fx)^2
var_x1
var_x2   <- (Fn(x2) - media_Fx)^2
var_x2

# Gráfica del estimador de kernel de la f.d.d usando
graf_kernel <- function(x, ancho_banda){

  # i) kernel uniforme
  densidad_uniforme <- density(x, kernel = "rectangular", bw = ancho_banda)

  # ii) kernel triangular
  densidad_triangular <- density(x, kernel = "triangular", bw = ancho_banda)

  # Grafica los resultados
  hist(x, breaks = 40,prob=T, main = paste("Est. kernel f.d.d con kernel uniforme y triangular, b=",round(ancho_banda,2)), xlab = "Variable", ylab = "Densidad")
  lines(densidad_uniforme, col = "red")
  lines(densidad_triangular, col = "blue")
  legend("topright", legend = c("Uniforme", "Triangular"), col = c("red", "blue"), pch = c(0,2))
}

# Usando el ancho de banda utilizando el método de referencia de la densidad normal.
# Este método es útil para datos univariados y se basa en la estimación de la varianza de los datos.
ancho_banda <- bw.nrd(x)
ancho_banda
graf_kernel(x,ancho_banda)

ancho_banda <- 7
graf_kernel(x,ancho_banda)

ancho_banda <- 5
graf_kernel(x,ancho_banda)

ancho_banda <- 3
graf_kernel(x,ancho_banda)


# Es importante elegir un ancho de banda que sea lo suficientemente pequeño como para capturar
# las características finas del conjunto de datos, pero lo suficientemente grande como para suavizar
# las fluctuaciones aleatorias. Una regla empírica común es utilizar el ancho de banda óptimo como
# aquel que minimiza el error cuadrático medio (MSE) entre la función de densidad verdadera y la función de densidad estimada.




##          MODELO TEÓRICO PARA X



# El análisis gráfico de la variable aleatoria (VA) presenta un sesgo positivo, una asimetría derecha y una curtosis alta.
# Esto implica que la VA tiene una mayor probabilidad de tomar valores bajos que altos, y que su distribución tiene una cola
# larga y pesada hacia la derecha. Estas características descartan que la VA siga una distribución uniforme, normal o t-Student,
# ya que estas son simétricas y tienen colas más cortas y ligeras. Tampoco se puede asumir que la VA siga una distribución beta,
# ya que esta está definida en el intervalo (0,1) y la VA tiene un rango mayor. Finalmente, la VA tampoco se ajusta a una
# distribución de Cauchy, ya que esta tiene colas largas y pesadas en ambos extremos y un sesgo nulo.



#####################################
# Densidad Empírica Vs. Exponencial
#####################################

x11();hist(x, breaks = 40,prob=T,main="Pdf Empírica vs. Pdf Exponencial",ylab="")
z <- seq(0,500,0.1)
theta <- mean
lines(z, dexp(z, 1/theta), col="red")
#
theta <- mean +30
lines(z, dexp(z, 1/theta), col="blue")
#
theta <- mean -10
lines(z, dexp(z, 1/theta), col="green")
#
theta <- mean -15
lines(z, dexp(z, 1/theta), col="darkolivegreen")
#

legend("topright", c("X empírica","Pdf Expon Lambda = xbarra", "Pdf Expon Lambda = xbarra+30", "Pdf Expon Lambda = xbarra-10", "Pdf Expon Lambda = xbarra-15"), cex=0.9, col=c("gray","red","blue", "green"), bty="n", lty=c(1,1,1,1))
#

# La función de densidad Exponencial describe apoximadamente bien la muestra aunque no capture los valores últimos de la densidad empírica,
# ni la moda que queda por encima de la función, es decir, al disminuir el valor del parámetro ajusta mejor algunos valores a la izquierda,
# pero pierde ajuste en la cola derecha.
rm(z,theta)


# ___________________________________________________________________________________________


# Densidad Empírica Vs. Chi-cuadrado

x11();hist(x, breaks = 40,prob=T,main="Pdf Empírica vs. Pdf Chi-cuadrado",ylab="",ylim = c(0, 0.06))
z <- seq(0,500,0.1)
gl <- (m-1)
lines(z, dchisq(z, gl), col="red")
#
z0 <- seq(0,500,0.1)
gl0 <- ((m-100)-1)
lines(z0, dchisq(z0, gl0), col="blue")
#
z1 <- seq(0,500,0.1)
gl1 <- ((m-140)-1)
lines(z1, dchisq(z1, gl1), col="green")
legend("topright", c("X empírica","Pdf chi gl=150", "Pdf chi gl=50", "Pdf chi gl=10"), cex=0.9, col=c("gray","red","blue", "green"), bty="n", lty=c(1,1,1,1))
#
# La función de densidad Chi-cuadrado no describe bien la muestra porque no captura de ninguna manera la densidad empírica al tener una forma bastante leptocúrtica,
# es decir, de colas livianas y delgada, sin captar comportamiento real de la VA.
rm(z,z0,z1,gl,gl0,gl1)


# ___________________________________________________________________________________________


# Densidad Empírica Vs. F de Fisher

x11();hist(x, breaks = 40,prob=T,main="Pdf Empírica vs. Pdf F de Fisher",ylab="")
z <- seq(0,500,0.1)
gl1 <- 1
gl2 <- 0.9
lines(z, df(z, gl1,gl2), col="red")
#
z0 <- seq(0,500,0.1)
gl10 <- 2
gl20 <- 0.6
lines(z0, df(z0, gl10,gl20), col="blue")
#
z1 <- seq(0,500,0.1)
gl11 <- 1
gl21 <- 0.3
lines(z1, df(z1, gl11,gl21), col="green")
legend("topright", c("X empírica","Pdf F gl1=1 gl2=0.9", "Pdf F gl1=2 gl2=0.6", "Pdf F gl1=1 gl2=0.3"), cex=0.9, col=c("gray","red","blue", "green"), bty="n", lty=c(1,1,1,1))
#
# La función de densidad F-Fisher no describe bien la muestra porque no captura la densidad,
# pues queda muy por debajo de la distribución empírica.
rm(z,z0,z1,gl1,gl2,gl10,gl20,gl11,gl21)


# ___________________________________________________________________________________________


# Densidad Empírica Vs. Gamma

x11();hist(x, breaks = 40,prob=T,main="Pdf Empírica vs. Pdf Gamma",ylab="")
z <- seq(0,500,0.1)
a <- 2
b <- 1/25
lines(z, dgamma(z, a,b), col="red")
#
z0 <- seq(0,500,0.1)
a0 <- 1.5
b0 <- 1/25
lines(z0, dgamma(z0, a0,b0), col="blue")
#
z1 <- seq(0,500,0.1)
a1 <- 0.8
b1 <- 1/20
lines(z1, dgamma(z1, a1,b1), col="darkolivegreen")
legend("topright", c("X empírica","Pdf Gamma a=2 b=1/25", "Pdf Gamma a=1.5 b=1/25", "Pdf Gamma a=0.8 b=1/20"), cex=0.9, col=c("gray","red","blue", "darkolivegreen"), bty="n", lty=c(1,1,1,1))
#
# La función de densidad Gamma describe más o menos bien la muestra aunque no captura la densidad empírica de manera exacta en todo su intervalo.
rm(z,z0,z1,a,b,a0,b0,a1,b1)


# ___________________________________________________________________________________________


# Densidad Empírica Vs. logística

x11();hist(x, breaks = 40,prob=T,main="Pdf Empírica vs. Pdf logística",ylab="")
z <- seq(0,500,0.1)
lines(z, dlogis(z), col="red")
#
# La función de densidad Logística no describe bien la muestra porque no captura de ninguna manera la densidad empírica.
# La muestra no se ajusta bien a la función de densidad logística, ya que la densidad empírica está siempre por encima
# de la densidad logística.
rm(z)


# ___________________________________________________________________________________________


# Densidad Empírica Vs. Weibull

x11();hist(x, breaks = 40,prob=T,main="Pdf Empírica vs. Pdf Weibull",ylab="")
z <- seq(0,500,0.1)
a <- 2.2
b <- 50
lines(z, dweibull(z, a,b), col="red")

#
z0 <- seq(0,500,0.1)
a0 <- 2
b0 <- 20
lines(z0, dweibull(z0, a0,b0), col="blue")
#
z1 <- seq(0,500,0.1)
a1 <- 1.7
b1 <- 15
lines(z1, dweibull(z1, a1,b1), col="green")
legend("topright", c("X empírica","Pdf Weibull a=2.2 b=50", "Pdf Weibull a=2 b=20", "Pdf Weibull a=1.7 b=15"), cex=0.9, col=c("gray","red","blue", "green"), bty="n", lty=c(1,1,1,1))
#
# La función de densidad Weibull no describe la muestra porque no captura bien la cola derecha de la densidad empírica.
rm(z,z0,z1,a,b,a0,b0,a1,b1)


# ___________________________________________________________________________________________

# Densidad Empírica Vs. inverse weibull

x11();hist(x, breaks = 50,prob=T,main="Pdf Empírica vs. Pdf dinvweibull",ylab="")
z <- seq(0,500,0.1)
a <- 1.5
b <- 0.06
lines(z, dinvweibull(z, a, b), col="red")
#
z0 <- seq(0,500,0.1)
a0 <- 1.5
b0 <- 0.1
lines(z0, dinvweibull(z0, a0,b0), col="blue")
#
z1 <- seq(0,500,0.1)
a1 <- 1.5
b1 <- 0.15
lines(z1, dinvweibull(z1, a1,b1), col="darkolivegreen")
legend("topright", c("X empírica","Pdf dinvweibull a=1.5 b=0.06", "Pdf dinvweibull a=1.5 b=0.1", "Pdf dinvweibull a=1.5 b=0.15"), cex=0.9, col=c("gray","red","blue", "darkolivegreen"), bty="n", lty=c(1,1,1,1))
#
# La función de densidad dinvweibull no describe tan bien la muestra porque no captura el comportamiento de la cola derecha de la densidad empírica.
rm(z,z0,z1,a,b,a0,b0,a1,b1)


########################################
# Densidad Empírica Vs. Log-Normal
########################################

x11();hist(x, breaks = 50,prob=T,main="Pdf Empírica vs. Pdf Log-Normal",ylab="")
z <- seq(0,500,0.1)
a <- 5.3
b <- 1.7
lines(z, dlnorm(z, a, b), col="red")
#
z0 <- seq(0,500,0.1)
a0 <- 4
b0 <- 1.7
lines(z0, dlnorm(z0, a0,b0), col="darkolivegreen")
#
z1 <- seq(0,500,0.1)
a1 <- 3
b1 <- 2
lines(z1, dlnorm(z1, a1,b1), col="blue")
legend("topright", c("X empírica","Pdf Log-Normal a=5.3 b=1.7", "Pdf Log-Normal a=4 b=1.7", "Pdf Log-Normal a=3 b=2"), cex=0.9, col=c("gray","red","darkolivegreen", "blue"), bty="n", lty=c(1,1,1,1))
#
# La función de densidad Log-Normal describe la muestra porque captura el comportamiento tanto leptocúrtica a la izquierda
# como su cola pesada a la derecha.

# Debido a que la distribución Log-Normal es la función que mejor aproxima la variable "X" empírica, se elige ésta como f*(x),
# como la distribución teórica.






