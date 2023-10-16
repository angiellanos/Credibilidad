
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
# Por ende, no aparenta tener datos raros. Se continúa trabajando la variable tal cual está.

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


# Presenta las métricas anteriores de forma organizada
summ <- data.frame(
  "Mean" = mean, "Median" = median, "Mode" =  mode,
  "Variance" = var, "Std_deviation" = std, "Coeff_variation" =  cv,
  "Value_at_Risk_95" = VaR_95, "Tale_Value_at_Risk_95" = TVaR_95,
  "Skewness" =  skew,"Curtosis" =  kurt, row.names = "Valor"
  )
t(summ)



##      Gráficos descriptivos de x:

# Histograma
x11();ggplot(datos, aes(x=premiums_written_in_millions)) + geom_histogram(binwidth = 25)

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
  h <- 5
  z_01 <- seq(0,2300-h,h)
  z <- seq(0+h,2300,h)
  f <- (sapply(z, Fn)-sapply(z_01, Fn))/h   #Derivada de la Distribución Acumulada
  x11();plot( z, f, type = "s", xlab = "x", ylab = "fn(x)", main = titulo_d)
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
  x11();plot(densidad_uniforme, main = "Estimador de kernel de la f.d.d con kernel uniforme y triangular", xlab = "Variable", ylab = "Densidad", col = "red")
  lines(densidad_triangular, col = "blue")
  legend("topright", legend = c("Uniforme", "Triangular"), col = c("red", "blue"), pch = c(0,2))
}

# Usando el ancho de banda utilizando el método de referencia de la densidad normal.
# Este método es útil para datos univariados y se basa en la estimación de la varianza de los datos.
ancho_banda <- bw.nrd(x)
ancho_banda
graf_kernel(x,ancho_banda)

ancho_banda <- 8
graf_kernel(x,ancho_banda)

ancho_banda <- 30
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



# rchisq
# rexp
# rf
# rgamma
# rlogis
# rweibull
# Erlang, Pareto, exponencial negativa

#####################################
# Densidad Empírica Vs. Exponencial
#####################################

hist(x, breaks = 40,prob=T,main="Pdf Empírica vs. Pdf Exponencial",ylab="")
z <- seq(0,4000,0.1)
theta <- mean
lines(z, dexp(z, 1/theta), col="red")
#
z <- seq(0,4000,0.1)
theta <- mean +30
lines(z, dexp(z, 1/theta), col="blue")
#
z <- seq(0,4000,0.1)
theta <- mean -30
lines(z, dexp(z, 1/theta), col="green")
#
z <- seq(0,4000,0.1)
theta <- mean -50
lines(z, dexp(z, 1/theta), col="darkolivegreen")
#

legend("topright", c("X empírica","Pdf Expon Lambda = xbarra", "Pdf Expon Lambda = xbarra+30", "Pdf Expon Lambda = xbarra-30", "Pdf Expon Lambda = xbarra-50"), cex=0.9, col=c("gray","red","blue", "green"), bty="n", lty=c(1,1,1,1))
#

# La función de densidad Exponencial describe apoximadamente bien la muestra aunque no capture los valores últimos de la densidad empírica,
# ni la moda que queda por encima de la función, es decir, al disminuir el valor del parámetro ajusta mejor algunos valores a la izquierda,
# pero pierde ajuste en la cola derecha.
rm(z,theta)


# ___________________________________________________________________________________________


# Densidad Empírica Vs. Chi-cuadrado

hist(x, breaks = 40,prob=T,main="Pdf Empírica vs. Pdf Chi-cuadrado",ylab="",ylim = c(0, 0.06))
z <- seq(0,4000,0.1)
gl <- (m-1)
lines(z, dchisq(z, gl), col="red")
#
z0 <- seq(0,4000,0.1)
gl0 <- ((m-50)-1)
lines(z0, dchisq(z0, gl0), col="blue")
#
z1 <- seq(0,4000,0.1)
gl1 <- ((m-130)-1)
lines(z1, dchisq(z1, gl1), col="green")
legend("topright", c("X empírica","Pdf chi gl=157", "Pdf chi gl=107", "Pdf chi gl=27"), cex=0.9, col=c("gray","red","blue", "green"), bty="n", lty=c(1,1,1,1))
#
# La función de densidad Chi-cuadrado no describe bien la muestra porque no captura de ninguna manera la densidad empírica al tener una forma bastante leptocúrtica,
# es decir, de colas livianas y delgada, sin captar comportamiento real de la VA.
rm(z,z0,z1,gl,gl0,gl1)


# ___________________________________________________________________________________________


# Densidad Empírica Vs. F de Fisher

hist(x, breaks = 40,prob=T,main="Pdf Empírica vs. Pdf F de Fisher",ylab="")
z <- seq(0,4000,0.1)
gl1 <- 1
gl2 <- 0.9
lines(z, df(z, gl1,gl2), col="red")
#
z0 <- seq(0,4000,0.1)
gl10 <- 100
gl20 <- 0.4
lines(z0, df(z0, gl10,gl20), col="blue")
#
z1 <- seq(0,4000,0.1)
gl11 <- 500
gl21 <- 0.9
lines(z1, df(z1, gl11,gl21), col="green")
legend("topright", c("X empírica","Pdf F gl1=1 gl2=0.9", "Pdf F gl1=100 gl2=0.4", "Pdf F gl1=500 gl2=0.9"), cex=0.9, col=c("gray","red","blue", "green"), bty="n", lty=c(1,1,1,1))
#
# La función de densidad F-Fisher no describe bien la muestra porque no captura la densidad,
# pues queda muy por debajo de la distribución empírica.
rm(z,z0,z1,gl1,gl2,gl10,gl20,gl11,gl21)


# ___________________________________________________________________________________________


# Densidad Empírica Vs. Gamma

hist(x, breaks = 40,prob=T,main="Pdf Empírica vs. Pdf Gamma",ylab="")
z <- seq(0,4000,0.1)
a <- 2
b <- 1/25
lines(z, dgamma(z, a,b), col="red")
#
z0 <- seq(0,4000,0.1)
a0 <- 2.5
b0 <- 1/25
lines(z0, dgamma(z0, a0,b0), col="blue")
#
z1 <- seq(0,4000,0.1)
a1 <- 2.5
b1 <- 1/20
lines(z1, dgamma(z1, a1,b1), col="green")
legend("topright", c("X empírica","Pdf Gamma a=2 b=1/25", "Pdf Gamma a=2.5 b=1/25", "Pdf Gamma a=2.5 b=1/20"), cex=0.9, col=c("gray","red","blue", "green"), bty="n", lty=c(1,1,1,1))
#
# La función de densidad Gamma no describe bien la muestra porque no captura la densidad empírica de manera correcta en todo su intervalo.
rm(z,z0,z1,a,b,a0,b0,a1,b1)


# ___________________________________________________________________________________________


# Densidad Empírica Vs. logística

hist(x, breaks = 40,prob=T,main="Pdf Empírica vs. Pdf logística",ylab="")
z <- seq(0,4000,0.1)
lines(z, dlogis(z), col="red")
#
# La función de densidad Logística no describe bien la muestra porque no captura de ninguna manera la densidad empírica.
# La muestra no se ajusta bien a la función de densidad logística, ya que la densidad empírica está siempre por encima
# de la densidad logística.
rm(z)


# ___________________________________________________________________________________________


# Densidad Empírica Vs. Weibull

hist(x, breaks = 40,prob=T,main="Pdf Empírica vs. Pdf Weibull",ylab="")
z <- seq(0,4000,0.1)
a <- 2.2
b <- 50
lines(z, dweibull(z, a,b), col="red")

#
z0 <- seq(0,4000,0.1)
a0 <- 1.9
b0 <- 60
lines(z0, dweibull(z0, a0,b0), col="blue")
#
z1 <- seq(0,4000,0.1)
a1 <- 2.7
b1 <- 70
lines(z1, dweibull(z1, a1,b1), col="green")
legend("topright", c("X empírica","Pdf Weibull a=2.2 b=50", "Pdf Weibull a=1.9 b=60", "Pdf Weibull a=2.7 b=70"), cex=0.9, col=c("gray","red","blue", "green"), bty="n", lty=c(1,1,1,1))
#
# La función de densidad Weibull no describe la muestra porque no captura la densidad empírica.
rm(z,z0,z1,a,b,a0,b0,a1,b1)


# ___________________________________________________________________________________________


# Densidad Empírica Vs. Log-Normal

hist(x, breaks = 50,prob=T,main="Pdf Empírica vs. Pdf Log-Normal",ylab="")
z <- seq(0,4000,0.1)
a <- 5.3
b <- 1.7
lines(z, dlnorm(z, a, b), col="red")
#
z0 <- seq(0,4000,0.1)
a0 <- 3
b0 <- 3
lines(z0, dlnorm(z0, a0,b0), col="blue")
#
z1 <- seq(0,4000,0.1)
a1 <- 6
b1 <- 3
lines(z1, dlnorm(z1, a1,b1), col="green")
legend("topright", c("X empírica","Pdf Log-Normal a=5.3 b=1.7", "Pdf Log-Normal a=3 b=3", "Pdf Log-Normal a=6 b=3"), cex=0.9, col=c("gray","red","blue", "green"), bty="n", lty=c(1,1,1,1))
#
# La función de densidad Log-Normal no describe la muestra porque no captura la densidad empírica, debido que, hay bastante información que
# se espaca por encima de la función de densidad, sobre todo en intervalo de masa de probabilidad alrededor de cero.
rm(z,z0,z1,a,b,a0,b0,a1,b1)


########################################
# Densidad Empírica Vs. inverse weibull
########################################

hist(x, breaks = 50,prob=T,main="Pdf Empírica vs. Pdf dinvweibull",ylab="")
z <- seq(0,4000,0.1)
a <- 1.5
b <- 0.06
lines(z, dinvweibull(z, a, b), col="red")
#
z0 <- seq(0,4000,0.1)
a0 <- 1.5
b0 <- 0.04
lines(z0, dinvweibull(z0, a0,b0), col="blue")
#
z1 <- seq(0,4000,0.1)
a1 <- 1.3
b1 <- 0.03
lines(z1, dinvweibull(z1, a1,b1), col="green")
legend("topright", c("X empírica","Pdf dinvweibull a=1.5 b=0.06", "Pdf dinvweibull a=1.5 b=0.04", "Pdf dinvweibull a=1.3 b=0.03"), cex=0.9, col=c("gray","red","blue", "green"), bty="n", lty=c(1,1,1,1))
#
# La función de densidad dinvweibull describe particularmente la muestra porque captura de cierta manera la densidad empírica.
rm(z,z0,z1,a,b,a0,b0,a1,b1)


# Debido a que distribución inverse weibull es la función que mejor aproxima la variable "X" empírica, se elige ésta como f*(x),
# como la distribución teórica.



#++++++++++++++++++++++++++++++++++++++++++________________________________________________________________________________________



###      Ahora, estimando los parámetros:

# Método de los momentos:

# Primer momento: Mu(x)= \alpha \Gamma(1- (1 / \beta) )
# Segundo momento: Mu(x)= \alpha^2 \Gamma(1- (2 / \beta) )
sol_nl <- function(x) {
  y <- numeric(2)
  y[1] <- x[1] * gamma(abs(1- 1/ x[2])) - mean
  y[2] <- (x[1])^2 * gamma(abs(1- 2/x[2])) - (x[1])^2 * (gamma(abs(1- 1/ x[2])))^2 - var
  y
}
x0 <- c(1.5,0.04)
fb <- sol_nl(x0)
x0
fb
xp <- nleqslv(x0, sol_nl, control=list(btol=.01))

# Observando la gráfica de la distribución de los parámetros resultantes, se tiene:

hist(x, breaks = 50,prob=T,main="Pdf Empírica vs. Pdf dinvweibull",ylab="")
z <- seq(0,4000,0.1)
am <- abs(xp$x[1])
bm <- abs(xp$x[2])
am
bm
lines(z, dinvweibull(z, am, bm), col="red")

# Comparando medidas de tendencia central
c(mean, am * gamma(abs(1- 1/bm)))  # xbarra=mu.est.momentos
c(var, (am^2 * gamma(1+ 2/bm)) - (am^2 *(gamma(1+ 1/bm))^2))  # S^2= sigma^2.est.momentos


# ESTIMACION MAXVER
## FUNCION DE "menos" LOG-VEROSIMILITUD DE UNA POBLACION INVERSA WEIBULL
#
l=function(param){
  alpha=param[1]
  betha=param[2]
  -(log(betha^m*(alpha^(m*betha)))+sum(log(x^(-betha-1)*exp(-(x/alpha)^(-betha))))) # negativo de la función, para usar optim x maximizar
}

## OPTIM() minimiza una función

optim(c(1.3,0.03),l)           # debo indicar los puntos de arranque x la función a optimizar

# Observando la gráfica de la distribución de los parámetros resultantes, se tiene:

av <- optim(c(1.1,0.01),l)$par[1]
bv <- optim(c(1.5,0.04),l)$par[2]
lines(z, dinvweibull(z, av, bv), col="blue")

# Medidas de tendencia central
c(mean_y, av * gamma(1- 1/bv))  # xbarra=mu.est.MAXVER
c(var_y, av^2 * gamma(1+ 2/bv) - av^2 *(gamma(1+ 1/bv))^2)  # S^2= sigma^2.est.MAXVER


# Presenta las métricas anteriores de forma organizada
mu <- data.frame(
  "ybarra" = c(mean_y), "mu.est.momentos" = c(am * gamma(1- 1/bm)), "mu.est.MAXVER" =  c(av * gamma(1- 1/bv)),
  row.names = c('values:')
)
vari <- data.frame(
  "S^2" = c(var_y), "sigma^2.est.momentos" = c((am^2 * gamma(1+ 2/bm)) - (am^2 *(gamma(1+ 1/bm))^2)),
  "sigma^2.est.MAXVER" =  c(av^2 * gamma(1+ 2/bv) - av^2 *(gamma(1+ 1/bv))^2),
  row.names = c('values:')
)
mu;vari

#SELECCIÓN DEFINITIVA DE PARÁMETROS:

a <- am
b <- bm

# Para la distribución teórica de Y veamos las siguientes estadísticas (fórmulas):
Mean_T    <- a * gamma(1- 1/ b)
Mode_T    <- mlv(dinvweibull(z, a, b), method = "meanshift")[1]
Median_T  <- (0.5/exp(-a))^(-1/b)
Var_T     <- a^2 * gamma(1+ 2/b) - a^2 *(gamma(1+ 1/b))^2
Sd_T      <- sqrt(Var_T)
Cv_T      <- Sd_T/Mean_T
VaRx_95_T <- Mean_T - Sd_T *qnorm(.95,0,1)


## Evaluando las estadísticas anteriores en las estimaciones de los parámetros:
## Es decir, para volver a estimar los parámetros.
sol_nl_T <- function(x) {
  y <- numeric(2)
  y[1] <- x[1] * gamma(1- 1/ x[2]) - Mean_T
  y[2] <- (x[1])^2 * gamma(1- 2/x[2]) - (x[1])^2 * (gamma(1- 1/ x[2]))^2 - Var_T
  y
}
xT <- c(100,0.9)
fT <- sol_nl(x0)
xT
fT
x <- nleqslv(xT, sol_nl_T, control=list(btol=.01))

# Observando la gráfica de la distribución de los parámetros resultantes, se tiene:

hist(y, breaks = 50,prob=T,main="Pdf Empírica vs. Pdf dinvweibull",ylab="")
z <- seq(0,4000,0.1)
an <- abs(x$x[1])
bn <- abs(x$x[2])
an
bn
lines(z, dinvweibull(z, an, bn), col="red")

#Calculando de nuevo las estadísticas:
Mean_T    <- an * gamma(1- 1/ bn)
Mode_T    <- mlv(dinvweibull(z, an, bn), method = "meanshift")[1]
Median_T  <- (0.5/exp(-an))^(-1/bn)
Var_T     <- an^2 * gamma(1+ 2/bn) - an^2 *(gamma(1+ 1/bn))^2
Sd_T      <- sqrt(Var_T)
Cv_T      <- Sd_T/Mean_T
VaRx_95_T <- -Mean_T + Sd_T *qnorm(.95,0,1)


# Presenta cuadro comparativo entre estas siete estadísticas y los valores obtenidos en la TAREA 2.
estadis <- data.frame(
  "mean" = c(Mean_T, mean_y), "median" = c(Median_T, median_y), "mode" =  c(Mode_T,mode_y),
  "variance" = c(Var_T, var_y), "std_deviation" = c(Sd_T, std_y), "coeff_variation" =  c(Cv_T, cv_y),
  "Value_at_Risk_95" = c(VaRx_95_T, VaR_y_95),
  row.names = c('Teóricos','Empíricos')
)
estadis


# Comparando gráficamente el modelo empírico y el modelo teórico de Y:
# Fn(y) vs F*(y)

fdd_fdD(y,n,"Densidad","Comparación Func. de Distribución") ;lines(z, pinvweibull(z, a, b),col = "red",main="Comparación Func. de Distribución")
legend("bottomright", c("F.d.D. Empírica","F.d.D. Teórica"), cex=0.9, col=c("black","red"), bty="n", lty=c(1,1))

# Histograma(y) vs f*(y)
hist(y, breaks = 50,prob=T,main="Comparación Hist y f.d.d. Teórica",ylab="")
lines(z, dinvweibull(z, a, b), col="red")
legend("topright", c("Histograma","F.d.d. Teórica"), cex=0.9, col=c("gray","red"), bty="n", lty=c(1,1))

# Dn(y)
D <- function(y){
  Fn(y) - pinvweibull(y, a, b)
}
D(7.0)
D(12.2)
D(18.0)
x11();plot( z, sapply(z, D), type = "l", xlab = "x", ylab = "", main = "D(x) = Fn(x) - F*(x)")
max(sapply(z, D))

# P-P plot
x11();plot( sapply(y, Fn), pinvweibull(y, a, b), main = "P-P plot", xlab = "Fn(x)", ylab = "F*(x)")


# Q-Q plot
q <- seq(0.01,0.99,0.005)
q
x11();plot( quantile(y,q), qinvweibull(q, a, b), main = "Q-Q plot", xlab = "^pi_q", ylab = "pi*_q")
#
qqnorm(y)
qqline(z, col = 2)
#
qqplot(y, rinvweibull(n, a, b))
#


#Ahora, Usando pruebas de hipótesis para validar que el modelo teórico si representa los datos de la variable Y

### KS TEST
#
ks.test(y, "pinvweibull", a, b)  # se rechaza H0, es decir que nuestro conjunto de datos no se distribuye Weibull
#

### Chi-squared test
#
s <- seq(0,799)
chisq.test(f,dinvweibull(s, a, b), correct=FALSE)  #Muchos grados de libertad, por valores repetidos
# y p.value grande, por ende, se acepta la hipótesis nula y se rechaza la alternativa, es decir,
# Los datos sí se distribuyen inversa de weibull.
#

### Shapiro Wilk (test de normalidad)
#
shapiro.test(y) # se rechaza H0, es decir que nuestro conjunto de datos no se distribuye Normal, como era de esperarse
#

### Anderson Darling test (test de normalidad)
#
require(nortest)
ad.test(y) # se rechaza H0, es decir que nuestro conjunto de datos no se distribuye Normal, como era de esperarse
#

# Debido a que el test KS rechaza la hipótesis nula, significa que puede existir un mejor
# modelo que aproxime o replique el comportamiento de los datos.

















