
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
  "Skewness" =  skew,"Curtosis" =  kurt
  )
summ


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



