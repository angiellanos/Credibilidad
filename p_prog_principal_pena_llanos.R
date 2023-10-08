
# Modelo de estimación y credibilidad
# CREDIBILIDAD Y MODELOS DE PÉRDIDA
# UNIVERSIDAD NACIONAL DE COLOMBIA
# Maestría en Actuaría y Finanzas
# 2023-II


# Presentado por: Camila Peña, Angie Llanos
# Presentado a: Edwin Riveros


## Tarea 1
## This is just for a proof
rm(list=ls())
options(scipen=999)
options(digits=8)

#Carga de librerias
library(lubridate)
library(dplyr)
library(ggplot2)
library(readxl)
library(janitor)
library(pastecs)

data<- read_excel("d_datos_orig_pena_llanos.xlsx")

data <- data %>% clean_names()
data <-filter(data,filing_year==2016)
sum(duplicated(data$premiums_written_in_millions))
summary(data$premiums_written_in_millions)

## Tarea 2

stat.desc(data$premiums_written_in_millions, norm = T)
ggplot(data, aes(x=premiums_written_in_millions)) + geom_histogram(binwidth = 25)

## Tarea 3


## Tarea 4

