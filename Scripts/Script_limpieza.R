#Problem set 2
#Paquetes
  install.packages(data.table)
require(data.table)
library(dplyr)
library(tidyverse)
setwd("C:/Users/Camila/OneDrive - Universidad de los Andes/1.Big Data/Problem Set 2")
#Unimos bases hogares e individuos ya que medimos pobreza por hogar y queremos tener más caracteristicas
#Para poder identificarlas agregamos una variable que nos dice de que base son

train_hogares$base<-Hogares
train_personas$base<-Personas
train_unida <-merge(train_hogares, train_personas)


test_unida <-merge(test_hogares, test_personas)

#Vemos que variables en comun hay entre train y test
var_train<-colnames(train_unida)
var_test<-colnames(test_unida)
var_train_h <- colnames(train_hogares)
var_train_p <- colnames(train_personas)
var_test_h <- colnames(test_hogares)
var_test_p <- colnames(test_personas)

interc<-intersect(var_train,var_test)
interc_h<-intersect(var_train_h,var_test_h)
interc_p<-intersect(var_train_p,var_test_p)
interc

#Las variables que no esten en la lista interc se borran

train_h<- subset(train_hogares, select=interc_h)
train_p<- subset(train_personas, select=interc_p)

train_x<- subset(train_unida, select=interc)
str(train)
interc_h

#Tenemos 164960 hogares 
count(unique(train_x$id))

#Miramos una a una las variables en el diccionario, borramos variables irrelevantes
#Con las variables ya filtradas renombramos algunas variables
train_x<- subset(train_x, select=-c(Oficio, P6610, P6620, P7090, P7110, P7120, P7150, P7160, P7310, P7350, P7422, P7495, Fex_c, Fex_dpto, P6210s1))

#Renombrar
train_p <-train_p %>%
  rename(
    sexo=P6020,
    edad=P6090,
    niveleduc=P6210
    )

#Missings
missings<-as.data.frame(map(train_p, ~sum(is.na(.))))
miss<-transpose(as.data.frame(missings))
miss$prop <- miss$V1/nrow(train_p)*100
miss<-as.data.frame(miss)
colnames(train_h)

#Hay vars con más de 400.000 missings, las borramos

train_p<-subset(train_p, select=-c(P7510s1,P7510s2,P7510s3, P7510s5, P7510s6, P7510s7))
train_p<-subset(train_p, select=-c(P7500s2,P7500s3))
train_p<-subset(train_p, select=-c(P7045,P7050))
colnames(train_p)