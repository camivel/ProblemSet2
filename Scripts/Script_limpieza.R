#Problem set 2
#Paquetes
install.packages("dplyr")
install.packages("tidyverse")
install.packages("caret")
install.packages("glmnet")
require(data.table)
library(dplyr)
library(tidyverse)
setwd("C:/Users/Camila/OneDrive - Universidad de los Andes/1.Big Data/Problem Set 2")

#Vemos que variables en comun hay entre train y test
var_train_h <- colnames(train_hogares)
var_train_p <- colnames(train_personas)
var_test_h <- colnames(test_hogares)
var_test_p <- colnames(test_personas)

interc_h<-intersect(var_train_h,var_test_h)
interc_p<-intersect(var_train_p,var_test_p)


#Las variables que no esten en la lista interc se borran

train_h<- subset(train_hogares, select=interc_h)
train_p<- subset(train_personas, select=interc_p)

#Vamos a extraer las variables consideramos relevantes en las bases de personas

#Para hacer la limpieza en ambas bases (train y test)

#Identificamos para luego separar
test_personas$base <- "test"
train_p$base <- "train"
df<-rbind(test_personas, train_p)

df2<-subset(df, select=c(id, Clase, Orden, P6020,P6040,P6050,P6090,P6100,P6210,P6800,P6920,P7045,P7422,P7495, P7500s2, P7500s3, P7505, base))

#Creamos variables
df2$jefe<-ifelse(df2$P6050==1,1,0)
df2$sexojefe<-ifelse(df2$jefe==1,df2$P6020,NA)
df2$edadjefe<-ifelse(df2$jefe==1,df2$P6040,NA)
df2$niveleduc_jefe<-ifelse(df2$jefe==1,df2$P6210,NA)
df2$menores<-ifelse(df2$P6040<18,1,0) #dummy de menores
niños<-df2 %>%group_by(id) %>% tally(menores)
niños<-rename(niños, No_menores=n)
df2<-merge(df2, niños)

df2$mayores<-ifelse(df2$P6040>59,1,0) #dummy de adultos mayores
mayores<-df2 %>%group_by(id) %>% tally(mayores)
mayores<-rename(mayores, No_adulto_mayor=n)
df2<-merge(df2, mayores)

#Número de menores de edad en el hogar
df2$informal<-ifelse(df2$P6100==3&4,1,0)
df2$otro_dividendos<-ifelse(df2$P7505==1,1,0)
dividendos<-df2 %>%group_by(id) %>% tally(otro_dividendos)
dividendos<-rename(dividendos, ing_dividendo=n)
df2$otro_arri_pens<-ifelse(df2$P7495==1,1,0)
arriendo<-df2 %>%group_by(id) %>% tally(otro_arri_pens)
arriendo<-rename(arriendo, ing_arrie_pen=n)

df2<-merge(df2, arriendo)
df2<-merge(df2, dividendos)

#Filtramos la base para que quede en unidades de hogares

df2<- filter(df2, jefe==1)
df2<-subset(df2, select=c(id, Clase, sexojefe, edadjefe, niveleduc_jefe, 
                          No_menores, No_adulto_mayor, informal, ing_arrie_pen, ing_dividendo, base))
df2$ing_dividendo<-ifelse(df2$ing_dividendo>0,1,0)
df2$ing_arrie_pen<-ifelse(df2$ing_arrie_pen>0,1,0)
str(df2)
df3<-df2

#Como factores las variables que lo son

df2$sexojefe <- factor(df2$sexojefe,levels=c(1,2), labels=c("Hombre", "Mujer"))
df2$niveleduc_jefe <- factor(df2$niveleduc_jefe,levels= c(1,2,3,4,5,6,7),labels=c("Ninguno", "Preescolar", "Primaria", "Secundaria", "Media", "Superior", "NS/NI"))
df2$informal <- factor(df2$informal, levels=c(0,1), labels=c("Formal", "Informal"))
df2$ing_arrie_pen <- factor(df2$ing_arrie_pen, levels=c(0,1), labels=c("No", "Si"))
df2$ing_dividendo <- factor(df2$ing_dividendo,levels=c(0,1), labels=c("No", "Si"))
df2$Clase<- factor(df2$Clase, levels=c(0,1), labels=c("Cabecera", "Resto"))

str(df2)

#Ya tenemos la bases de personas en unidades de hogares y vamos a dividir por train y test de nuevo

train_pf<-filter(df2,base=="train")
train_pf<-subset(train_pf,select=-c(base))
test_pf<- filter(df2,base=="test")
test_pf<-subset(test_pf,select=-c(base))


#Ahora pegamos con las de hogares
train_h<-subset(train_h,select=-c(Clase))
train<-merge(train_h, train_pf)
test_hogares<-subset(test_hogares,select=-c(Clase))
test<-merge(test_hogares, test_pf)

#Arreglamos bases finales
colnames(test)
test<-subset(test, select=-c(P5100))
test<-subset(test, select=-c(Fex_c, Fex_dpto))
test<-test %>% rename(Nhabitaciones=P5000,
                Ndormitorios=P5010,
                Tipovivienda=P5090, #propia o etc
                Est_arriendo_prop=P5130,
                Arriendo=P5140)

#Creamos una variable de arriendo con los estimados y los reportados

test$Arriendo<-ifelse(is.na(test$Arriendo), test$Est_arriendo_prop, test$Arriendo)
test<-subset(test, select=-c(Depto))
test<-subset(test, select=-c(Est_arriendo_prop))
test<-subset(test, select=-c(Clase))

#Declaramos como factores las que nos faltaba
test$Dominio<- factor(test$Dominio)
test$Tipovivienda<- factor(test$Tipovivienda, levels=c(1,2,3,4,5,6), labels=c("Propia", "Propia_pagando", "Arriendo", "Usufructo","Posesión", "Otra" ))

str(test) 


#Lo mismo para train

colnames(train)
train<-subset(train, select=-c(P5100))
train<-subset(train, select=-c(Fex_c, Fex_dpto))
train<-train %>% rename(Nhabitaciones=P5000,
                      Ndormitorios=P5010,
                      Tipovivienda=P5090, #propia o etc
                      Est_arriendo_prop=P5130,
                      Arriendo=P5140)

#Creamos una variable de arriendo con los estimados y los reportados

train$Arriendo<-ifelse(is.na(train$Arriendo), train$Est_arriendo_prop, train$Arriendo)
train<-subset(train, select=-c(Depto))
train<-subset(train, select=-c(Clase))
#Declaramos como factores las que nos faltaba
train$Dominio<- factor(train$Dominio)
train$Tipovivienda<- factor(train$Tipovivienda, levels=c(1,2,3,4,5,6), labels=c("Propia", "Propia_pagando", "Arriendo", "Usufructo","Posesión", "Otra" ))
train<-subset(train, select=-c(Est_arriendo_prop))
str(train) 

#MISSINGS-----------------------------------------------------------------------
missings<-as.data.frame(map(train, ~sum(is.na(.))))
miss<- data.frame(t(missings))
miss$prop<-miss$t.missings/nrow(train)*100

#Vemos que la unica con missings es informal, imputamos missings =Formal al ser la moda
train$informal4<-ifelse(is.na(train$informal),1,train$informal)
train$informal4 <- factor(train$informal4, levels=c(1,2), labels=c("Formal", "Informal"))
train<-subset(train, select=-c(informal))
train<-train %>% rename(informal=informal4)

#Finalmente agregamos las variables dependientes a la base train, desde hogares train
y<- subset(train_hogares, select=c(id,Ingpcug, Pobre))
train_yx <- merge(y, train)
train_yx$Pobre <- factor(train_yx$Pobre,levels=c(0,1), labels=c("No Pobre", "Pobre"))
