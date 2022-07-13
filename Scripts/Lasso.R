rm(list=ls())
setwd("C:/Users/Diana Contreras/OneDrive - Universidad de los Andes/1.Big Data/Problem Set 2")

install.packages("themis")
require(data.table)
library(dplyr)
library(tidyverse)
require(caret)
library(glmnet)
library(ggplot2)
library(pROC)
library(themis)
library(cowplot)
str(train_yx)

train_yx$Pobre <- factor(train_yx$Pobre,levels=c(0,1), labels=c("No Pobre", "Pobre"))

#Miramos que tan balnaceada está la base de datos, vemos que solo el 20% es pobre.
prop.table(table(train_yx$Pobre))

base<-subset(train_yx,select=-c(Ingtotug, Ingtotugarr, Indigente))
#Partición de la base

set.seed(1111)

split1<- createDataPartition(base$Pobre, p=.7)[[1]]
length(split1)/nrow(base) #Quedó bien

trainSplit<-base[split1,]
testSplit<-base[-split1,]

nrow(trainSplit)/nrow(base)
nrow(testSplit)/nrow(base)

split2<-createDataPartition(testSplit$Pobre, p=1/3)[[1]]

testing <-testSplit[-split2,]
evaluation <-testSplit[split2,]

nrow(testing)/nrow(train_yx)
nrow(evaluation)/nrow(train_yx) #20% de la muestra y 10%, bien

#Primero voy ha hacer el modelo sin balancear la base, luego balanceando
#Logit-------------------------------------------------------------------
set.seed(1111)
logit<-train(Pobre~ Dominio+Nhabitaciones + Ndormitorios + Arriendo + Tipovivienda +
               Nper + Npersug + sexojefe + edadjefe + niveleduc_jefe + 
               No_menores + No_adulto_mayor + ing_arrie_pen + ing_dividendo +
               informal,
             data=trainSplit,
             method="glm",
             trControl=trainControl("cv", number =10, savePredictions=T),
             family="binomial",
             preProcess=c("center", "scale"),
             na.action=na.omit)
#Regularización----------------------------------------------------------
#Lasso 
str(trainSplit)

lamda<- 10^seq(-2,3, length=300)
lasso<- train(Pobre~ Dominio+Nhabitaciones + Ndormitorios + Arriendo + Tipovivienda +
                Nper + Npersug + sexojefe + edadjefe + niveleduc_jefe + 
                No_menores + No_adulto_mayor + ing_arrie_pen + ing_dividendo +
                informal, 
              data=trainSplit, 
              method= "glmnet",
              trControl=trainControl("cv", number =10, savePredictions="all"),
              tuneGrid=expand.grid(alpha =1, lambda=lamda),
              preProcess=c("center", "scale"),
              na.action=na.omit)
lasso
ridge<- train(Pobre~ Dominio+Nhabitaciones + Ndormitorios + Arriendo + Tipovivienda +
                Nper + Npersug + sexojefe + edadjefe + niveleduc_jefe + 
                No_menores + No_adulto_mayor + ing_arrie_pen + ing_dividendo +
                informal, 
              data=trainSplit, 
              method= "glmnet",
              trControl=trainControl("cv", number =10, savePredictions="all"),
              tuneGrid=expand.grid(alpha =0, lambda=lamda),
              preProcess=c("center", "scale"),
              na.action=na.omit)
ridge
el<- train(Pobre~ Dominio+Nhabitaciones + Ndormitorios + Arriendo + Tipovivienda +
                Nper + Npersug + sexojefe + edadjefe + niveleduc_jefe + 
                No_menores + No_adulto_mayor + ing_arrie_pen + ing_dividendo +
                informal, 
           data=trainSplit, 
           method= "glmnet",
           trControl=trainControl("cv", number =10, savePredictions="all"),
           preProcess=c("center", "scale"),
           na.action=na.omit)
el
models<- list(ridge= ridge, lasso=lasso, elastic=el)
colnames(trainSplit)

#Coeficientesl
coef_lasso<-  as.matrix(round(coef(lasso$finalModel, lasso$bestTune$lambda),3))
coef_lasso
coef_ridge<-as.matrix(round(coef(ridge$finalModel, ridge$bestTune$lambda),3))
coef_elastic<-as.matrix(round(coef(el$finalModel, el$bestTune$lambda),3))

coef <-cbind(coef_lasso,coef_ridge)
coef <-cbind(coef,coef_elastic)
colnames(coef)<- c("Lasso", "Ridge", "ElasticNet")


#Importancia  de las variables
imp_lasso<-as.data.frame(varImp(lasso)$importance)
imp_ridge<-as.data.frame(varImp(ridge)$importance)
imp_el<-as.data.frame(varImp(el)$importance)

imp_lasso$Var<-row.names(imp_lasso)
imp_ridge$Var<-row.names(imp_ridge)
imp_el$Var<-row.names(imp_el)


ilasso<-ggplot(imp_lasso, aes(x=reorder(Var, -Overall), y=Overall)) +
         geom_bar(stat="identity", fill="aquamarine") + 
  theme_classic()+
  labs(y="", x="")+
  ggtitle("Lasso, lambda=") +
  theme(axis.text.x = element_text(angle=90, size=6, vjust = 0.5))

iridge <-ggplot(imp_ridge, aes(x=reorder(Var, -Overall), y=Overall)) +
  geom_bar(stat="identity", fill="coral2") + 
  theme_classic()+
  labs(y="", x="")+
  ggtitle("Ridge lambda") +
  theme(axis.text.x = element_text(angle=90, size=6, vjust = 0.5))

iel <-ggplot(imp_ridge, aes(x=reorder(Var, -Overall), y=Overall)) +
  geom_bar(stat="identity", fill="coral2") + 
  theme_classic()+
  labs(y="", x="")+
  ggtitle("Elastinc net, lambda=") +
  theme(axis.text.x = element_text(angle=90, size=6, vjust = 0.5))

plot_grid(ilasso,iridge, iel, nrow=3,ncol=1)

#Alternative cutoff-----------------------------------------------------
evaluation<-na.omit(evaluation)
evalresults<-data.frame(evaluation$Pobre)

#Predecimos
evalresults$logit<-predict(logit, newdata=evaluation, type="prob")[,1]
evalresults$lasso <-predict(lasso, newdata=evaluation, type="prob")[,1]
evalresults$ridge <-predict(ridge, newdata=evaluation, type="prob")[,1]
evalresults$elastic<-predict(el, newdata=evaluation, type="prob")[,1]

#AUC
rfROC<-roc(evalresults$evaluation.Pobre, evalresults$lasso) #lasso
rfROC #0.84

rfROC2<-roc(evalresults$evaluation.Pobre, evalresults$ridge) #ridge
rfROC2 # AUC 0.853

rfROC3<-roc(evalresults$evaluation.Pobre, evalresults$elastic) #elastic net
rfROC3 # AUC 0.8553

rfROC4<-roc(evalresults$evaluation.Pobre, evalresults$logit) #logit
rfROC4 #AUC 0.8566

#Metricas, umbral
rfThresh_la <-coords(rfROC, x="best", best.method ="closest.topleft") #0.7868418
rfThresh_ri <-coords(rfROC2, x="best", best.method ="closest.topleft") #0.774319
rfThresh_el <-coords(rfROC3, x="best", best.method ="closest.topleft") #0.780188
rfThresh_lo <-coords(rfROC4, x="best", best.method ="closest.topleft") #0.7776

Thresh_la<-rfThresh_la$threshold
Thresh_ri<-rfThresh_ri$threshold
Thresh_el<-rfThresh_el$threshold
Thresh_lo<-rfThresh_lo$threshold

#UPSAMPLIN-----------------------------------------------------------------------
  
set.seed(1111)
receta <- recipe(Pobre ~ ., data = trainSplit) %>%
themis::step_upsample(Pobre) #Upsample
training_up <- bake(prep(receta), new_data=NULL)
prop.table(table(training_up$Pobre))

#Logit
set.seed(1111)
logit_up<-train(Pobre~ Dominio+Nhabitaciones + Ndormitorios + Arriendo + Tipovivienda +
               Nper + Npersug + sexojefe + edadjefe + niveleduc_jefe + 
               No_menores + No_adulto_mayor + ing_arrie_pen + ing_dividendo +
               informal,
             data=training_up,
             method="glm",
             trControl=trainControl("cv", number =10, savePredictions=T),
             family="binomial",
             preProcess=c("center", "scale"),
             na.action=na.omit)
#Elastic Net

el_up<- train(Pobre~ Dominio+Nhabitaciones + Ndormitorios + Arriendo + Tipovivienda +
             Nper + Npersug + sexojefe + edadjefe + niveleduc_jefe + 
             No_menores + No_adulto_mayor + ing_arrie_pen + ing_dividendo +
             informal, 
           data=training_up, 
           method= "glmnet",
           trControl=trainControl("cv", number =10, savePredictions="all"),
           preProcess=c("center", "scale"),
           na.action=na.omit) # lambda=0.004202
#Predicción por fuera de muestra------------------------------------
testing<-na.omit(testing)
test_Results<-data.frame(Pobre=testing$Pobre)

test_Results$lasso<-predict(lasso, 
                            newdata=testing,
                            type="prob")[,1]

test_Results$ridge<-predict(ridge, 
                            newdata=testing,
                            type="prob")[,1]
test_Results$el<-predict(el, 
                            newdata=testing,
                            type="prob")[,1]
test_Results$logit<-predict(logit, 
                         newdata=testing,
                         type="prob")[,1]
test_Results$logit_up<-predict(logit_up, 
                            newdata=testing,
                            type="prob")[,1]
test_Results$el_up<-predict(el_up, 
                               newdata=testing,
                               type="prob")[,1]

#Clasificación segun el umbral
test_Results2 <- test_Results %>% 
  mutate(logit_thresh=ifelse(logit>Thresh_lo, "1", "0"),
         lasso_thresh=ifelse(lasso>Thresh_la, "1", "0"),
         ridge_thresh=ifelse(ridge>Thresh_ri, "1", "0"),
         elasticN_thresh=ifelse(el>Thresh_el, "1", "0"),
         logit_up=ifelse(logit_up>0.5, "1", "0"),
         elasticN_up=ifelse(el_up>0.5, "1", "0"))
#Confusion matrix-----------------------------
#Pongo todos en los mismos niveles
t<-test_Results2
t$logit_thresh <- factor(t$logit_thresh,levels=c(0,1), labels=c("No Pobre", "Pobre"))
t$lasso_thresh <- factor(t$lasso_thresh,levels=c(0,1), labels=c("No Pobre", "Pobre"))
t$ridge_thresh <- factor(t$ridge_thresh,levels=c(0,1), labels=c("No Pobre", "Pobre"))
t$elasticN_thresh <- factor(t$elasticN_thresh,levels=c(0,1), labels=c("No Pobre", "Pobre"))
t$elasticN_up <- factor(t$elasticN_up,levels=c(0,1), labels=c("No Pobre", "Pobre"))
t$logit_up <- factor(t$logit_up,levels=c(0,1), labels=c("No Pobre", "Pobre"))

#Sensivilidad
s0<-caret::sensitivity(t$logit_thresh, t$Pobre, positive="Pobre")
s1<-caret::sensitivity(t$lasso_thresh, t$Pobre, positive="Pobre")
s2<-caret::sensitivity(t$ridge_thresh, t$Pobre, positive="Pobre")
s3<-caret::sensitivity(t$elasticN_thresh, t$Pobre, positive="Pobre")
s4<-caret::sensitivity(t$elasticN_up, t$Pobre, positive="Pobre")
s5<-caret::sensitivity(t$logit_up, t$Pobre, positive="Pobre")

caret::confusionMatrix(t$logit_thresh, t$Pobre, positive="Pobre")
caret::confusionMatrix(t$lasso_thresh, t$Pobre, positive="Pobre")
caret::confusionMatrix(t$ridge_thresh, t$Pobre, positive="Pobre")
caret::confusionMatrix(t$elasticN_thresh, t$Pobre, positive="Pobre")
caret::confusionMatrix(t$elasticN_up, t$Pobre, positive="Pobre")
caret::confusionMatrix(t$logit_up, t$Pobre, positive="Pobre")


#install.packages("yardstick")
library("yardstick")

conf_mat(data= t, estimate=logit_thresh, truth=Pobre)
conf_mat(data= t, estimate=lasso_thresh, truth=Pobre)
conf_mat(data= t, estimate=ridge_thresh, truth=Pobre)
conf_mat(data= t, estimate=elasticN_thresh, truth=Pobre)
conf_mat(data= t, estimate=elasticN_up, truth=Pobre)
conf_mat(data= t, estimate=logit_up, truth=Pobre)

sens(data= t, estimate=logit_thresh, truth=Pobre)
sens(data= t, estimate=lasso_thresh, truth=Pobre)
sens(data= t, estimate=ridge_thresh, truth=Pobre)
sens(data= t, estimate=elasticN_thresh, truth=Pobre)
sens(data= t, estimate=elasticN_up, truth=Pobre)
sens(data= t, estimate=logit_up, truth=Pobre)

f_meas(data= t, estimate=logit_thresh, truth=Pobre)
f_meas(data= t, estimate=lasso_thresh, truth=Pobre)
f_meas(data= t, estimate=ridge_thresh, truth=Pobre)
f_meas(data= t, estimate=elasticN_thresh, truth=Pobre)
f_meas(data= t, estimate=elasticN_up, truth=Pobre)
f_meas(data= t, estimate=logit_up, truth=Pobre)
str(t)


#Evaluacion modelos----------------------------------------------------
str(test_Results2)
t<-subset(test_Results2, select = c(Pobre, logit_thresh, logit_up, lasso_thresh, 
                                    ridge_thresh, elasticN_thresh, elasticN_up))
t$Pobre<-as.numeric(t$Pobre)
t$Pobre<-ifelse(t$Pobre==1, 0, 1)

#True positive/ TP+ FN -> Precision
P1<-sum(t$Pobre&t$logit_thresh==1)/(sum(t$Pobre&t$logit_thresh==1)+ sum(t$Pobre==0&t$logit_thresh==1))
P2<-sum(t$Pobre&t$logit_up==1)/ (sum(t$Pobre&t$logit_up==1)+ sum(t$Pobre==0&t$logit_up==1))
P3<-sum(t$Pobre&t$lasso_thresh==1) /(sum(t$Pobre&t$lasso_thresh==1)+sum(t$Pobre==0&t$lasso_thresh==1))
P4<-sum(t$Pobre&t$ridge_thresh==1)/(sum(t$Pobre&t$ridge_thresh==1)+ sum(t$Pobre==0&t$ridge_thresh==1))
P5<-sum(t$Pobre&t$elasticN_thresh==1)/ (sum(t$Pobre&t$elasticN_thresh==1)+ sum(t$Pobre==0&t$elasticN_thresh==1))
P6<-sum(t$Pobre&t$elasticN_up==1)/(sum(t$Pobre&t$elasticN_up==1)+sum(t$Pobre==0&t$elasticN_up==1))
precision<-c(P1,P2,P3,P4,P5,P6)
#Recall
R1<-sum(t$Pobre&t$logit_thresh==1)/(sum(t$Pobre&t$logit_thresh==1)+ sum(t$Pobre==1&t$logit_thresh==0))
R2<-sum(t$Pobre&t$logit_up==1)/ (sum(t$Pobre&t$logit_up==1)+ sum(t$Pobre==1&t$logit_up==0))
R3<-sum(t$Pobre&t$lasso_thresh==1) /(sum(t$Pobre&t$lasso_thresh==1)+sum(t$Pobre==1&t$lasso_thresh==0))
R4<-sum(t$Pobre&t$ridge_thresh==1)/(sum(t$Pobre&t$ridge_thresh==1)+ sum(t$Pobre==1&t$ridge_thresh==0))
R5<-sum(t$Pobre&t$elasticN_thresh==1)/ (sum(t$Pobre&t$elasticN_thresh==1)+ sum(t$Pobre==1&t$elasticN_thresh==0))
R6<-sum(t$Pobre&t$elasticN_up==1)/(sum(t$Pobre&t$elasticN_up==1)+sum(t$Pobre==1&t$elasticN_up==0))
recall<-c(R1,R2,R3,R4,R5,R6)
#Accuracy
A1<-sum(t$Pobre==t$logit_thresh)/nrow(t)
A2<-sum(t$Pobre==t$logit_up)/nrow(t)
A3<-sum(t$Pobre==t$lasso_thresh)/nrow(t)
A4<-sum(t$Pobre==t$ridge_thresh)/nrow(t)
A5<-sum(t$Pobre==t$elasticN_thresh)/nrow(t)
A6<-sum(t$Pobre==t$elasticN_up)/nrow(t)
accure<-c(A1,A2,A3,A4,A5,A6)
F1

F1_1<-R1*P1/(R1+P1)
F1_2<-R1*P1/(R1+P1)
F1_3
F1_4
F1_5
F1_6

