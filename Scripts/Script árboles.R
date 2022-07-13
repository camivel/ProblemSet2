rm(list=ls())

install.packages("pacman")
install.packages("themis")
require(pacman)
p_load(tidyverse , rio , caret ,  modelsummary , gamlr,
       ROCR, # ROC
       pROC, ggplot2, doParallel, rattle, MLmetrics,
       janitor, fastDummies, tidymodels)


prop.table(table(train_yx$Pobre))
train_yx <- subset(train_yx, select = -c(id, Ingtotug, Ingtotugarr, Ingpcug, Indigente))

set.seed(1111)

#Crear base de entrenamiento
split1 <- createDataPartition(train_yx$Pobre , p = 0.7)[[1]]
training <- train_yx[split1,]

#Crear bases de evalaución y prueba
other <- train_yx[-split1,]
split2 <- createDataPartition(other$Pobre , p = 1/3)[[1]]

evaluation <- other[split2,]
testing <- other[-split2,]

prop.table(table(training$Pobre))
prop.table(table(testing$Pobre))
prop.table(table(evaluation$Pobre))

### Estimación Modelo 1 - árbol
testing$Pobre <- factor(testing$Pobre)
training$Pobre <- factor(training$Pobre)
evaluation$Pobre <- factor(evaluation$Pobre)

modelo1 <- decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("classification")
modelo1_fit <- fit(modelo1, Pobre ~ ., data = training)

#Importancia de variables en Modelo 1
importancia <- varImp(modelo1_fit$fit)
importancia <- importancia %>%
  data.frame() %>%
  rownames_to_column(var = "Variable") %>%
  mutate(Porcentaje = Overall/sum(Overall)) %>%
  filter(Porcentaje > 0) %>%
  arrange(desc(Porcentaje))

ggplot(importancia, aes(x = Porcentaje, 
                        y = reorder(Variable, Porcentaje))) +
  geom_bar(stat = "identity", fill = "darkblue", alpha = 0.8) +
  labs(y = "Variable") +
  scale_x_continuous(labels = scales::percent) +
  theme_classic()

#Matriz de confusión y evaluación de modelo

y_hat_insample <- predict(modelo1_fit, training)$.pred_class
cm_insample <- confusionMatrix(training$Pobre, y_hat_insample,
                               positive = "1")$table
cm_insample

y_hat_outsample <- predict(modelo1_fit, testing)$.pred_class
cm_outsample <- confusionMatrix(testing$Pobre, y_hat_outsample,
                                positive = "1")$table
cm_outsample

#Estadísticos
acc_in <- Accuracy(y_true = training$Pobre, y_pred = y_hat_insample)
acc_in <- round(100*acc_in, 2)
pre_in <- Precision(y_true = training$Pobre, y_pred = y_hat_insample,
                    positive = "1")
pre_in <- round(100*pre_in, 2)
recall_in <- Recall(y_true = training$Pobre, y_pred = y_hat_insample,
                    positive = "1")
recall_in <- round(100*recall_in, 2)
f1_in <- F1_Score(y_true = training$Pobre, y_pred = y_hat_insample,
                  positive = "1")
f1_in <- round(100*f1_in, 2)

acc_out <- Accuracy(y_true = testing$Pobre, y_pred = y_hat_outsample)
acc_out <- round(100*acc_out, 2)
pre_out <- Precision(y_true = testing$Pobre, y_pred = y_hat_outsample,
                     positive = "1")
pre_out <- round(100*pre_out, 2)
recall_out <- Recall(y_true = testing$Pobre, y_pred = y_hat_outsample,
                     positive = "1")
recall_out <- round(100*recall_out, 2)
f1_out <- F1_Score(y_true = testing$Pobre, y_pred = y_hat_outsample,
                   positive = "1")
f1_out <- round(100*f1_out, 2)

resultados_arbol1 <- data.frame(Modelo = "Árbol 1", Base = c("Training", "Test"), 
                         Accuracy = c(acc_in, acc_out), 
                         Precision = c(pre_in, pre_out),
                         Recall = c(recall_in, recall_out),
                         F1 = c(f1_in, f1_out))


### Estimación Modelo 2 - Árbol mejorado con hiperparámetros

modelo2 <- decision_tree(
  cost_complexity = tune(),
  tree_depth = tune(),
  min_n = tune()
) %>%
  set_engine("rpart") %>%
  set_mode("classification")

#Se fijan los hiperparámetros
tree_grid <- crossing(
  cost_complexity = c(0.0001),
  min_n = c(2, 14, 27),
  tree_depth = c(4, 8, 16)
)

set.seed(1111)
folds <- vfold_cv(training, strata = Pobre, v = 5)

set.seed(1111)
modelo2_cv <- tune_grid(
  modelo2,
  Pobre ~ .,
  resamples = folds,
  grid = tree_grid,
  metrics = metric_set(f_meas),
  control = control_grid(event_level = 'second')
)

#Selección de mejor modelo usando hiperparámetros
modelo2 <- finalize_model(modelo2, select_best(modelo2_cv))
modelo2_fit <- fit(modelo2, Pobre ~ ., training)

importancia <- varImp(modelo2_fit$fit)
importancia <- importancia %>%
  data.frame() %>%
  rownames_to_column(var = "Variable") %>%
  mutate(Porcentaje = Overall/sum(Overall)) %>%
  filter(Porcentaje > 0) %>%
  arrange(desc(Porcentaje))

ggplot(importancia, aes(x = Porcentaje, 
                        y = reorder(Variable, Porcentaje))) +
  geom_bar(stat = "identity", fill = "darkblue", alpha = 0.8) +
  labs(y = "Variable") +
  scale_x_continuous(labels = scales::percent) +
  theme_classic()

#Evaluación modelo 2
y_hat_insample <- predict(modelo2_fit, training)$.pred_class
cm_insample <- confusionMatrix(training$Pobre, y_hat_insample,
                               positive = "1")$table

y_hat_outsample <- predict(modelo2_fit, testing)$.pred_class
cm_outsample <- confusionMatrix(testing$Pobre, y_hat_outsample,
                                positive = "1")$table

#Estadísticos
acc_in <- Accuracy(y_true = training$Pobre, y_pred = y_hat_insample)
acc_in <- round(100*acc_in, 2)
pre_in <- Precision(y_true = training$Pobre, y_pred = y_hat_insample,
                    positive = "1")
pre_in <- round(100*pre_in, 2)
recall_in <- Recall(y_true = training$Pobre, y_pred = y_hat_insample,
                    positive = "1")
recall_in <- round(100*recall_in, 2)
f1_in <- F1_Score(y_true = training$Pobre, y_pred = y_hat_insample,
                  positive = "1")
f1_in <- round(100*f1_in, 2)

acc_out <- Accuracy(y_true = testing$Pobre, y_pred = y_hat_outsample)
acc_out <- round(100*acc_out, 2)
pre_out <- Precision(y_true = testing$Pobre, y_pred = y_hat_outsample,
                     positive = "1")
pre_out <- round(100*pre_out, 2)
recall_out <- Recall(y_true = testing$Pobre, y_pred = y_hat_outsample,
                     positive = "1")
recall_out <- round(100*recall_out, 2)
f1_out <- F1_Score(y_true = testing$Pobre, y_pred = y_hat_outsample,
                   positive = "1")
f1_out <- round(100*f1_out, 2)

resultados_arbol2 <- data.frame(Modelo = "Árbol 2: Grid search", Base = c("Train", "Test"), 
                          Accuracy = c(acc_in, acc_out), 
                          Precision = c(pre_in, pre_out),
                          Recall = c(recall_in, recall_out),
                          F1 = c(f1_in, f1_out))

resultados <- rbind(resultados, resultados2)

###Estimación Modelo 3 - Árbol corrigiendo imbalance
set.seed(1111)
receta <- recipe(Pobre ~ ., data = training) %>%
  themis::step_upsample(Pobre) #Upsample
training_smote <- bake(prep(receta), new_data=NULL)
prop.table(table(training_smote$Pobre))

modelo3 <- decision_tree(
  cost_complexity = tune(),
  tree_depth = tune()
) %>%
  set_engine("rpart") %>%
  set_mode("classification")

wflow <- workflow() %>%
  add_recipe(receta) %>%
  add_model(modelo3)

tree_grid <- crossing(
  cost_complexity = c(0.01, 0.001, 0.0001),
  tree_depth = c(10, 15, 20, 25, 30) #Aumento debido a que hay más observaciones por balanceo
)

set.seed(1111)
folds <- vfold_cv(training, strata = Pobre, v = 5)

set.seed(1111)
modelo3_cv <- tune_grid(
  wflow,
  resamples = folds,
  grid = tree_grid,
  metrics = metric_set(f_meas),
  control = control_grid(event_level = 'second')
)

#Selección de mejor modelo usando hiperparámetros y upsample
modelo3 <- finalize_model(modelo3, select_best(modelo3_cv))
modelo3_fit <- fit(modelo3, Pobre ~ ., training)

importancia <- varImp(modelo3_fit$fit)
importancia <- importancia %>%
  data.frame() %>%
  rownames_to_column(var = "Variable") %>%
  mutate(Porcentaje = Overall/sum(Overall)) %>%
  filter(Porcentaje > 0) %>%
  arrange(desc(Porcentaje))

ggplot(importancia, aes(x = Porcentaje, 
                        y = reorder(Variable, Porcentaje))) +
  geom_bar(stat = "identity", fill = "darkblue", alpha = 0.8) +
  labs(y = "Variable") +
  scale_x_continuous(labels = scales::percent) +
  theme_classic()

#Evaluación Modelo 3
y_hat_insample <- predict(modelo3_fit, training)$.pred_class
cm_insample <- confusionMatrix(training$Pobre, y_hat_insample,
                               positive = "1")$table

y_hat_outsample <- predict(modelo3_fit, testing)$.pred_class
cm_outsample <- confusionMatrix(testing$Pobre, y_hat_outsample,
                                positive = "1")$table

#Estadísticos
acc_in <- Accuracy(y_true = training$Pobre, y_pred = y_hat_insample)
acc_in <- round(100*acc_in, 2)
pre_in <- Precision(y_true = training$Pobre, y_pred = y_hat_insample,
                    positive = "1")
pre_in <- round(100*pre_in, 2)
recall_in <- Recall(y_true = training$Pobre, y_pred = y_hat_insample,
                    positive = "1")
recall_in <- round(100*recall_in, 2)
f1_in <- F1_Score(y_true = training$Pobre, y_pred = y_hat_insample,
                  positive = "1")
f1_in <- round(100*f1_in, 2)

acc_out <- Accuracy(y_true = testing$Pobre, y_pred = y_hat_outsample)
acc_out <- round(100*acc_out, 2)
pre_out <- Precision(y_true = testing$Pobre, y_pred = y_hat_outsample,
                     positive = "1")
pre_out <- round(100*pre_out, 2)
recall_out <- Recall(y_true = testing$Pobre, y_pred = y_hat_outsample,
                     positive = "1")
recall_out <- round(100*recall_out, 2)
f1_out <- F1_Score(y_true = testing$Pobre, y_pred = y_hat_outsample,
                   positive = "1")
f1_out <- round(100*f1_out, 2)

resultados_arbol3 <- data.frame(Modelo = "Modelo 3: Grid search + Oversampling", Base = c("Train", "Test"), 
                          Accuracy = c(acc_in, acc_out), 
                          Precision = c(pre_in, pre_out),
                          Recall = c(recall_in, recall_out),
                          F1 = c(f1_in, f1_out))

resultados_arboles <- rbind(resultados_arbol1, resultados_arbol2, resultados_arbol3)

#Estimación modelo 4 - Árbol con menos variables

set.seed(1111)
receta <- recipe(Pobre ~ No_menores+Arriendo+informal+Npersug+ing_arrie_pen+Tipovivienda, data = training) %>%
  themis::step_upsample(Pobre) #Upsample 
training_smote <- bake(prep(receta), new_data=NULL)
prop.table(table(training_smote$Pobre))

modelo4 <- decision_tree(
  cost_complexity = tune(),
  tree_depth = tune()
) %>%
  set_engine("rpart") %>%
  set_mode("classification")

wflow <- workflow() %>%
  add_recipe(receta) %>%
  add_model(modelo4)

tree_grid <- crossing(
  cost_complexity = c(0.01, 0.001, 0.0001),
  tree_depth = c(10, 15, 20, 25, 30) #Aumento debido a que hay más observaciones por balanceo
)

set.seed(1111)
folds <- vfold_cv(training, strata = Pobre, v = 5)

set.seed(1111)
modelo4_cv <- tune_grid(
  wflow,
  resamples = folds,
  grid = tree_grid,
  metrics = metric_set(f_meas),
  control = control_grid(event_level = 'second')
)

#Selección de mejor modelo usando hiperparámetros y upsample
modelo4 <- finalize_model(modelo4, select_best(modelo4_cv))
modelo4_fit <- fit(modelo4, Pobre ~ No_menores + Arriendo + informal+ Npersug+ ing_arrie_pen+ Tipovivienda, training)

importancia <- varImp(modelo4_fit$fit)
importancia <- importancia %>%
  data.frame() %>%
  rownames_to_column(var = "Variable") %>%
  mutate(Porcentaje = Overall/sum(Overall)) %>%
  filter(Porcentaje > 0) %>%
  arrange(desc(Porcentaje))

ggplot(importancia, aes(x = Porcentaje, 
                        y = reorder(Variable, Porcentaje))) +
  geom_bar(stat = "identity", fill = "darkblue", alpha = 0.8) +
  labs(y = "Variable") +
  scale_x_continuous(labels = scales::percent) +
  theme_classic()


#Evaluación Modelo 4

y_hat_insample <- predict(modelo4_fit, training)$.pred_class
cm_insample <- confusionMatrix(training$Pobre, y_hat_insample,
                               positive = "1")$table

y_hat_outsample <- predict(modelo4_fit, testing)$.pred_class
cm_outsample <- confusionMatrix(testing$Pobre, y_hat_outsample,
                                positive = "1")$table

#Estadísticos
acc_in <- Accuracy(y_true = training$Pobre, y_pred = y_hat_insample)
acc_in <- round(100*acc_in, 2)
pre_in <- Precision(y_true = training$Pobre, y_pred = y_hat_insample,
                    positive = "1")
pre_in <- round(100*pre_in, 2)
recall_in <- Recall(y_true = training$Pobre, y_pred = y_hat_insample,
                    positive = "1")
recall_in <- round(100*recall_in, 2)
f1_in <- F1_Score(y_true = training$Pobre, y_pred = y_hat_insample,
                  positive = "1")
f1_in <- round(100*f1_in, 2)

acc_out <- Accuracy(y_true = testing$Pobre, y_pred = y_hat_outsample)
acc_out <- round(100*acc_out, 2)
pre_out <- Precision(y_true = testing$Pobre, y_pred = y_hat_outsample,
                     positive = "1")
pre_out <- round(100*pre_out, 2)
recall_out <- Recall(y_true = testing$Pobre, y_pred = y_hat_outsample,
                     positive = "1")
recall_out <- round(100*recall_out, 2)
f1_out <- F1_Score(y_true = testing$Pobre, y_pred = y_hat_outsample,
                   positive = "1")
f1_out <- round(100*f1_out, 2)

resultados_arbol4 <- data.frame(Modelo = "Modelo 4: Grid search + Oversampling + Selected Predictors", Base = c("Train", "Test"), 
                                Accuracy = c(acc_in, acc_out), 
                                Precision = c(pre_in, pre_out),
                                Recall = c(recall_in, recall_out),
                                F1 = c(f1_in, f1_out))

resultados_arboles <- rbind(resultados_arbol1, resultados_arbol2, resultados_arbol3,
                            resultados_arbol4)





