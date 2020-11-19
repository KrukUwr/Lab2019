library(data.table)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)
library(Metrics)


# Zadanie 1 -------------------------------------------------------------------------------------------------------
# Wykonaj samodzielnie (bez używania dedykowanych pakietów/funkcji) wykres ROC. 
# Wejściem będzie wektor prawdopodobieństw i wektor oznaczeń good/bad.

rocPlot <- function(pred, prob) {
  
  roc_summary <- data.table(Pred=pred, Prob=prob)
  setorder(roc_summary, -Prob)
  roc_summary[,':='(TPR=cumsum(Pred)/sum(Pred), FPR=cumsum(!Pred)/sum(!Pred))]
  
  plot(x=roc_summary$FPR, y=roc_summary$TPR, 
    type='l', xlab='FPR', ylab='TPR', xlim=c(0,1), ylim=c(0,1))
  abline(a=0, b=1, lty=2, col="gray")
  
}


# Zadanie 2 -------------------------------------------------------------------------------------------------------
# - Zdefiniuj zmienną celu IfPayment6M jako pojawienie się wpłaty w pierwszych 6ciu miesiącach obsługi dla każdego CaseId. 
    # Poza nowo utworzoną zmienną zachowaj tylko i wyłącznie dane z tabeli cases.
# - Następnie podziel zbiór danych przy użyciu funkcji createDataPartition z pakietu caret na uczący i testowy 
    # w proporcji 70% i 30%.
# - Zachowaj tylko właściwe zmienne i skonwertuj do innego typu danych jeżeli jest taka potrzeba.
# - Podzielony i przygotowane zbiory danych zapisz jako cases_train i cases_test
# - Sprawdź jak wygląda rozkład zmiennej celu IfPayment6M w zbiorze uczącym i testowym.


load("./data/KrukUWr2020.RData")

events[is.na(events)] <- 0
if_payment_6m <- events[Month<=6, .(IfPayment6M=ifelse(sum(NumberOfPayment)>0, 1, 0)), by=CaseId]
if_payment_6m[,.N, by=IfPayment6M]

setkey(cases, CaseId)
setkey(if_payment_6m, CaseId)
dataset <- cases[if_payment_6m]

train_rows <- createDataPartition(dataset$IfPayment6M, p=0.7, list=FALSE)

column_names <- c("Land", "Gender", "Product", "ExternalAgency", "Bailiff", "ClosedExecution")
dataset[,(column_names):=lapply(.SD, as.factor), .SDcols=column_names]
dataset[, CaseId:=NULL]

cases_train <- dataset[train_rows]
cases_test <- dataset[-train_rows]

round(prop.table(table(cases_train$IfPayment6M)), 2)
round(prop.table(table(cases_test$IfPayment6M)), 2)


# Zadanie 3 -------------------------------------------------------------------------------------------------------
# Utwórz drzewo klasyfikacyjne do modelowania zjawiska dokonania wpłaty w pierwszych 6 miesięcy obsługi. 
# Skorzystaj z przygotowanych danych z zadania 2. Zadanie wykonaj wykorzystując pakiet rpart.

# - Ile węzłów zawiera wygenerowane drzewo?
# - Wyświetl podsumowanie dla zbudowanego modelu.
# - Wygeneruj wizualizację drzewa przy użyciu pakietu rpart.plot.

tree1 <- rpart(IfPayment6M~., data=cases_train, method="class")

summary(tree1)
rpart.plot(tree1)


# Zadanie 4 -------------------------------------------------------------------------------------------------------
# Zmodyfikuj drzewo klasyfikacyjne z poprzedniego zadania zmieniając wartości parametrów cp, maxdepth, minsplit. 
# Co można zauważyć?

tree2 <- rpart(IfPayment6M~., data=cases_train, method="class", control=rpart.control(minsplit = 1000, cp=0.0001))
tree3 <- rpart(IfPayment6M~., data=cases_train, method="class", control=rpart.control(minsplit = 100, cp=0.0001))

trees_list <- list(Tree1=tree1, Tree2=tree2, Tree3=tree3)
par(mfrow=c(2,2))
sapply(trees_list, rpart.plot)
par(mfrow=c(1,1))

par(mfrow=c(2,2))
sapply(trees_list, plotcp)
par(mfrow=c(1,1))


# Zadanie 5 -------------------------------------------------------------------------------------------------------
# Dokonaj predykcji klasy na podstawie zbudowanych modeli drzew zarówno dla zbioru uczącego jak i testowego.
# Wyniki zapisz do zmiennych.

pred_train <- lapply(trees_list, predict, newdata=cases_train, type="class")
pred_test <- lapply(trees_list, predict, newdata=cases_test, type="class")


# Zadanie 6 -------------------------------------------------------------------------------------------------------
# Wygeneruj macierz konfuzji przy użyciu funkcji confusionMatrix z pakietu caret dla najbardziej 
# i najmniej złożonego drzewa z poprzedniego zadania dla zbioru uczącego i testowego. 
# Jak kształtują się miary Accuracy, Precision i Sensitivity w obydwu macierzach?

confusionMatrix(pred_train$Tree1, factor(cases_train$IfPayment6M), positive = "1")
# confusionMatrix(pred_train$Tree2, factor(cases_train$IfPayment6M), positive = "1")
confusionMatrix(pred_train$Tree3, factor(cases_train$IfPayment6M), positive = "1")

confusionMatrix(pred_test$Tree1, factor(cases_test$IfPayment6M), positive = "1")
# confusionMatrix(pred_test$Tree2, factor(cases_test$IfPayment6M), positive = "1")
confusionMatrix(pred_test$Tree3, factor(cases_test$IfPayment6M), positive = "1")


# Zadanie 7 -------------------------------------------------------------------------------------------------------
# Wygeneruj wykres ROC dla zbioru uczącego i testowego za pomocą napisanej przez Ciebie funkcji z zadania 1.

prob_train <- lapply(trees_list, predict, newdata=cases_train, type="prob")
prob_test <- lapply(trees_list, predict, newdata=cases_test, type="prob")

rocPlot(pred = cases_train$IfPayment6M, prob = prob_train$Tree3[,2])
rocPlot(pred = cases_test$IfPayment6M, prob = prob_test$Tree3[,2])


# Zadanie 8 -------------------------------------------------------------------------------------------------------
# Zbuduj drzewo regresyjne w oparciu o dane aplikacyjne i behawioralne z pierwszych trzech miesięcy 
# i oszacuj skuteczność skumulowaną od 4 do 12 miesiąca obsługi.

## Zmienna objaśniana
payments_12m <- events[Month>=4, .(Payments_4_12=sum(PaymentAmount)), by="CaseId"]
setkey(payments_12m, CaseId)
cases <- cases[payments_12m, nomatch=0L]
cases[,SR_4_12 := Payments_4_12/TOA]
cases[,Payments_4_12:=NULL]

## Dołączenie zmiennych behawioralnych:
behavioral_variables <- setdiff(names(events), c("CaseId", "Month"))
cases_behav <- events[Month<=3, lapply(.SD, sum), by="CaseId", .SDcols=behavioral_variables]
setkey(cases_behav, CaseId)

cases_behav <- cases[cases_behav]
cases_behav[,CaseId:=NULL]

## Przekonwertowanie zmiennych jakościowych + podział na zbiory train-test jak w zadaniu 2
column_names <- c("Land", "Gender", "Product", "ExternalAgency", "Bailiff", "ClosedExecution")
cases_behav[,(column_names):=lapply(.SD, as.factor), .SDcols=column_names]

train_rows <- createDataPartition(cases_behav$SR_4_12, p=0.7, list=FALSE)
cases_behav_train <- cases_behav[train_rows]
cases_behav_test <- cases_behav[-train_rows]

## Budowa modelu:
tree_regr1 <- rpart(SR_4_12~., data=cases_behav_train, method="anova")



# Zadanie 9 -------------------------------------------------------------------------------------------------------
# Zbuduj drzewa regresyjne do modelowania tego samego problemu jak w zadaniu 7. 
# Tym razem modeluj dla wszystkich kombinacji wartości parametrów maxdepth = seq(2,6,1), minsplit = c(500,1000,5000)
# Do zbudowania siatki kombinacji wartości parametrów użyj funkcji expand.grid.

parameters_grid <- expand.grid(maxdepth=2:6, minsplit = c(500,1000,5000))

trees_regr <- list()

for(param_row in 1:nrow(parameters_grid)) {
  
  ctrl <- rpart.control(
    maxdepth = parameters_grid[param_row,]$maxdepth, 
    minsplit = parameters_grid[param_row,]$minsplit,
    cp=0.0001)
  
  trees_regr[[param_row]] <- rpart(SR_4_12~., data=cases_behav_train, 
    method="anova", control=ctrl)
  
}

## Predykcje na zbiorze testowym zbudowanych modeli
predictions_regr <- sapply(trees_regr, predict, newdata=cases_behav_test)

## Porównanie zbuowanych modeli za pomocą RMSE
parameters_grid$RMSE <- apply(predictions_regr, 2, rmse, actual=cases_behav_test$SR_4_12)
parameters_grid[order(parameters_grid$RMSE),]


# Zadanie 10 ------------------------------------------------------------------------------------------------------
# Zbuduj po jednym modelu klasyfikacyjnym i regresyjnym metodą lasów losowych (pakiet randomForest). 
# Czy wyniki uzyskane tą metodą są lepsze od najlepszych uzyskanych w zadaniach 6 i 9 (stosując odpowiednio kryterium RMSE i AUC)

## Wersja uproszczona z usunięciem wierszy z brakami danych - model wymaga zbioru danych bez braków
rf0 <- randomForest(formula=SR_4_12~., data=cases_behav_train, nodesize=1000, ntrees=500, maxnodes=2^6, 
  na.action = "na.omit")
# Za pomocą argumentu maxnodes można wskazać maksymalną głębokość pojedynczych drzew (analogiczny parameter jak maxdepth dla drzewa z pakietu rpart) 
# Głbokość d wyznaczamy jako 2^d w argumencie maxnodes (inaczej niż powiedziałem na zajęciach)

## Wersja zalecana z imputacją za braki danych:
  ## średniej dla kolumn zmiennoprzecinkowych (double)
  ## najczęściej występująca wartość dla kolumn z wartościami całkowitymi jako integer
  ## najczęściej występująca wartość dla factorów

getValuesForImputation <- function(x) {
  
  if(is.double(x)) { # średnia dla wartości zmiennoprzecinkowych
    val <- mean(x, na.rm=TRUE)
  } else if(is.integer(x)) { # moda dla kolumn z wartościami całkowitymi - jako integer
    val <- as.integer(names(sort(-table(x)))[1])
  } else { # moda dla pozostałych
    val <- names(sort(-table(x)))[1]
  }
  return(val)
}

values_imputation <- lapply(cases_behav_train, getValuesForImputation)

for(column_name in names(cases_behav_train)) {
  cases_behav_train[is.na(get(column_name)), c(column_name):=values_imputation[[column_name]]]
  cases_behav_test[is.na(get(column_name)), c(column_name):=values_imputation[[column_name]]]
}

rf1 <- randomForest(formula=SR_4_12~., data=cases_behav_train, nodesize=1000, ntrees=500, maxnodes=2^6)

pred_rf1 <- predict(rf1, newdata=cases_behav_test)
rmse(actual = cases_behav_test$SR_4_12, predicted = pred_rf1)
parameters_grid[order(parameters_grid$RMSE),]


# GBM -----------------------------------------------------------------------------------------------------------
# Jak w zadaniu 10 tylko z wykorzystaniem modelu gbm 
library(gbm)

boost_model <- gbm(SR_4_12~., data=cases_behav_train, distribution = "gaussian", 
  n.trees = 500, interaction.depth = 6, shrinkage = 0.01, n.minobsinnode = 1000)

pred_boost <- predict(boost_model, newdata=cases_behav_test)
rmse(actual = cases_behav_test$SR_4_12, predicted = pred_boost)


