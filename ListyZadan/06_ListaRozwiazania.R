

library(data.table)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)
library(Metrics)

load("KrukUwr2018.RData")

set.seed(1234)
## Zadanie 1

# Wykonaj samodzielnie (bez używania dedykowanych pakietów/funkcji) wykres ROC. 
# Wejściem będzie wektor prawdopodobieństw i wektor oznaczeń good/bad.


roc_plot <- function(GoodBad, Scores){
  tmp <- data.table(GoodBad = GoodBad, Score = Scores)[order(-Score)]
  tmp[,`:=`(TPR = cumsum(GoodBad)/sum(GoodBad), FPR = cumsum(!GoodBad)/sum(!GoodBad))]
  plot(tmp$FPR, tmp$TPR, type = "l", ylab = "TPR", xlab = "FPR", xlim = c(0, 1), ylim = c(0, 1))
}


## Zadanie 2

#  - Zdefiniuj zmienną celu `IfPayment6M` jako pojawienie się wpłaty w pierwszych 6ciu miesięcach 
# obsługi dla każdego CaseId. Poza nowo utworzoną zmienną zachowaj tylko i wyłącznie dane z tabeli cases.


events[is.na(events)] <- 0
dataset_classif <- cases[events][Month<=6, .(IfPayment6M = ifelse(sum(NumberOfPayment)>0, 1, 0)), by = CaseId][cases]

# Następnie podziel zbiór danych przy użyciu funkcji `createDataPartition` z pakietu `caret` 
# na uczący i testowy w proporcji 70% i 30%.


train_ix <- createDataPartition(dataset_classif$IfPayment6M, p= 0.7, list = FALSE)

#  Zachowaj tylko właściwe zmienne i skonwertuj do innego typu danych jeżeli jest taka potrzeba.

cols <- c("Land", "Gender", "Product", "ExternalAgency", "Bailiff", "ClosedExecution")
dataset_classif[, (cols) := lapply(.SD, as.factor), .SDcols = cols]
dataset_classif[, CaseId := NULL]

# - Podzielony i przygotowane zbiory danych zapisz jako `cases_train` i `cases_test`

cases_train <- dataset_classif[train_ix, ]
cases_test <- dataset_classif[-train_ix, ]

# - Sprawdź jak wygląda rozkład zmiennej celu `IfPayment6M` w zbiorze uczącym i testowym.

round(prop.table(table(cases_train$IfPayment6M)), 3)
round(prop.table(table(cases_test$IfPayment6M)), 3)

## Zadanie 3

# Utwórz drzewo klasyfikacyjne do modelowania zjawiska dokonania wpłaty w  pierwszych 6 miesięcy obsługi. 
# Skorzystaj z przygotowanych danych z zadania 2. Zadanie wykonaj wykorzystując pakiet `rpart`. 
# - Ile węzłów zawiera wygenerowane drzewo?
# - Wyśietl podsumowanie dla zbudowanego modelu.
# - Wygeneruj wizualzację drzewa przy użyciu pakietu `rpart.plot`.

tree1 <- rpart(IfPayment6M~., data = cases_train, method = "class")
summary(tree1)
rpart.plot(tree1)


## Zadanie 4

# Zmodyfikuj drzewo klasyfikacyjne z poprzedniego zadania zmieniając wartości parametrów cp, maxdepth, minsplit. 
# Co można zauważyć?

#controls <- rpart.control(minsplit = 10, cp = 0.0001)

tree2 <- rpart(IfPayment6M~., data = cases_train, method = "class", control = rpart.control(minsplit = 1000, cp = 0.0001))
tree3 <-rpart(IfPayment6M~., data = cases_train, method = "class", control = rpart.control(minsplit = 100, cp = 0.0001))

trees_list <- list(t1 = tree1, t2 = tree2, t3 = tree3) # zapisujemy wygenerowane modele do listy

par(mfrow = c(2, 2))

sapply(trees_list, plot)  #wizualizacja utworzonych drzew

par(mfrow = c(2, 2))
sapply(trees_list, plotcp) # wizualzacja warotsci bledu w konstekscie wzrostu zlozonosci drzewa

dev.off()

## Zadanie 5

# Dokonaj predykcji klasy na podstawie zbudowanych modeli drzew zarówno dla zbioru uczącego jak i testowego
# Wyniki zapisz do zmiennych

pred_tr_class <- lapply(trees_list, predict, newdata = cases_train, type = "class")
pred_tst_class <- lapply(trees_list, predict, newdata = cases_test, type = "class")


## Zadanie 6

# Wygeneruj macierz konfuzji przy użyciu funkcji `confusionMatrix` z pakietu `caret` dla najbardziej i najmniej złożonego 
# drzewa z poprzedniego zadania dla zbioru uczącego i testowego. 
# Jak kształtują się miary Accuracy, Precision i Sensitivity w obydwu macierzach?

confusionMatrix(pred_tr_class$t1, as.factor(cases_train$IfPayment6M)) 
confusionMatrix(pred_tst_class$t1, as.factor(cases_test$IfPayment6M))

confusionMatrix(pred_tr_class$t3, as.factor(cases_train$IfPayment6M))
confusionMatrix(pred_tst_class$t3, as.factor(cases_test$IfPayment6M))


## Zadanie 7

# Wygeneruj wykres ROC dla zbioru uczącego i testowego za pomocą napisanej przez Ciebie funkcji z zadania 1.

# do generowania krzywej ROC muismy znac prawdopodobienstwa

pred_tr_prob <- lapply(trees_list, predict, newdata = cases_train, type = "prob") 
pred_tst_prob <- lapply(trees_list, predict, newdata = cases_test, type = "prob")

roc_plot(GoodBad = cases_train$IfPayment6M, Scores = pred_tr_prob$t3[,2]) # ROC dla treningowego
roc_plot(GoodBad = cases_test$IfPayment6M, Scores = pred_tst_prob$t3[,2]) # ROC dla testowego


## Zadanie 8

# Zbuduj drzewo regresyjne w oparciu o dane aplikacyjne i behawioralne z pierwszych trzech miesięcy i 
# oszacuj skuteność skumulowaną od 4 do 12 miesiąca obsługi.

tmp1 <- cases[events][Month > 3, .(SR12m = sum(PaymentAmount)/max(TOA)), by = CaseId] # okreslamy zmienną celu
cases_behav <- events[Month <= 3, lapply(.SD, sum), by = CaseId, .SDcols = setdiff(names(events),c("Month", "CaseId"))][cases][tmp1]
cases_behav[, CaseId :=  NULL]

# behawior do 3M
cols <- c("Land", "Gender", "Product", "ExternalAgency", "Bailiff", "ClosedExecution")
cases_behav[, (cols) := lapply(.SD, as.factor), .SDcols = cols]

#dla ulatwienia zostawiamy tylko kompletne sprawy
cases_behav <- na.omit(cases_behav)

tree4 <- rpart(SR12m ~., data = cases_behav, method = "anova")


## Zadanie 9 

# Zbuduj drzewa regresyjne do modelowania tego samego problemu jak w zadaniu 7. Tym razem modeluj
# dla wszystkich kombinacji wartości parametrów `maxdepth = seq(2,6,1)`, `minsplit = c(500,1000,5000)`
# 
# Do zbudowania siatki kombinacji wartości parametrów użyj funkcji `expand.grid`.

train_ix <- createDataPartition(cases_behav$SR12m, p = 0.7, list = FALSE)

grid <- expand.grid(maxdepth = c(2:6), minsplit = c(500, 1000, 5000))

trees_parms <- list()

for (i in 1:nrow(grid)) {
  
  minsplit <- grid$minsplit[i]
  maxdepth <- grid$maxdepth[i]
  
  
  trees_parms[[i]]<- rpart(SR12m~.,
                           data = cases_behav,
                           subset = train_ix,
                           method = "anova",
                           minsplit = minsplit,
                           maxdepth = maxdepth,
                           cp = 0.0001)
  
}


## Zadanie 10

#  Do modelowania tego samego problemu regersyjnego użyj lasu losowego z pakietu randomForest. 
# Czy przy domyślnych parametrach wynik mierzony miarą RMSE jest lepszy od najlepszego wyniku z zdania 9?


las <- randomForest(formula = SR12m ~ .,
                    data = cases_behav,
                    subset = train_ix,
                    nodesize = 1000 # jednak rezygnuje z wartosci domyslnych
)

pr_forest <- predict(las, newdata = cases_behav[-train_ix,]) # prognoza na testowym dla lasu
pr_trees <- sapply(trees_parms, predict, newdata =cases_behav[-train_ix,]) # prognoza na tesotwym dla kazdego z 15 drzew z zadania 9

zussamen <- cbind(pr_forest, pr_trees) # polaczenie progonzy z zdania 9 i 10

trees_names <- c(paste0("tree_",c(1:length(trees_parms)))) # nazwy dla kolmn prognoz z zadania 9
colnames(zussamen) <- c("rf",trees_names) # jeszcze dodamy nazwy kolumn, by bylo wiadomo o co loto


rmse_all <- c()

for (i in 1: ncol(zussamen)){
  rmse_all[i] <- rmse(actual = cases_behav[-train_ix]$SR12m, predicted = zussamen[, i])
  names(rmse_all)[i] <- colnames(zussamen)[i]
}

sort(rmse_all) 