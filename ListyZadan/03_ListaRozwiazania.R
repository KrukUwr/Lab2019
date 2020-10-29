library(data.table)

load("./data/KrukUWr2020.RData")

# Zadanie 1 -------------------------------------------------------------------------------------------------------

# Przygotowanie danych aplikacyjnych:
# 
# * Do zbioru cech aplikacyjnych dodaj zmienną wydzielającą klientów, którzy dokonali jakiejkolwiek wpłaty w pierwszych 6 miesiącach obsługi (klienci „dobrzy"). Jaki jest udział dobrych klientów w zbiorze?
# * Podziel w sposób losowy zbiór danych aplikacyjnych na treningowy `train` i testowy `test`. Każdy z nich powinien zawierać około połowę wszystkich spraw. 
# * Zakoduj w wybrany sposób cechy jakościowe `Product` oraz `Gender`. 
# * Uzupełnij braki danych tych zmiennych aplikacyjnych, dla których jest to możliwe i sensowne.
# * Usuń obserwacje odstające cech `LoanAmount`, `DPD` oraz `LastPaymentAmount`. 
# * Zestandaryzuj cechy aplikacyjne. ?kmeans

cases


setkey(cases,CaseId)
setkey(events,CaseId)

## Dodanie informacji czy klient dobry/zły (G/B)

payments <- events[Month <= 6,.(Payments6M = sum(ifelse(is.na(PaymentAmount),0,PaymentAmount))), by=.(CaseId)]
setkey(payments,CaseId)

cases <- cases[payments[,.(CaseId,Payments6M)],nomatch=0]
cases[ ,Client := ifelse(Payments6M>0, "G", "B")]


## Podział zbioru na treningowy i testowy

set.seed(222)
if_train <- sample(c(TRUE, FALSE), cases[,.N], replace=TRUE)

train <- cases[if_train]
test <- cases[!if_train]


## Kodowanie Product i Gender
train[,CreditCard := ifelse(Product=="Credit card",1,0)]
train[,Female := ifelse(Gender=="FEMALE",1,0)]
test[,CreditCard := ifelse(Product=="Credit card",1,0)]
test[,Female := ifelse(Gender=="FEMALE",1,0)]


## Uzupełnianie braków danych:

variables = c(         
    "LoanAmount",
    "TOA",
    "Principal",
    "Interest",
    "Other",
    "D_ContractDateToImportDate",
    "DPD",
    "PopulationInCity",
    "Age",
    "LastPaymentAmount",
    "M_LastPaymentToImportDate",
    "GDPPerCapita",
    "MeanSalary"
  )

avg_in_columns <- sapply(train[,.SD,.SDcols=variables], mean, na.rm=TRUE)


for(var_name in variables) {
  
  # w celu uniknięcia warningów na temat niezgodności klasy danych pomiędzy kolumnami o wektorem średnich
  train[,c(var_name):=as.numeric(get(var_name))]
  test[,c(var_name):=as.numeric(get(var_name))]
  
  avg_for_variable <- unname( avg_in_columns[var_name] )
  train[is.na(get(var_name)), c(var_name):=avg_for_variable]
  test[is.na(get(var_name)), c(var_name):=avg_for_variable]
  
}


## Usunięcie wierszy z wartościami odstającymi:

epsilon = 0.001

# kolejność modyfikacji zbiorów istotna: najpierw testowy, następnie uczący
test <- test[LoanAmount<quantile(train[["LoanAmount"]], probs=1-epsilon),]
test <- test[DPD<quantile(train[["DPD"]], probs=1-epsilon),]
test <- test[LastPaymentAmount<quantile(train[["LastPaymentAmount"]], probs=1-epsilon),]

train <- train[LoanAmount<quantile(train[["LoanAmount"]], probs=1-epsilon),]
train <- train[DPD<quantile(train[["DPD"]], probs=1-epsilon),]
train <- train[LastPaymentAmount<quantile(train[["LastPaymentAmount"]], probs=1-epsilon),]


## Standaryzacja:

avg_in_columns <- sapply(train[,.SD,.SDcols=variables], mean, na.rm=TRUE)
std_in_columns <- sapply(train[,.SD,.SDcols=variables], sd, na.rm=TRUE)

for(var_name in variables) {
  
  avg <- unname( avg_in_columns[var_name] )
  std <- unname( std_in_columns[var_name] )
  
  train[,c(var_name):=(get(var_name)-avg)/std]
  test[,c(var_name):=(get(var_name)-avg)/std]
  
}

summary(train)
summary(test)


# Zadanie 2 -------------------------------------------------------------------------------------------------------

# Na zbiorze danych uczących, na wybranych cechach zbuduj model za pomocą algorytmu k-średnich. Czy skupienia istotnie różnicują dobrych i złych klientów w zbiorze testowym?
# 
# Dla zbioru treningowego stwórz macierz kontyngencji rzeczywistej oraz prognozowanej dobroci klienta przyjmując, że w danym skupieniu wszystkie sprawy są uznawane za dobre jeżeli udział dobrych klientów w skupieniu jest wyższy od udziału dobrych klientów w całym zbiorze danych.
# 
# Wyniki klasyfikacji kmeans przedstaw na wykresie.
# 
# Następnie wykonaj predykcję na zbiorze testowym, wykorzystując wcześniejsze założenia dotyczące zbioru uczącego. Wyniki również przedstaw za pomocą tabeli kontyngencji.
# 
# Przetestuj różne liczby skupień. Jaka ich liczba daje najlepsze wyniki klasyfikacji?


variables <- c("TOA","M_LastPaymentToImportDate")

k <- 5

kmeans_clusters <- kmeans(x=train[, .SD, .SDcols=variables],
  centers=k, iter.max=100, nstart=10)

good_prop <- train[Client=="G", .N]/train[,.N]

train[,Cluster:=kmeans_clusters$cluster]

kmeans_dictionary <- train[,.(NoOfCases=.N, GoodProportion=sum(ifelse(Client=="G", 1, 0)/.N)),by='Cluster']
kmeans_dictionary[,Prediction:=ifelse(GoodProportion>=good_prop, "G", "B")]
setorder(kmeans_dictionary, Cluster)

train <- kmeans_dictionary[train, on="Cluster"][,.SD,.SDcols=c(names(train), "Prediction")]

### Classification matrix
#             | Predicted Good | Predicted Bad  |
# Actual Good |      TP        |       FN       | = P
# Actual Bad  |      FP        |       TN       | = N
#
# sensitivity = true positive rate = TP/P = TP/(TP + FN)
# specificity = true negative rate = TN/N = TN/(FP + TN)

table(Actual=train$Client, Predicted=train$Prediction)
prop.table(table(Actual=train$Client, Predicted=train$Prediction), margin = 1)


plot(train[Client == "G", ][["TOA"]],
  train[Client == "G", ][["M_LastPaymentToImportDate"]],
  pch=1, col=train[Client == "G", ]$Cluster, cex=0.7,
  xlab="", ylab="", main="",
  xlim = 1.05*range(train[["TOA"]]),
  ylim = 1.05*range(train[["M_LastPaymentToImportDate"]]))
points(train[Client == "B", ][["TOA"]],
  train[Client == "B", ][["M_LastPaymentToImportDate"]],
  pch=6, col=train[Client == "B", ]$Cluster, cex=0.7)
points(kmeans_clusters$centers, pch=18, cex=2)

library(ggplot2)
ggplot() +
  geom_point(data=train, aes(x=TOA, y=M_LastPaymentToImportDate, color=as.character(Cluster)), alpha=0.4) +
  geom_point(data=as.data.frame(kmeans_clusters$centers), 
    aes(x=TOA, y=M_LastPaymentToImportDate), color="black", size = 3) +
  facet_wrap(~Client) +
  theme_bw() +
  theme(legend.position = "bottom")


predict.kmeans <- function(model, newdata) {
  y <- apply(newdata, 1, function(r) {
    which.min(colSums((t(model$centers) - r)^2))
  })
  return(y)
}
  
## Predykcja na zbiorze testowym
predictions <- predict(kmeans_clusters, test[,.SD,.SDcols=variables])

test[,Cluster:=predictions]
test <- kmeans_dictionary[test, on="Cluster"][,.SD,.SDcols=c(names(test), "Prediction")]

### Classification matrix
#             | Predicted Good | Predicted Bad  |
# Actual Good |      TP        |       FN       | = P
# Actual Bad  |      FP        |       TN       | = N
#
# sensitivity = true positive rate = TP/P = TP/(TP + FN)
# specificity = true negative rate = TN/N = TN/(FP + TN)

table(Actual=test$Client, Predicted=test$Prediction)
prop.table(table(Actual=test$Client, Predicted=test$Prediction), margin = 1)


# Zadanie 3 -------------------------------------------------------------------------------------------------------

# Stwórz prognozę dobroci klienta z wykorzystaniem algorytmu k-najbliższych sąsiadów `knn` z pakietu `class` 
# (eksperymentuj z różną liczbą najbliższych sąsiadów). Jaka jest optymalna specyfikacja modelu?
# Predykcję wykonaj zarówno dla zbioru uczącego jak i testowego.
  

k <- 5
variables <- c("TOA","M_LastPaymentToImportDate")

## Balanced or not? 

knn_train <- class::knn(
  train=train[, .SD, .SDcols=variables],
  test=train[, .SD, .SDcols=variables],
  cl=train$Client,
  k=k, use.all=FALSE)

table(knn_train, train$Client)
prop.table(table(knn_train, train$Client), margin=1)


knn_test <- class::knn(
  train=train[, .SD, .SDcols=variables],
  test=test[, .SD, .SDcols=variables],
  cl=train$Client,
  k=k, use.all=FALSE)

table(knn_test, test$Client)
prop.table(table(knn_test, test$Client), margin=1)



# Zadanie 4 -------------------------------------------------------------------------------------------------------

# Zbuduj model Map Samoorganizujących Kohonena (pakiet `kohonen`). Wykorzystaj w tym celu zbiory uczący i testowy.
# 
# * Stwórz wykres podsumowujący uczenie się modelu (`type="changes"`), liczby spraw w neuronach (`type="count"`) oraz wykres obrazujący odległości pomiędzy neuronami (`type="dist.neighbours"`).
# *	Stwórz wykresy rozkładu cech na mapie (`„heatmaps”`) – czy zauważasz potencjalne skupienia?
# *	Przeprowadź analizę skupień za pomocą metody k-średnich na neuronach mapy i zaproponuj optymalną liczbę skupień.
# *	Wykonaj hierarchiczną analizę skupień na neuronach z obraną liczbą skupień i przedstaw skupienia na mapie. 
# *	Sprawdź zróżnicowanie dobroci klienta w skupieniach i porównaj z wynikami skupień z zadania 2.
# 
# Meteriał pomocniczy do wykonania zadania: https://www.r-bloggers.com/2014/02/self-organising-maps-for-customer-segmentation-using-r/

library(kohonen)

variables <- c("TOA","M_LastPaymentToImportDate")

train_matrix <- as.matrix(train[,.SD,.SDcols=variables])

som_grid <- somgrid(xdim = 20, ydim=20, topo="hexagonal")

som_model <- som(
  X = train_matrix, 
  grid=som_grid, 
  rlen=500, # odpowiednik liczby epok z tradycyjnych sieci neuronowych
  alpha=c(0.05,0.01), #stop szybkosci uczenia sie - learning rate
  keep.data = TRUE )

#Training progress for SOM
plot(som_model, type="changes")

#Node count plot
plot(som_model, type="count", main="Node Counts")

# U-matrix visualisation
plot(som_model, type="dist.neighbours", main = "SOM neighbour distances")


# Kohonen Heatmap creation
coolBlueHotRed <- function(n, alpha = 1) {rainbow(n, end=4/6, alpha=alpha)[n:1]}
pretty_palette <- c("#1f77b4","#ff7f0e","#2ca02c", "#d62728","#9467bd","#8c564b",
  "#e377c2","#27d65b","#d627d6","#f0fa2f")

par(mfrow=c(2,1))
#TOA
plot(som_model, type = "property", property = getCodes(som_model)[,1], main=colnames(getCodes(som_model))[1], 
  palette.name=coolBlueHotRed)
#M_LastPaymentToImportDate
plot(som_model, type = "property", property = getCodes(som_model)[,2], main=colnames(getCodes(som_model))[2], 
  palette.name=coolBlueHotRed)
par(mfrow=c(1,1))


mydata <- som_model$codes[[1]]

wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var)) 
for (i in 2:15) { # i=399
  wss[i] <- sum(kmeans(mydata, centers=i)$withinss)
}
plot(wss)


## use hierarchical clustering to cluster the codebook vectors
som_cluster <- cutree(hclust(dist(som_model$codes[[1]])), 6)

# plot these results:
plot(som_model, type="mapping", bgcol = pretty_palette[som_cluster], main = "Clusters", cex = 0.001) 
add.cluster.boundaries(som_model, som_cluster)


# Prognozowane klastry vs użyte cechy
par(mfrow=c(2,2))
#TOA
plot(som_model, type = "property", property = getCodes(som_model)[,1], 
  main=colnames(getCodes(som_model))[1], palette.name=coolBlueHotRed)
#M_LastPaymentToImportDate
plot(som_model, type = "property", property = getCodes(som_model)[,2], 
  main=colnames(getCodes(som_model))[2], palette.name=coolBlueHotRed)

plot(som_model, type="mapping", bgcol = pretty_palette[som_cluster], main = "Clusters", cex = 0.001) 
add.cluster.boundaries(som_model, som_cluster)

par(mfrow=c(1,1))


## Prognoza dobroci klienta dla zbioru testowego

### utworzenie słownika na podstawie modelu na zbiorze uczącym
clusters <- data.table(
  Neuron = as.integer( substr(names(som_cluster), 2, length(names(som_cluster))-1) ), 
  Cluster = as.integer(som_cluster*1))

som_train_forecast <- data.table(Neuron=som_model$unit.classif, Client=train$Client)

setkey(clusters, "Neuron")  
setkey(som_train_forecast, "Neuron")
som_dictionary <- som_train_forecast[clusters, nomatch=0][, 
  .(.N, GoodClientProportion = sum(ifelse(Client=="B", 0, 1)*1.0)*1.0/.N) , by = Cluster]
som_dictionary[,ClassPrediction:=ifelse(GoodClientProportion>good_prop,"G", "B")]

### Prognoza dla zbioru uczącego
test_matrix <- as.matrix(test[,.SD,.SDcols=variables])

som_forecasts <- predict(som_model, newdata = test_matrix)

som_test_forecast <- data.table(Forecast = som_forecasts$unit.classif, ClientTrue = test$Client)
som_test_forecast <- som_test_forecast[clusters, on=c("Forecast"="Neuron"), nomatch=0L]
som_test_forecast <- som_test_forecast[som_dictionary, on = c("Cluster"="Cluster")]

table(som_test_forecast$ClientTrue, som_test_forecast$ClassPrediction)
prop.table(table(som_test_forecast$ClientTrue, som_test_forecast$ClassPrediction), margin = 1)



# Przypomnienie i porównanie prognoz ze wszystkich trzech modeli --------------------------------------------------

# kmeans:
table(Actual=test$Client, Predicted=test$Prediction)
prop.table(table(Actual=test$Client, Predicted=test$Prediction), margin = 1)
# knn
table(Actual=test$Client, Predicted=knn_test)
prop.table(table(Actual=test$Client, Predicted=knn_test), margin=1)
# kohonen
table(Actual=som_test_forecast$ClientTrue, Predicted=som_test_forecast$ClassPrediction)
prop.table(table(Actual=som_test_forecast$ClientTrue, Predicted=som_test_forecast$ClassPrediction), margin = 1)


