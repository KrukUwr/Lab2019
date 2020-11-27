library(data.table)

load("./data/KrukUWr2020.RData")

# Zadanie 1 -------------------------------------------------------------------------------------------------------
# Przeanalizuj wartości NA na zmiennej LoanAmount. Czy wszystkie wartości NA w przypadku tej zmiennej oznaczają brak danych?

summary(cases$LoanAmount)

# Sprawdźmy jak wygląda liczba braków danych w LoanAmount według produktu
cases[,.N, by=.(Product, is.na(LoanAmount))][order(Product)]
# Większość braków danych dotyczy kart kredytowych

# Wartość LoanAmount jest powiązana z kolumną Principal
# Principal jest to część pożyczonej kwoty, która pozostaje do spłaty
# Porównajmy rozkłady obu cech
cases[!is.na(LoanAmount) & Product=="Credit card", .(LoanAmount, Principal)]

summary(cases[!is.na(LoanAmount) & Product=="Credit card", .(LoanAmount, Principal)])

# Wizulizacja rozkładu LoanAmount i Principal
plot(density(cases[!is.na(LoanAmount) & Product=="Credit card"][["LoanAmount"]]), 
  main="", xlab="Value", xlim=c(0,15000))
lines(density(cases[!is.na(LoanAmount) & Product=="Credit card"][["Principal"]]), 
  col="red")
legend("topright", legend = c("LoanAmount", "Principal"), col = c("black", "red"), 
  lty = 1)
title("LoanAmount & Principal distributions")
## Rozkład Principal jest lekko przesunięty w lewo w porównaniu z LoanAmount


## Sprawdźmy jaki jest stosunek LoanAmount i Principal spraw, dla których posiadamy wartości
## w różnych punktach odcięcia
probs <- 1:10/10
quantile(cases[!is.na(LoanAmount) & Product=="Credit card",]$LoanAmount, probs = probs)/
  quantile(cases[!is.na(LoanAmount) & Product=="Credit card", ]$Principal, probs = probs)

## Sprawdźmy jaką średnio część Principal stanowi LoanAmount - wykluczamy po 10% wartości skranych z obu stron
multiplier <- mean(cases[!is.na(LoanAmount) & Product=="Credit card" & Principal!=0,(Prop=LoanAmount/Principal)], trim=0.1)

# Podstawiamy nową wartość dla kart kredytowych
cases[is.na(LoanAmount) & Product=="Credit card",LoanAmount:=Principal*multiplier]

# Zadanie 2 -------------------------------------------------------------------------------------------------------
# Podaj biznesowy (ekspercki) sposób uzupełnienia wartości NA dla zmiennej Other.

## Z wykładu 1:
## TOA = Principal + Interst + Other
## Other = TOA - Principal - Interest

cases[is.na(Other), Other := TOA - Principal - Interest]

## srawdzenie
anyNA(cases[["Other"]])


# Zadanie 3 -------------------------------------------------------------------------------------------------------
# Uzupełnij braki danych na zmiennej Land wykorzystując rozkład empiryczny. 
# Jak wykorzystać nowo pozyskane informacje w uzupełnieniu zmiennych GDPPerCapita i MeanSalary?

## Liczba NA w zmiennej Land
(land_na_num <- cases[is.na(Land), .N])

## Rozkład empiryczny zmiennej land
(land_distr <- cases[!is.na(Land), .(N=.N), by=Land][order(Land)])
## Wykres słupkowy rozkadu:
barplot(data = land_distr, N~Land)

land_population <- cases[!is.na(Land)][["Land"]]
cases[is.na(Land), Land:=sample(x = land_population, size = land_na_num, replace = TRUE)]

summary(cases$Land)

## Kolumny GDPPerCapita oraz MeanSalary zawierają informacje opisujące jednostkę terytorialną z kolumny Land
## Każdemu Landowi możemy przypiasć pojedynczą wartość GDPPerCapita i MeanSalary
land_values <- cases[!is.na(GDPPerCapita) & !is.na(MeanSalary),
  .(Land, GDPPerCapita, MeanSalary)]

## Zostawiamy tylko wartości unikatowe
land_values <- land_values[!duplicated(land_values)][order(Land)]

## Podstawiamy powyższe wartości za NA dla wskazanych Landów
summary(cases)

cases <- cases[land_values, on="Land"]
cases[is.na(GDPPerCapita),GDPPerCapita:=i.GDPPerCapita]
cases[is.na(MeanSalary),MeanSalary:=i.MeanSalary]
cases[,`:=`(GDPPerCapita=NULL, MeanSalary=NULL)]

summary(cases)

# Zadanie 4 -------------------------------------------------------------------------------------------------------
# Zweryfikuj dokładność uzupełniania braków danych dla zmiennej TOA poprzez modele lasów losowych i najbliższych sąsiadów 
# (Wsk. Braki danych w TOA należy zasymulować).

library(randomForest)
library(Metrics)

set.seed(2)
is_trn <- sample(c(TRUE, FALSE), cases[,.N], replace=TRUE)

cases_trn <- copy(cases[is_trn, .(TOA, Principal, Interest, Other)])
cases_tst <- copy(cases[!is_trn, .(TOA, Principal, Interest, Other)])

rf_toa <- randomForest(data=cases_trn, TOA~., nodesize=1000, ntrees=2000)
rf_toa_pred <- predict(rf_toa, newdata=cases_tst)

Metrics::mae(actual = cases_tst$TOA, predicted = rf_toa_pred)
plot(density(cases_tst$TOA))
lines(density(rf_toa_pred), col="red")


## kNN
## Standaryzacja danych do modelu

independent_variables <- c("Principal", "Interest", "Other")
trn_avgs <- sapply(cases_trn[,.SD, .SDcols=independent_variables], mean)
trn_stds <- sapply(cases_trn[,.SD, .SDcols=independent_variables], sd)

cases_trn_std <- copy(cases_trn)
cases_tst_std <- copy(cases_tst)

for(col_name in independent_variables) {
  
  avg <- trn_avgs[col_name]
  std <- trn_stds[col_name]
  
  cases_trn_std[,c(col_name):= (get(col_name)-avg)/std ]
  cases_tst_std[,c(col_name):= (get(col_name)-avg)/std ]
  
}

library(FNN) # funkcja knn.reg

y_trn <- cases_trn_std$TOA
y_tst <- cases_tst_std$TOA
cases_trn_std[,TOA:=NULL]
cases_tst_std[,TOA:=NULL]

knn_toa_pred <- knn.reg(train = cases_trn_std, test = cases_tst_std, y = y_trn, k=5)

Metrics::mae(actual = y_tst, predicted = knn_toa_pred$pred)
plot(density(y_tst))
lines(density(knn_toa_pred$pred), col="red")

# Zadanie 5 -------------------------------------------------------------------------------------------------------
# Zweryfikuj różnice pomiędzy wartościami średnich oraz przeciętnych dla rozkładów poszczególnych zmiennych opisujących sprawy. 
# Oceń jaki wpływ na różnice mają wartości skrajne.

summary(cases)

## Rozważmy LoanAmount, ponieważ średnia jest o wiele większa niż mediana
plot(density(cases$LoanAmount, na.rm=TRUE))
boxplot(cases$LoanAmount~cases$Product)

cases[,.(
  Avg = mean(LoanAmount, na.rm=TRUE),
  Avg01 = mean(LoanAmount, na.rm=TRUE, trim=0.01),
  Avg05 = mean(LoanAmount, na.rm=TRUE, trim=0.05),
  Avg10 = mean(LoanAmount, na.rm=TRUE, trim=0.1),
  Median = median(LoanAmount, na.rm=TRUE))]


# Zadanie 6 -------------------------------------------------------------------------------------------------------
# Posługując się wykresami typu boxplot zidentyfikuj wartości odstające (jaka reguła jest przyjęta w funkcji boxplot) 
# na poszczególnych zmiennych opisujących sprawy. Usuń przypadki z wartościami odstającymi, a następnie wykonaj wykres ponownie.
# Czy nadal możesz zaobserwować wartości odstające?

variables <- setdiff(names(cases), 
  c("CaseId", 
    "Product", "Gender", "Land", # cechy kategoryczne
    "ExternalAgency", "Bailiff", "ClosedExecution")) # cechy binarne (2 kategorie)

## Pojedynczy boxplot dla wybranej cechy
boxplot(cases[["D_ContractDateToImportDate"]]) 
bp <- boxplot(cases[["D_ContractDateToImportDate"]]) 
bp$stats


## Reguła: 1.5 roztępu międzykwartylowego od 1 i 3 kwartyla
IQR(cases[["D_ContractDateToImportDate"]] , na.rm = TRUE)
quantile(cases[["D_ContractDateToImportDate"]], na.rm=TRUE)
2137-1112 # Q75% - Q25%

lowerWhisker <- function(x, na.rm=FALSE) {
  
  whisker_value <- quantile(x, probs = 0.25, na.rm=na.rm) - IQR(x, na.rm=na.rm) * 1.5
  min_x <- min(x, na.rm=na.rm)
  lower_whisker <- max(whisker_value, min_x)
  return(lower_whisker)
  
}

upperWhisker <- function(x, na.rm=FALSE) { # x=cases[["D_ContractDateToImportDate"]]
  
  whisker_value <- quantile(x, probs = 0.75, na.rm=na.rm) + IQR(x, na.rm=na.rm) * 1.5
  max_x <- max(x, na.rm=na.rm)
  upper_whisker <- min(whisker_value, max_x)
  return(upper_whisker)
  
}

(lower_whisker <- lowerWhisker(cases[["D_ContractDateToImportDate"]], na.rm=TRUE))
(upper_whisker <- upperWhisker(cases[["D_ContractDateToImportDate"]], na.rm=TRUE))
bp$stats
## Różnice wynikają z tego że funkcja boxplot wskazuje skrajne oberwacje w "wąsach"
## natomiast nasze funkcje obliczają wartości teoretyczne

variable_cut <- cases[D_ContractDateToImportDate>lower_whisker & 
    D_ContractDateToImportDate<upper_whisker][["D_ContractDateToImportDate"]]

boxplot(variable_cut) 
## boxplot dalej wskazuje wartości odstające, ponieważ wraz z usunięciem obserwacji zmieniły się też parametry rozkładu


## Uwaga! Funkcja jako ciekawostka, aby sprawdzić jak dużo obserwacji należałoby usunąć, aby nie było outlierów w danej zmiennej.
## Nie należy z niej korzystać w ramach projektu do usuwania obserwacji odstających.
reduceOutliers <- function(x, na.rm=TRUE) {
  
  if(na.rm) {
    x <- na.omit(x)
  }
  
  lower_whisker <- lowerWhisker(x)
  upper_whisker <- upperWhisker(x)
  
  if(!any(x<lower_whisker) && !any(x>upper_whisker)) {
    return(x)
  }
  
  rem_obs_num <- sum(x<lower_whisker | x>upper_whisker)
  
  cat(paste0("Remove ", rem_obs_num, " observations.\n"))
  
  x <- x[x>lower_whisker&x<upper_whisker]
  
  y <- reduceOutliers(x, na.rm = FALSE)
  
  return(y)
}

toa_reduced <- reduceOutliers(cases$TOA)
length(toa_reduced)
cases[,.N]
# Jaki % spraw usunięto
1 - length(toa_reduced)/cases[,.N]

dcont_reduced <- reduceOutliers(cases$D_ContractDateToImportDate)
length(dcont_reduced)
cases[,.N]
1 - length(dcont_reduced)/cases[,.N]
