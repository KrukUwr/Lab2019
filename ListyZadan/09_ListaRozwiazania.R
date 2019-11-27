
library(data.table)
library(car)
library(MASS)
load("KrukUWr2018.Rdata")
set.seed(1234)

### Zadanie 1 ####

#  Przygotuj rankę danych `cases_loanamount` bazując na tabeli `cases` tylko z przypadkami kredytów gotówkowych. 

cases_loanamount <- cases[Product == "Cash loan",]

# * Sprawdź miary pozycyjne wszystkich cech w utworzonej ramce.

summary(cases_loanamount)

# * Sprawdź liczność braków danych dla wszystkich cech.

sapply(cases_loanamount, function(x){sum(is.na(x))})

#### Zadanie 2 ####

# Interesującą nas cechą będzie `LoanAmount`, której wartość będziemy modelować w celu zastpąpienia `NA's`.

# * Metodą losowania z rozkładu zastąp braki danych w cesze `Land`.

 dist <- prop.table(table(cases_loanamount$Land))
 sampled <- sample(x = unique(cases_loanamount[!is.na(Land), Land]),
                   size = sum(is.na(cases_loanamount[,Land])),
                   prob = dist,
                   replace = TRUE)
 
 cases_loanamount[is.na(Land) , Land := sampled]
 
# * Metodą ekspercką zastąp braki danych w cechach: `Other`,`Gender`, `MeanSalary` i `GDPPerCapita`.
 
 tmp <- cases[!is.na(MeanSalary), .(MS = min(MeanSalary), GDP= min(GDPPerCapita)), by = Land]
 cases_loanamount <- cases_loanamount[tmp, on = "Land"]
 cases_loanamount[, ':='(MeanSalary = MS, GDPPerCapita = GDP)]
 cases_loanamount[,':='(MS = NULL, GDP = NULL)]
 
 cases_loanamount[is.na(Other), Other:=TOA-Principal-Interest]
 cases_loanamount[is.na(Gender), Gender:="Company"]
 
# * Dokonaj dyskretyzacji cechy DPD a wartościom `NA` przypisz poziom `brak danych`, cechę zapisz jako `D_DPD`

 cases_loanamount[, D_DPD := cut(DPD, breaks = c(-Inf, 180, 360, 720, Inf))]
 
cases_loanamount[is.na(D_DPD), "D_DPD"] <- "brak danych"
cases_loanamount[,DPD:=NULL]
 
 #### Zadanie 3 ####
 
 # Na podstawie `cases_loanamount` przygotuj ramki danych:

 #   * `cases_loanamount_nas`, która zawiera wszystkie przypadki brakujacych wartości zmiennej `LoanAmount`.
 
cases_loanamount_nas <- cases_loanamount[is.na(LoanAmount),]
cases_loanamount_wonas <- cases_loanamount[!is.na(LoanAmount),]
cases_loanamount_wonas <- cases_loanamount_wonas[sample(1:nrow(cases_loanamount_wonas), 10000),]
 
ix_trn <- sample(1:nrow(cases_loanamount_wonas), nrow(cases_loanamount_wonas)*0.7)
ix_tst <- c(1:nrow(cases_loanamount_wonas))[-ix_trn]

#### Zadanie 4 ####

# Zbadaj rozkład  `cases_loanamount_wonas$LoanAmount`. Jeżeli jest taka potrzeba zaproponuj transformację. 
 
summary(cases_loanamount_wonas$LoanAmount)
boxplot(cases_loanamount_wonas$LoanAmount)
plot(density(cases_loanamount_wonas$LoanAmount))

cases_loanamount_wonas[LoanAmount==0, LoanAmount:=1]

plot(density(log(cases_loanamount_wonas$LoanAmount)))

cases_loanamount_wonas[, LoanAmount_log := log(LoanAmount)]

boxplot(cases_loanamount_wonas$LoanAmount_log)

#### Zadanie 5 ####

# Zbuduj model regresji  liniowej `m1` gdzie zmienną modelowaną jest `LoanAmount` a zmiennymi objaśniającymi :
# `TOA`, `Principal`, `Interest`, `Other`, `GDPPerCapita`, `MeanSalry`, `D_DPD`, `Age`, `Gender` 
 
fmla <- as.formula(LoanAmount_log~  TOA + Other + Interest + Principal + D_DPD + Age + Gender + GDPPerCapita)

m1 <- lm(fmla, data = cases_loanamount_wonas, subset = ix_trn)
m1
summary(m1) 
 
#### Zadanie 6 #####

plot(density(m1$residuals))
plot(m1)

shapiro.test(sample(m1$residuals, 5000))


# p-value < 0.05 - odrzucamy hipotezę o normlaności rozkładu

#### Zadanie 7 #####

# Korzystając ze zbioru testowego dokonaj predykcji (wyniki zapisz w obiekcie `m1_pred`), 
# a następinie oblicz bez używania gotowych funkcji: RSS, RSE, TSS i R^2.

RSS_trn <- sum(m1$residuals^2)

p <- length(m1$coefficients)-1
n <- nrow(cases_loanamount_wonas[-ix_trn])

RSE_trn <- sqrt(RSS_trn/(n - p - 1))
TSS_trn <- sum((cases_loanamount_wonas[ix_trn]$LoanAmount_log - mean(cases_loanamount_wonas[ix_trn]$LoanAmount_log))^2)
(R2_trn <- 1 - RSS_trn/TSS_trn)


m1_pred_tst <- predict.lm(m1, newdata = cases_loanamount_wonas[-ix_trn,])
rsids_tst <- cases_loanamount_wonas[-ix_trn]$LoanAmount_log - m1_pred_tst

RSS_tst <- sum(rsids_tst^2)
RSE_tst <- sqrt(RSS_tst/(n - p - 1))

TSS_tst <- sum((cases_loanamount_wonas[-ix_trn]$LoanAmount_log - mean(cases_loanamount_wonas[-ix_trn]$LoanAmount_log))^2)

(R2 <- 1 - RSS_tst/TSS_tst) #zgodnie z oczekiwaniami R^2 gorszy na tesotwym

#### Zadanie 8 #####

# Dokonaj oceny jakości predykcji za pomocą znanych Ci miar


# Zmierzymy za pomoca RMSE (Root Mean Square Error) i MAPE (Mean Absolute Percentage Error)

m1_pred_tst <- exp(m1_pred_tst)
rsids_tst <- cases_loanamount_wonas[-ix_trn]$LoanAmount - m1_pred_tst
RMSE <- sqrt(mean(rsids_tst^2))
APE <- abs(rsids_tst)/cases_loanamount_wonas[-ix_trn]$LoanAmount
summary(APE)
quantile(APE, seq(0, 1, .05))
MAPE <-mean(APE)


### Zadanie 9 ####

# Sprawdź jak obserwacje odstające wpływają na współczynniki modelu oraz na ocenę jakości
# za pomocą wybranych przez Ciebie miar w zadaniu 9).

library(broom)
library(Metrics)

pretty <- data.table(augment(m1))

pretty[order(-.hat)][1:50,] # 20 obserwacji z najwyzszym odcyleniem LoanAmount od sredniej LoanAmount
outliers <- pretty[order(-.cooksd)][1:20, .rownames] #20 obserwacji z najwiekszym dystansem Cooka

outliers <- as.integer(outliers)

new_ix_trn <- setdiff(ix_trn, outliers)

m2 <- lm(fmla, data = cases_loanamount_wonas, subset = new_ix_trn)

summary(m2)
plot(m2)

actual_test <- exp(cases_loanamount_wonas[-ix_trn]$LoanAmount_log)
m2_pred_test <- exp(predict(m2, cases_loanamount_wonas[-ix_trn,]))

rmse(actual_test, m2_pred_test)
APE_m2 <- ape(actual_test, m2_pred_test)
summary(APE_m2)
MAPE_2 <- mape(exp(cases_loanamount_wonas[-ix_trn]$LoanAmount_log), exp(m2_pred_test))

# a jaki byl by blad gdybysmy zastapili srednia lub mediana?

LoanAmount_mean <- mean(cases_loanamount_wonas$LoanAmount)
LoanAmount_median <- median(cases_loanamount_wonas$LoanAmount)


rmse(actual_test, LoanAmount_mean)
APE_mean <- ape(actual_test, LoanAmount_mean)
summary(APE_mean)
MAPE_mean <- mape(actual_test, LoanAmount_mean)

rmse(actual_test, LoanAmount_median)
APE_median <- ape(actual_test, LoanAmount_median)
summary(APE_median)
MAPE_median <- mape(actual_test, LoanAmount_median)


#### Zadanie 10 #####

# Czy w modelu `m1` istnieją cechy, które charakteryzują się współliniowością?
# Jeżeli tak jak zmieni się model i ocena jakości po ich wyeliminowaniu.

cor_matrix <- cor(cases_loanamount_wonas[, .(TOA ,
                                             Principal,
                                             Interest,
                                             Other,
                                             Age,
                                             GDPPerCapita)])
cor_matrix

vif(m1)

fmla2 <- as.formula(LoanAmount_log~  TOA + D_DPD + Age + Gender + GDPPerCapita)

m3 <- lm(fmla2, data = cases_loanamount_wonas, subset = ix_trn)
summary(m3)
m3_pred_tst <- exp(predict(m2, newdata = cases_loanamount_wonas[-ix_trn,]))

vif(m3)

#APE - "na sucho"
#APE_m2 - bez outlierów
#APE_mean
#APE_median
#APE_m3 - bez skorelowanych

rmse(actual_test, m3_pred_tst)
APE_3 <- ape(actual_test, m3_pred_tst)

summary(APE)
summary(APE_m2)
summary(APE_mean)
summary(APE_median)
summary(APE)
summary(APE_3)

### Zadanie 11 ####

# Zaproponuj własny model regresji liniowej korzystając z wiedzy zdobytej na wykładzie
# (transformacje zmiennych, interakcje między cechami, dyskretyzacja cech ciągłych, itp...)
#, który będzie cechować się lepszymi parametrami oceny jakości predyckji od wyestymowanego na ćwiczeniach.


## Liczę na Państwa zaangażowanie i kreatywność :) Do dzieła !!! #############



