
library(data.table)
library(car)
library(MASS)
load("KrukUWr2019.Rdata")
set.seed(1)


### Zadanie 1 ####       

#  Przygotuj ramkę danych `cases_loanamount` bazując na tabeli `cases` tylko z przypadkami kredytów gotówkowych. 

cases_loanamount <- cases[Product == "Cash loan",]


# * Sprawdź miary pozycyjne wszystkich cech w utworzonej ramce.

summary(cases_loanamount)


# * Sprawdź liczność braków danych dla wszystkich cech.


sapply(cases_loanamount, function(x){sum(is.na(x))})





#### Zadanie 2 ####

# Interesującą nas cechą będzie `LoanAmount`, której wartość będziemy modelować w celu zastąpienia `NA's`.

# * Metodą losowania z rozkładu zastąp braki danych w cesze `Land`.

 dist <- prop.table(table(cases_loanamount$Land))
 sampled <- sample(x = sort(unique(cases_loanamount[!is.na(Land), Land])),
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


# p-value < 0.05 - odrzucamy hipotezę o normalności rozkładu




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

(R2 <- 1 - RSS_tst/TSS_tst) #zgodnie z oczekiwaniami R^2 gorszy na testowym




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








