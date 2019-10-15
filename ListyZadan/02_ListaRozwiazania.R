

## Wymagane pakiety: 

library(data.table)
library(corrgram)
library(corrplot)

## Opcjonalne pakiety:

library(tidyverse)       # wizualzacja, manipulacja, wczytywanie i inne..
library(xray)            # EDA
library(funModeling)     # EDA

load("KrukUWr2018.RData")

### Zadanie 1 ######################################################################################

# Wyznacz skuteczność (suma wpłat przez wartość zadłułenia) w różnych horyzontach
# czasu (np. 3M, 6M, 12M) i wyznacz jej statystyki opisowe (kwantyle) w podziale na:
#   
#   - Gender
# - ExtrernalAgency
# - Bailiff
# - ClosedExecution
# - M_LastPaymentToImportDate (zaproponuj podział wg tej zmiennej)
# - DPD (zaproponuj podział wg tej zmiennej)
# - Age (zaproponuj podział wg tej zmiennej)
# - TOA (zaproponuj podział wg tej zmiennej)


summary(events)
summary(cases) # nie pokaże libczy NA w przypadku typu character 

## czy w ramce danych znajuje się jakiekolwiek wystąpienie NA?
anyNA(cases)

# mozna je zwrocic m. in. w taki sposob

sapply(cases, function(x){sum(is.na(x))})


# mozna też uzyc dostepnych pakietow: xray czy FunModeling

xray::anomalies(cases,0)
funModeling::df_status(cases)


## zastepujemy NA <- 0 , chetni moga poszukac we wskazanych pakietach gotowych funkcji

events[is.na(PaymentAmount), PaymentAmount := 0] 
events[is.na(events)] <- 0 # mozna tez za jednym zamachem pod warunkiem, że ma to sens merytoryczny wynikający ze znaczenia NA's
anyNA(events) # nie zadnego NA w ramce danych


# wybrane historgramy

hist(cases$M_LastPaymentToImportDate)
hist(cases$DPD)
hist(cases$Age)
hist(cases$TOA)

boxplot(cases$TOA)

## Podzial wg wybranych zmiennych ciaglych. konieczna dyskretyzacja

## dyskretyzacja manualna

cases[, D_DPD := cut(DPD, breaks = c(180, 360, 720, 1080))]

## przedzialy o równej gęstości

cases[, D_TOA := cut(TOA, breaks = quantile(TOA, seq(0, 1, 0.2)), include.lowest = TRUE)]
cases[, D_M_LastPaymentToImportDate := cut(TOA, breaks = quantile(TOA, seq(0, 1, 0.2)), include.lowest = TRUE)]

## przedzialy o równej szerokości

cases[, D_Age := cut(Age, breaks = 10)] # wiek zakodowany jako -1 i tak wpadnie w osobny band
cases[, D_M_LastPaymentToImportDate := cut(M_LastPaymentToImportDate, breaks = 10)]

### Statystyki opisowe wybranych kolumn

### 3M

tmp_3m <- cases[events][Month <= 3, .(SR3M = sum(PaymentAmount)/max(TOA)), by = CaseId][cases]

tmp_3m[, .(N = .N,
           min = min(SR3M),
           q05 = quantile(SR3M, 0.05),
           q1 = quantile(SR3M, 0.25),
           med = median(SR3M, 0.5),
           mean = mean(SR3M),
           q3 = quantile(SR3M, 0.75),
           q95 = quantile(SR3M, 0.95),
           max = max(SR3M))
       , by = .(D_Age)][order(D_Age)]

tmp_3m[, .(N = .N,
           q05 = quantile(SR3M, 0.05),
           min = min(SR3M),
           q1 = quantile(SR3M, 0.25),
           med = median(SR3M, 0.5),
           mean = mean(SR3M),
           q3 = quantile(SR3M, 0.75),
           q95 = quantile(SR3M, 0.95),
           max = max(SR3M))
       , by = .(Gender)]

tmp_3m[, .(N = .N,
           min = min(SR3M),
           q05 = quantile(SR3M, 0.05),
           q1 = quantile(SR3M, 0.25),
           med = median(SR3M, 0.5),
           mean = mean(SR3M),
           q3 = quantile(SR3M, 0.75),
           q95 = quantile(SR3M, 0.95),
           max = max(SR3M))
       , by = .(D_TOA)][order(D_TOA)]

# Analogicznie postępujemy dla innych badanych okresów (konieczne stworzenie obiektow tmp_6m i tmp_12m do dalszych zadań)

# Wyniki zaprezentuj również na wykresie. Które zmienne najlepiej różnicują 
# skuteczność (co rozumiesz poprzez "różnicują"")?

# przykladowy boxplot dla cechy SR12M w celu sprawdzenia jak różnicuje D_TOA

ggplot(data =tmp_3m, aes(y = SR3M, x = D_TOA)) + geom_boxplot()

### przez obserwacje odstające cieżko wnioskować na podstawie powyższej wizualizacji

### po odfiltrowaniu obserwacji odstających różnicowanie jest już widoczne:

ggplot(data = tmp_3m[SR3M<1 & SR3M>0,], aes(D_TOA, SR3M)) +geom_boxplot()

#### analogicznie można sprawdzić dla innych cech czy okresów obsługi

# Wyznacz korelacje pomiędzy zmienną skuteczności a wskazanymi zmiennymi.

corSR12 <- cor(tmp_12m[, .SD, .SDcols = c("Age", "TOA", "DPD", "M_LastPaymentToImportDate")] , tmp_12m$SR12M,
               use = "pairwise.complete.obs")

corSR6 <- cor(tmp_6m[, .SD, .SDcols = c("Age", "TOA", "DPD", "M_LastPaymentToImportDate")], tmp_6m$SR6M,
              use = "pairwise.complete.obs")

corSR3 <- cor(tmp_3m[, .SD, .SDcols = c("Age", "TOA", "DPD", "M_LastPaymentToImportDate")], tmp_3m$SR3M,
              use = "pairwise.complete.obs")

# Czy w zbiorze danych istnieją inne zmienne (od wskazanych) z dużą wartością
# korelacji?

corAll <- cor(tmp_12m[, .SD, .SDcols = setdiff(names(tmp_12m),
                                               c("CaseId",
                                                 "D_Age",
                                                 "D_TOA", 
                                                 "D_DPD",
                                                 "D_M_LastPaymentToImportDate",
                                                 "Product",
                                                 "Bailiff",
                                                 "ClosedExecution",
                                                 "Land",
                                                 "Gender",
                                                 "SR12M"
                                               ))],
              tmp_12m$SR12M,
              use = "pairwise.complete.obs")

### Zadanie 2 ######################################################################################

# Wyznacz dotarcie per sprawa (czy był kontakt w sprawie telefoniczny, lub bezpośredni)
# w róznych horyzontach czasu i wyznacz jej statystyki opisowe (kwantyle) w podziale na wybrane zmienne
# (np. zmienne z zadania 1, lub zmienne, które różnicują dotarcie).


## przeksztalcenie Na na 0 w tabeli event

tmp_3m_reach <- cases[events][Month <= 3, .(reach_3m = ifelse(sum(NumberOfCallsWithClient + NumberOfVisitsWithClient)>0, 1, 0)), by = CaseId][cases]

## analigicznie dla 6M i 12 M

tmp_3m_reach[, .(N = .N,
                 mean = mean(reach_3m))
             , by = .(D_Age)][order(D_Age)]

tmp_3m_reach[, .(N = .N,
                 mean = mean(reach_3m))
             , by = .(Bailiff)]

tmp_3m_reach[,.(N = .N,
                mean = mean(reach_3m))
             , by = .(D_TOA)][order(D_TOA)]

tmp_3m_reach[,.(N= .N,
                mean = mean(reach_3m)),
             by = .(D_DPD)][order(D_DPD)]


### Zadanie 3 ######################################################################################

# Czy istnieje zaleśność pomiędzy ilością wykonywanych telefonów, wizyt, lub 
# wysyłanych listów, a zmiennymi opisującymi sprawę (zmienne w cases).

# zakladamy 12 miesieczny okres obserwowania

tmp_cases_events <- cases[events][, .(AllCalls = sum(NumberOfCalls),
                                      AllLetters = sum(NumberOfLettersSent),
                                      AllVisits = sum(NumberOfVisits)), by = CaseId][cases]

# przykladowy scatterplot dla dwoch zmiennych

plot(tmp_cases_events$AllCalls, tmp_cases_events$M_LastPaymentToImportDate, pch = ".")

# wielokrotny scatterplot

pairs(AllVisits ~ TOA + DPD + Age + LastPaymentAmount + D_ContractDateToImportDate,data = tmp_cases_events, pch =".")

corMatrix <- cor(tmp_cases_events[, .SD, .SDcols = setdiff(names(tmp_cases_events),
                                                           c("CaseId",
                                                             "D_Age",
                                                             "D_TOA", 
                                                             "D_DPD",
                                                             "D_M_LastPaymentToImportDate",
                                                             "Product",
                                                             "Bailiff",
                                                             "ClosedExecution",
                                                             "Land",
                                                             "Gender"))],
                 use = "pairwise.complete.obs")

corrgram(corMatrix)
corrplot::corrplot(corMatrix, method = "circle")


#### Zadanie 4 #####################################################################################

# Dla wybranych zmiennych stwórz zmienne: 
#   
# - standaryzowane (o średniej zero i warinacji 1) 
# - normalizowane (przekształcenie wartości zmiennej na odcinek [0, 1]). 
# - logarytmowane
# - pierwiatskowane 
# 
# # Wyznacz korelację dla zmiennych orginalnych oraz korelację ich przekształconych 
# # odpowiedników. Co można zauważyć?
  


tmp_cor <- cases[, .(LoanAmount, TOA, Principal, Interest, GDPPerCapita, MeanSalary)]
tmp_cor_stand <- tmp_cor[, lapply(.SD, scale)]

normalize <- function(x){(x - min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T))}
tmp_cor_norm <- tmp_cor[, lapply(.SD, normalize)]


tmp_cor_log <- tmp_cor[, lapply(.SD, log10)]
tmp_cor_sqrt <- tmp_cor[, lapply(.SD, sqrt)]



corMatrix <- cor(tmp_cor, use = "pairwise.complete.obs")
corMatrix_stand <- cor(tmp_cor_stand, use = "pairwise.complete.obs")
corMatrix_norm <- cor(tmp_cor_norm, use = "pairwise.complete.obs")
corMatrix_log <- cor(tmp_cor_log, use = "pairwise.complete.obs")
corMatrix_sqrt <- cor(tmp_cor_sqrt, use = "pairwise.complete.obs")


### Zadanie 6. #####################################################################################

vars <- setdiff(names(cases),
                c("CaseId", "Product", "Gender", "Land", "TOAband"))

corrSignificance <- data.table()

for (i in 1:(length(vars) - 1)) {
  for (j in (i+1):length(vars)) {
    test <- cor.test(cases[[vars[i]]], cases[[vars[j]]],
                     method="pearson", alternative="two.sided", conf.level=0.95)
    
    corrSignificance <- rbindlist(list(corrSignificance,
                                       data.table(V1=vars[i], V2=vars[j], pValue=test$p.value)))
  }
}

corrSignificance[pValue > 0.05, ]

