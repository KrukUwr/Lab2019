

library(data.table)
library(randomForest)
library(ranger)
library(FNN)

load("KrukUWr2019.RData")

### zadanie 1 #####

# Przeanalizuj wartości `NA` na zmiennej `LoanAmount`. Czy wszystkie wartości `NA` 
# w przypadku tej zmiennej oznaczają brak danych? 


summary(cases$LoanAmount)

#### jak można zauwazyć problem dotyczy głownie kart #####
cases[,.(.N), by = .(Product, is.na(LoanAmount))][order(Product)] 

# na pierwszy rzut oka wartość Principal jest równa LoanAmount
head(cases[!is.na(LoanAmount) & Product == "Credit card", 1:7])

# porównajmy statystyki opisowe

summary(cases[!is.na(LoanAmount) & Product == "Credit card" , LoanAmount])
summary(cases[!is.na(LoanAmount) & Product == "Credit card" , Principal])

# tak wyglada to wizualnie

plot(density(cases[!is.na(LoanAmount) & Product == "Credit card" , LoanAmount]))
lines(density(cases[!is.na(LoanAmount) & Product == "Credit card" , Principal]), col = "red")

# po odcięciu najwyższych 10%

q90 <- quantile(cases[!is.na(Principal),Principal], probs = 0.9)

plot(density(cases[Product == "Credit card" & LoanAmount < q90, LoanAmount],na.rm = TRUE))
lines(density(cases[Product == "Credit card" & LoanAmount < q90, Principal], na.rm = TRUE), col = "red")


#### w przypadku kart kredytowych wartośc LoanAmount możemy zastapić Principal #####

cases[Product == "Credit card" & is.na(LoanAmount), LoanAmount := Principal]
anyNA(cases[Product == "Credit card", LoanAmount]) # sprawdzamy czy w tym zbiorze zostaly NA


#### Zadanie 2 ######

# Podaj biznesowy (ekspercki) sposób uzupełnienia wartości `NA` dla zmiennej `Other`.

# jak na wykładzie nr 1 .... TOA = Prinicpal + Interest + Other, zatem...
# Other = TOA - Principal - Interest

cases[is.na(Other), Other := TOA - Principal - Interest]

anyNA(cases[,Other])

#### Zadanie 3 ######

# Uzupełnij braki danych na zmiennej `Land` wykorzystując rozkład empiryczny. 
# Jak wykorzystać nowo pozyskane informacje w uzupełnieniu zmiennych `GDPPerCapita` i `MeanSalary`?


land_distribution <- prop.table(table(cases$Land)) #Pamietajmy, ze Land to factor!

barplot(land_distribution) #wizualizacja

# probkowanie z rozkladu Land

sampled <- sample(unique(cases[!is.na(Land),Land]),
                  size = sum(is.na(cases$Land)),
                  prob = land_distribution,
                  replace = TRUE)

cases[is.na(Land), Land:= sampled] # zastapienie brakow danych 

anyNA(cases$Land) #nie ma juz NA's

# Prosze zauwazyc, ze kazdy Land ma przypisana 1 wartosc MeanSalary i GDPPerCapita, 
# przyklad: wartosc min i max dla interesujacych nas cech jest taka sama

cases[!is.na(MeanSalary), .(minMeanSalry = min(MeanSalary, na.rm =T),
          maxMeanSalry = max(MeanSalary, na.rm = T),
          minGDPPerCapita = min(GDPPerCapita, na.rm =T),
          maxGDPPerCapita = max(GDPPerCapita, na.rm = T)),
          by = Land]

# kazdy land na 1 unikalna wartosc

cases[!is.na(MeanSalary), .(GDP_uniqe_values = length(unique(GDPPerCapita)),
                            MS_unique_values = length(unique(MeanSalary))),
      by = Land]

tmp <- cases[!is.na(MeanSalary), .(MS = min(MeanSalary), GDP= min(GDPPerCapita)), by = Land]

cases <- cases[tmp, on = "Land"]
cases[, ':='(MeanSalary = MS, GDPPerCapita = GDP)]
cases[,':='(MS = NULL, GDP = NULL)]

####################################### Zadanie 4 ##################################################

# Zweryfikuj dokadność uzupełniania braków danych dla zmiennej `TOA` poprzez modele
# lasów losowych i najblizszych sąsiadów (Wsk. Braki danych w `TOA` należy zasymulować).

#### symulujemy braki danych ####

set.seed(42)
ix_na <- sample(1: nrow(cases), size = 10000)

cases_nas <- cases[ix_na,] # wydzielona ramka dla na
cases_wna <- cases[-ix_na,] # ramka bez na do budowy modeli

trn_ix <- sample(1:nrow(cases_wna), size = 0.66 *nrow(cases_wna))

cases_trn <- cases_wna[trn_ix,] #ramka do uczenia
cases_tst <- cases_wna[-trn_ix,] #ramka do testowania

#### uczymy las losowy ####

rndm_frst <- randomForest(TOA~Principal+Interest+Other, data = cases_trn, nodesize = 1000, ntree = 500)
# rndm_frst_rngr <- ranger::ranger(TOA~Principal+Interest+Other, data = cases_trn, min.node.size = 1000, num.trees = 500)

rndm_pred <-predict(rndm_frst, newdata = cases_tst,type = "response")
# rndm_rngr_pred <- predict(rndm_frst_rngr, data = cases_tst, type = 'response')
# obliczamy blad procentowy predyckcji 'Absolute Percentage Error' dla kazdego przypadku 
# (Grzesiek pokazal Panstwu na wykladzie, z tym ze "krok po kroku", tutaj jest to gotowa funkcja)

blad_rf<-Metrics::ape(cases_tst$TOA, rndm_pred) 
# blad_rngr_rf <- Metrics::ape(cases_tst$TOA, rndm_frst_rngr$predictions)

#### uczymy metoda KNN ####

cases_trn_std <- cases_trn[, lapply(.SD, scale), .SDcols = c("TOA", "Principal", "Interest", "Other")]
cases_tst_std <- cases_tst[, lapply(.SD, scale), .SDcols = c("TOA", "Principal", "Interest", "Other")]

knn <- knn.reg(train = cases_trn_std, test = cases_tst_std, y = cases_trn$TOA, k = 5)

blad_knn <- abs(Metrics::ape(cases_tst$TOA, knn$pred))

erros <- cbind(blad_rf, blad_knn)

# jak mozemy zauwazyc przy tych parametrach budowy modeli z mniejszym bledem szacowalismy metoda KNN


summary(erros)

### Zadanie 5 ###
# 
# Zweryfikuj różnice pomiędzy wartościami średnich oraz przeciętnych dla rozkładów 
# poszczególnych zmiennych opisujących sprawy. Oceń jaki wpływ na różnice mają 
# wartości skrajne.

summary(cases)

# Wezmy pod lupe zmienna LoanAmount, tutaj widac ze srednia jest o wiele wieksza niz mediana

plot(density(cases$LoanAmount, na.rm=T))
boxplot(cases$LoanAmount ~ cases$Product)

cases[,.(avg = mean(LoanAmount, na.rm = TRUE),
         avg01 = mean(LoanAmount, na.rm = TRUE, trim = 0.01),
         avg05 = mean(LoanAmount, na.rm = TRUE, trim = 0.05),
         avg10 = mean(LoanAmount, na.rm = TRUE, trim = 0.1),
         median = median(LoanAmount, na.rm = TRUE))
      ]


### Zadanie 6 #####

# Posługując się wykresami typu boxplot zidentyfikuj wartości odstające (jaka 
# reguła jest przyjęta w funkcji `boxplot`) na poszczgólnych zmiennych opisujących 
# sprawy. Usuń przypadki z wartościami odstającymi, a następnie wykonaj wykres
# ponownie. Czy nadal możesz zaobserwować wartości odstające?

str(cases) # sprawdzmy jakie typy danych mamy w ramce i gdzie boxplot ma sens

### wybieramy te sensowne: ####
cols.choosen <- setdiff(names(cases),
                        c("CaseId",
                          "Product", "Gender", # cechy kategoryczne
                          "ExternalAgency", "Bailiff", "ClosedExecution", #tak naprawde cechy logiczne
                          "Land" #tak naprawde cecha kategoryczna
                        ))


str(cases[,.SD, .SDcols=cols.choosen])


boxplots <- list()
for (i in cols.choosen){
  boxplots[[i]] <- boxplot(cases[,.SD, .SDcols= i], main = i)
}

##### jaka regula jest przyjeta w box plot? ######

# 1.5 rozstepu miedzykwantylowego (IQR) od 1 i 3 kwantyla

IQR(cases$TOA)
quantile(cases$TOA)

upper_whisker <- min(max(cases$TOA), quantile(cases$TOA)[4]+IQR(cases$TOA)*1.5)
lower_whisker <- max(min(cases$TOA), quantile(cases$TOA)[2]-IQR(cases$TOA)*1.5)

TOA_cut <- cases[TOA>lower_whisker & TOA<upper_whisker, TOA]

boxplot(TOA_cut, main = "TOA_cut") # boxplot dalej wykazuje outliery, bo 

IQR(TOA_cut) 
quantile(TOA_cut)





