

### Zadanie 1 ####

#   Stwórz strukturę danych x = data.table(U=runif(10)). Następnie napisz funkcję 
#   goodBadProp, która przyjmuje jako argumenty strukturę tab (data.table) oraz 
#   liczbę p z przedziału (0, 1). W ciele funkcji wykonaj operacje:
#
#   - dodaj do struktury tab kolumnę GoodBad, która przyjmuje wartości 1, gdy U > p, 
#     a 0 w przeciwnym przypadku,
#   - jako wynik funkcji zwróć strukturę data.table z wyliczonymi częstościami
#     goodów i badów.
#   - zadbaj o to, by kod funkcji nie zależał od wprost od U (nazwy kolumny w x)

#### rozwiązanie ####

library(data.table)

x <- data.table(U = runif(10))

goodBadProp <- function(tab, p){
  
  stopifnot(is.double(p), p < 1, p > 0)
  
  tab$GoodBad <- ifelse(tab[, 1] > p, 1, 0)
  #return(tab[, mean(.GoodBad), by = GoodBad])
  prop.table(table(tab$GoodBad))
  
}

z <- goodBadProp(x, 0.2)

class(z)

#### Zadanie 2 ####

#   Stwórz tabelę o nazwie rndNumbers (klasa data.table) o 100k wierszach i kolumnach:

#   - U z wartościami rozkładu jednostajnego na (0, 1)
#   - Z z wartościami rozkładu normalnego 
#   - E z wartościami rozkaldu eksponencjalnego
#   - G z wartościami 0, 1 
#   - P z wartościami z rozkładu Poissona o średniej 2

#   Wyznacz (korzystając z funckjonalności data.table) statystyki opisowe 
#   wybranych 2 kolumn. 
#   Wyznacz (korzystając z funckjonalności data.table) statystyki opisowe 
#   kolumn U, Z, E w rozbiciu względem kolumny G. 
#   Wyznacz (korzystając z funckjonalności data.table) statystyki opisowe 
#   kolumn U, Z, E w rozbiciu względem czy P jest większe od swojej śedniej. Czy
#   można to zrobić bez dodawania nowych kolumn, wyliczania "na boku" średniej P?


### Twoje rozwiązanie ###


n <- 10^5

rndNumbers <- data.table(U = runif(n),
                         Z = rnorm(n),
                         E = rexp(n),
                         G = rbinom(n, 1, 0.7),
                         P = rpois(n, 2)
)

rndNumbers[, .(meanU = mean(U),
               medianU = median(U),
               sdU = sd(U),
               meanZ = mean(Z),
               medianZ = median(Z),
               sdZ = sd(Z),
               meanE = mean(E),
               medianE = median(E),
               sdE = sd(E))]

rndNumbers[, .(meanU = mean(U),
               medianU = median(U),
               sdU = sd(U),
               meanZ = mean(Z),
               medianZ = median(Z),
               sdZ = sd(Z),
               meanE = mean(E),
               medianE = median(E),
               sdE = sd(E)),
               by = G]

rndNumbers[, .(meanU = mean(U),
               medianU = median(U),
               sdU = sd(U),
               meanZ = mean(Z),
               medianZ = median(Z),
               sdZ = sd(Z),
               meanE = mean(E),
               medianE = median(E),
               sdE = sd(E))]

rndNumbers[, .(meanU = mean(U),
               medianU = median(U),
               sdU = sd(U),
               meanZ = mean(Z),
               medianZ = median(Z),
               sdZ = sd(Z),
               meanE = mean(E),
               medianE = median(E),
               sdE = sd(E)),
               by = P > mean(P)]


summary(rndNumbers)



#### Zadanie 3 ####

#   Do tabeli rndNumbers z zadania 2 dodaj kolumnę Id przypisując do niej ".I".
#   Stwórz tabelę rndNumbers2 jako podzbiór tabeli rndNumbers dla Id <= 10.
#   Stwórz tabelę tab (10 wierszy) z kolumnami:

#   - Id o wartościach ze zbioru 1, 2, 3 przypisanymi losowo
#   - Id2 o wartościach ze zbioru 6, 7, 8 przypisanymi losowo

#   Nałóż klucz Id na tabelę rndNumbers2. Nałóż klucz Id na tabelę tab.
#   Wykonaj joiny tabel tab i rndNumbers2 w różnej kolejności (np. tab[rndNumbers2], 
#   lub rndNumbers2[tab]). 
#   Co otrzymujesz w wyniku (po jakiej kolumnie wykonywany
#   jest join; wymuś joina po drugiej kolumnie)? 
#   Do czego służą parametry nomatch, allow.cartesian?

### Twoje rozwiązanie ###

rndNumbers[, Id:= .I]

rndNumbers2 <- rndNumbers[Id <= 10]

tab <- data.table(Id = sample(c(1:3), 10, replace = TRUE),
                  Id2 = sample(c(6:8), 10, replace = TRUE))

setDT(rndNumbers2, key ="Id")
setDT(tab, key = "Id2")

tab[rndNumbers2]
tab[rndNumbers2, nomatch=0]

tab[rndNumbers2]
rndNumbers2[tab]
rndNumbers2[tab, on = .(Id, Id2), nomatch = 0] #bląd




#### Zadanie 4 ####

#   Stwórzcie data.table x o 1mln wierszy i dwóch kolumnach z liczbami z rozkładu 
#   jendostajnego i normalnego. Zapiszcie tabelę x do pliku csv. Zweryfikujcie czas
#   zapisu przy użyciu funkcji standardowych funkcji zapisu do csv np. write.table,
#   write.csv oraz fwrite (data.table). 
#   Wczytajcie utworzony plik uzywając funckji read.table oraz fread porównując czas 
#   wczytywania danych.
#   Zwróćcie uwagę na ilość zajmowanego miejsca przez zmienną w R oraz wielkość 
#   pliku csv.

### Twoje rozwiązanie ###

x <- data.table(z = rnorm(10^6), u = runif(10^6))
y <- data.frame(z = rnorm(10^6), u = runif(10^6))

system.time(write.csv(x, "x.csv"))
system.time(write.table(x, "x.csv"))
system.time(fwrite(x, "y.csv"))


system.time(read.csv("x.csv"))
system.time(read.table("x.csv"))
system.time(fread("y.csv"))

object.size(x)
object.size("x.csv")



