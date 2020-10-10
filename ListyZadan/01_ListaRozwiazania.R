
library(data.table)


# Zadanie 1 -------------------------------------------------------------------------------------------------------

# Stwórz następujące wektory zawierające 10^5 elementów każdy:
# 
# - `toa` z rozkładu jednostajnego o wartościach pomiędzy 100 i 150000;
# - `principal` jako losowa część toa, wyznaczona z rozkładu jednostajnego z przedziału (0.6, 1);
# - `other` jako różnicę wektorów toa i principal;
# - `gender` o wartościach "K" i "M" w losowej kolejności;
# - `salary` o wartościach z rozkładu gamma o parametrach shape=2 i scale=2 przemnożonych przez 600;
# - `success_rate` - wartości z rozkładu normalnego o średniej 0 i odchyleniu standardowym 1/3, następnie zamień wartości <0 na 0.0;
# - `temporary` - wartości z rozkładu Bernoulliego o liczbie prób 1 i prawdopodobieństwie sukcesu 0.5;
# - `sequence` - wektor kolejnych liczb całkowitych od 101 do 100100;
# - `one_element`, `two_elements` - odpowiednio wektor jednoelementowy i dwuelementowy (wyjątek od założenia w zadaniu) o dowolnych wartościach.

n <- 10^5

toa <- runif(n = n, min = 100, max = 150000)
?runif # wyświetlenie pomocy dla funkcji runif
# Inne rozkłady:
?distribution

principal <- toa * runif(n, 0.6, 1.0)
other <- toa - principal

gender <- sample(c("K", "M"), n, replace=TRUE)

# Sprawdzenie klas wektorów:
class(toa) # numeric
class(gender) # character

salary <- rgamma(n, shape=2, scale=2) 
salary <- salary * 600
#wykres gęstości prawdopodobieństwa wektora salary
plot(density(salary))
# podstawowe statystyki opisowe wektora salary
summary(salary)

success_rate <- rnorm(n, mean = 0, sd=1/3)
success_rate[success_rate<0] <- 0.0

temporary <- rbinom(n, size=1, prob=0.5)

sequance <- 101:100100
one_element <- "a"
two_elements <- c(123, 4)



# Zadanie 2 -------------------------------------------------------------------------------------------------------


# Wykorzystując odpowiednie wektory z poprzedniego zadania stwórz tabelę (klasa data.table) o nazwie `cases`. 
# Kolumny nazwij: `TOA`, `Principal`, `Other`, `Gender`, `Salary`, `SuccessRate`, `Temporary`. 
# Wartości w kolumnach `TOA`, `Principal`, `Other`, `Salary` zaokrąglij do 2 miejsc po przecinku, 
#   a wartości w kolumnie `SuccessRate` do 4 miejsc po przecinku.
# Dodaj kolumnę `Id` przypisując do niej `.I`. Usuń kolumnę `Temporary`. Zmień kolejność kolumn w tabeli, 
#   tak aby kolumna `Id` znalazła się na pierwszym miejscu.
# Dodaj kolumnę `IfGood`, która przyjmie wartości 1, gdy `SuccessRate` >0 oraz 0 w przeciwnym wypadku

cases <- data.table(
  TOA=toa, 
  Principal=principal,
  Other=other, 
  Gender=gender, 
  Salary=salary, 
  SuccessRate=success_rate, 
  Temporary=temporary)

cases[,Id:=.I]
cases[,Temporary:=NULL]

setcolorder(cases, neworder = c("Id", "TOA", "Principal", "Other", "Gender", "Salary", "SuccessRate") )

cases[,`:=`(
  TOA=round(TOA, digits = 2), 
  Principal=round(Principal, 2), 
  Other=round(Other), 
  Salary=round(Salary, 2), 
  SuccessRate=round(SuccessRate, 4))]

cases[,IfGood:=ifelse(SuccessRate>0, 1L, 0L)]
# lub
cases[,IfGood:=as.integer(SuccessRate>0)]

summary(cases)


# Zadanie 3 -------------------------------------------------------------------------------------------------------

# Podziel tabelę z zadania 2 na portfele, których nazwy podane są w wektorze: 
# `portfolio_names <- c("PKO1", "PKO2", "mBank1", "Provident3", "Wonga6")`
# W tym celu wykorzystaj np. funkcje `sample()` i/lub `rep()` i/lub... 
#   Wygenerowane nazwy portfeli przypisz do kolumny `Portfolio`.
# Sprawdź wymiary tabeli `cases` (liczba wierszy, liczba kolumn).
# Sprawdź, jakiej klasy jest obiekt cases oraz kolumny z tej tabeli.

portfolio_names <- c("PKO1", "PKO2", "mBank1", "Provident3", "Wonga6")
portfolio_num <- length(portfolio_names)

# kilka wybranych możliwości:
portfolio_1 <- sample(portfolio_names, size = n, replace = TRUE)
table(portfolio_1)
portfolio_2 <- sample(portfolio_names, size = n, replace = TRUE, prob = c(0.3, 0.25, 0.2, 0.15, 0.1))
table(portfolio_2)
portfolio_3 <- rep(portfolio_names, each=n/portfolio_num)
table(portfolio_3)
portfolio_4 <- rep(portfolio_names, length.out=n)
table(portfolio_4)
portfolio_5 <- sample(portfolio_3)
table(portfolio_5)

# Wybieram z powyższych wektor portfolio_2 i dodaje jako kolumnę do tabeli
cases[,Portfolio:=portfolio_2]

cases[,.N]
# liczba wierszy:
nrow(cases)
# liczba kolumn
ncol(cases)
length(cases)
# liczba wierszy i kolumn
dim(cases)

class(cases)

# same klasy kolumn w tabeli cases
sapply(cases, class)
# informacje o strukturze tabeli cases, w tym typach danych w kolumnach
str(cases)


# Zadanie 4 -------------------------------------------------------------------------------------------------------

# Stwórz tabele `cases_subset` wybierając 10000 dowolnych wierszy z tabeli `cases`.
# Zmień `Id` w nowej tabeli na wartości całkowite większe niż 100k.
# Dołącz do tabeli `cases` wiersze wybrane w tabeli `cases_subset`. 
# Połączone tabele przypisz do tabeli o nazwie `all_cases`.
# Stwórz tabele `toa_components` poprzez wybranie kolumn `Id`, `Principal`, `Other` z tabeli `all_cases`.

# kilka możliwych sposobow wyboru części wierszy:
n <- 10^4
# pierwsze n wierszy:
cases_subset <- head(cases, n)
# lub równoważnie
cases_subset <- cases[1:n]
# lub równoważnie
cases_subset <- cases[Id<=n] # bo id od 1 do 100k co 1

# ostatnie wiersze
cases_subset <- tail(cases, n)
# losowanie
cases_subset <- cases[sample(1:.N, n)]


cases_subset[,Id:=Id+100000]

all_cases <- rbindlist(list(cases, cases_subset))

toa_components <- all_cases[,.(Id, Principal, Other)]
# równoważnie:
selected_columns <- c("Id", "Principal", "Other")
toa_components <- all_cases[,.SD, .SDcols=selected_columns]


# Zadanie 5 -------------------------------------------------------------------------------------------------------

# Połącz tabele `events` (kod poniżej) oraz `all_cases` przy użyciu kolumn `Id`. 
# Zwróć uwagę, że w tabeli `events` jedno id może wystąpić więcej niż raz.
# Tak wygenerowaną tabelę zapiszcie do pliku csv i RData.
# Informacje o JOINowaniu tabel klasy `data.table` możecie znaleźć w poniższym linku:
# https://rstudio-pubs-static.s3.amazonaws.com/52230_5ae0d25125b544caab32f75f0360e775.html

set.seed(123)
events <- data.table(
  Id=sample(all_cases[["Id"]], 50000, replace = TRUE), 
  Event=sample(c(1, NA_integer_), 50000, replace = TRUE, prob = c(0.95, 0.05)))

events_aggr <- events[,.(Event=sum(Event, na.rm=TRUE)), by=Id]

# Poniżej cztery równoważne sposoby JOINowania:

# i. wskazanie kolumn w argumencie 'on' po których mają zostać połączone 
all_cases <- events_aggr[all_cases, on=c("Id"="Id")]

# ii. Nałożenie klucza na tabele za pomocą funkcji setDT. Gdy użyjemy funkcję setDT na obiekt data.frame, 
## przkeształci ona ten obiekt na klasę data.table
setDT(all_cases, key="Id")
setDT(events_aggr, key="Id")
all_cases <- events_aggr[all_cases]

# iii. Nałożenie klucza na tabele za pomocą funkcji setkey
setkey(all_cases, Id)
setkey(events_aggr, Id)
all_cases <- events_aggr[all_cases]

# iv. Za pomocą funkcji merge
all_cases <- merge(x=all_cases, y=events, by.x="Id", by.y="Id", all.x=TRUE, all.y=FALSE)

write.table(x=all_cases, file="./data/cases_lab1.csv", row.names = FALSE, sep=";", dec=",")
# lub funkcja z data.table (możliwość użycia wielu rdzeni procesora) - zalecana przy dużych zbiorach danych
fwrite(x=all_cases, file="./data/cases_lab1.csv", row.names = FALSE, sep=";", dec=",", nThread=2L )
save(all_cases, file="./data/cases_lab1.RData")


