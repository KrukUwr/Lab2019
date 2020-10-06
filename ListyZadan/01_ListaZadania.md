

# Modelowanie Statystyczne w Zarządzaniu Wierzytelnościami Masowymi.

## Laboratorium \#1. Struktury danych.

__Polecane kursy i pomoce:__

Internetowy tutorial na temat pakietu data.table:
https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html

Ściągawka z najczęściej używanymi funkcjonalnościami pakietu data.table:
https://s3.amazonaws.com/assets.datacamp.com/blog_assets/datatable_Cheat_Sheet_R.pdf

Kurs w języku polskim o R (brakuje w nim data.table, ale zawiera dużo przydatnych informacji):
http://pogromcydanych.icm.edu.pl/


__Proponowane funkcje do rozwiązania zadań:__

`as.integer()`, `c()`, `class()`, `data.table()`, `fwrite()`, `ifelse()`, `list()`, `merge`, `rbindlist()`, `rbinom()`, `rep()`, `rgamma()`, `rnorm()`, `round()`, `runif()`, `sample()`, `save()`, `setcolorder()`, `setDT()`, `setkey()`, `sum()`, `summary()`, `write.table()`


### Zadanie 1

Stwórz następujące wektory zawierające 10^5 elementów każdy:

- `toa` z rozkładu jednostajnego o wartościach pomiędzy 100 i 150000;
- `principal` jako losowa część toa, wyznaczona z rozkładu jednostajnego z przedziału (0.6, 1);
- `other` jako różnicę wektorów toa i principal;
- `gender` o wartościach "K" i "M" w losowej kolejności;
- `salary` o wartościach z rozkładu gamma o parametrach shape=2 i scale=2 przemnożonych przez 600;
- `success_rate` - wartości z rozkładu normalnego o średniej 0 i odchyleniu standardowym 1/3, następnie zamień wartości <0 na 0.0;
- `temporary` - wartości z rozkładu Bernoulliego o liczbie prób 1 i prawdopodobieństwie sukcesu 0.5;
- `sequence` - wektor kolejnych liczb całkowitych od 101 do 100100;
- `one_element`, `two_elements` - odpowiednio wektor jednoelementowy i dwuelementowy (wyjątek od założenia w zadaniu) o dowolnych wartościach.


### Zadanie 2

Wykorzystując odpowiednie wektory z poprzedniego zadania stwórz tabelę (klasa data.table) o nazwie `cases`. 

Kolumny nazwij: `TOA`, `Principal`, `Other`, `Gender`, `Salary`, `SuccessRate`, `Temporary`. Wartości w kolumnach `TOA`, `Principal`, `Other`, `Salary` zaokrąglij do 2 miejsc po przecinku, a wartości w kolumnie `SuccessRate` do 4 miejsc po przecinku.

Dodaj kolumnę `Id` przypisując do niej `.I`. Usuń kolumnę `Temporary`. Zmień kolejność kolumn w tabeli, tak aby kolumna `Id` znalazła się na pierwszym miejscu.

Dodaj kolumnę `IfGood`, która przyjmie wartości 1, gdy `SuccessRate` >0 oraz 0 w przeciwnym wypadku


### Zadanie 3

Podziel tabelę z zadania 2 na portfele, których nazwy podane są w wektorze: 
`portfolio_names <- c("PKO1", "PKO2", "mBank1", "Provident3", "Wonga6")`

W tym celu wykorzystaj np. funkcje `sample()` i/lub `rep()` i/lub... Wygenerowane nazwy portfeli przypisz do kolumny `Portfolio`.

Sprawdź wymiary tabeli `cases` (liczba wierszy, liczba kolumn).

Sprawdź, jakiej klasy jest obiekt cases oraz kolumny z tej tabeli.


### Zadanie 4

Stwórz tabele `cases_subset` wybierając 10000 dowolnych wierszy z tabeli `cases`.

Zmień `Id` w nowej tabeli na wartości całkowite większe niż 100k.

Dołącz do tabeli `cases` wiersze wybrane w tabeli `cases_subset`. Połączone tabele przypisz do tabeli o nazwie `all_cases`.

Stwórz tabele `toa_components` poprzez wybranie kolumn `Id`, `Principal`, `Other` z tabeli `all_cases`.


### Zadanie 5

Połącz tabele `events` (kod poniżej) oraz `all_cases` przy użyciu kolumn `Id`. 
Zwróć uwagę, że w tabeli `events` jedno id może wystąpić więcej niż raz.

Tak wygenerowaną tabelę zapiszcie do pliku csv i RData.

Informacje o JOINowaniu tabel klasy `data.table` możecie znaleźć w poniższym linku:

https://rstudio-pubs-static.s3.amazonaws.com/52230_5ae0d25125b544caab32f75f0360e775.html

```
set.seed(123)
events <- data.table(
  Id=sample(all_cases[["Id"]], 50000, replace = TRUE), 
  Event=sample(c(1, NA_integer_), 50000, replace = TRUE, prob = c(0.95, 0.05))
  )
```
