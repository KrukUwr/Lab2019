
# Laboratorium 3 - analiza skupień

## Zadanie 1

Przygotowanie danych aplikacyjnych:

* Do zbioru cech aplikacyjnych dodaj zmienną wydzielającą klientów, którzy dokonali jakiejkolwiek wpłaty w pierwszych 6 miesiącach obsługi (klienci „dobrzy"). Jaki jest udział dobrych klientów w zbiorze?
* Podziel w sposób losowy zbiór danych aplikacyjnych na treningowy `train` i testowy `test`. Każdy z nich powinien zawierać około połowy wszystkich spraw. 
* Zakoduj w wybrany sposób cechy jakościowe `Product` oraz `Gender`. 
* Uzupełnij braki danych tych zmiennych aplikacyjnych, dla których jest to możliwe i sensowne.
* Usuń obserwacje odstające cech `LoanAmount`, `DPD` oraz `LastPaymentAmount`. 
* Zestandaryzuj cechy aplikacyjne. 


## Zadanie 2

Na zbiorze danych uczących, na wybranych cechach zbuduj model za pomocą algorytmu k-średnich. Czy skupienia istotnie różnicują dobrych i złych klientów w zbiorze uczącym?

Dla zbioru treningowego stwórz macierz kontyngencji rzeczywistej oraz prognozowanej dobroci klienta przyjmując, że w danym skupieniu wszystkie sprawy są uznawane za dobre jeżeli udział dobrych klientów w skupieniu jest wyższy od udziału dobrych klientów w całym zbiorze danych.

Wyniki klasyfikacji przedstaw na wykresie.

Następnie wykonaj predykcję na zbiorze testowym, wykorzystując wcześniejsze założenia dotyczące zbioru uczącego. Wyniki również przedstaw za pomocą tabeli kontyngencji. Do wykonania predykcji użyj poniższą funkcję:

```
predict.kmeans <- function(model, newdata) {
  y <- apply(newdata, 1, function(r) {
    which.min(colSums((t(model$centers) - r)^2))
  })
  return(y)
}
```

Przetestuj różne liczby skupień. Jaka ich liczba daje najlepsze wyniki klasyfikacji?


## Zadanie 3

Stwórz prognozę dobroci klienta z wykorzystaniem algorytmu k-najbliższych sąsiadów `knn` z pakietu `class` (eksperymentuj z różną liczbą najbliższych sąsiadów). Jaka jest optymalna specyfikacja modelu? Predykcję wykonaj zarówno dla zbioru uczącego jak i testowego.

## Zadanie 4 \*

Zbuduj model Map Samoorganizujących Kohonena (pakiet `kohonen`). Wykorzystaj w tym celu zbiory uczący i testowy.

* Stwórz wykres podsumowujący uczenie się modelu (`type="changes"`), liczby spraw w neuronach (`type="count"`) oraz wykres obrazujący odległości pomiędzy neuronami (`type="dist.neighbours"`).
*	Stwórz wykresy rozkładu cech na mapie (`„heatmaps”`) – czy zauważasz potencjalne skupienia?
*	Przeprowadź analizę skupień za pomocą metody k-średnich na neuronach mapy i zaproponuj optymalną liczbę skupień.
*	Wykonaj hierarchiczną analizę skupień na neuronach z obraną liczbą skupień i przedstaw skupienia na mapie. 
*	Sprawdź zróżnicowanie dobroci klienta w skupieniach i porównaj z wynikami skupień z zadania 2.

Metriał pomocniczy do wykonania zadania: https://www.r-bloggers.com/2014/02/self-organising-maps-for-customer-segmentation-using-r/

