
# Metody Statystyczne w Zarządzaniu Wierzytelnościami Masowymi
# Laboratorium 3

## Zadanie 1

Eksploracja i przygotowanie danych aplikacyjnych

* Zakoduj w wybrany sposób cechy jakościowe `Product` oraz `Gender`. 
* Uzupełnij braki danych tych zmiennych aplikacyjnych, dla których jest to możliwe i sensowne.
* Usuń obserwacje odstające cech `LoanAmount`, `DPD` oraz `LastPaymentAmount`. 
* Zestandaryzuj cechy aplikacyjne. 
* Do zbioru cech aplikacyjnych dodaj zmienną wydzielającą klientów, którzy dokonali jakiejkolwiek wpłaty w pierwszych 6 miesiącach obsługi (klienci „dobrzy). Jaki jest udział dobrych klientów w zbiorze?

## Zadanie 2

Na wybranych cechach zbioru danych aplikacyjnych przeprowadź analizę skupień za pomocą algorytmu k-średnich. 

* Czy skupienia istotnie różnicują dobrych i złych klientów?
* Stwórz macierz kontyngencji rzeczywistej oraz prognozowanej dobroci klienta przyjmując, że w danym skupieniu wszystkie sprawy są uznawane za dobre jeżeli udział dobrych klientów w skupieniu jest wyższy od udziału dobrych klientów w całym zbiorze danych. Jaka jest jakość klasyfikacji?
* Przedstaw wyniki klasyfikacji na trójwymiarowym wykresie (pakiet `ggplot2`).
* Jaka liczba skupień daje najlepsze wyniki klasyfikacji?

## Zadanie 3

Stwórz prognozę dobroci klienta z wykorzystaniem algorytmu k-najbliższych sąsiadów `knn` z pakietu `class` (eksperymentuj z różną liczbą najbliższych sąsiadów). Jaka jest optymalna specyfikacja modelu?

## Zadanie 4

Zbuduj model Map Samoorganizujących Kohonena (pakiet `kohonen`):

* Stwórz wykres podsumowujący uczenie się modelu (`type="changes"`), liczby spraw w neuronach (`type="count"`) oraz wykres obrazujący odległości pomiędzy neuronami (`type="dist.neighbours"`).
*	Stwórz wykresy rozkładu cech na mapie (`„heatmaps”`) – czy zauważasz potencjalne skupienia?
*	Przeprowadź analizę skupień za pomocą metody k-średnich na neuronach mapy i zaproponuj optymalną liczbę skupień.
*	Wykonaj hierarchiczną analizę skupień na neuronach z obraną liczbą skupień i przedstaw skupienia na mapie. 
*	Sprawdź zróżnicowanie dobroci klienta w skupieniach i porównaj z wynikami skupień z zadania 2.
