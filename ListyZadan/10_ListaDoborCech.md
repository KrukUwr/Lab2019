
# Metody Statystyczne w Zarządzaniu Wierzytelnościami Masowymi
# Laboratorium 10

## Zadanie 1

Przygotuj dane aplikacyjne do analizy i wybierz zbiór cech - kandydatek do modelowania w ramach modelu regresji liniowej.

## Zadanie 2

Zadaniem jest wybór optymalnego zbioru cech objaśniających w liniowym modelu regresji modelującym `SR12M`. W tym celu stwórz wykres obrazujący wszystkie możliwe kombinacje wybranych cech objaśniających oraz `RSS` (residual sum of squares, patrz poprzednie laboratorium) modelu obliczony na podstawie próby walidacyjnej:

* Podziel zbiór danych na zbiory uczący oraz walidacyjny.
* Jaka jest liczba wszystkich możliwych kombinacji cech objaśniających, jeżeli do dyspozycji jest *p* cech?
* Stwórz ramkę danych zawierającą wszystkie możliwe kombinacje cech objaśniających (np. `expand.grid`).
* W pętli oszacuj modele na danych uczących i oblicz `RSS` na danych walidacyjnych dla kombinacji cech.
* Przedstaw wyniki na wykresie (oś odciętych to liczba cech objaśniających, zaś oś rzędnych to `RSS`).
*	Jaka jest optymalna kombinacja cech?
*	Dokonaj interpretacji oszacowanych parametrów wybranej kombinacji.

## Zadanie 3

Jaka jest kombinacja cech objaśniających wskazywana przez *forward* oraz *backward stepwise selection*? Jak wyglądają ścieżki tych dwóch podejść na wykresie z zadania 2?

## Zadania 4-5 opcjonalne - w ich miejsce zajmiemy się modelowaniem z wykorzystaniem sieci nieuronowych

## Zadanie 4

Oszacuj liniowy model regresji grzbietowej na wyjściowym zbiorze cech objaśniających dla różnych poziomów parametru regularyzacyjnego λ (λ nieujemna):

*	Narysuj wykres oszacowania błędu testowego w zależności od parametru λ.
* Jaka jest optymalna wartość parametru λ?

## Zadanie 5

Oszacuj liniowy model regresji z regularyzacją lasso na wyjściowym zbiorze cech objaśniających dla różnych poziomów parametru regularyzacyjnego λ (λ nieujemna):

*	Narysuj wykres oszacowania błędu testowego w zależności od parametru λ.
* Jaka jest optymalna wartość parametru λ?



Listę zadań podsumuj wypełniając oszacowaniami parametrów tabelę cecha-sposób doboru (użycie wszystkich cech, globalnie najlepszy podzbiór cech, *ridge*, *lasso*, *forward stepwise selection*, *backward stepwise selection*).




