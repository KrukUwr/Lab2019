
# Metody Statystyczne w Zarządzaniu Wierzytelnościami Masowymi
# Laboratorium 10

## Zadanie 1


Przygotuj dane aplikacyjne do analizy.

## Zadanie 2

Zadaniem jest wybór optymalnego zbioru cech objaśniających w liniowym modelu regresji modelującym `SR12M`. W tym celu stwórz wykres obrazujący wszystkie możliwe kombinacje cech objaśniających oraz `RSS` (residual sum of squares) modelu obliczony na podstawie próby walidacyjnej (oś odciętych to liczba cech objaśniających, zaś oś rzędnych to `RSS`).

* Jaka jest liczba wszystkich możliwych kombinacji cech objaśniających, jeżeli do dyspozycji jest *p* cech?

*	Jaka jest optymalna kombinacja cech?

*	Dokonaj interpretacji oszacowanych parametrów wybranej kombinacji.

## Zadanie 3

Jaka jest kombinacja cech objaśniających wskazywana przez *forward* oraz *backward stepwise selection*? Jak wyglądają ścieżki tych dwóch podejść na wykresie z zadania 2?

## Zadanie 4

Oszacuj liniowy model regresji grzbietowej na wyjściowym zbiorze cech objaśniających dla różnych poziomów parametru regularyzacyjnego λ (λ nieujemna):

<br />

![](http://latex.codecogs.com/gif.latex?%24%24%5Chat%7B%5Calpha%7D%5E%7Bridge%7D%3D%5Ctext%7Bargmin%7D_%7B%5Calpha%7D%5Cleft%5C%7B%5Csum_%7Bi%3D1%7D%5E%7BN%7D%28y_%7Bi%7D-%5Calpha_%7B0%7D-%5Csum_%7Bj%3D1%7D%5E%7Bp%7Dx_%7Bij%7D%5Calpha_%7Bj%7D%29%5E%7B2%7D%2B%5Clambda%5Csum_%7Bj%3D1%7D%5E%7Bp%7D%5Calpha_%7Bj%7D%5E2%5Cright%5C%7D%24%24) 

<br />

*	Narysuj wykres oszacowania błędu testowego w zależności od parametru λ.
* Jaka jest optymalna wartość parametru λ?

## Zadanie 5

Oszacuj liniowy model regresji z regularyzacją lasso na wyjściowym zbiorze cech objaśniających dla różnych poziomów parametru regularyzacyjnego λ (λ nieujemna):

<br />

![](http://latex.codecogs.com/gif.latex?%24%24%5Chat%7B%5Calpha%7D%5E%7Blasso%7D%3D%5Ctext%7Bargmin%7D_%7B%5Calpha%7D%5Cleft%5C%7B%5Csum_%7Bi%3D1%7D%5E%7BN%7D%28y_%7Bi%7D-%5Calpha_%7B0%7D-%5Csum_%7Bj%3D1%7D%5E%7Bp%7Dx_%7Bij%7D%5Calpha_%7Bj%7D%29%5E%7B2%7D%2B%5Clambda%5Csum_%7Bj%3D1%7D%5E%7Bp%7D%7C%5Calpha_%7Bj%7D%7C%5Cright%5C%7D%24%24) 

<br />

*	Narysuj wykres oszacowania błędu testowego w zależności od parametru λ.
* Jaka jest optymalna wartość parametru λ?

<br />
<br />

Listę zadań podsumuj wypełniając oszacowaniami parametrów tabelę cecha-sposób doboru (wszystkie cechy, najlepszy globalnie podzbiór, *ridge*, *lasso*, *forward-stepwise selection*, *backward-stepwise selection*).




