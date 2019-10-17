
# Metody Statystyczne w Zarządzaniu Wierzytelnościami Masowymi
# Laboratorium 4

## Zadanie 1

Przygotuj dane aplikacyjne jak w Liście 3.

## Zadanie 2

Sprawdź oszacowanie treningowego błędu prognozy (zbiór uczący to jednocześnie zbiór testowy) regresyjnego modelu k-najbliższych sąsiadów prognozującego 6-cio miesięczną skuteczność za pomocą cech aplikacyjnych:

*	Wykorzystaj funkcję `knn.reg` z pakietu `FNN`.
*	Sprawdź dla liczb najbliższych sąsiadów od 1 do 30 oraz dla wąskiego oraz szerokiego zbioru cech objaśniających.
*	W tym i kolejnych zadaniach jako funkcję straty przyjmij funkcję kwadratową:
$$L(Y,\hat{f}(X))=(Y-\hat{f}(X))^2$$
*	Jakie są problemy związane z bezpośrednim prognozowaniem skuteczności?
*	Jaka jest optymalna liczba cech objaśniających oraz liczba najbliższych sąsiadów?

## Zadanie 3

Sprawdź oszacowanie testowego błędu prognozy regresyjnego modelu k-najbliższych sąsiadów prognozującego 6-cio miesięczną skuteczność za pomocą cech aplikacyjnych.

*	Wykorzystaj próbę treningową, walidacyjną oraz testową w proporcjach 50%, 25%, 25%.
*	Załóż stały zbiór cech objaśniających wybrany w poprzednim zadaniu.
*	Jak zachowuje się oszacowanie błędu testowego w zależności od liczby najbliższych sąsiadów?
*	Jaka jest optymalna liczba najbliższych sąsiadów i związane z nią oszacowanie testowego błędu prognozy?

## Zadanie 4

Sprawdź oszacowanie testowego błędu prognozy regresyjnego modelu k-najbliższych sąsiadów prognozującego 6-cio miesięczną skuteczność za pomocą cech aplikacyjnych wykorzystując 5 i 10-krotną kroswalidację. Sprawdź dla liczb najbliższych sąsiadów od 1 do 30 oraz porównaj z wynikami z poprzednich punktów.

## Zadanie 5

Sprawdź oszacowanie testowego błędu prognozy regresyjnego modelu k-najbliższych sąsiadów prognozującego 6-cio miesięczną skuteczność za pomocą cech aplikacyjnych wykorzystując metodę bootstrap oraz leave one-out bootstrap dla optymalnej liczby najbliższych sąsiadów wybranej w poprzednim punkcie. 

## Zadanie 6

Zbierz oraz porównaj wyniki uzyskane w punktach 2-5.
