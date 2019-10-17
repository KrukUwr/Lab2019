
# Metody Statystyczne w Zarz¹dzaniu Wierzytelnoœciami Masowymi
# Laboratorium 4

## Zadanie 1

Przygotuj dane aplikacyjne jak w Liœcie 3.

## Zadanie 2

SprawdŸ oszacowanie treningowego b³êdu prognozy (zbiór ucz¹cy to jednoczeœnie zbiór testowy) regresyjnego modelu k-najbli¿szych s¹siadów prognozuj¹cego 6-cio miesiêczn¹ skutecznoœæ za pomoc¹ cech aplikacyjnych:

*	Wykorzystaj funkcjê `knn.reg` z pakietu `FNN`.
*	SprawdŸ dla liczb najbli¿szych s¹siadów od 1 do 30 oraz dla w¹skiego oraz szerokiego zbioru cech objaœniaj¹cych.
*	W tym i kolejnych zadaniach jako funkcjê straty przyjmij funkcjê kwadratow¹: $$L(Y,\hat{f}(X))=(Y-\hat{f}(X))^2$$
*	Jakie s¹ problemy zwi¹zane z bezpoœrednim prognozowaniem skutecznoœci?
*	Jaka jest optymalna liczba cech objaœniaj¹cych oraz liczba najbli¿szych s¹siadów?

## Zadanie 3

SprawdŸ oszacowanie testowego b³êdu prognozy regresyjnego modelu k-najbli¿szych s¹siadów prognozuj¹cego 6-cio miesiêczn¹ skutecznoœæ za pomoc¹ cech aplikacyjnych.

*	Wykorzystaj próbê treningow¹, walidacyjn¹ oraz testow¹ w proporcjach 50%, 25%, 25%.
*	Za³ó¿ sta³y zbiór cech objaœniaj¹cych wybrany w poprzednim zadaniu.
*	Jak zachowuje siê oszacowanie b³êdu testowego w zale¿noœci od liczby najbli¿szych s¹siadów?
*	Jaka jest optymalna liczba najbli¿szych s¹siadów i zwi¹zane z ni¹ oszacowanie testowego b³êdu prognozy?

## Zadanie 4

SprawdŸ oszacowanie testowego b³êdu prognozy regresyjnego modelu k-najbli¿szych s¹siadów prognozuj¹cego 6-cio miesiêczn¹ skutecznoœæ za pomoc¹ cech aplikacyjnych wykorzystuj¹c 5 i 10-krotn¹ kroswalidacjê. SprawdŸ dla liczb najbli¿szych s¹siadów od 1 do 30 oraz porównaj z wynikami z poprzednich punktów.

## Zadanie 5

SprawdŸ oszacowanie testowego b³êdu prognozy regresyjnego modelu k-najbli¿szych s¹siadów prognozuj¹cego 6-cio miesiêczn¹ skutecznoœæ za pomoc¹ cech aplikacyjnych wykorzystuj¹c metodê bootstrap oraz leave one-out bootstrap dla optymalnej liczby najbli¿szych s¹siadów wybranej w poprzednim punkcie. 

## Zadanie 6

Zbierz oraz porównaj wyniki uzyskane w punktach 2-5.