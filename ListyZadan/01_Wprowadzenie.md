

# Metody Statystyczne w Zarządzaniu Wierzytelnościami Masowymi.

## Zadanie 1

Stwórz strukturę danych `x = data.table(U=runif(10))`. Następnie napisz funkcję `goodBadProp`, która przyjmuje jako argumenty strukturę `tab` (data.table) oraz liczbę `p` z przedziału (0, 1). W ciele funkcji wykonaj operacje:

  - dodaj do struktury tab kolumnę `GoodBad`, która przyjmuje wartości 1, gdy U > p,
    a 0 w przeciwnym przypadku,
  - jako wynik funkcji zwróć strukturę data.table z wyliczonymi częstościami
    goodów i badów.
  - zadbaj o to, by kod funkcji nie zależał od wprost od U (nazwy kolumny w x)

## Zadanie 2

Stwórz tabelę o nazwie `rndNumbers` (klasa data.table) o 100k wierszach i kolumnach:

- U z wartościami rozkładu jednostajnego na (0, 1)
- Z z wartościami rozkładu normalnego
- E z wartościami rozkaldu eksponencjalnego
- G z wartościami 0, 1
- P z wartościami z rozkładu Poissona o średniej 2

Wyznacz (korzystając z funckjonalności data.table) statystyki opisowe wybranych 2 kolumn.
Wyznacz (korzystając z funckjonalności data.table) statystyki opisowe kolumn U, Z, E w rozbiciu względem kolumny G.
Wyznacz (korzystając z funckjonalności data.table) statystyki opisowe kolumn U, Z, E w rozbiciu względem czy P jest większe od swojej śedniej. Czy można to zrobić bez dodawania nowych kolumn, wyliczania "na boku" średniej P?

