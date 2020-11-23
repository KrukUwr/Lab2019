
# Metody Statystyczne w Zarządzaniu Wierzytelnościami Masowymi
# Laboratorium 8

## Zadanie 1

Przygotuj dane aplikacyjne do analizy eksploracyjnej oraz wytypuj cechy do analizy korelacji.

## Zadanie 2

Do zbioru cech aplikacyjnych dodaj cechę wydzielającą klientów, którzy dokonali przynajmniej 3 wpłat lub przekroczyli skuteczność 0.5% w pierwszych 6 miesiącach obsługi (klienci dobrzy).

## Zadanie 3

Stwórz i dokonaj analizy wykresów uzyskanych za pomocą funkcji `corrgram` oraz `corrplot` – czy widzisz grupy zmiennych powielających informację?

## Zadanie 4

Stwórz tabelę zawierającą współczynnik **VIF** dla każdej ze zmiennych – dokonaj korekty listy potencjalnych zmiennych objaśniających i przelicz tabelę **VIF**.


## Rozgrzewka

Wygeneruj losowy zbiór obserwacji z dwuwymiarowego rozkładu normalnego, a następnie:

*	Zdekomponuj zbiór danych na czynniki obliczając wektory własne macierzy kowariancji lub korelacji.
*	Zaprezentuj wylosowane dane na wykresie i zaznacz kierunki główne wskazywane przez wektory własne.

## Zadanie 6

Zestandaryzuj cechy objaśniające. Dlaczego jest to ważne w kontekście **PCA**?

## Zadanie 7

Oblicz wartości własne oraz wyznacz wektory własne macierzy korelacji za pomocą funkcji `eigen` lub `prcomp`, a następnie:

* Oblicz współrzędne danych w przestrzeni głównych składowych.
* Stwórz oraz zinterpretuj wykres przedstawiający udział wariancji całkowitej wyjaśnianej przez poszczególne główne składowe.
* Oblicz macierz korelacji oryginalnych zestandaryzowanych danych z głównymi składowymi – czy cechy najsilniej skorelowane z dobrocią klienta są silnie skorelowane z pierwszymi głównymi składowymi?
* Zaznacz na wykresie cechy objaśniające w przestrzeni korelacji cech z dwiema pierwszymi głównymi składowymi.
* Zinterpretuj wybraną główną składową.

## Zadanie 8

Przedstaw zbiór danych na wykresach w przestrzeniach wybranych par głównych składowych w podziale na dobroć klienta – która para głównych składowych wizualnie najlepiej dyskryminuje grupy?

## Zadanie 9

Dla każdej pary głównych składowych oblicz wybraną miarę siły dyskryminacji klientów dobrych i złych – czy najlepsze są pierwsze główne składowe?






