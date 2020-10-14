
# Zadanie 1

Wczytaj dane z pliku KrukUWr2020.RData, a następnie wyświetl wczytane tabele. 

Sprawdź poprawność danych w tabeli cases:
 - Czy dla każdej sprawy suma składowych zadłużenia równa się zadłużeniu całkowitemu?
 - Czy wartość udzielonej pożyczki jest zawsze większa niż kapitał pozostały do spłaty?
 - Czy czas trwania pożyczki przyjmuje prawidłowe wartości?
 - Czy dla każdej sprawy wiek jest poprawny?

Sprawdź poprawność danych w tabeli events:
 - Za pomocą funkcji summary sprawdź, czy wartości w każdej kolumnie są sensowne.
 - Zamień NA na wartość 0 w odpowiednich kolumnach

Zdecyduj co zrobić z wartościami błędnymi.


# Zadanie 2

Wyznacz skuteczność (suma wpłat przez wartość zadłużenia) w różnych horyzontach 6M i wyznacz jej
 statystyki opisowe (liczba spraw, średnia, wybrane kwantyle) w podziale na:
- Gender,
- ExternalAgency
- Bailiff
- ClosedExecution
- M_LastPaymentToImportDate (zaproponuj podział wg tej zmiennej)
- DPD (zaproponuj podział wg tej zmiennej)
- Age (zaproponuj podział wg tej zmiennej)
- TOA (zaproponuj podział wg tej zmiennej)


# Zadanie 3 

Wyniki zaprezentuj również na wykresie. Które zmienne najlepiej różnicują skuteczność (co rozumiesz poprzez "różnicują")?


# Zadanie 4

Wyznacz dotarcie per sprawa (czy był kontakt w sprawie telefoniczny, lub bezpośredni) w rożnych horyzontach czasu
i wyznacz jej statystyki opisowe (kwantyle) w podziale na wybrane zmienne (np. zmienne z zadania 1, lub zmienne, które różnicują dotarcie).


# Zadanie 5

Czy istnieje zależność pomiędzy ilością wykonywanych telefonów, wizyt, lub wysyłanych listów, a zmiennymi opisującymi sprawę (zmienne w cases).


# Zadanie 6

Dla wybranych zmiennych dokonaj przekształceń i zapisz jako nowe zmienne:
- standaryzowane (o średniej zero i wariancji 1)
- normalizowane (przekształcenie wartości zmiennej na odcinek [0, 1]).
- logarytmowane
- pierwiastkowanie

Wyznacz korelację dla zmiennych oryginalnych oraz korelację ich przekształconych odpowiedników.
Co można zauważyć?


# Zadanie 7.

Wyznacz wykres liniowy pokazujący skumulowana skuteczność SR w kolejnych miesiącach obsługi dla następujących typów spraw:
- SR w sprawach bez kontaktu (zarówno telefoniczny jak i wizyta)
- SR w sprawach z kontaktem
- SR w sprawach z ugód?
- SR w sprawach przekazanych do sądu.
