
# Metody Statystyczne w Zarządzaniu Wierzytelnościami Masowymi.

## Zadanie 0 

- Zapoznaj się z artykułem [*The Landscape of R Packages for
Automated Exploratory Data Analysis*](https://journal.r-project.org/archive/2019/RJ-2019-033/RJ-2019-033.pdf)
- Za pomocą wybranych przez siebie opisanych w artykule pakietów w ramkach danych `cases` i `events` zweryfikuj:
    - typy danych,
    - wystąpienia braków danych i ich liczności oraz częstości,
    - wystąpnienia wartości 0 i ich liczności oraz częstości,
    - wykonaj wizualizacji braków w ramce danych (np. za pomocą pakietu `visdat`),
    - wartości statystyk opisowych,
    - liczby unikalnych wartości w każdej z kolumn.
- Zapoznaj się z zawartością [**repozytorium o Automatycznej Eksploracji Danych**](https://github.com/mstaniak/autoEDA-resources)
- Pomóż promować w społeczności Data Science pracę wskazaną w punkcie wyżej:
    - Załóż konto w serwisie [**Github**](https://github.com),
    - Zostaw 'gwiazdkę' na tym [repozytorium](https://github.com/mstaniak/autoEDA-resources) :)  
 
## Zadanie 1
Wyznacz skuteczność (suma wpłat przez wartość zadłułenia) w różnych horyzontach czasu (np. 3M, 6M, 12M) i wyznacz jej
 statystyki opisowe (kwantyle) w podziale na:
- Gender,
- ExtrernalAgency
- Bailiff
- ClosedExecution
- M_LastPaymentToImportDate (zaproponuj podział wg tej zmiennej)
- DPD (zaproponuj podział wg tej zmiennej)
- Age (zaproponuj podział wg tej zmiennej)
- TOA (zaproponuj podział wg tej zmiennej)

# Zadanie 2

Wyniki zaprezentuj również na wykresie. Które zmienne najlepiej różnicują skuteczność (co rozumiesz poprzez "różnicują"")?

# Zadanie 3

Wyznacz korelacje pomiędzy zmienną skuteczności 12M a innymi zmiennymi. Stwórz ranking korelcaji.

# Zadanie 4

Wyznacz dotarcie per sprawa (czy był kontakt w sprawie telefoniczny, lub bezpośredni) w róznych horyzontach czasu
i wyznacz jej statystyki opisowe (kwantyle) w podziale na wybrane zmienne (np. zmienne z zadania 1, lub zmienne, które różnicują dotarcie).

# Zadanie 5

Czy istnieje zaleśność pomiędzy ilością wykonywanych telefonów, wizyt, lub wysyłanych listów, a zmiennymi opisującymi sprawę (zmienne w cases).

# Zadanie 6

Dla wybranych zmiennych dokonaj przekształceń i zapisz jako nowe zmienne:
- standaryzowane (o średniej zero i warinacji 1)
- normalizowane (przekształcenie wartości zmiennej na odcinek [0, 1]).
- logarytmowane
- pierwiatskowane

Wyznacz korelację dla zmiennych orginalnych oraz korelację ich przekształconych odpowiedników.
Co można zauważyć?

# Zadanie 7.
Wyznacz wykres liniowy pokazujący skumuluwaną skuteczność SR w kolejnych miesiącach obsługi dla następujących typów spraw:
- SR w sprawach bez kontaktu (zarówno telefoniczny jak i wizyta)
- SR w sprawach z kontaktem
- SR w sprawach z ugód?
- SR w sprawach przekazanych do sądu.

Powyższe zdarzenia narzucają hierarchię procesu, tzn. jeżli na sprawie był kontakt w 3M to sprawa ta jest uważana za sprawę z kontaktem do 12M (do końca).
Jeżli w sprawie był kontakt w 2M oraz w tej sprawie została podpisana ugoda w 2M, to zaliczamy tą sprawę (jej skuteczność) do kategorii spraw z ugodą w 2M.



