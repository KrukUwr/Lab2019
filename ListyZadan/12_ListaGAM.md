
# Metody Statystyczne w Zarządzaniu Wierzytelnościami Masowymi
# Laboratorium 12



### Wymagane biblioteki 

```
library(gam)
```

## Zadanie 1

Przygotuj dane aplikacyjne do analizy.

## Zadanie 2. 

Zbuduj na próbie uczącej oraz przetestuj na próbie testowej uogólniony model addytywny prognozujący poziom wpłat w horyzoncie 12 miesięcy obsługi w dwóch podejściach:

  1. Bezpośrednia prognoza `SR12M` dla spraw w modelu z ciągłą cechą objaśnianą (wybierz funkcję wiążącą oraz zapisz postać modelu).
  2. Przełożenie `SR12M` w przedziałach scorów uzyskanych w modelu z binarną cechą objaśnianą, określającą dobroć klienta (wybierz cechę objaśnianą i funkcję wiążącą oraz zapisz postać modelu).

* Modele estymuj per sprawa zaś odchylenie oblicz per portfel (jest to realizacja podejścia: model per sprawa &rarr; struktura porfeli na zbiorze Val/Tst &rarr; odchylenie per portfel).
* W każdym z modeli wytypuj cechy &ndash; kandydatki do nieparametrycznej reprezentacji.
* Wykonaj automatyczny dobór cech za pomocą funkcji `step.gam`  oraz wybierz najlepszą Twoim zdaniem specyfikację modelu.
* Oceń statystyczną istotność modeli.
* Dla każdego modelu przeanalizuj wykresy cząstkowej predykcji (*partial prediction plots*) – które cechy najsilniej wpływają na dobroć klientów?
* W przypadku modelu logitowego narysuj wykres krzywej ROC i zinterpretuj bazujące na nim miary jakości modelu. 
* Który model jest lepszy ze statystycznego punktu widzenia (*explained deviance*, *residual sum of squares*, *AIC* lub inne), a który lepiej prognozuje wpłaty?
* W każdym z podejść sprawdź czy nie wystąpiło zjawisko „współkrzywoliniowości” (*concurvity*). 
