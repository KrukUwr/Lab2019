
library(data.table)


# Zadanie 1 ---------------------------------------------
# 
# Wczytaj dane z pliku KrukUWr2020.RData, a następnie wyświetl wczytane tabele. 
#
# Sprawdź poprawność danych w tabeli cases:
#   - Czy dla każdej sprawy suma składowych zadłużenia równa się zadłużeniu całkowitemu?
#   - Czy wartość udzielonej pożyczki jest zawsze większa niż kapitał pozostały do spłaty?
#   - Czy czas trwania pożyczki przyjmuje prawidłowe wartości?
#   - Czy dla każdej sprawy wiek jest poprawny?
#   
# Sprawdź poprawność danych w tabeli events:
# - Za pomocą funkcji summary sprawdź, czy wartości w każdej kolumnie są sensowne.
# - Zamień NA na wartość 0 w odpowiednich kolumnach
# 
# Zdecyduj co zrobić z wartościami błędnymi.

load("data/KrukUWr2020.RData")

print(cases)
summary(cases)

# === tabela cases:

###  Sprawdzenie składowych zadłużenia:
cases[,SumOfComponents:=Principal+Interest+ifelse(is.na(Other), 0, Other)]
cases[,Difference:=round(TOA-SumOfComponents, 2)]

cases[abs(Difference) >= 1e-12, .N]
cases[abs(Difference) >= 1e-12, .(Difference)]
summary(cases$Difference)

# Rozwiązanie idealne ale w naszym przypadku niemożliwe: 
#   wysyłamy pytanie do podmiotu sprzedające z prośbą o wyjaśnienie

# Rozwiązanie 1: nie robimy nic, ponieważ nie wiemy, która wartość jest nieprawidłowa
#   zakładając że błedy w danych są sprawą naturalną

# Rozwiązanie 2: aktualizujemy wartość kolumny Other o wartość różnicy
cases[Difference!=0.0, Other:=ifelse(is.na(Other), 0.0, Other)+Difference]

# sprawdzamy jeszcze raz
cases[,SumOfComponents:=Principal+Interest+ifelse(is.na(Other), 0, Other)]
cases[,Difference:=round(TOA-SumOfComponents, 2)]
cases[abs(Difference) >= 1e-12, .N]
cases[abs(Difference) >= 1e-12, .(Difference)]
summary(cases$Difference)

# sprawdzenie czy w kolumnie Other nie ma wartości ujemnych
summary(cases$Other)

# Usunięcie kolumn pomocniczych
cases[,`:=`(Difference=NULL, SumOfComponents=NULL)]

### sprawdzenie relacji kwoty udzielonej pożyczki i kapitału pozostałego do spłaty

cases[!is.na(LoanAmount) & LoanAmount<Principal, .N]

# Kwota udzielenie pożyczki nie jest tym samym w przypadku tradycyjnej pożyczki gotówkowej
# i długu na karcie kredytowej. Można rozważyć zamianę wartości w kolumnie LoanAmount dla produktu Credit card 
# na wartości ujemne, np -1.
cases[!is.na(LoanAmount) & LoanAmount<Principal, .(NoOfCases=.N), by=.(Product)]

# 8974 spraw z nieprawidłową parą wartości LoanAmount i Principal,
# bez kontaktu z podmiotem sprzedającym sprawy, nie jesteśmy wskazać źródła błędu,
# stąd sugeruje zostawić wartości niezmienione

# Czy czas trwania pożyczki przyjmuje wartości dodatnie
cases[D_ContractDateToImportDate<=0]
# Zawartości błędne podstawiamy NA
cases[D_ContractDateToImportDate<=0, D_ContractDateToImportDate:=NA_integer_]

### Czy wiek prawidłowy:
summary(cases$Age)
# Wartości -1 w kolumnie Age są prawidłowe; takie wartości przyjmują pożyczki zaciągnięte przez 
# np. osoby prawne, firmy, spółki, dla których nie można wyznaczyć wieku

# === tabela events:
summary(events)

# wartości ujemne wpłat mogą być spowodowane np. korektami księgowymi,
# zamieniamy je na wartość 0

events[ , `:=`(
  NumberOfCalls=ifelse(is.na(NumberOfCalls), 0, NumberOfCalls),
  NumberOfLettersSent=ifelse(is.na(NumberOfLettersSent), 0, NumberOfLettersSent),
  NumberOfVisits=ifelse(is.na(NumberOfVisits), 0, NumberOfVisits),
  NumberOfCallsWithClient=ifelse(is.na(NumberOfCallsWithClient), 0, NumberOfCallsWithClient),
  NumberOfVisitsWithClient=ifelse(is.na(NumberOfVisitsWithClient), 0, NumberOfVisitsWithClient),
  NumberOfLettersReceived=ifelse(is.na(NumberOfLettersReceived), 0, NumberOfLettersReceived),
  NumberOfAgreementConcluded=ifelse(is.na(NumberOfAgreementConcluded), 0, NumberOfAgreementConcluded),
  NumberOfAgreementSigned=ifelse(is.na(NumberOfAgreementSigned), 0, NumberOfAgreementSigned),
  TransferToLegalProcess=ifelse(is.na(TransferToLegalProcess), 0, TransferToLegalProcess),
  NumberOfPayment=ifelse(is.na(NumberOfPayment), 0, NumberOfPayment),
  PaymentAmount=ifelse(is.na(PaymentAmount), 0, PaymentAmount)
)]

# lub krócej, ale w tym przypadku trzeba uważać, czy takie podstawienie ma sens dla wszystkich wartości w tabeli
# zalecene rozwiązanie dłuższe znajdujące się powyżej
events[is.na(events)] <- 0

# wartości NA w kolumnach ze zdarzeniami oznaczają, że dane zdarzenie nie wystąpiło w danym miesiącu


# Zadanie 2 -----------------------------------------------------
# 
# Wyznacz skuteczność (suma wpłat przez wartość zadłużenia) w różnych horyzontach 6M i wyznacz jej
# statystyki opisowe (liczba spraw, średnia, wybrane kwantyle) w podziale na:
# - Gender,
# - ExternalAgency
# - Bailiff
# - ClosedExecution
# - M_LastPaymentToImportDate (zaproponuj podział wg tej zmiennej)
# - DPD (zaproponuj podział wg tej zmiennej)
# - Age (zaproponuj podział wg tej zmiennej)
# - TOA (zaproponuj podział wg tej zmiennej)

payments_6m <- events[Month<=6,.(Payments6M=sum(PaymentAmount)), by="CaseId"]
cases <- payments_6m[cases, on="CaseId"][,.SD, .SDcols=c(names(cases), "Payments6M")]
cases[,SR6M:=Payments6M/TOA]

cases[,`:=`(
  M_LastPaymentToImportDate_Groups=cut(M_LastPaymentToImportDate, breaks=10, include.lowest=TRUE, ordered_result=TRUE),
  DPD_Groups=cut(DPD, breaks=5, include.lowest=TRUE, ordered_result=TRUE),
  Age_Groups=cut(Age, breaks = c(-1, 0, 18, 24, 34, 44, 54, 64, 74), include.lowest = TRUE, ordered_result=TRUE),
  TOA_Groups=cut(TOA, breaks = c(0, 10000, 30000, max(TOA)), labels = c("Niskie saldo", "Srednie saldo", "Duze saldo"), 
    ordered_result = TRUE)
)]


showStats <- function(x, variable, by, na.rm=TRUE) {
  
  statistics <- x[, .(
    NoOfCases=.N,
    Min=min(get(variable), na.rm=na.rm),
    Q05=quantile(get(variable), 0.05, na.rm=na.rm),
    Q25=quantile(get(variable), 0.25, na.rm=na.rm),
    Median=median(get(variable), na.rm=na.rm),
    Mean=mean(get(variable), na.rm=na.rm),
    Q75=quantile(get(variable), 0.75, na.rm=na.rm),
    Q95=quantile(get(variable), 0.95, na.rm=na.rm),
    Max=max(get(variable), na.rm=na.rm)), 
    by=c(by)]
  
  setorderv(statistics, cols = by, na.last = TRUE)
  
  return(statistics)
  
}

showStats(cases, "SR6M", "Gender")
showStats(cases, "SR6M", "ExternalAgency")
showStats(cases, "SR6M", "Bailiff")
showStats(cases, "SR6M", "ClosedExecution")
showStats(cases, "SR6M", "M_LastPaymentToImportDate_Groups")
showStats(cases, "SR6M", "DPD_Groups")
showStats(cases, "SR6M", "Age_Groups")
showStats(cases, "SR6M", "TOA_Groups")


# Zadanie 3 -----------------------------------------------------
# 
# Wyniki zaprezentuj również na wykresie. Które zmienne najlepiej różnicują skuteczność (co rozumiesz poprzez "różnicują")?

library(ggplot2)

ggplot(data=cases, aes(x=TOA_Groups, y=SR6M)) + 
  geom_boxplot() +
  theme_bw()

# ograniczenie widoku osi y
ggplot(data=cases, aes(x=TOA_Groups, y=SR6M)) + 
  geom_boxplot() +
  ylim(c(0,1)) +
  theme_bw()

# wybranie z tabeli interesujących wierszy
ggplot(data=cases[SR6M>0 & SR6M<=1.5], aes(x=TOA_Groups, y=SR6M)) + 
  geom_boxplot() +
  theme_bw()

boxplot(cases$SR6M~cases$TOA_Groups)

# Analogicznie pozostałe cechy

  
# Zadanie 4 -----------------------------------------------------
  
# Wyznacz dotarcie per sprawa (czy był kontakt w sprawie telefoniczny, lub bezpośredni) w rożnych horyzontach czasu
# i wyznacz jej statystyki opisowe (kwantyle) w podziale na wybrane zmienne (np. zmienne z zadania 2, lub zmienne, które różnicują dotarcie).

reach_6m <- events[Month<=6, .(Reach6M=ifelse(sum(NumberOfCallsWithClient+NumberOfVisitsWithClient)>0, 1, 0)), 
  by=.(CaseId)]
cases <- reach_6m[cases, on="CaseId"][,.SD, .SDcols=c(names(cases), "Reach6M")]

showStats(cases, variable = "Reach6M", by="TOA_Groups")
showStats(cases, variable = "Reach6M", by="Product")


# Zadanie 5 -----------------------------------------------------

# Czy istnieje zależność pomiędzy ilością wykonywanych telefonów, wizyt, lub wysyłanych listów, a zmiennymi opisującymi sprawę (zmienne w cases).

cases_events <- events[,.(AllCalls = sum(NumberOfCalls),
  AllLetters = sum(NumberOfLettersSent),
  AllVisits = sum(NumberOfVisits)), by=CaseId]

cases <- cases_events[cases, on="CaseId"][,.SD, .SDcols=c(names(cases), "AllCalls", "AllLetters", "AllVisits")]

# wykresy rozrzutu
par(mfrow=c(1,3))
plot(cases$TOA, cases$AllCalls, pch=".", main="Calls")
plot(cases$TOA, cases$AllLetters, pch=".", main="Letters")
plot(cases$TOA, cases$AllVisits, pch=".", main="Visits")
par(mfrow=c(1,1))

# wielokrotny wykres rozrzutu
pairs(AllCalls~TOA+DPD+Age+LastPaymentAmount, data=cases, pch=".")

# macierz korelacji
# wybrane zmienne do wyznaczenia macierzy korelacji
numeric_variables <- c("AllCalls", "AllLetters", "AllVisits", "TOA", "Principal", "Age", "LastPaymentAmount", "SR6M")

corr_matrix <- cor(cases[,.SD, .SDcols=numeric_variables]) # wartości NA dla LastPaymetnAmount
corr_matrix <- cor(cases[,.SD, .SDcols=numeric_variables], use="pairwise.complete.obs")

spearman_matrix <- cor(cases[,.SD, .SDcols=numeric_variables], use="pairwise.complete.obs", method = "spearman")

library(corrgram)
corrgram(corr_matrix)
corrgram(spearman_matrix)

library(corrplot)
corrplot(corr_matrix)


# Zadanie 6 -----------------------------------------------------
# 
# Dla wybranych zmiennych dokonaj przekształceń i zapisz jako nowe zmienne:
#   - standaryzowane (o średniej zero i wariancji 1)
# - normalizowane (przekształcenie wartości zmiennej na odcinek [0, 1]).
# - logarytmowane
# - pierwiastkowanie
# 
# Wyznacz korelację dla zmiennych oryginalnych oraz korelację ich przekształconych odpowiedników.
# Co można zauważyć?

selected_columns <- c("TOA", "D_ContractDateToImportDate", "GDPPerCapita", "MeanSalary")

cases_original <- cases[,.SD, .SDcols=selected_columns]
cases_stand <- cases_original[, lapply(.SD, scale)]
mean(cases_stand$TOA)
sd(cases_stand$TOA)

normalize <- function(x, na.rm=TRUE) {
  result <- (x - min(x, na.rm = na.rm))/(max(x, na.rm = na.rm) - min(x, na.rm = na.rm)) 
  return(result)
}

cases_norm <- cases_original[, lapply(.SD, normalize)]
cases_log <- cases_original[, lapply(.SD, log10)]
cases_sqrt <- cases_original[, lapply(.SD, sqrt)]

corr_original <- cor(cases_original, use="pairwise.complete.obs")
corr_stand <- cor(cases_stand, use="pairwise.complete.obs")
corr_norm <- cor(cases_norm, use="pairwise.complete.obs")
corr_log <- cor(cases_log, use="pairwise.complete.obs")
corr_sqrt <- cor(cases_sqrt, use="pairwise.complete.obs")

corrgram(corr_original, main="Original")
corrgram(corr_stand, main="Stand")
corrgram(corr_norm, main="Norm")
corrgram(corr_log, main="Log")
corrgram(corr_sqrt, main="Sqrt")

  
# Zadanie 7. ------------------------------------------------------------
  
#   Wyznacz wykres liniowy pokazujący skumulowana skuteczność SR w kolejnych miesiącach obsługi dla następujących typów spraw:
# - SR w sprawach bez kontaktu (zarówno telefoniczny jak i wizyta)
# - SR w sprawach z kontaktem
# - SR w sprawach z ugód?
# - SR w sprawach przekazanych do sądu.

### przykład dla spraw z kontaktem i bez kontaktu

contact <- events[,.(
    IfContact=ifelse(sum(NumberOfCallsWithClient+NumberOfVisitsWithClient+NumberOfLettersReceived)>0,1,0)),
  by="CaseId"]
events <- contact[events, on="CaseId"][,.SD,.SDcols=c(names(events), "IfContact")]
events <- cases[,.(CaseId, TOA)][events, on="CaseId"]

events[,CumulativePayments:=cumsum(PaymentAmount), by="CaseId"]

comulative_sr_contact <- events[IfContact==1,.(SR=sum(CumulativePayments)/sum(TOA)), by="Month"]
comulative_sr_noncontact <- events[IfContact==0,.(SR=sum(CumulativePayments)/sum(TOA)), by="Month"]

plot(x=comulative_sr_contact$Month, y=comulative_sr_contact$SR, type='l')
lines(x=comulative_sr_noncontact$Month, y=comulative_sr_noncontact$SR, type='l', col="red")
legend(x="topleft", legend=c("Contact", "Non-contact"), lty=1, col=c("black", "red"))


