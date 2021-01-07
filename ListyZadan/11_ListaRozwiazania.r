
setwd("")
load("KrukUWr2020.RData")

library(data.table)
library(InformationValue)
library(pROC)
library(gam)



############# Zadanie 1

# A glance at the data

summary(cases)
summary(events)

Cases <- data.table(cases)
Events <- data.table(events)


#Cases <- Cases[sample((1:dim(Cases)[1]),0.2*dim(Cases)[1])]

# zmienna sukcesu

setkey(Cases,CaseId)
setkey(Events,CaseId)

Payments <- Events[Month <= 6,.(P6M = sum(ifelse(is.na(PaymentAmount),0,PaymentAmount))), by=.(CaseId)]
setkey(Payments,CaseId)

Cases <- Cases[Payments[,.(CaseId,P6M)],nomatch=0][,target:=ifelse(P6M>300,1,0)]

summary(Cases)


# usuń braki danych w cechach, w których jest to możliwe do wykonania.

Variables = c(         "LoanAmount",
                       "TOA",
                       "Principal",
                       "Interest",
                       "Other",
                       "D_ContractDateToImportDate",
                       "DPD",
                       "PopulationInCity",
                       "Age",
                       "LastPaymentAmount",
                       "M_LastPaymentToImportDate",
                       "GDPPerCapita",
                       "MeanSalary"
                       #"CreditCard",
                       #"Gender"
                       #"Bailiff",
                       #"ClosedExecution",
                       #"ExternalAgency
)

nullCounts <- lapply(Cases[,.SD,.SDcols=Variables], function(x) sum(is.na(x)))



for (variable in Variables) {      ## variable = 'Age'
  if (eval(parse(text=paste("nullCounts$",variable,sep=""))) > 0) {
    avg <- eval(parse(text=paste("mean(Cases[,",variable,"],na.rm=TRUE)",sep="")))
    eval(parse(text=paste("Cases[is.na(",variable,"), ",variable,":=avg]",sep="")))
  }           
}


# Other imputation

Cases[is.na(Bailiff),Bailiff:= ifelse(runif(cases[is.na(Bailiff),.N],0,1)<Cases[,mean(Bailiff,na.rm=TRUE)],1,0)]
Cases[is.na(ExternalAgency),ExternalAgency:= ifelse(runif(cases[is.na(ExternalAgency),.N],0,1)<Cases[,mean(ExternalAgency,na.rm=TRUE)],1,0)]
Cases[is.na(ClosedExecution) & Bailiff==0, ClosedExecution:= 0]
Cases[is.na(ClosedExecution), ClosedExecution:= ifelse(runif(Cases[is.na(ClosedExecution),.N],0,1)<Cases[,mean(ClosedExecution,na.rm=TRUE)],1,0)]

summary(Cases)


#  Proportion of tail data to be removed from the dataset

Proportion = 0.001

Cases <- Cases[LoanAmount<quantile(Cases[,LoanAmount], probs=1-Proportion),]
Cases <- Cases[DPD<quantile(Cases[,DPD], probs=1-Proportion),]
Cases <- Cases[LastPaymentAmount<quantile(Cases[,LastPaymentAmount], probs=1-Proportion),]


#  Correlation analysis

Variables = c(         "LoanAmount",
                       "TOA",
                       "Principal",
                       "Interest",
                       "Other",
                       "D_ContractDateToImportDate",
                       "DPD",
                       "ExternalAgency",
                       "Bailiff",
                       "ClosedExecution",
                       "PopulationInCity",
                       "Age",
                       "LastPaymentAmount",
                       "M_LastPaymentToImportDate",
                       "GDPPerCapita",
                       "MeanSalary"
)



library(corrplot)
corrplot(cor(Cases[,.SD,.SDcols = Variables]), order = "hclust", tl.col='black', tl.cex=.75)


#	Usuń zmienne skorelowane

Cases[,c('PopulationInCity',"Interest",'Other','ClosedExecution') := NULL] 


# kategorialne na dummy 

table(Cases[,Gender])
Cases[is.na(Gender), Gender:='brak']
Cases[, isMale := ifelse(Gender=="MALE",1,ifelse(Gender=="FEMALE",0,-1))]
table(Cases$isMale)


table(Cases[,Product])
Cases[, CashLoan := ifelse(Product=="Cash loan",1,0)]
table(Cases$CashLoan)


# usuwamy przekodowane zmienne oraz Land

Cases[,c('Gender',"Product","Land") := NULL] 


summary(Cases)



############# Zadanie 2 + 3

# sprawdzenie nieliniowych zależności

formula <- as.formula("target~
                      s(LoanAmount,2)+
                      s(TOA,2)+
                      s(Principal,2)+
                      s(D_ContractDateToImportDate,2)+
                      s(DPD,2)+
                      ExternalAgency+
                      Bailiff+
                      s(Age,3)+
                      s(LastPaymentAmount,2)+
                      s(M_LastPaymentToImportDate,2)+
                      s(GDPPerCapita,2)+
                      s(MeanSalary,2)+
                      isMale+
                      CashLoan")

model_gam <- gam(formula, family=binomial(link = "logit"), data = Cases)
summary(model_gam)
plot.gam(model_gam, ask=TRUE)





# WOE + IV dla Age

Cases2<-copy(Cases)

summary(Cases2[,Age])
Cases2[Age == -1,Age_d:="brak"]
Cases2[Age != -1, Age_d:=cut(Cases2$Age,c(min(Cases2[Age != -1,Age]),33,39,45,62,max(Cases2[Age != -1, Age])),include.lowest = F)]
table(Cases2[,Age_d])

WOETable(X=as.factor(Cases2[, Age_d]), Y=as.factor(Cases2[,target]))
WOE(X=as.factor(Cases2[, Age_d]), Y=as.factor(Cases2[,target]))
IV(X=as.factor(Cases2[, Age_d]), Y=as.factor(Cases2[,target]))

Cases2[, Age_woe:=WOE(X=as.factor(Cases2[, Age_d]), Y=as.factor(Cases2[,target]))]

# Sprawdzenie modelu z WOE

Formula <- as.formula("target~s(LoanAmount,2)+s(LoanAmount,2)+s(TOA,2)+s(Principal,2)+s(D_ContractDateToImportDate,2)+s(DPD,2)+ExternalAgency+Bailiff+s(Age_woe,3)+s(LastPaymentAmount,2)+s(M_LastPaymentToImportDate,2)+s(GDPPerCapita,2)+s(MeanSalary,2)+isMale+CashLoan")
model_gam <- gam(Formula, family=binomial(link = "logit"), data = Cases2)

plot.gam(model_gam, ask=TRUE)



# WOE + IV dla M_LastPaymentToImportDate


summary(Cases2[,M_LastPaymentToImportDate_d])
Cases2[M_LastPaymentToImportDate == 50, M_LastPaymentToImportDate_d :="50"]
Cases2[M_LastPaymentToImportDate != 50, M_LastPaymentToImportDate_d:=cut(M_LastPaymentToImportDate,c(min(Cases2[M_LastPaymentToImportDate != 50,M_LastPaymentToImportDate]),10,20,30,max(Cases2[M_LastPaymentToImportDate != 50, M_LastPaymentToImportDate])),include.lowest = T)]
table(Cases2[,M_LastPaymentToImportDate_d])

WOETable(X=as.factor(Cases2[, M_LastPaymentToImportDate_d]), Y=as.factor(Cases2[,target]))
WOE(X=as.factor(Cases2[, M_LastPaymentToImportDate_d]), Y=as.factor(Cases2[,target]))
IV(X=as.factor(Cases2[, M_LastPaymentToImportDate_d]), Y=as.factor(Cases2[,target]))

Cases2[, M_LastPaymentToImportDate_woe:=WOE(X=as.factor(Cases2[, M_LastPaymentToImportDate_d]), Y=as.factor(Cases2[,target]))]

# Sprawdzenie modelu z WOE

Formula <- as.formula("target~s(LoanAmount,2)+s(LoanAmount,2)+s(TOA,2)+s(Principal,2)+s(D_ContractDateToImportDate,2)+s(DPD,2)+ExternalAgency+Bailiff+s(Age_woe,3)+s(LastPaymentAmount,2)+s(M_LastPaymentToImportDate_woe,2)+s(GDPPerCapita,2)+s(MeanSalary,2)+isMale+CashLoan")
model_gam <- gam(Formula, family=binomial(link = "logit"), data = Cases2)

plot.gam(model_gam, ask=TRUE)



# WOE + IV dla isMale


summary(Cases2[,isMale])
table(Cases2[,isMale])

WOETable(X=as.factor(Cases2[, isMale]), Y=as.factor(Cases2[,target]))
WOE(X=as.factor(Cases2[, isMale]), Y=as.factor(Cases2[,target]))
IV(X=as.factor(Cases2[, isMale]), Y=as.factor(Cases2[,target]))

Cases2[, isMale_woe:=WOE(X=as.factor(Cases2[, isMale]), Y=as.factor(Cases2[,target]))]

# Sprawdzenie modelu z WOE

Formula <- as.formula("target~s(LoanAmount,2)+s(LoanAmount,2)+s(TOA,2)+s(Principal,2)+s(D_ContractDateToImportDate,2)+s(DPD,2)+ExternalAgency+Bailiff+s(Age_woe,3)+s(LastPaymentAmount,2)+s(M_LastPaymentToImportDate_woe,2)+s(GDPPerCapita,2)+s(MeanSalary,2)+isMale_woe+CashLoan")
model_gam <- gam(Formula, family=binomial(link = "logit"), data = Cases2)

plot.Gam(model_gam, ask=TRUE)



############# Zadanie 4


# podział na uczący i walidacyjny



Variables = c(         "LoanAmount",
                       "TOA",
                       "Principal",
                       "D_ContractDateToImportDate",
                       "DPD",
                       "ExternalAgency",
                       "Bailiff",
                       "Age_woe",
                       "LastPaymentAmount",
                       "M_LastPaymentToImportDate_woe",
                       "GDPPerCapita",
                       "MeanSalary",
                       "isMale_woe"
)


Cases <- copy(Cases2[,.SD,.SDcols = c(Variables,"target")])

n = dim(Cases)[1]                           
index.learn = sample(1:n, dim(Cases)[1]/2)

DaneTst = Cases[-index.learn,]
DaneTrn = Cases[index.learn,]
summary(DaneTrn)
summary(DaneTst)



# model

(formula1<-as.formula(paste('target ~ ',paste(Variables,collapse = '+'))))
model1 <- glm(formula1, data = DaneTrn, family = binomial(link = 'logit'))
summary(model1)



# informacja o modelu

coefficients(model1) # model coefficients
confint(model1, level=0.95) # CIs for model parameters 
fitted(model1) # predicted values



# krzywa ROC

scores <- predict.glm(model1,newdata=DaneTst,type="response")
wynik <- data.frame(actuals=DaneTst$target)

plotROC(actuals=wynik, predictedScores=scores)
r <- roc(DaneTst[,target], scores, direction="<")
plot(r,col="yellow", lwd=3, main="ROC")


# macierz klasyfikacji

cut_off <- 0.17
forecast <- data.table(forecast=scores>cut_off)*1
table(wynik$actuals, forecast[,forecast])


# wykres separacji

library(ggplot2)

tabela <- data.frame(actuals=factor(wynik$actuals), scores=scores)

p <- ggplot(tabela, aes(x=scores, color=actuals)) + geom_density()
p




# Przykład w Excelu

write.csv(Cases2[1:10000,c("target","TOA","M_LastPaymentToImportDate","DPD")],
          ".../daneExcel2.csv", row.names = FALSE)




