

load("KrukUWr2020.RData")
library(data.table)

set.seed(1)


######################################################## Zadanie 1 

# A glance at the data

summary(cases)
summary(events)

Cases <- data.table(cases)
Events <- data.table(events)



#1  Decoding variables

Cases[,CreditCard := ifelse(Product=="Credit card",1,0)]
Cases[,Female := ifelse(Gender=="FEMALE",1,0)]



#2 Handling missing data

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
                       "MeanSalary",
                       "CreditCard",
                       "Female",
                       "Bailiff",
                       "ClosedExecution"
                       )

nullCounts <- lapply(Cases[,.SD,.SDcols=Variables], function(x) sum(is.na(x)))



#2 Imputation with avg

variables <- c(        "LoanAmount",
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
                       )
                                    
for (variable in Variables) {      ## variable = 'Age'
    if (eval(parse(text=paste("nullCounts$",variable,sep=""))) > 0) {
          avg <- eval(parse(text=paste("mean(Cases[,",variable,"],na.rm=TRUE)",sep="")))
          eval(parse(text=paste("Cases[is.na(",variable,"), ",variable,":=avg]",sep="")))
    }           
}



#2 Other imputation

Cases[is.na(Female),Female:= ifelse(runif(nullCounts$Female,0,1)<Cases[,mean(Female,na.rm=TRUE)],1,0)]
Cases[is.na(Bailiff),Bailiff:= ifelse(runif(nullCounts$Bailiff,0,1)<Cases[,mean(Bailiff,na.rm=TRUE)],1,0)]

Cases[is.na(ClosedExecution) & Bailiff==0, ClosedExecution:= 0]
Cases[is.na(ClosedExecution), ClosedExecution:= ifelse(runif(Cases[is.na(ClosedExecution),.N],0,1)<Cases[,mean(ClosedExecution,na.rm=TRUE)],1,0)]

summary(Cases)


#3   Adding payment data from events

setkey(CasesStd,CaseId)
setkey(Events,CaseId)

Payments <- Events[Month <= 6,.(Payments6M = sum(ifelse(is.na(PaymentAmount),0,PaymentAmount))),by=.(CaseId)]
setkey(Payments,CaseId)

Cases <- Cases[Payments[,.(CaseId,Payments6M)],nomatch=0][,SR6M := Payments6M/TOA]



#4  Proportion of tail data to be removed from the dataset

summary(Cases)

Proportion = 0.001

Cases <- Cases[LoanAmount<quantile(Cases[,LoanAmount], probs=1-Proportion),]
Cases <- Cases[DPD<quantile(Cases[,DPD], probs=1-Proportion),]
Cases <- Cases[LastPaymentAmount<quantile(Cases[,LastPaymentAmount], probs=1-Proportion),]
Cases <- Cases[SR6M<quantile(Cases[,SR6M], probs=1-Proportion),]

Cases <- Cases[SR6M >= 0,]



#5  Standardization of variables

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
                       #"Female",
                       #"Bailiff",
                       #"ClosedExecution"
                       )


CasesStd <- data.table(cbind(CaseId=Cases[,CaseId], SR6M=Cases[,SR6M], scale(Cases[,.SD,.SDcols = Variables])))



summary(CasesStd)
summary(Events)



######################################################## Zadanie 2 trn=tst 

k <- 5
Variables <- c("TOA","M_LastPaymentToImportDate")
Variables <- c(        "LoanAmount",
                       "TOA",
                       #"Principal",
                       #"Interest",
                       #"Other",
                       "D_ContractDateToImportDate",
                       #"DPD",
                       "PopulationInCity",
                       "Age",
                       "LastPaymentAmount",
                       "M_LastPaymentToImportDate",
                       "GDPPerCapita"
                       #"MeanSalary"
                       #"CreditCard",
                       #"Female",
                       #"Bailiff",
                       #"ClosedExecution"
                       )

kNearest <- FNN::knn.reg(
  train=CasesStd[, .SD, .SDcols=Variables],
  test=CasesStd[, .SD, .SDcols=Variables],
  y=CasesStd$SR6M,
  k=k, algorithm="kd_tree")

error <- sum((kNearest$pred-CasesStd[,SR6M])^2)/dim(CasesStd)[1]


knnError <- data.table()
for (k in seq(from=1, to=29, by=2)) {
  kNearest <- FNN::knn.reg(
    train=CasesStd[, .SD, .SDcols=Variables],
    test=CasesStd[, .SD, .SDcols=Variables],
    y=CasesStd$SR6M,
    k=k, algorithm="kd_tree")

  knnError <- rbindlist(list(knnError,
    data.table(K=k, Error=sum((kNearest$pred-CasesStd[,SR6M])^2)/dim(CasesStd)[1])))
  
  print(k)
}



######################################################## Zadanie 3 Zbiór trn/val/tst

n <- CasesStd[, .N]
K <- 29
CasesStd[, Set:=sample(1:2, n, replace=TRUE)]
l <- dim(CasesStd[Set == 2,])[1]
CasesStd[Set == 2, Set:=sample(2:3, l, replace=TRUE)]

#1 - train
#2 - validation
#3 - test

# Choice of k

knnError <- data.table()
for (k in seq(from=1, to=29, by=2)) { # to=29 by=10
  kNearest <- FNN::knn.reg(
    train=CasesStd[Set == 1, .SD, .SDcols=Variables],
    test=CasesStd[Set == 2, .SD, .SDcols=Variables],
    y=CasesStd[Set == 1, SR6M],
    k=k, algorithm="kd_tree")


  knnError <- rbindlist(list(knnError,
    data.table(K=k, Error=sum((kNearest$pred-CasesStd[Set == 2,SR6M])^2)/dim(CasesStd[Set == 2,])[1])))
}

plot(knnError)

# Estimating generalisation error with a test sample

kNearest <- FNN::knn.reg(
  train=CasesStd[Set < 3, .SD, .SDcols=Variables],
  test=CasesStd[Set == 3, .SD, .SDcols=Variables],
  y=CasesStd[Set < 3, SR6M],
  k=20, algorithm="kd_tree")

error <- sum((kNearest$pred-CasesStd[Set == 3,SR6M])^2)/dim(CasesStd[Set == 3,])[1]




######################################################## Zadanie 4 Kroswalidacja

# 5-fold cross-validation
n <- CasesStd[, .N]
f <- 10
CasesStd[, Fold:=sample(1:f, n, replace=TRUE)]

Error <- c()
for (i in 1:f) {
    kNearest <- FNN::knn.reg(
      train=CasesStd[Fold != i, .SD, .SDcols=Variables],
      test=CasesStd[Fold == i, .SD, .SDcols=Variables],
      y=CasesStd[Fold != i, SR6M],
      k=20, algorithm="kd_tree")

      Error <- c(Error,sum((kNearest$pred-CasesStd[Fold == i,SR6M])^2)) 
  }

generalization_error <- sum(Error)/n




n <- CasesStd[, .N]
f <- 5
CasesStd[, Fold:=sample(1:f, n, replace=TRUE)]



# Choice of k

knnError <- data.table()
for (k in seq(from=1, to=29, by=2)) {
  Error <- c()
  for (i in 1:f) {
      kNearest <- FNN::knn.reg(
        train=CasesStd[Fold != i, .SD, .SDcols=Variables],
        test=CasesStd[Fold == i, .SD, .SDcols=Variables],
        y=CasesStd[Fold != i, SR6M],
        k=k, algorithm="kd_tree")
  
        Error <- c(Error,sum((kNearest$pred-CasesStd[Fold == i,SR6M])^2)) 
    }
    knnError <- rbindlist(list(knnError,
    data.table(K=k, Error = sum(Error)/n)))
}




######################################################## Zadanie 5 Bootstrap

n <- CasesStd[, .N]
b <- 100

pb <- winProgressBar(title = "progress bar", min = 0,
                     max = 1, width = 300)

Error <- c()
for (i in 1:b) {

    Boot <- sample(1:n, n, replace=TRUE)
    kNearest <- FNN::knn.reg(
      train=CasesStd[Boot, .SD, .SDcols=Variables],
      test=CasesStd[, .SD, .SDcols=Variables],
      y=CasesStd[Boot, SR6M],
      k=20, algorithm="kd_tree")

      Error <- c(Error,sum((kNearest$pred-CasesStd[,SR6M])^2)) 
      setWinProgressBar(pb, i/b, title=paste( round(i*1.0/b*100, 0), "% done"))      

  }

close(pb)
generalization_error <- sum(Error)/(n*b)


# Leave one-out bootstrap

n <- CasesStd[, .N]
CasesStd[,Element:=1:n]

b <- 100

pb <- winProgressBar(title = "progress bar", min = 0,
                     max = 1, width = 300)

Error <- c()
NoOfCases <- data.frame()
for (i in 1:b) {

    Boot <- sample(1:n, n, replace=TRUE)
    kNearest <- FNN::knn.reg(
      train=CasesStd[Boot, .SD, .SDcols=Variables],
      test=CasesStd[, .SD, .SDcols=Variables],
      y=CasesStd[Boot, SR6M],
      k=20, algorithm="kd_tree")
      Rows <- CasesStd[is.element(Element, Boot)==FALSE,Element]
      Error <- c(Error,sum((kNearest$pred[Rows]-CasesStd[Rows,SR6M])^2)/length(Rows)) 
      
      setWinProgressBar(pb, i/b, title=paste( round(i*1.0/b*100, 0), "% done"))      

  }
close(pb)
generalization_error <- sum(Error)/(b)




############################################### Checking the avg predictor


# Train, test and validation datasets

#1 - train
#2 - validation
#3 - test

AvgTrain <- mean(CasesStd[Set < 3,SR6M])
error <- sum((AvgTrain-CasesStd[Set == 3,SR6M])^2)/dim(CasesStd[Set == 3,])[1]


