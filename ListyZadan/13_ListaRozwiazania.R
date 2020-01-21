
load("KrukUWr2019.RData")
library(data.table)


# A glance at the data

summary(cases)
summary(events)

Cases <- data.table(cases)
Events <- data.table(events)



#1  Decoding variables

Cases[,CreditCard := ifelse(Product=="Credit card",1,0)]
Cases[,Female := ifelse(Gender=="FEMALE",1,0)]



# Handling missing data

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



# Imputation with avg

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



# Other imputation

Cases[is.na(Female),Female:= ifelse(runif(nullCounts$Female,0,1)<Cases[,mean(Female,na.rm=TRUE)],1L,0L)]
Cases[is.na(Bailiff),Bailiff:= ifelse(runif(nullCounts$Bailiff,0,1)<Cases[,mean(Bailiff,na.rm=TRUE)],1L,0L)]

Cases[is.na(ClosedExecution) & Bailiff==0, ClosedExecution:= 0L]
Cases[is.na(ClosedExecution), ClosedExecution:= ifelse(runif(dim(Cases[is.na(ClosedExecution),])[1],0,1)<Cases[,mean(ClosedExecution,na.rm=TRUE)],1L,0L)]



#   Adding payment data from events

setkey(Cases,CaseId)
setkey(Events,CaseId)

Payments <- Events[Month <= 12,.(Payments12M = sum(ifelse(is.na(PaymentAmount),0,PaymentAmount))),by=.(CaseId)]
setkey(Payments,CaseId)

Cases <- Cases[Payments,nomatch=0][,SR12M := Payments12M*1.0/TOA]
Cases <- Cases[SR12M >= 0,] 

Cases[,Client := 0][SR12M > 0, Client := 1]



#  Proportion of tail data to be removed from the dataset

summary(Cases)

Proportion = 0.001

Cases <- Cases[LoanAmount<quantile(Cases[,LoanAmount], probs=1-Proportion),]
Cases <- Cases[DPD<quantile(Cases[,DPD], probs=1-Proportion),]
Cases <- Cases[LastPaymentAmount<quantile(Cases[,LastPaymentAmount], probs=1-Proportion),]
Cases <- Cases[SR12M<quantile(Cases[,SR12M], probs=1-Proportion),]



#  Correlation analysis

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

library(corrplot)
corrplot(cor(Cases[,.SD,.SDcols = Variables]), order = "hclust", tl.col='black', tl.cex=.75)



# Training and test sets

n = dim(Cases)[1]                           
index.learn = sample(1:n, dim(Cases)[1]/2)

CasesTst = copy(Cases[-index.learn,])
CasesTrn = copy(Cases[index.learn,])



#2 Classification models


# GAM-Logit

library(gam)

         #  Model

model_GAM <- gam(as.formula(paste("Client~",paste(Variables,collapse="+"))), family=binomial(link = "logit"), data = CasesTrn)

summary(model_GAM)


         #  Stepwise selection

Variables_Discrete = c("CreditCard","Female","Bailiff","ClosedExecution")

Lista <- c()
for (i in 1:length(Variables)) {

    if (sum(Variables[i] == Variables_Discrete) == 0) {
    Lista[[i]] = as.formula(paste("~1+", Variables[i],
                  "+s(", Variables[i], ",2)", "+s(",
                  Variables[i], ",3)", sep = ""))
                  }
    else {
                Lista[[i]] <- as.formula(paste("~1+", Variables[i],
                  sep = ""))
    }
}

step.model_SR12M <-step.gam(model_GAM, scope=Lista, dir='both')

summary(step.model_SR12M)


         #  Final model

model_GAM <- gam(
                   Client ~
                        s(TOA,2) +
                        LoanAmount +
                        Interest +
                        s(Other, 3) +
                        s(D_ContractDateToImportDate, 3) +
                        s(DPD, 3) +
                        s(Age, 3) +
                        s(LastPaymentAmount, 3) +
                        s(M_LastPaymentToImportDate, 3) +
                        s(GDPPerCapita, 3) +
                        s(MeanSalary, 3) +
                        CreditCard +
                        Female
, family=binomial(link = "logit"), data = CasesTrn)

summary(model_GAM)


scores_GAM <- predict.gam(model_GAM, CasesTst, type = "response")


# Linear model


model_LM <- lm( Client ~
                        LoanAmount +
                        Interest +
                        Other +
                        D_ContractDateToImportDate +
                        DPD +
                        Age +
                        LastPaymentAmount +
                        M_LastPaymentToImportDate +
                        GDPPerCapita +
                        MeanSalary +
                        CreditCard +
                        Female
, data = CasesTrn)


scores_LM = predict(model_LM, CasesTst)




model_LM2 <- lm( Client ~
                        Female + TOA + Age
, data = CasesTrn)


scores_LM2 = predict(model_LM2, CasesTst)


# Boosting

 library(ada)
 model_ADA <- ada(CasesTrn[,.SD,.SDcols = Variables],CasesTrn[,Client],type="real",nu=0.1,iter=150,loss="exponential")
 scores_ADA <- predict(model_ADA,CasesTst[,.SD,.SDcols = Variables],type="prob")[,2]




library(pROC)

roc_GAM <- roc(CasesTst[,Client], scores_GAM, direction="<")
roc_LM <- roc(CasesTst[,Client], scores_LM, direction="<")
roc_LM2 <- roc(CasesTst[,Client], scores_LM2, direction="<")
roc_ADA <- roc(CasesTst[,Client], scores_ADA, direction="<")




plot(roc_GAM, col=2, lwd=3, main="ROC_comparison")
plot(roc_LM, col=3, lwd=3, add=TRUE)
plot(roc_LM2, col=4, lwd=3, add=TRUE)
plot(roc_ADA, col=5, lwd=3, add=TRUE)
legend(0.3, 0.67, c('GAM', 'LM', 'LM2','ADA'), 2:5)


# Confusion matrix

table(ifelse(scores_GAM>0.35,1,0), CasesTst[,Client])


#McNemar's Test

Qual_GAM <- (ifelse(scores_GAM>0.35,1,0) == CasesTst[,Client])*1
Qual_LM <- (ifelse(scores_LM>0.35,1,0) == CasesTst[,Client])*1

tab <- table(Qual_GAM, Qual_LM)

chi_test <- ((abs(tab[2,1]-tab[1,2])-1)^2)/(tab[2,1]+tab[1,2])
qchisq(.95, df=1)
mcnemar.test(tab, correct = TRUE)

 
 
# Resampled paired t-test

noOfTrials <- 10
n <- dim(Cases)[1] 

p <- c()
   
for (i in 1:noOfTrials) { # i=1

                          
    index.learn = sample(1:n, dim(Cases)[1]/2)
    CasesTst = copy(Cases[-index.learn,])
    CasesTrn = copy(Cases[index.learn,])


# Model A

  model_A <- gam(
                   Client ~
                        s(TOA,2) +
                        LoanAmount +
                        Interest +
                        s(Other, 3) +
                        s(D_ContractDateToImportDate, 3) +
                        s(DPD, 3) +
                        s(Age, 3) +
                        s(LastPaymentAmount, 3) +
                        s(M_LastPaymentToImportDate, 3) +
                        s(GDPPerCapita, 3) +
                        s(MeanSalary, 3) +
                        CreditCard +
                        Female
                        , family=binomial(link = "logit"), data = CasesTrn)    
    
    scores_A <- predict.gam(model_GAM, CasesTst, type = "response")



# Model B  

  model_B <- lm( Client ~
                        LoanAmount +
                        Interest +
                        Other +
                        D_ContractDateToImportDate +
                        DPD +
                        Age +
                        LastPaymentAmount +
                        M_LastPaymentToImportDate +
                        GDPPerCapita +
                        MeanSalary +
                        CreditCard +
                        Female
                        , data = CasesTrn)


  scores_B = predict(model_LM, CasesTst)



  Tab_A <- table(ifelse(scores_A>0.35,1,0), CasesTst[,Client])
  Tab_B <- table(ifelse(scores_B>0.35,1,0), CasesTst[,Client])
  
  Misclass_A <- (Tab_A[2,1]+Tab_A[1,2])/dim(CasesTst)[1]
  Misclass_B <- (Tab_B[2,1]+Tab_B[1,2])/dim(CasesTst)[1]
  
  p <- c(p, Misclass_A-Misclass_B)
  
}


t_stat <- mean(p)*sqrt(noOfTrials)/sqrt(sum((p-mean(p))^2)/(noOfTrials-1))
qt(.025, df=noOfTrials-1)






