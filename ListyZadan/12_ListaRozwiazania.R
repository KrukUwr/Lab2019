
load("KrukUWr2019.RData")

library(data.table)
library(gam)




################  Zadanie 1 - Analiza eksploracyjna

summary(cases)
summary(events)

Cases <- data.table(cases)
Events <- data.table(events)


# Decoding variables

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



for (variable in variables) {      ## variable = 'Age'
  if (eval(parse(text=paste("nullCounts$",variable,sep=""))) > 0) {
    avg <- eval(parse(text=paste("mean(Cases[,",variable,"],na.rm=TRUE)",sep="")))
    eval(parse(text=paste("Cases[is.na(",variable,"), ",variable,":=avg]",sep="")))
  }           
}


#  Other imputation

summary(Cases)

Cases[is.na(Female),Female:= ifelse(runif(nullCounts$Female,0,1)<Cases[,mean(Female,na.rm=TRUE)],1L,0L)]
Cases[is.na(Bailiff),Bailiff:= ifelse(runif(nullCounts$Bailiff,0,1)<Cases[,mean(Bailiff,na.rm=TRUE)],1L,0L)]

Cases[is.na(ClosedExecution) & Bailiff==0, ClosedExecution:= 0L]
Cases[is.na(ClosedExecution), ClosedExecution:= ifelse(runif(dim(Cases[is.na(ClosedExecution),])[1],0,1)<Cases[,mean(ClosedExecution,na.rm=TRUE)],1L,0L)]


#  Proportion of tail data to be removed from the dataset

summary(Cases)
Proportion = 0.001

Cases <- Cases[LoanAmount<quantile(Cases[,LoanAmount], probs=1-Proportion),]
Cases <- Cases[DPD<quantile(Cases[,DPD], probs=1-Proportion),]
Cases <- Cases[LastPaymentAmount<quantile(Cases[,LastPaymentAmount], probs=1-Proportion),]


# Cecha modelowana regresyjnie - SR12M

setkey(Cases,CaseId)
setkey(Events,CaseId)

Payments <- Events[Month <= 12,.(P12M = sum(ifelse(is.na(PaymentAmount),0,PaymentAmount)), Qty12M = sum(ifelse(is.na(PaymentAmount),0,1))),by=.(CaseId)]
setkey(Payments,CaseId)


Cases <- Cases[Payments[,.(CaseId,P12M,Qty12M)],nomatch=0][,Client := 'B']
Cases[P12M*1.0/TOA > 0.005 | Qty12M >= 3, Client := 'G']
Cases[, Good:=ifelse(Client=='G',1,0)]
Cases[, SR12M:=P12M*1.0/TOA]

Cases <- Cases[SR12M<quantile(Cases[,SR12M], probs=1-Proportion),]
Cases <- Cases[SR12M >= 0,]

summary(Cases)


# Korelacja

library(corrplot)
corrplot(cor(Cases[,.SD,.SDcols = Variables]), order = "hclust", tl.col='black', tl.cex=.75)




################  Zadanie 2 - modelowanie

################ 2.1 GAM modelling SR12M

library(gam)



# Zbiór trn/val

n <- Cases[, .N]
indexTrn <- sample(1:n, 0.5*n)
CasesTrn <- Cases[indexTrn,]
CasesTst <- Cases[-indexTrn,]

summary(CasesTrn)
summary(CasesTst)



# Sztuczne portfele w zbiorze Tst (tu dla uproszczenia uzyskane w wyniku analizy skupień - jedno skupienie traktowane jak jeden portfel)

library(cluster)
skupienia = clara(CasesTst[,.SD,.SDcols = Variables], k = 30, metric = "euclidean", stand = FALSE, samples = 5, sampsize = 1000, trace = 0, medoids.x = TRUE, keep.data = FALSE, rngR = TRUE)


#skupienia$clusinfo   #Info o skupieniach 
#skupienia$medoids    #Współrzędne medoidów
#skupienia$i.med      #Medoidy
#skupienia$clustering #NumerySkupień

CasesTstP <- copy(CasesTst)
CasesTstP[,skupienie := skupienia$clustering]



# Skupienia jako portfele

CasesTstP[,.N,by=skupienie]



#  Model

model_SR12M <- gam(as.formula(paste("SR12M~",paste(Variables,collapse="+"))), family=gaussian(link=identity), data = CasesTrn)
                                       
summary(model_SR12M)
summary(CasesTrn)


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

step.model_SR12M <-step.gam(model_SR12M, scope=Lista, dir='both')

summary(step.model_SR12M)


#  Final model

model_SR12M <- gam(
                   SR12M~
                          s(LoanAmount,3) +                    
                          s(TOA,3) +                            
                          s(Principal,3) +                       
                          s(D_ContractDateToImportDate,3) +     
                          s(DPD,3) +                             
                          PopulationInCity +                                                         
                          s(Age,3) +                             
                          s(LastPaymentAmount,3) +               
                          s(M_LastPaymentToImportDate,3) +       
                          GDPPerCapita +                                                             
                          MeanSalary +                                                               
                          Female                              
, family=gaussian(link=identity), data = CasesTrn)

summary(model_SR12M)


#  Partial prediction plots

plot.gam(model_SR12M,ask=TRUE)


#  Payments 12M Forecast

forecast <- predict.gam(model_SR12M, newdata=CasesTst, type='response')*CasesTst[,TOA]
CasesTstP <- data.table(cbind(CasesTstP, as.data.frame(forecast)))


# Obliczenie średniego odchylenia per portfel

dev <- mean(CasesTstP[,.(dev=(abs(sum(P12M)-sum(forecast)))/sum(P12M)),by=skupienie][,dev])
dev <- mean(CasesTstP[,.(dev=(abs(sum(P12M)-sum(ifelse(forecast<0,0,forecast))))/sum(P12M)),by=skupienie][,dev]) # tu ujemne prognozy są zerowane




################ 2.2 GAM modelling SR12M  - binary response variable

#  Model

model_SR12M <- gam(as.formula(paste("Good~",paste(Variables,collapse="+"))), family=binomial(link = "logit"), data = CasesTrn)

summary(model_SR12M)


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

step.model_SR12M <-step.gam(model_SR12M, scope=Lista, dir='both')

summary(step.model_SR12M)


#  Final model

model_SR12M <- gam(
                   Good ~
                        s(LoanAmount,3) +
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


summary(model_SR12M)


#  Partial prediction plots

plot.gam(model_SR12M,ask=TRUE)


#  Payments 12M Forecast

CasesTrn[,Forecast := predict.gam(model_SR12M, newdata=CasesTrn, type='response')]
CasesTst[,Forecast := predict.gam(model_SR12M, newdata=CasesTst, type='response')]
Cases[,Forecast := predict.gam(model_SR12M, newdata=Cases, type='response')]

Cases[,Band := cut(Forecast, breaks = 10)]
CasesTrn[,Band := Cases[indexTrn,Band]]
CasesTst[,Band := Cases[-indexTrn,Band]]
CasesTstP[,Band := Cases[-indexTrn,Band]]

Payments_Group <- tapply(CasesTrn[,P12M],CasesTrn[,Band],sum)
TOA_Group <- tapply(CasesTrn[,TOA],CasesTrn[,Band],sum)
SR_Group <- Payments_Group/TOA_Group

Forecast <- data.table(cbind(Band=row.names(SR_Group),SR=SR_Group*1.0))
CasesTstP <- CasesTstP[Forecast, on = "Band"]
CasesTstP[, forecast := as.numeric(SR)*as.numeric(TOA)]


# Obliczenie średniego odchylenia per portfel

dev <- mean(CasesTstP[,.(dev=(abs(sum(P12M)-sum(forecast)))/sum(P12M)),by=skupienie][,dev])



#  Gini

scores <- predict.Gam(model_SR12M, CasesTst, type = "response")

library(pROC)
plot(roc(CasesTst[,Good], scores, direction="<"), col="yellow", lwd=3, main="ROC_BinaryModel")


#  Concurvity

library(mgcv)

model_SR12M <- gam(
                   Good ~
                        LoanAmount +
                        Interest +
                        s(Other) +
                        s(D_ContractDateToImportDate) +
                        s(DPD) +
                        s(Age) +
                        s(LastPaymentAmount) +
                        s(M_LastPaymentToImportDate) +
                        s(GDPPerCapita) +
                        s(MeanSalary) +
                        CreditCard +
                        Female
, family=binomial(link = "logit"), data = CasesTrn)


summary(model_SR12M)
plot(model_SR12M)

concurvity(model_SR12M,full=TRUE)
concurvity(model_SR12M,full=FALSE)



vis.concurvity(model_SR12M)



vis.concurvity <- function(b, type="estimate"){
   cc <- concurvity(b, full=FALSE)[[type]]

   diag(cc) <- NA
   cc[lower.tri(cc)]<-NA

   layout(matrix(1:2, ncol=2), widths=c(5,1))
   opar <- par(mar=c(5, 6, 5, 0) + 0.1)
   # main plot
   image(z=cc, x=1:ncol(cc), y=1:nrow(cc), ylab="", xlab="",
         axes=FALSE, asp=1, zlim=c(0,1))
   axis(1, at=1:ncol(cc), labels = colnames(cc), las=2)
   axis(2, at=1:nrow(cc), labels = rownames(cc), las=2)
   # legend
   opar <- par(mar=c(5, 0, 4, 3) + 0.1)
   image(t(matrix(rep(seq(0, 1, len=100), 2), ncol=2)),
         x=1:3, y=1:101, zlim=c(0,1), axes=FALSE, xlab="", ylab="")
   axis(4, at=seq(1,101,len=5), labels = round(seq(0,1,len=5),1), las=2)
   par(opar)
}


