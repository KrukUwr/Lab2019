
load("KrukUWr2019.RData")
library(data.table)

set.seed(1)


############################ Zadanie 1 

summary(cases)
summary(events)

Cases <- data.table(cases)
Events <- data.table(events)


Cases <- Cases[sample((1:dim(Cases)[1]),0.2*dim(Cases)[1])]

# Adding payment data from events

setkey(Cases,CaseId)
setkey(Events,CaseId)

Payments <- Events[Month <= 12,.(P12M = sum(ifelse(is.na(PaymentAmount),0,PaymentAmount))), by=.(CaseId)]
setkey(Payments,CaseId)

Cases <- Cases[Payments[,.(CaseId,P12M)],nomatch=0][,SR12M := P12M/TOA]
Cases <- Cases[SR12M > 0,]


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
                                    
for (variable in Variables) {      ## variable = 'Age'
    if (eval(parse(text=paste("nullCounts$",variable,sep=""))) > 0) {
          avg <- eval(parse(text=paste("mean(Cases[,",variable,"],na.rm=TRUE)",sep="")))
          eval(parse(text=paste("Cases[is.na(",variable,"), ",variable,":=avg]",sep="")))
    }           
}


# Other imputation

Cases[is.na(Female),Female:= ifelse(runif(1,0,1)<Cases[,mean(Female,na.rm=TRUE)],1,0)]
Cases[is.na(Bailiff),Bailiff:= ifelse(runif(1,0,1)<Cases[,mean(Bailiff,na.rm=TRUE)],1,0)]

Cases[is.na(ClosedExecution) & Bailiff==0, ClosedExecution:= 0]
Cases[is.na(ClosedExecution), ClosedExecution:= ifelse(runif(1,0,1)<Cases[,mean(ClosedExecution,na.rm=TRUE)],1,0)]


# Proportion of tail data to be removed from the dataset

summary(Cases)

Proportion = 0.001

Cases <- Cases[LoanAmount<quantile(Cases[,LoanAmount], probs=1-Proportion),]
Cases <- Cases[DPD<quantile(Cases[,DPD], probs=1-Proportion),]
Cases <- Cases[LastPaymentAmount<quantile(Cases[,LastPaymentAmount], probs=1-Proportion),]
Cases <- Cases[SR12M<quantile(Cases[,SR12M], probs=1-Proportion),]


# Correlation analysis

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
                       "ClosedExecution",
                       "SR12M"
                       )

cor(Cases[,.SD,.SDcols = Variables], use="pairwise.complete.obs", method="pearson")
cor(Cases[,.SD,.SDcols = Variables], use="pairwise.complete.obs", method="spearman")

library(Hmisc)
rcorr(as.matrix(Cases[,.SD,.SDcols = Variables]), type="spearman")

library(corrplot)
corrplot(cor(Cases[,.SD,.SDcols = Variables]), order = "hclust", tl.col='black', tl.cex=.75)


# VIF quick analysis

options(scipen = 999)
vif <- data.table()

for (i in 1:length(Variables)) {
    model_lm <- lm(paste(Variables[i],paste(Variables[-i], collapse="+"),sep="~"), data = Cases)
    vif = rbind(vif,data.table(Variable = Variables[i], AdjR2 = summary(model_lm)$adj.r.squared, VIF = summary(model_lm)$adj.r.squared/(1-summary(model_lm)$adj.r.squared)))
}


# Variables

Variables = c(         #"LoanAmount",
                       "TOA",
                       "Principal",
                       #"Interest",
                       "Other",
                       #"D_ContractDateToImportDate",
                       "DPD",
                       #"PopulationInCity",
                       "Age",
                       "LastPaymentAmount",
                       "M_LastPaymentToImportDate",
                       #"GDPPerCapita",
                       #"MeanSalary",
                       #"CreditCard",
                       "Female"
                       #"Bailiff",
                       #"ClosedExecution",
                       #"SR12M"
                       )

summary(Cases)


# Training and test sets

Cases <- Cases[,.SD,.SDcols = c(Variables,"SR12M")]
      
n = dim(Cases)[1]                           
index.learn = sample(1:n, dim(Cases)[1]/2)

DaneTst = Cases[-index.learn,]
DaneTrn = Cases[index.learn,]
summary(DaneTrn)



############################ Zadanie 2  Seeking the best subset

sets <- expand.grid(rep( list(0:1), 8))[-1,]
for (i in 1:8) {
    sets[,i] = sets[,i]*i
}

# Liczba możliwych podzbiorów cech objaśniających bez zbioru pustego

n <- 2^length(Variables)-1

# Chmura wszystkich możliwych kombinacji cech objaśniających

recap <- data.table()
for (i in 1:n) {         # i = 10   , j=1

    variables <- c()

    for (j in 1:8) {  
        if (sets[i,j] > 0) (variables <- c(variables,Variables[sets[i,j]]))
    }

    Formula <- as.formula(paste("SR12M~",paste(variables,collapse="+")))
    
    model <- lm(formula = Formula, data = DaneTrn)
    
    rss <- sum((DaneTst[,SR12M] - (model$coefficients[1] + (model$coefficients[-1])%*%t(DaneTst[,.SD,.SDcols = variables])))^2)/dim(DaneTst)[1]
    noOfVars <- length(variables)
    
    recap = rbind(recap,data.frame(vars = paste(variables,collapse=","), RSS = rss, k = noOfVars))
    
}

plot(recap$k, recap$RSS )   #ylim=c(0.07,0.1)
                                     
recap[recap$RSS==min(recap$RSS),]
 


############################ Zadanie 3 Stepwise selection


# Definicja modeli ekstremalnych

null <- lm(SR12M~1, data=DaneTrn)
full <- lm(paste("SR12M~",paste(Variables,collapse="+")), data=DaneTrn)


# Forward selection - nie jest to zwykle model na linii optymalnych modeli (algorytm lokanie optymalny)

model_forward <- step(null,scope=list(lower=null, upper=full), direction="forward")

 
# Backward selection - nie jest to zwykle model na linii optymalnych modeli (algorytm lokanie optymalny)

model_backward <- step(full,scope=list(lower=null, upper=full), direction="backward")



############################ Zadanie 4  Ridge regression

#ridge

lambda = seq(from=0.0, to=0.005,by=0.0001)
x = data.matrix(DaneTrn[,.SD,.SDcols = Variables])
y = DaneTrn[,SR12M]

ridge.mod <- glmnet(x, y , family="gaussian", alpha = 0, lambda = lambda)

n <- length(lambda)
recap <- data.table()

for (i in 1:n) {        

    lam <- lambda[i]   
    rss <- sum((DaneTst[,SR12M] - (predict(ridge.mod, s = lam, exact = F, type = 'coefficients')[1] + (predict(ridge.mod, s = lam, exact = F, type = 'coefficients')[-1])%*%t(DaneTst[,.SD,.SDcols = Variables])))^2)/dim(DaneTst)[1]
    recap = rbind(recap,data.frame(RSS = rss, lambda = lam))
}

plot(recap$lam, recap$RSS)
min(recap$RSS)



############################ Zadanie 5  Lasso

#lasso

lambda = seq(from=0, to=0.005,by=0.0001)
x = data.matrix(DaneTrn[,.SD,.SDcols = Variables])
y = DaneTrn[,SR12M]

ridge.mod <- glmnet(x, y , family="gaussian", alpha = 1, lambda = lambda)


n <- length(lambda)
recap <- data.table()

for (i in 1:n) {        

    lam <- lambda[i]   
    rss <- sum((DaneTst[,SR12M] - (predict(ridge.mod, s = lam, exact = F, type = 'coefficients')[1] + (predict(ridge.mod, s = lam, exact = F, type = 'coefficients')[-1])%*%t(DaneTst[,.SD,.SDcols = Variables])))^2)/dim(DaneTst)[1]
    recap = rbind(recap,data.frame(RSS = rss, lambda = lam))
}

plot(recap$lam, recap$RSS)





