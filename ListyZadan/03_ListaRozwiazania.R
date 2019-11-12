
load("KrukUWr2019.RData")
library(data.table)


######################################################## Zadanie 1

# A glance at the data

View(cases)
summary(cases)
summary(events)

Cases <- data.table(cases)
Events <- data.table(events)



###  Decoding variables

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



# Other imputation

summary(Cases)

Cases[is.na(Female),Female:= ifelse(runif(nullCounts$Female,0,1)<Cases[,mean(Female,na.rm=TRUE)],1L,0L)]
Cases[is.na(Bailiff),Bailiff:= ifelse(runif(nullCounts$Bailiff,0,1)<Cases[,mean(Bailiff,na.rm=TRUE)],1L,0L)]

Cases[is.na(ClosedExecution) & Bailiff==0, ClosedExecution:= 0L]
Cases[is.na(ClosedExecution), ClosedExecution:= ifelse(runif(dim(Cases[is.na(ClosedExecution),])[1],0,1)<Cases[,mean(ClosedExecution,na.rm=TRUE)],1L,0L)]




###  Proportion of tail data to be removed from the dataset

summary(Cases)

Proportion = 0.001

Cases <- Cases[LoanAmount<quantile(Cases[,LoanAmount], probs=1-Proportion),]
Cases <- Cases[DPD<quantile(Cases[,DPD], probs=1-Proportion),]
Cases <- Cases[LastPaymentAmount<quantile(Cases[,LastPaymentAmount], probs=1-Proportion),]



###  Standardization of variables

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


CasesStd <- data.table(cbind(CaseId=Cases[,CaseId],scale(Cases[,.SD,.SDcols = Variables])))

summary(CasesStd)
summary(Events)


###   Adding payment data from events

setkey(CasesStd,CaseId)
setkey(Events,CaseId)

Payments <- Events[Month <= 6,.(Payments6M = sum(ifelse(is.na(PaymentAmount),0,PaymentAmount))),by=.(CaseId)]
setkey(Payments,CaseId)

CasesStd <- CasesStd[Payments[,.(CaseId,Payments6M)],nomatch=0][,Client := 'B']
CasesStd[Payments6M > 0, Client := 'G']


# Proportion of goods
(G_prior <- CasesStd[Client == "G", .N]/CasesStd[, .N])




######################################################## Zadanie 2                    

summary(CasesStd)

Variables <- c("TOA","M_LastPaymentToImportDate")

k <- 5
kCluster <- kmeans(x=CasesStd[, .SD, .SDcols=Variables],
  centers=k, iter.max=100, nstart=10)

# Assigning clusters
CasesStd[, kClass:=kCluster$cluster]


# Graph 
CasesStd[, colClass:=rainbow(k)[kCluster$cluster]]

plot(CasesStd[Client == "G", ][["TOA"]],
  CasesStd[Client == "G", ][["M_LastPaymentToImportDate"]],
  pch=1, col=CasesStd[Client == "G", ]$colClass, cex=0.7,
  xlab="", ylab="", main="",
  xlim = 1.05*range(CasesStd[["TOA"]]),
  ylim = 1.05*range(CasesStd[["M_LastPaymentToImportDate"]]))
points(CasesStd[Client == "B", ][["TOA"]],
  CasesStd[Client == "B", ][["M_LastPaymentToImportDate"]],
  pch=6, col=CasesStd[Client == "B", ]$colClass, cex=0.7)
points(kCluster$centers, pch=18, cex=2)


### Discrimination of clients

CasesStd[,.(Qty=.N,sum(ifelse(Client == "B",1,0))/.N,sum(ifelse(Client == "G",1,0))/.N),by=kClass]



### Classification matrix
#             | Predicted Good | Predicted Bad  |
# Actual Good |      TP        |       FN       | = P
# Actual Bad  |      FP        |       TN       | = N
#
# sensitivity = true positive rate = TP/P = TP/(TP + FN)
# specificity = true negative rate = TN/N = TN/(FP + TN)

tmp <- CasesStd[, .(.N,
  B_count=sum(ifelse(Client == "B", 1,0)),
  G_count=sum(ifelse(Client == "G", 1,0)),
  G_percent=sum(ifelse(Client == "G", 1,0))/.N), by=kClass][order(kClass)]



# Forecast od Payment6M
tmp[, G_predict:=ifelse(G_percent > G_prior, 1, 0)]

confMatrix <- matrix(
  c(tmp[G_predict == 1, sum(G_count)], tmp[G_predict == 0, sum(G_count)],
    tmp[G_predict == 1, sum(B_count)], tmp[G_predict == 0, sum(B_count)]),
  nrow=2, ncol=2, byrow=TRUE)
colnames(confMatrix) <- paste0("Predicted: ", c("G", "B"))
rownames(confMatrix) <- paste0("Actual: ", c("G", "B"))
confMatrix

# Alternative to above
setkey(tmp,kClass)
setkey(CasesStd,kClass)
CasesStd <- CasesStd[tmp[,.(kClass,G_predict)],nomatch=0]
CasesStd[, GClient:= ifelse(Client == "B",0,1)]
table(CasesStd[,GClient],CasesStd[,G_predict])


### Classification quality
classSuccess <- sum(diag(confMatrix))/sum(confMatrix)


### Classification graph

library(ggplot2)

qplot(TOA,M_LastPaymentToImportDate,data=CasesStd, colour = Client, label=Client, xlab = "TOA", ylab = "M_LastPaymentToImportDate",size=I(1))

library(scatterplot3d)
library(rgl)
library(car)
scatter3d(x = CasesStd[,TOA], y = CasesStd[,M_LastPaymentToImportDate], z = CasesStd[,kClass], groups = as.factor(CasesStd[,Client]), surface=FALSE)



### The choice of k and its effect on classification quality

(G_prior <- CasesStd[Client == "G", .N]/CasesStd[, .N])

kClassSuccess <- data.table()
for (k in 2:30) {
  kCluster <- kmeans(x=CasesStd[, .SD, .SDcols=Variables],
    centers=k, iter.max=100, nstart=10)

  CasesStd[, kClass:=kCluster$cluster]

  tmp <- CasesStd[, .(.N,
    B_count=sum(ifelse(Client == "B", 1,0)),
    G_count=sum(ifelse(Client == "G", 1,0)),
    G_percent=sum(ifelse(Client == "G", 1,0))/.N), by=kClass][order(kClass)]
  tmp[, G_predict:=ifelse(G_percent > G_prior, 1, 0)]

  confMatrix <- matrix(
    c(tmp[G_predict == 1, sum(G_count)], tmp[G_predict == 0, sum(G_count)],
    tmp[G_predict == 1, sum(B_count)], tmp[G_predict == 0, sum(B_count)]),
    nrow=2, ncol=2, byrow=TRUE)

  kClassSuccess <- rbindlist(list(kClassSuccess,
    data.table(K=k, kClassSuccess=sum(diag(confMatrix))/sum(confMatrix))))
}

plot(kClassSuccess)




######################################################## Zadanie 3       

k <- 5
Variables <- c("TOA","M_LastPaymentToImportDate")

## Balanced or not? 

kNearest <- class::knn(
  train=CasesStd[, .SD, .SDcols=Variables],
  test=CasesStd[, .SD, .SDcols=Variables],
  cl=CasesStd$Client,
  k=k, use.all=FALSE)

cfMatrix <- table(kNearest, CasesStd$Client)
sum(diag(cfMatrix))/sum(cfMatrix)

knnClassSuccess <- data.table()
for (k in seq(from=1, to=30, by=5)) {
  kNearest <- class::knn(
    train=CasesStd[, .SD, .SDcols=Variables],
    test=CasesStd[, .SD, .SDcols=Variables],
    cl=CasesStd$Client,
    k=k, use.all=FALSE)

    cfMatrix <- table(kNearest, CasesStd$Client)

    knnClassSuccess <- rbindlist(list(knnClassSuccess,
    data.table(K=k, ClassSuccess=sum(diag(cfMatrix))/sum(cfMatrix))))
}


##### Sprawdzenie wystêpowania spraw z jednakowym TOA oraz M_LastPaymentToImportDate

#K ClassSuccess
#1:  1    0.9963149
#2:  6    0.7828094
#3: 11    0.7732157
#4: 16    0.7671388
#5: 21    0.7669705
#6: 26    0.7657480

temp <- CasesStd[,.SD,.SDcols = Variables]
temp <- cbind(temp, CaseId = CasesStd[,CaseId])
setkey(temp, TOA, M_LastPaymentToImportDate)
temp[temp][,.N,by=CaseId][N>1,]


######################################################## Zadanie 4  
### Przyklad bazuje na: https://www.r-bloggers.com/self-organising-maps-for-customer-segmentation-using-r/

#  SOM using Kohonen package  

set.seed(1)

# Creating Self-organising maps in R
# Load the kohonen package 
require(kohonen)

# Create a training data set (rows are samples, columns are variables
# Here I am selecting a subset of my variables available in "data"
data_train <- Cases[,.SD,.SDcols = Variables]

# Change the data frame with training data to a matrix
# Also center and scale all variables to give them equal importance during
# the SOM training process. 
data_train_matrix <- as.matrix(scale(data_train))

# Create the SOM Grid - you generally have to specify the size of the 
# training grid prior to training the SOM. Hexagonal and Circular 
# topologies are possible
som_grid <- somgrid(xdim = 30, ydim=30, topo="hexagonal")

# Finally, train the SOM, options for the number of iterations,
# the learning rates, and the neighbourhood are available
som_model <- som(data_train_matrix, 
    grid=som_grid, 
    rlen=500, # odpowiednik liczby epok z tradycyjnych sieci neuronowych
    alpha=c(0.05,0.01), #stop szybkoci uczenia siê - learning rate
    keep.data = TRUE )



#Training progress for SOM
plot(som_model, type="changes")

#Node count plot
plot(som_model, type="count", main="Node Counts")

# U-matrix visualisation
plot(som_model, type="dist.neighbours", main = "SOM neighbour distances")

# Kohonen Heatmap creation
coolBlueHotRed <- function(n, alpha = 1) {rainbow(n, end=4/6, alpha=alpha)[n:1]}
pretty_palette <- c("#1f77b4","#ff7f0e","#2ca02c", "#d62728","#9467bd","#8c564b","#e377c2","#27d65b","#d627d6","#f0fa2f")

#TOA
plot(som_model, type = "property", property = getCodes(som_model)[,1], main=colnames(getCodes(som_model))[1], palette.name=coolBlueHotRed)
#M_LastPaymentToImportDate
plot(som_model, type = "property", property = getCodes(som_model)[,2], main=colnames(getCodes(som_model))[2], palette.name=coolBlueHotRed)


# Viewing WCSS for kmeans
#mydata <- som_model$codes 
mydata <- som_model$codes[[1]]
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var)) 
for (i in 2:15) {
  wss[i] <- sum(kmeans(mydata, centers=i)$withinss)
}
plot(wss)
# Visualising cluster results
## use hierarchical clustering to cluster the codebook vectors
som_cluster <- cutree(hclust(dist(som_model$codes[[1]])), 10)

# plot these results:
plot(som_model, type="mapping", bgcol = pretty_palette[som_cluster], main = "Clusters", cex = 0.001) 
add.cluster.boundaries(som_model, som_cluster)




# forecasts
forecasts <- predict(som_model, newdata = scale(data_train_matrix))

Cases[, Client:=ifelse(Client=="B",0,1)]
forecasts <- data.table(Forecast = forecasts$unit.classif, Client = Cases$Client)


#som_cluster to clusters

clusters <- data.table(Neuron = as.integer(substr(names(som_cluster), 2, length(names(som_cluster))-1)),Cluster = as.integer(som_cluster*1))

# forecast ---> cluster

setkey(clusters, "Neuron")  
setkey(forecasts, "Forecast")

# Quality of discrimination of good/bad clients in clusters - compare with Ex.2
forecasts[clusters, nomatch=0][, .(.N, GoodClientProportion = sum(Client*1.0)*1.0/.N) , by = Cluster]

