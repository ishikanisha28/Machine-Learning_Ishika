# Importing packages from library
```{r}

library(caret)
library(e1071)
library(reshape2)
library(gmodels)
```
#Setting the directory
```{r}
setwd("~/Documents/GitHub/64060_-inisha")
```

```{r}
getwd() #To check present working directory
Assign <-read.csv("UniversalBank (6).csv") 
#I have saved the data in the data frame named Assign. I will use the data frame when ever i need.
```

Assign
head(Assign)

# Change numerical variables into categorical variables
```{r}
UA<- Assign[,c(10,13,14)]
UA$Personal.Loan<-factor(UA$Personal.Loan)
UA$Online<-factor(UA$Online)
UA$CreditCard<-factor(UA$CreditCard)
```
#Dividing the data into training(60) and testing set(40)
```{r}
set.seed(30)
train.assign <- sample(row.names(UA), 0.6*dim(UA)[1])  
test.assign <- setdiff(row.names(UA), train.assign) 
train.df <- UA[train.assign, ]
test.df <- UA[test.assign, ]
train <- UA[train.assign, ]
test = UA[train.assign,]
head(test)
```

#A)Created a pivot table for the training data with Online as a column variable, CC as a row variable, and Loan as a secondary row variable.
```{r}
melted.UA= melt(train,id=c("Personal.Loan","CreditCard"),variable= "Online")
recast.UA= dcast(melted.UA,Personal.Loan+CreditCard~Online,length)
recast.UA
```

#B)Consider the task of classifying a customer who owns a bank credit card and is actively using
#online banking services. Looking at the pivot table, what is the probability that this customer will
#accept the loan offer? [This is the probability of loan acceptance (Loan = 1) conditional on having
#a bank credit card (CC = 1) and being an active user of online banking services (Online = 1)].

Probability of Loan acceptance given having a bank credit card and user of online services is 72/884=0.0814

#C)Create two separate pivot tables for the training data. Onewill have Loan (rows) as a function of Online (columns) and the other will have Loan (rows) as a function of CC.
```{r}
table(train[,c(1,2)])
table(train[,c(1,3)])
```

#D)Compute the following quantities [P(A | B) means “the probability ofA given B”]:
  i) P(CC = 1 | Loan = 1)
```{r}
Probability1 <- table(train[,c(3,1)])
PR1<-Probability1[2,2]/(Probability1[2,2]+Probability1[1,2])
PR1
```
ii)P(Online = 1 | Loan = 1) 
```{r}
Probability2 <- table(train[,c(2,1)])
PR2<-Probability2[2,2]/(Probability2[2,2]+Probability2[1,2])
PR2
```
iii)P(Loan = 1) (the proportion of loan acceptors) 
```{r}
Probability3 <- table(train[1])
PR3<-Probability3[2]/(Probability3[2]+Probability3[1])
PR3
```
iv)P(CC = 1 | Loan = 0) 
```{r}
Probability4 <- table(train[,c(3,1)])
PR4<-Probability4[2,1]/(Probability4[2,1]+Probability4[1,1])
PR4
```
v)P(Online = 1 | Loan = 0)
```{r}
Probability5 <- table(train[,c(2,1)])
PR5<-Probability5[2,1]/(Probability4[2,1]+Probability4[1,1])
PR5
```
vi)P(Loan = 0)
```{r}
Probability6 <- table(train[,1])
PR6<-Probability6[1]/(Probability6[1]+Probability6[2])
PR6
```

#E)Using the quantities computed above to compute the naive Bayes probability P(Loan = 1 | CC = 1, Online = 1).
#Naive_Bayes Probabiliity =(F1*F2*F3)/[(F1*F2*F3)+(F4*F5*F6)]

O.267658*0.5799257*0.08966667/(O.267658*0.5799257*0.08966667)+(0.297327*0.585866*0.9103333)
=0.013918217/0.172492615
0.08068877

#F) Comparing the value E with the one obtained from the pivot table in (B). Which is a more accurate estimate?

The value obtained from E IS 0.0806 and the value obtained from the pivot table B is 0.0814 which are almost similar.
The value obtained from pivot table is more accurate as Naive bayes assumes conditional independence among the variables.

#G) Which of the entries in this table are needed for computing P(Loan = 1 | CC = 1, Online = 1)? Run naive Bayes on the data. Examine the model output on training data, and find the entry that corresponds to P(Loan = 1 | CC = 1, Online = 1). Compare this to the number you obtained in (E).

```{r}
NaiveBY <- train[,c(1,2,3)]
NaiveBY1 <- naiveBayes(Personal.Loan~., data=NaiveBY)
NaiveBY1
```
On comparisition this value to that of E , its exactly the same

