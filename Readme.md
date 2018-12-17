### Bank Marketing Analysis
> This project is based on the direct marketing campaigns of a Portuguese banking institution. 
> This marketing campaigns were based on phone calls. 
> A client was contacted more than once in order to assess if the product (bank term deposit) would be subscribed ('yes') or not subscribed (‘no’).
### Code
#loading the libraries
library(forecast)
library(readr)
library(ggplot2)
library(lattice)
library(plyr)
library(dplyr)
library(caret)
library(mlbench)
library(foreign)
library(ggplot2)
library(reshape)

#loading the data
alldata <- read.csv('../bank-additional-full.csv', stringsAsFactors = T, sep = ';')
age = alldata$age
job = alldata$job
marital = alldata$marital
education = alldata$education
default = alldata$default
housing = alldata$housing
loan = alldata$loan
contact = alldata$contact
month = alldata$month
day_of_week = alldata$day_of_week
duration = alldata$duration
campaign = alldata$campaign
pdays = alldata$pdays
previous = alldata$previous
poutcome = alldata$poutcome
emp.var.rate = alldata$emp.var.rate
cons.price.idx = alldata$cons.price.idx
cons.conf.idx = alldata$cons.conf.idx
euribor3m= alldata$euribor3m
nr.employed = alldata$nr.employed
term_deposit = alldata$y

#analyzing all the column values and its levels
str(alldata)
# getting the summary of the x variables
summary(alldata)
#Analyzing each x variable seperately
table(alldata$month)
table(alldata$poutcome)
table(alldata$job)
table(alldata$y)

#creating histograms
par( mfcol =c(1,1))
hist(alldata$age , xlab ="Age", prob =TRUE, main="Histogram", col = 'pink')
xfit <-seq (min(alldata$age),max(alldata$age),length=40)
yfit <-dnorm(xfit,mean=mean(alldata$age),sd=sd(alldata$age))

hist(marital , xlab ="marital status", prob =TRUE, main="Histogram", col = 'pink')

#Analyzing the plots of each x variable
par( mfcol =c(5,4))
plot(table(age), col = 'red')
plot(table(job), col = 'red')
plot(table(marital), col = 'red')
plot(table(education), col = 'red')
plot(table(default), col = 'red')
plot(table(housing), col = 'red')
plot(table(loan), col = 'red')
plot(table(contact), col = 'red')
plot(table(month), col = 'red')
plot(table(day_of_week), col = 'red')
plot(table(duration), col = 'red')
plot(table(campaign), col = 'red')
plot(table(pdays), col = 'red')
plot(table(previous), col = 'red')
plot(table(poutcome), col = 'red')
plot(table(emp.var.rate), col = 'red')
plot(table(cons.price.idx), col = 'red')
plot(table(cons.conf.idx), col = 'red')
plot(table(euribor3m), col = 'red')
plot(table(nr.employed), col = 'red')
par( mfcol =c(1,1))
plot(table(duration), col = 'blue')


#term deposit view
counts <- table(alldata$y)
barplot(counts,col=c("darkblue","red"),legend = rownames(counts), main = "Term Deposit")




#to check for missing values
head(is.na(alldata))
head(is.na(alldata))
any(data_knn)
#Data exploration
install.packages("Amelia")
library (Amelia)
(Amelia)
missmap(alldata,main="Missing Data - Bank Subscription", col=c("red","grey"),legend=FALSE)

## DATA VISUALISATION
## Box plots (Only for continuous variables)- To Check Ouliers
boxplot(alldata$age~alldata$y, main=" AGE",ylab="age of customers",xlab="term deposit", col = c("red","yellow"))
boxplot(alldata$campaign~alldata$y, main=" Campaign",ylab="Number of outbound calls performed during this campaign",xlab="term deposit", col=c("red","yellow"))
boxplot(alldata$pdays~alldata$y, main="pdays",ylab="Number of days passed since last contacting client for a PREVIOUS campaign",xlab="term deposit", col=c("red","yellow"))
boxplot(alldata$previous~alldata$y, main="previous",ylab="Number of outbound calls performed before this campaign and for this client",xlab="term deposit", col=c("red","yellow"))
boxplot(alldata$nr.employed~alldata$y, main=" nr.employed",ylab="Number of employees",xlab="term deposit",col= c("red","yellow"))
boxplot(alldata$emp.var.rate~alldata$y, main="emp.var.rate",ylab="Employment rate",xlab="term deposit", col=c("red","yellow"))
boxplot(alldata$cons.price.idx~alldata$y, main="cons.price.idx",ylab="Consumer price index",xlab="term deposit", col=c("red","yellow"))
boxplot(alldata$cons.conf.idx~alldata$y, main="cons.conf.idx",ylab="Consumer confidence index",xlab="term deposit",col= c("red","yellow"))
boxplot(alldata$euribor3m~alldata$y, main="euribor3m",ylab="Euribor rate",xlab="term deposit",col= c("red","yellow"))

#for categorical
barplot(table(alldata$job),col="red",main="JOB")
barplot(table(alldata$marital),col="red",main="Marital")
barplot(table(alldata$education),col="red",main="Education")
barplot(table(alldata$default),col="red",main="Credit Default")
barplot(table(alldata$housing),col="red",main="Housing")
barplot(table(alldata$loan),col="red",main="Loan")
barplot(table(alldata$contact),col="red",main="Contact")
barplot(table(alldata$month),col="red",main="Month")
barplot(table(alldata$poutcome),col="red",main="pOutcome")

#correlation
alldata.cont<-data.frame(alldata$age,alldata$campaign,alldata$pdays,alldata$previous,alldata$emp.var.rate,alldata$cons.price.idx,alldata$cons.conf.idx, alldata$euribor3m, alldata$nr.employed)
str(alldata.cont)
cor(alldata.cont)

#upsampling
x=alldata[1:20]
y=alldata$y
alldata_n=upSample(x, y, list = FALSE, yname = "y")
str(alldata_n)
table(alldata_n$y)
alldata_n = alldata[,-11] # Removing duration column

#Hypothesis Testing
age1 <- read.csv('../age1.csv') #age of people who said yes for term deposit
age2 <- read.csv('../age2.csv') #age of people who said yes for term deposit
age_y=age1$age
age_n=age2$age
hypo1 <- z.test(age_y, age_n, alternative= "less", mu = 0, sigma.x=sd(age_y), sigma.y=sd(age_x), conf.level=0.95)
hypo1

#Logistic Regression
# converting nominal to numerical variables
library(dummies)
data_knn=alldata_n
data_knn=dummy.data.frame(data_knn,names=c("job","marital","education","default","housing","loan","contact", "month", "day_of_week","poutcome"))
num.vars <- sapply(data_knn,is.numeric)
data_knn[num.vars] <- lapply(data_knn[num.vars],scale)
datasetu = data_knn
#Building model using all x-variables: upsampling
fullu = glm(Class~., datasetu, family = binomial())
summary(fullu)
#Building a base model with one x-variable and building a step-wise forward model using it: upsampling
baseu = glm(Class~age, datasetu, family = binomial())
forwardu = step(baseu, scope = list(upper = fullu, lower = ~1), direction = 'forward', trace = F)
summary(forwardu)
#Building a stepwise backward model: upsampling
backwardu = step(fullu, direction = 'backward', trace = F)
summary(backwardu)
install.packages('boot')
library(boot)
cv.glm(fullu, data = datasetu, K=10)$delta
cv.glm(baseu, data = datasetu, K=10)$delta
cv.glm(forwardu, data = datasetu, K=10)$delta
cv.glm(backwardu, data = datasetu, K=10)$delta

install.packages('pROC')
library(pROC)

#For full upsampled
pred_fullu = predict(fullu, datasetu[,-63])
head(pred_fullu)
#ROC AUC for full upsampled
head(lapply(pred_fullu, as.numeric)) # converting list to numeric
length(pred_fullu) # checked the length of the predicted values
roc_fullu <- roc(Class ~ as.numeric(pred_fullu), data = datasetu)
plot(roc_fullu, col = "red")
auc(roc_fullu)

#For stepwise forward upsampled
pred_forwardu = predict(forwardu, datasetu[,-63])
head(pred_forwardu)
#ROC AUC for full upsampled
head(lapply(pred_forwardu, as.numeric)) # converting list to numeric
length(pred_forwardu) # checked the length of the predicted values
roc_forwardu <- roc(Class ~ as.numeric(pred_forwardu), data = datasetu)
lines(roc_forwardu, col = "blue")
auc(roc_forwardu)

#For stepwise backward upsampled
pred_backwardu = predict(backwardu, datasetu[,-63])
head(pred_backwardu)
#ROC AUC for full upsampled
head(lapply(pred_backwardu, as.numeric)) # converting list to numeric
length(pred_backwardu) # checked the length of the predicted values
roc_backwardu <- roc(Class ~ as.numeric(pred_backwardu), data = datasetu)
lines(roc_backwardu, col = "green")
auc(roc_backwardu)


#KNN Model
# converting nominal to numerical variables
library(dummies)
data_knn=alldata_n
data_knn=dummy.data.frame(data_knn,names=c("job","marital","education","default","housing","loan","contact", "month", "day_of_week","poutcome"))
num.vars <- sapply(data_knn,is.numeric)
data_knn[num.vars] <- lapply(data_knn[num.vars],scale)
head(data_knn)
levels(data_knn)
str(data_knn)
data_knn$y = ifelse(data_knn$y== "no",1,0)
data_knn$y=factor(data_knn$y)
head(data_knn$y)
levels(data_knn$y)
x=data_knn[c(1:62)]
y=data_knn$y
knnmodel = train(x,y,'knn',trControl=trainControl(method='cv',number=10),tuneGrid=expand.grid(k = 1:20))
#Confusion Matrix
cm_knn <- predict(knnmodel,x)
tab_knn <- table(cm_knn, data_knn$y)
confusionMatrix(tab_knn)
#ROC KNN
roc_knn <- roc(y ~ as.numeric(cm_knn), data = data_knn)
plot(roc_knn, col = "red")
auc(roc_knn) # Area under curve

#Naive bayes
str(alldata_n)
data_nb = alldata_n
summary(age)# to analyze the numerical variable and make proper groups of each
summary(campaign)
summary(pdays)
summary(previous)
summary(emp.var.rate)
summary(cons.conf.idx)
summary(cons.price.idx)
summary(euribor3m)
summary(nr.employed)
#converting numerical variable to categorical for naive bayes classification
data_nb$age <- cut(data_nb$age, breaks = c(-Inf,32,38,47,Inf), labels= c("young", "middle-aged", "old", "very-old"), right = FALSE)
data_nb$campaign <- cut(data_nb$campaign, breaks = c(-Inf,2.568,Inf), labels= c("c1", "c2"), right = FALSE)
data_nb$pdays <- cut(data_nb$pdays, breaks = c(-Inf,962.5,Inf), labels= c("p1", "p2"), right = FALSE)
data_nb$previous <- cut(data_nb$previous, breaks = c(-Inf,0.173,Inf), labels= c("prev1", "prev2"), right = FALSE)
data_nb$emp.var.rate <- cut(data_nb$emp.var.rate, breaks = c(-Inf,0.08189,Inf), labels= c("emp1", "emp2"), right = FALSE)
data_nb$cons.conf.idx <- cut(data_nb$cons.conf.idx, breaks = c(-Inf,-40.5,Inf), labels= c("conf1", "conf2"), right = FALSE)
data_nb$cons.price.idx <- cut(data_nb$cons.price.idx, breaks = c(-Inf,93.58,Inf), labels= c("cons_price1", "cons_price2"), right = FALSE)
data_nb$euribor3m <- cut(data_nb$euribor3m, breaks = c(-Inf,3.621,Inf), labels= c("low", "high"), right = FALSE)
data_nb$nr.employed <- cut(data_nb$nr.employed, breaks = c(-Inf,5167, Inf), labels= c("n1", "n2"), right = FALSE)
str(data_nb)
#Building Naive Bayes model
library(caret)
data_nb <- data_nb[-11]# removing duration column
x=data_nb[1:19]
str(x)
y=data_nb$y
str(y)
nbmodel = train(x,y,'nb',trControl=trainControl(method='cv',number=10),na.action=na.pass)
print(nbmodel) 
#confusion matrix
cm_nb <- predict(nbmodel,x)
tab1 <- table(cm_nb, data_nb$y)
confusionMatrix(tab1)
#prediction
pred_nb <- predict(nbmodel, data_nb, type = 'prob')
#ROC curve for naive bayes
lapply(cm_nb[,2], as.numeric) # converting list to numeric
length(pred_nb[,2]) # checked the length of the predicted values
p1 = pred_nb[,2]
roc_nb <- roc(y ~ as.numeric(cm_nb), data = data_nb)
plot(roc_nb, col = "red")
auc(roc_nb) # Area under curve

#Decision tree
install.packages("partykit")
library(partykit)
tree_model <- ctree(y ~.,data = data_nb)
plot(tree_model)
ctrl_rf <- trainControl(method = "cv", number = 10)
predict_tree <- predict(tree_model, data=data_nb, trControl=ctrl_rf)
table(predict_tree)
confusionMatrix(table(predict_tree, data_nb$y))
library(pROC)
roc_tree <- roc(y ~ as.numeric(predict_tree), data = data_nb)
plot(roc_tree, col='red')
auc(roc_tree)

#Random Forest
install.packages("randomForest")
library(randomForest)
bank_rf = data_nb
ctrl_rf <- trainControl(method = "cv", number = 10)
model_rf2 <- randomForest(y~., data = data_nb, do.trace = TRUE, importance = TRUE, ntree = 500, mtry = 6, forest = TRUE, trControl = ctrl_rf)
predict_rf2 <- predict(model_rf2, data = data_nb, na.action = na.pass)
tab3 <- table(predict_rf2, data_nb$y)
confusionMatrix(tab3)
roc_rf2 <- roc(y ~ as.numeric(predict_rf2), data = data_nb)
plot(roc_rf2, col = "red")
auc(roc_rf2) # Area under curve

