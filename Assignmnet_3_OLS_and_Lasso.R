rm(list =ls())
install.packages("corrplot")
install.packages("ISLR")
install.packages("pROC")
install.packages("glmnet")
library(ISLR)
library("corrplot")
library("pROC")
library("glmnet")
dat=Caravan.csv<-read.csv(file.choose(), header=TRUE)
dim(dat)
dat$Purchase<-ifelse(dat$Purchase=="Yes",1,0)
dat<-subset(dat, select = -c(X))
head(dat)
dim(dat)
names(dat)
summary(dat)
table(dat$Purchase)
mean(dat$Purchase)
X <- scale(dat[,1:85])
tst <- 1:1000
X.trn <- X[-tst,]
X.tst <- X[tst,]
Y.trn <- dat[-tst,86]
Y.tst <- dat[tst,86]

mean(Y.trn)
mean(Y.tst)

train <- dat[-tst,]
test <- dat[tst,]

dim(X.trn)
dim(X.tst)
length(Y.trn)
length(Y.tst)

X<-dat[,1:85]
summary(X)

corrplot(cor(X), method='color')

form1 <- formula(Purchase~.)
g1 <- glm(form1, family="binomial", data=train)
summary(g1)

yhat.g1 <- predict(g1, test, type="response")
pred.g1 <- rep(0,1000)
pred.g1[yhat.g1 > .5] <- 1

table(pred.g1,Y.tst)
TPR(pred.g1,Y.tst)

g1.roc <- roc(Y.tst, yhat.g1, direction="<")
g1.roc

plot(g1.roc, lwd=3)

X <- as.matrix(train[,1:85])
Y <- train[,86]
X.tst <- as.matrix(test[,1:85])
cv1 <- cv.glmnet(X, Y, family="binomial")
gn1 <- glmnet(X, Y, family="binomial")

par(mfrow=c(1,2))
plot(gn1, lwd=3)
plot(cv1)

lambda <- cv1$lambda.min
lambda

beta <- coef(gn1, s=lambda)
c(beta[,1],coef(g1))

yhat.gn1 <- predict(gn1, X.tst, s=lambda, type="response")
pred.gn1 <- rep(0,1000)
pred.gn1[yhat.gn1 > .5] <- 1

table(pred.gn1,Y.tst)

gn1.roc <- roc(Y.tst, yhat.gn1, direction="<")
gn1.roc
plot(gn1.roc, lwd=3)
