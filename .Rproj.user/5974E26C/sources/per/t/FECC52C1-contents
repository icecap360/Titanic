library(tree)
library(tidyverse)
library(randomForest)
library(glmnet)
set.seed(1101)

#finds best threshold
best_thresh <- function(fit, x,y,s) {
  threshes <- seq(from=0.2,to=0.8,by=0.005)
  results <- sapply(threshes, function(t) {
    mean(y == ifelse(predict(fit, x, type="response",s=s)>t,1,0))})
  #plot(results)
  #print(max(results))
  threshes[which.max(results)]
}
#give predictions given a threshold
preds <- function(fit, d, t, lam) {
  ifelse(predict(fit, d, s=lam,type="response")>t,1,0)
}


load("data/titanic.rda")
set.seed(1)
X <- model.matrix(Survived~.,dat)[,-1]
y <- dat$Survived
testind <- sample(1:nrow(dat) , 0.2*nrow(dat))
testx <- X[testind,]
trainx <- X[-testind,]
testy <- y[testind]
trainy <- y[-testind]

#RIDGE
lambda <- seq(2,18)
lambda <- 2^(-lambda)
fit <- glmnet(trainx,trainy,alpha=0, lambda=lambda, family="binomial")
summ <- summary(fit) #plotting the deviance index 6 or 7 seems to be best

t <- best_thresh(fit,trainx,trainy,0.004)
mean(preds(fit,testx,t,0.004) == testy)
table(testy, preds(fit,testx,t,0.004)) #0.82 is the accuracy


#LASSO
fit <- glmnet(trainx,trainy,alpha=1, lambda=lambda, family="binomial")
summ <- summary(fit) #plotting the deviance index 6 or 7 seems to be best
t <- best_thresh(fit,trainx,trainy,0.004)
mean(preds(fit,testx,t,0.004) == testy)
table(testy, preds(fit,testx,t,0.004)) #0.83 is the accuracy




fit <- cv.glmnet(X,y,alpha=0,family="binomial", lambda = lambda)
fit$lambda.min
t <- best_thresh(fit,trainx,trainy,fit$lambda.min)
mean(preds(fit,testx,t,fit$lambda.min) == testy) #0.85 test! NEW RECORD!
table(testy, preds(fit,testx,t,fit$lambda.min)) 




fit <- cv.glmnet(X,y,alpha=1,family="binomial", lambda = lambda)
fit$lambda.min
t <- best_thresh(fit,trainx,trainy,fit$lambda.min)
mean(preds(fit,testx,t,fit$lambda.min) == testy) #0.84 test! Very good
table(testy, preds(fit,testx,t,fit$lambda.min)) 



#trying NONLINEARITY
X <- model.matrix(Survived~.+poly(Age,3)+poly(Fare,3)+Embarked:Sex+
                    Pclass:Sex+Pclass:Embarked+SibSp:Pclass+Pclass:Sex,dat)[,-1]
y <- dat$Survived
testind <- sample(1:nrow(dat) , 0.2*nrow(dat))
testx <- X[testind,]
trainx <- X[-testind,]
testy <- y[testind]
trainy <- y[-testind]

fit <- cv.glmnet(X,y,alpha=0,family="binomial", lambda = lambda)
fit$lambda.min
t <- best_thresh(fit,trainx,trainy,fit$lambda.min)
mean(preds(fit,testx,t,fit$lambda.min) == testy) #0.85 test! NEW RECORD!
table(testy, preds(fit,testx,t,fit$lambda.min)) 

fit <- cv.glmnet(X,y,alpha=1,family="binomial", lambda = lambda)
fit$lambda.min
t <- best_thresh(fit,trainx,trainy,fit$lambda.min)
mean(preds(fit,testx,t,fit$lambda.min) == testy) #0.87 test! NEW RECORD!
table(testy, preds(fit,testx,t,fit$lambda.min)) 
y <- dat$Survived