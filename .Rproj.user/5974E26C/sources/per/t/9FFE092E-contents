library(tree)
library(tidyverse)
library(randomForest)
library(glmnet)
set.seed(1101)

library(tree)
library(tidyverse)
library(randomForest)
library(glmnet)
set.seed(1101)
load("Data/titanic.rda")
load("Data/test.rda")
dat <- dat %>% select(-Boarding,-Level) %>%  
  mutate(Embarked = fct_explicit_na(Embarked, "W")) %>%
  filter(Embarked!="W") %>% 
  mutate(Embarked = droplevels(Embarked))
datt <- datt %>% select(-Boarding,-Level) 
ids <- datt$PassengerId
datt$PassengerId <- NULL


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
  ifelse(predict(fit, newx=d, s=lam,type="response")>t,1,0)
}


set.seed(1)
X <- model.matrix(Survived~.+poly(Age,2)+poly(Fare,2)+Embarked:Sex+
     Pclass:Sex+Pclass:Embarked+SibSp:Pclass+Pclass:Sex,dat)[,-1]
  
  #model.matrix(Survived~.,dat)[,-1]
y <- dat$Survived
lambda <- seq(2,18)
lambda <- 2^(-lambda)

ridge <- cv.glmnet(X,y,alpha=0,family="binomial", lambda = lambda)
lam1 <- ridge$lambda.min
t1 <- best_thresh(ridge,X,y,lam1)

lasso <- cv.glmnet(X,y,alpha=1,family="binomial", lambda = lambda)
lam2 <- lasso$lambda.min
t2 <- best_thresh(lasso,X,y,lasso$lambda.min)


datt$Survived <- 1
datt <-datt %>% select(Survived, everything())
newX <- model.matrix(Survived~.+poly(Age,2)+poly(Fare,2)+Embarked:Sex+
      Pclass:Sex+Pclass:Embarked+SibSp:Pclass+Pclass:Sex,datt)[,-1]
pre<-as.vector(ifelse(
  predict(lasso,newX, s=lam2,type="response")>t2,1,0))
answer<- data.frame(
  PassengerId=ids,Survived = pre)
write.csv(answer, "Final_Model/testans.csv", row.names = F)

