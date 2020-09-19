library(skimr)
library(caret)
library(tidyverse)
library(car)
library(leaps)
set.seed(42)
load("Data/titanic.rda")
#skim(dat)

#reshuffling the rows and making labels numeric
rows <- sample(nrow(dat))
dat <- dat[rows,]
dat$Survived<- as.numeric(dat$Survived)-1
dat$Survived

#baseline model
fit <- lm(Survived ~., dat)
preds <- predict(fit, dat)

#selecting threshold
threshs <- seq(0.5, 2.0, length=500)
prds <- sapply(threshs, function(t){ ifelse(preds>t, 1, 0)})
results <- apply(prds, 2, function(p) {mean(p == dat$Survived)})
t.best <- threshs[which.max(results)]

#calculating test error using cross validation
ind <- sample(1:10, size=nrow(dat),replace=TRUE)
errors <- sapply(1:10, function(k){
  indts <- k==ind
  train <- dat[-indts,]
  test <- dat[indts,]
  fit <- lm(Survived ~., train)
  preds <- predict(fit, test)
  print(mean(test$Survived == 
         ifelse(preds>t.best,1,0)))
})
test_acc <- mean(errors)



#Conclusion: 0.83% accuracy, pretty good for a baseline model



#interpreting the coefficients, let significance value be 0.05
#Fstat indicates that the null hypothesis is false
#all of boardings features seem to be quite unhelpful
#only one level has a significant pvalue
#sexmale, age, sibsp, embarked, and pclass are the only useful variables
#repeating model without levels and boarding
dat_2 <- dat %>% select(-Boarding, -Level)
fit2 <- lm(Survived ~., dat_2)
preds <- predict(fit2, dat_2)
threshs <- seq(0.5, 2.0, length=500)
prds <- sapply(threshs, function(t){ ifelse(preds>t, 1, 0)})
results <- apply(prds, 2, function(p) {mean(p == dat_2$Survived)})
t.best <- threshs[which.max(results)]
ind <- sample(1:10, size=nrow(dat_2),replace=TRUE)
errors <- sapply(1:10, function(k){
  indts <- k==ind
  train <- dat_2[-indts,]
  test <- dat_2[indts,]
  fit2 <- lm(Survived ~., train)
  preds <- predict(fit2, test)
  print(mean(test$Survived == 
               ifelse(preds>t.best,1,0)))
})
test_acc <- mean(errors)# no significant gain

#fstat to test if boarding, levels, Parch, TNumber and Room are relevant
dat_3 <- dat %>% select(-Boarding, -Level, -Parch, -TNumber, -Room)
fit2 <- lm(Survived ~., dat_3)
preds <- predict(fit2, dat_3)
threshs <- seq(0.5, 2.0, length=500)
prds <- sapply(threshs, function(t){ ifelse(preds>t, 1, 0)})
results <- apply(prds, 2, function(p) {mean(p == dat_3$Survived)})
t.best <- threshs[which.max(results)]
ind <- sample(1:10, size=nrow(dat_3),replace=TRUE)
errors <- sapply(1:10, function(k){
  indts <- k==ind
  train <- dat_3[-indts,]
  test <- dat_3[indts,]
  fit2 <- lm(Survived ~., train)
  preds <- predict(fit2, test)
  print(mean(test$Survived == 
               ifelse(preds>t.best,1,0)))
})
test_acc <- mean(errors)
rss0 <- sum(summary(fit2)$residuals^2)
rss <- sum(summary(fit)$residuals^2)
df_num <- 5
df_den <- ncol(dat)-1-ncol(dat_3)
fstat <- ((rss0-rss)/df_num)/(rss/(df_den)) 
pval <- pf(fstat, df_num, df_den) #we cannot remove such variables as pvalue is below significance

#checking our previous fit
#par(mfrow = c(2,2))
#plot(fit)
#outlierTest(fit)
#influencePlot(fit, id.method="identify", main="Influence Plot", sub="Circle size proportial to Cook's Distance" )
#vif(fit)
#1. nonlinearity is present in residual fit plot
#2. no outliers
#3. equal variance assumption is false
#5. vif shows no presence of collinierity
#6. point 43, 804, 719 seem to be high leverage points


#first lets deal with the high leverage points
dat_trim <- dat[c(-43,-719,-804),]
fit2 <- lm(Survived ~., dat_trim)
preds <- predict(fit2, dat_trim)
threshs <- seq(0.5, 2.0, length=500)
prds <- sapply(threshs, function(t){ ifelse(preds>t, 1, 0)})
results <- apply(prds, 2, function(p) {mean(p == dat_3$Survived)})
t.best <- threshs[which.max(results)]
ind <- sample(1:10, size=nrow(dat_trim),replace=TRUE)
errors <- sapply(1:10, function(k){
  indts <- k==ind
  train <- dat_trim[-indts,]
  test <- dat_trim[indts,]
  fit2 <- lm(Survived ~., train)
  preds <- predict(fit2, test)
  print(mean(test$Survived == 
               ifelse(preds>t.best,1,0)))
})
test_acc <- mean(errors) #looks like the leverage points are important


#Adding more features , forward
fit <- regsubsets(Survived~
  poly(SibSp, 2) +ma
  poly(TNumber, 2)+ poly(Age, 2) +
  Pclass*Room + Embarked*Age +Embarked*Sex+ Boarding*Pclass+
  Level*Embarked+ Sex*Age, dat, method="backward",
  nvmax=25)
dat_mat <- model.matrix(
  Survived~poly(SibSp, 2) + 
    poly(TNumber, 2)+ poly(Age, 2) +
    Pclass*Room + Embarked*Age +Embarked*Sex+ Boarding*Pclass+
    Level*Embarked+ Sex*Age , data = dat)
#14th seems to be the best
coefi <- coefficients(fit, id=14)
preds =dat_mat[,names(coefi)]%*% coefi
threshs <- seq(-2.0, 2.0, length=2000)
prds <- sapply(threshs, function(t){ ifelse(preds>t, 1, 0)})
results <- apply(prds, 2, function(p) {mean(p == dat$Survived)})
t.best <- threshs[which.max(results)]
test_acc <- mean(dat$Survived == ifelse(preds>t.best,1,0))
test_acc #could not find anything of better then base model
