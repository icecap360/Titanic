library(glmnet)
library(tidyverse)
library(boot)
library(leaps)
library(splines)
library(gam)

load("Data/titanic.rda")
set.seed(4)
testind <- sample(1:nrow(dat) , 100)
dat$Outcome <- as.numeric(dat$Survived) -1
dat <- dat %>% select(-Survived)
names(dat)
test <- dat[testind,]
dat <- dat[-testind,]
best_thresh <- function(fit, d) {
  threshes <- seq(from=0.2,to=0.8,by=0.005)
  results <- sapply(threshes, function(t) {
    mean(d$Outcome == ifelse(predict(fit, d)>t,1,0))})
  threshes[which.max(results)]
}
preds <- function(fit, d, t) {
  ifelse(predict(fit, d)>t,1,0)
}



model_base <- lm(Outcome~. ,dat)
model_polynomial <- lm(Outcome~ . +poly(Age,3,raw = T) +
                         poly(Room,raw = T),dat)
anova(model_base,model_polynomial) #the pvalue shows that ppolynomial features provide improvment


model_base <- lm(Outcome ~. ,dat)
model_interaction <- lm(Outcome ~ . +Sex:Embarked + Sex:SibSp 
                       +Pclass:Sex + Parch:Embarked + Parch:Pclass,dat)
anova(model_base,model_interaction) #the pvalue shows that ppolynomial features are improving the model


#combining the model
model_base <- lm(Outcome ~. ,dat)
model_polynomial_interaction <- lm(Outcome ~ . +Sex:Embarked + Sex:SibSp 
                        +Pclass:Sex + Parch:Embarked + Parch:Pclass+
                          poly(Age,3,raw = T) + poly(Fare,3,raw = T)+
                          poly(Room,raw = T),dat)
anova(model_base,model_polynomial_interaction) #the pvalue shows that ppolynomial features are improving the model
t <- best_thresh(model_polynomial_interaction,dat)
mean(test$Outcome == preds(model_polynomial_interaction,test,0.5))

#adding cut points
model_base <- lm(Outcome ~. ,dat)
model_cut_points <- lm(Outcome ~ .+ cut(Fare,3)+cut(Age,3),dat)
anova(model_base,model_cut_points) #the pvalue shows that adding cutpoints does not yield significant new knowledge


#trying loess regression
model_base <- lm(Outcome ~. ,dat)
model_loess <- loess(Outcome ~ Age+TNumber+Fare+Room,dat)
mean(ifelse(predict(model_loess, test)>0.5,1,0)==test$Outcome)


#trying basis functions
model_base <- lm(Outcome ~. ,dat)
model_bs <- lm(Outcome~. + bs(Age,df=6)+bs(Fare,df=6),
               dat)
t <- best_thresh(model_bs,dat)
mean(ifelse(predict(model_loess, test)>t,1,0)==test$Outcome) #loss in accuracy


#trying natural splines
model_base <- lm(Outcome ~ . ,dat)
model_bs <- lm(Outcome ~ . + ns(Age,df=6)+ns(Fare,df=6),
               dat)
t <- best_thresh(model_bs,dat)
mean(ifelse(predict(model_bs, test)>t,1,0)==test$Outcome) #no gain


#trying smoothing functions
model_base <- lm(Outcome ~ . ,dat)
model_ns <- lm(Outcome ~ . + ns(Age,df=6)+ns(Fare,df=6),
               dat)
t <- best_thresh(model_ns,dat)
mean(ifelse(predict(model_ns, test)>t,1,0)==test$Outcome) #no gain
