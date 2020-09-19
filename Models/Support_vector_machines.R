library(tidyverse)
load("Data/titanic.rda")
library(e1071)
library(caret)
library(forcats)
names(dat)
dat <- dat %>% #select("Survived", "Pclass","Age","SibSp", "Embarked", Fare) %>% 
  filter(Embarked=="C" | Embarked=="S" | Embarked=="Q") %>%
  select(-Boarding)

dat <- dat%>% mutate(Embarked= droplevels(Embarked), 
                     Level = fct_explicit_na(Level, "Z"))


names(dat)
testind <- createDataPartition(dat$Survived,times=1, p=0.2, list=FALSE)
test <- dat[testind,]
train <- dat[-testind,]
levels(test$Embarked)


fit <- svm(Survived ~ .
           , kernel="linear",data=train, cost=1)
mean(predict(fit) == train$Survived)

#cv <- tune(svm , Survived~., data=train, kernel="linear" , 
#            ranges=list(cost=c(0.001,0.01,0.1,1,2,5,10,20)))
#mean(predict(cv$best.model) == train$Survived)
#NOT good, so lets try removing some features


#cv <- tune(svm , Survived~. -TNumber-Room-Level, data=train, kernel="linear" , 
#            ranges=list(cost=c(0.001,0.01,0.1,1,2,5,10,20)))
#mean(predict(cv$best.model) == train$Survived)


#LINEAR KERNEL DID NOT WORK, TRY NONLINEAR KERNEL
#mean(predict(cv$best.model) == train$Survived)
#cv <- tune(svm , Survived~. -TNumber-Room-Level, data=train, kernel="polynomial" , 
#            ranges=list(cost=c(0.01,0.1,1,2,5,7,10,15,20), 
#                        d=c(1,2,3,4)))
#mean(predict(cv$best.model) == train$Survived) #0.84, in improvement


#Radial Kernel
cv <- tune(svm , Survived~. , data=train, kernel="radial" , 
            ranges=list(cost=c(0.1,1,3,6,9,15), 
                        gamma=c(0.2,0.5,1,2,3,4,8)))
mean(predict(cv$best.model) == train$Survived)

#Nothing really worked