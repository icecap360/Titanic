library(tidyverse)
library(tree)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)
library(caret)
load("Data/titanic.rda")
set.seed(1)
testind <- sample(1:nrow(dat), 100)
train <- dat[-testind,]
test <- dat[testind,]

class(dat$Survived)
fit <- rpart(Survived~ .,train, method = "class",minsplit = 2, 
             minbucket = 1, 
             cp = -1) #this way tree grows fully
best_cp <- fit$cp[which.min(fit$cp[,"xerror"])]
fit <- prune.rpart(fit,best_cp)
mean(predict(fit,train %>% select(-Survived), type = "class") == train$Survived)
#very good, 87%

#cross validating for best cp
#fit <- train(Survived ~ .,method = "rpart",
#             tuneGrid = data.frame(cp = 2^(seq(-10,-1,1))),
#             data = train)
#confusionMatrix(predict(fit), train$Survived)$overall["Accuracy"] 
#rpart library performs better then cross validated caret!


#BAGGING
fit <- randomForest(Survived ~ . , data=train, 
                    mtry=ncol(train)-1 , ntree=200)
mean(predict(fit) == train$Survived)


#RANDOMFORESTS
control <- trainControl(method='repeatedcv', 
                        number=5, 
                        #repeats=3, 
                        search='random')

#grid <- expand.grid(mtry =c(5,7,9,11)
#fit <- train(Survived ~ .,method = "rf",
#             tuneGrid = grid,
#             data = train, trControl=control, 
#             n.trees=100)
#confusionMatrix(predict(fit), train$Survived)$overall["Accuracy"]
#Accuracy is still 0.83, no significant improvements
#however specificity is even with sensitivity


#BOOSTING
control <- trainControl(method='repeatedcv', 
                        number=10, 
                        #repeats=3, 
                        search='random', allowParallel=T)
gbmGrid <-  expand.grid(interaction.depth = c(5,8,11), 
                        n.trees = (1:15)*50, 
                        shrinkage = 10^seq(-1, -5, -1),
                        n.minobsinnode = 20)
fit <- train(Survived ~ .,method = "gbm",
             tuneGrid = gbmGrid,
             data = train, trControl=control)
trellis.par.set(caretTheme())
plot(fit, metric = "Kappa", plotType = "level",
      scales = list(x = list(rot = 90)))
#no significant improvement
