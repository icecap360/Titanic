library(glmnet)
library(tidyverse)
library(boot)
library(MASS)
library(gridExtra)
library(heplots)
library(leaps)

load("Data/titanic.rda")
X <- model.matrix(Survived~., data= dat)
set.seed(1)
testind <- sample(1:nrow(dat) , 100)
test <- dat[testind,]
dat <- dat[-testind,]

#defining threshold function
best_thresh <- function(fit, d) {
  threshes <- seq(from=0.2,to=0.8,by=0.005)
  results <- sapply(threshes, function(t) {
    mean(d$Survived == ifelse(predict(fit, d, type="response")>t,1,0))})
  #plot(results)
  #print(max(results))
  threshes[which.max(results)]
}

#give predictions given a threshold
preds <- function(fit, d, t) {
  ifelse(predict(fit, d, type="response")>t,1,0)
}




set.seed(1)
fit <- glm(Survived ~ ., dat, family="binomial")
#summary(fit)

#remove boarding and levels
dat1 <- dat %>% dplyr::select(-Boarding, -Level)
test1 <- test %>% dplyr::select(-Boarding, -Level)
fit <- glm(Survived ~ . , dat1, family="binomial")
#summary(fit) #removing levels and boarding leads to lower AIC
error <- mean(test1$Survived == preds(fit, test1, best_thresh(fit, dat1))) 
print(error)
#summary(fit) #we see that by removing boarding we have reduced the AIC

cv.fit <- cv.glm(data = dat %>% dplyr::select(-Boarding, -Level), fit , K=5)
error <- cv.fit$delta[2] #14% error rate, not bad
#plotting the residuals that there is non-linearity in the data. we 
fit <- glm(Survived~poly(Fare,3)+poly(Age,3)+.,dat1, family = binomial)
# seeing the pvalues, age polynomial terms seem to be provide useful pvalues
fit <- glm(Survived~. + poly(Age,3)+poly(Room,2), dat1, family=binomial)
#so polynomial terms for room helps
fit <- glm(Survived~.+poly(Age,3)+poly(Room,2)+Embarked:Sex+Parch:Embarked, 
           dat1, family=binomial)
#pvalues show no improvement
fit <- glm(Survived~.+poly(Age,3)+poly(Room,2)+Embarked:Parch+Parch:SibSp, 
           dat1, family=binomial)
#pvalues show no improvement
fit <- glm(Survived~.+poly(Age,3)+poly(Room,2)+Pclass:Sex+Sex:Parch, 
           dat1, family=binomial)
#these interaction terms show improvement
mean(test1$Survived==preds(fit,test1,best_thresh(fit,dat1)))
#in the end no test sset accuracy improved


#LDA AND QDA
#checking the assumptions, first the equal variance assumption within classes
#the fares and TNumber show that this assumption is wrong
fig1 <- dat1 %>% ggplot(aes( y = Fare, fill = Survived)) +
  geom_boxplot(alpha = 0.2) 
fig2 <- dat1 %>% ggplot(aes( y = TNumber, fill = Survived)) +
  geom_boxplot(alpha = 0.2) 
lev <- leveneTest(Room ~ Survived, dat1) #  leventest shows that the variances are not equal for Age,Room and Far

#checking equal covariance assumption, there are countless examples of where this assumption is false1!
num_cols <- c("Age","SibSp","Parch","Fare","TNumber","Room")
heplots::covEllipses(dat1[,num_cols], 
                     dat1$Survived, 
                     fill = TRUE, 
                     pooled = FALSE, 
                     col = c("blue", "red"), 
                     variables = num_cols, 
                     fill.alpha = 0.2)
boxm <- heplots::boxM(dat1[,num_cols], dat1$Survived) # using columns 1 to 5 and 8
#pvalue is less then 0.05, so we know that this assumption is false

#The normal assumption we know is false from the data visualizations
#but I will try lda and qda anyway
fit <- lda(Survived ~ ., dat1)
mean(predict(fit)$class == dat1$Survived)

#fit <- qda(Survived ~ ., dat1) #rank deficiency with qda