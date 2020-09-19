library(ggplot2)
library(tidyverse)

load("Data/titanic.rda")
#pairs(dat)
sapply(dat, table)
#table(dat$Survived)
dat$Survived
dat$Survived <- as.numeric(dat$Survived)
nums <- sapply(dat , class)[sapply(dat , class) == "numeric"]
num_dat <- dat %>% select(names(nums))

#there appear to be no linear correlations among the data, except parch and sibsp
#pairs_plot <- pairs(num_dat)
#cor(num_dat)

#3 position: stack, fill (for proportions) and dodge
fig1 <- dat %>% ggplot( aes(x=Sex, fill=factor(Survived))) + 
  geom_bar(position="fill") + ggtitle("Women Survived more often then Men") +
  ggsave("Women_surived_often.png")

fig2<-dat %>% ggplot( aes(x=Sex, fill=factor(Survived))) + 
  geom_bar(position="dodge") + 
  ggtitle("However Males died significantly more often in the 3rd Class") +
  facet_wrap(.~Pclass)+ ggsave("Third_class_men_died.png")

fig3 <- dat %>% ggplot( aes(x=Sex, y=Fare)) + 
  #geom_bar(stat = "identity",position="dodge") + 
  geom_boxplot(outlier.shape =NA)+  facet_grid(.~factor(Survived)) +
  geom_jitter(aes(color=Survived),position=position_jitter(width=.1)) +
#geom_jitter(aes(0.1))+
  ggtitle("People the survived paid more Fare")+
  ggsave("High_Fare_people_survived.png")


#we see that Boarding is a quite good predictor of Survival
  #however becuase there are so many different Boardings that will probably change in the test set,
  #out model may begain to overfit, so we will remove the boarding variable when training
t1 <- prop.table(table(dat$Survived, dat$Boarding),2)

#people with more siblings died, people with few siblings seem to no correlation with survival
  #as there results seem normal with 60-40 split between Died-Survived
t2 <- table(dat$Survived, dat$SibSp)
t3 <- prop.table( table(dat$Survived, dat$SibSp) ,2)

#people that have Level information seem to survive more often
t4 <- table(dat$Survived, dat$Level)


fig4<- dat %>% ggplot( aes(x=factor(Survived), y=TNumber, fill=Survived)) + 
  geom_boxplot(outlier.shape =NA)+
  ggtitle("Higher Ticket Numbers were more likely to die")+
  ggsave("Higher_tickets_died.png")

#qqnorm plots of Sibsp and Fare indicate they are not normal

#distributions of fares among levels appears to be different among survived and not survived
fig5 <- dat%>% mutate(Level=reorder(Level,Fare, FUN=median)) %>%
  ggplot(aes(Level, Fare, color=Level)) +ggtitle("Different distributions among Died vs Survived")+
  facet_grid(.~Survived)+geom_boxplot() +
  scale_y_continuous(limits = c( 0, 5.0))+
  ggsave("Levels_and_fairs.png")

#passengers at S survived quite often
table(dat$Embarked, dat$Survived)
#this is probably because a lot of men embarked from S
table(dat$Embarked , dat$Sex)

#other tables not shows, 1st class passengers were more likely to survive, Parch/children were more likely to survive


#looks like that removing the NA by replacing with zeros has shrunken the quartiles, perhaps variable should be removed
fig6 <- dat%>%
  ggplot(aes(factor(Survived), Room)) +ggtitle("Different room distributions among Died vs Survived")+
  geom_boxplot() +geom_jitter(aes(color=Survived),position=position_jitter(width=.1))+
  ggsave("Room_distributions.png")

dat %>% group_by(Survived) %>% ggplot(aes(Age,fill=Survived)) + 
  geom_density()

