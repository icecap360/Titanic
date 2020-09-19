library(tidyverse)
dat <- read_csv("Data/Titanic.csv")

#finding out type of each column
classes <- sapply(dat, class)[sapply(dat, class)=="character"]

#cabin, name, sex, embarked and ticket are characters
#hence convert them to factors

#however I would rather just remove the name column
dat <- dat %>% select(-Name, -PassengerId)

classes <- classes[names(classes)!="Name"]
char_dat <- dat %>% select(names(classes))

#we see that there are a lot of NA's in Cabin, 687 in total, but that still leaves about 200 that may be useful
sum(is.na(dat$Cabin))
#However there mostly unique cabins
length(unique(dat$Cabin))
#So I will split the cabin into levels (which will be a letter) and room number (of numeric type)
cabin_split <- str_match(dat$Cabin , "^([A-Z]\\s?[A-Z]?)(\\d+)")
unmatched <- setdiff(dat$Cabin , str_subset(dat$Cabin , "^([A-Z]\\s?[A-Z]?)(\\d+)"))

char_dat$Level <-  cabin_split[,2]
char_dat$Room <- cabin_split[,3]
char_dat$Room <- parse_number(char_dat$Room)
#Now do the same for ticket
tickets_split <- str_match(char_dat$Ticket , "(STON/O\\s2\\.)\\s([0-9]*)|([A-Z.0-9/]*)\\s?([0-9]*)")
char_dat$Boarding <-  ifelse( is.na(tickets_split[,2]) , tickets_split[,4],tickets_split[,2])
char_dat$TNumber <- ifelse( is.na(tickets_split[,3]) , tickets_split[,5],tickets_split[,3])
char_dat$TNumber <- ifelse(is.na(as.numeric(char_dat$TNumber)) & 
                             !is.na(as.numeric(char_dat$Boarding)), char_dat$Boarding , char_dat$TNumber)
char_dat$Boarding <- ifelse(char_dat$Boarding==char_dat$TNumber, "", char_dat$Boarding)
char_dat$Boarding <- ifelse(char_dat$Boarding=="", NA ,char_dat$Boarding)
char_dat$TNumber <- parse_number(char_dat$TNumber)
char_dat$TNumber[is.na(char_dat$TNumber)] <- 0

#Remove numeric and proccessed columns from the char dataframe
dat$TNumber <- char_dat$TNumber
dat$Room <- char_dat$Room
char_dat <- char_dat %>% select(-TNumber , -Room, -Cabin, -Ticket)

#convert the chardat to factors
#char_dat <- sapply(char_dat, as.factor)

#finally replace the data in the original data with char_dat
dat <- dat %>% select(-names(classes))
dat <- cbind(dat, char_dat)
dat <- as_tibble(dat)
dat <- dat %>% mutate_if(is.character,factor)
dat <- dat %>% mutate_if(function(x){ is.factor(x) & anyNA(x)}, addNA)

dat$Survived <- factor(dat$Survived)
dat$Pclass <- ordered(dat$Pclass)

#replace numeric is.na with average
dat <- dat %>% mutate_if(is.numeric, function(x) {
  avg <- mean(x, na.rm = TRUE)
  ifelse(is.na(x), avg, x)
})

#scale numeric columns
dat <- dat %>% mutate_if(is.numeric, ~(scale(.) %>% as.vector))


save(dat, file = "Data/titanic.rda")