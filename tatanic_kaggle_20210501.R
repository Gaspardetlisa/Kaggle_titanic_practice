# Try to predict survival on the Titanic
# data cleaning and data mining part.
train <- read.csv('titanic/train.csv')
test  <- read.csv('titanic/test.csv')

library(dplyr)
combine  <- bind_rows(train, test) # combine two csv in order to explore dataframe

print(names(combine)) # explore name of each columns
print(dim(combine)) # explore the shape of dataframe
print(str(combine)) # explore content of dataframe

# Replace NULL into NA in order to clean data
combine$Pclass[combine$Pclass == ""] <- NA
combine$Name[combine$Name == ""] <- NA
combine$Sex[combine$Sex == ""] <- NA
combine$SibSp[combine$SibSp == ""] <- NA
combine$Parch[combine$Parch == ""] <- NA
combine$Ticket[combine$Ticket == ""] <- NA
combine$Fare[combine$Fare == ""] <- NA
combine$Cabin[combine$Cabin == ""] <- NA
combine$Embarked[combine$Embarked == ""] <- NA

# Check NA in each columns
print(sum(is.na(combine$PassengerId))) #0
print(sum(is.na(combine$Pclass))) #0
print(sum(is.na(combine$Name))) #0
print(sum(is.na(combine$Sex))) #0
print(sum(is.na(combine$Age))) #263, this can be solved by fill NA.
print(sum(is.na(combine$SibSp))) #0
print(sum(is.na(combine$Parch))) #0
print(sum(is.na(combine$Ticket))) #0
print(sum(is.na(combine$Fare))) #1, discard this data
print(sum(is.na(combine$Cabin))) #1014, too many, maybe discard this time
print(sum(is.na(combine$Embarked))) #2, discard these data

df <- data.frame(combine$PassengerId,combine$Survived,combine$Pclass,combine$Sex,combine$Age,combine$SibSp,combine$Parch,combine$Ticket,combine$Fare,combine$Embarked)

library(tidyr)
df <- df %>% drop_na(combine.Fare, combine.Embarked) # drop NA of Fare and Embarked

print(xtabs(~ combine.Pclass + combine.Survived , data = df)) #Survived (1) or died (0)
print(cor.test(df$combine.Survived,df$combine.Pclass))# cor=-0.3, weak correlation

print(xtabs(~ combine.SibSp + combine.Survived , data = df))#Survived (1) or died (0)
print(cor.test(df$combine.Survived,df$combine.SibSp))# cor=-0.03, no correlation

print(xtabs(~ combine.Parch + combine.Survived , data = df))#Survived (1) or died (0)
print(cor.test(df$combine.Survived,df$combine.Parch))# cor=0.08, no correlation

# Investigate chisq.test between Sex and Survival
barplot(table(df$combine.Sex))
print(chisq.test(table(df$combine.Survived,df$combine.Sex)))# (p<0.05) 
print(xtabs(~ combine.Sex + combine.Survived , data = df))#Survived (1) or died (0)
# Since train$Age shows up NA, it has to fill NA.
print(summary(df$combine.Age))  # Search for the best strategy to fill NA.
plot(y=df$combine.PassengerId,x=df$combine.Age, main="Scatter plot of Age",pch=2)
print(cor.test(df$combine.Survived,df$combine.Age))# cor=-0.07, no correlation

# Use mean value to fill NA.
df$combine.Age[is.na(df$combine.Age)] <-mean(df$combine.Age,na.rm=TRUE)
print(cor.test(df$combine.Survived,df$combine.Age))# cor=-0.07, no correlation

# Change categorical columns into numeric columns, such as combine$Sex...etc.
df$combine.Sex <- unclass(factor(df$combine.Sex))
df$combine.Ticket <- unclass(factor(df$combine.Ticket))

print(xtabs(~ combine.Embarked  + combine.Survived , data = df))
df$combine.Embarked <- unclass(factor(df$combine.Embarked))
print(cor.test(df$combine.Survived,df$combine.Embarked))# cor=-0.169, weak correlation

View(df)

#It seems the best strategy of this dataset to predict the survival would be Gender.
#Pclass and Embarked may be useful, but weak related.
