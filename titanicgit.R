# getting working directory
getwd()

# setting working directory
setwd("D:\\GitHub\\Titanic\\Data")

# reading data
titanic <- read.csv("train.csv")

# viewing data
View(titanic)

# structure of the data
str(titanic)

## removing insignificant variables
titanic <- subset(titanic, select = c(2,3,5,6,7,8,10,12))

## checking if the variable are categorical or not
is.factor(titanic$Sex)
# [1] TRUE
is.factor(titanic$Embarked)
# [1] TRUE

#removing missing value by Knn Approach
library(DMwR)
?knnImputation
titanic <- knnImputation(titanic)


#spliting data into train and test 
dim(titanic)
# [1] 891   8
train <- titanic[1:800,]
test <- titanic[801:891,]

# fitting the logistic regression when considering all the predictors
basemodel <- glm(Survived~., family = binomial(link = "logit"), data = train)

# summary of the fitted model
summary(basemodel)

# analysis of variance table of the fitted model
anova(basemodel, test = "Chisq")

# fitting of logistic regression when considering only the statistically significant predictors
model <- glm(Survived~.-Parch-Fare-Embarked, family = binomial(link = "logit"),data = train)

# summary of the fitted model
summary(model)

# analysis of variance table of the fitted model
anova(model, test = "Chisq")

# prediction of the response on the basis of fitted model
predict <- predict(model,newdata = test,type = "response")

# checking the accuracy
library(caret)
predict <- ifelse(predict > 0.5,1,0)
error <- mean(predict != test$Survived)
print(paste('Accuracy',1-error))
# [1] "Accuracy 0.824175824175824"


# Skill-Up Scale-Up !!!
