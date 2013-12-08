library(discretization)
library(stringr)

train <- read.csv("train.csv")
test <- read.csv("test.csv")

jr <- median(train$Age[complete.cases(train)][grep("Jr",train$Name[complete.cases(train)])])
mr <- median(train$Age[complete.cases(train)][grep("Mr",train$Name[complete.cases(train)])])
mrs <- median(train$Age[complete.cases(train)][grep("Mrs",train$Name[complete.cases(train)])])
miss <- median(train$Age[complete.cases(train)][grep("Miss",train$Name[complete.cases(train)])])
master <- median(train$Age[complete.cases(train)][grep("Master",train$Name[complete.cases(train)])])
sir <- median(train$Age[complete.cases(train)][grep("Sir",train$Name[complete.cases(train)])])

#individual 153 imputation
test[153, "Fare"] <- 0 

# Age imputation for training data
uncomplete <- complete.cases(train)==FALSE
train$Age[uncomplete][grep("Jr",train$Name[uncomplete])] <- jr
train$Age[uncomplete][grep("Mr",train$Name[uncomplete])] <- mr 
train$Age[uncomplete][grep("Mrs",train$Name[uncomplete])] <- mrs 
train$Age[uncomplete][grep("Miss",train$Name[uncomplete])] <- miss 
train$Age[uncomplete][grep("Master",train$Name[uncomplete])] <- master 
train$Age[uncomplete][grep("Sir",train$Name[uncomplete])] <- sir
train$Age[is.na(train$Age)] <- median(train$Age, na.rm=TRUE)

# Age imputation for testing data
uncomplete <- complete.cases(test)==FALSE
test$Age[uncomplete][grep("Jr",test$Name[uncomplete])] <- jr
test$Age[uncomplete][grep("Mr",test$Name[uncomplete])] <- mr 
test$Age[uncomplete][grep("Mrs",test$Name[uncomplete])] <- mrs 
test$Age[uncomplete][grep("Miss",test$Name[uncomplete])] <- miss 
test$Age[uncomplete][grep("Master",test$Name[uncomplete])] <- master 
test$Age[uncomplete][grep("Sir",test$Name[uncomplete])] <- sir
test$Age[is.na(test$Age)] <- median(train$Age, na.rm=TRUE)

# Age binning for testing data then training data
brk <- c(0, as.numeric(chiM(data.frame(train$Age, train$Survived))$cutp[[1]]), max(train$Age))
test$Age <- cut(test$Age, breaks=brk, include.lowest=FALSE)
train$Age <- cut(train$Age, breaks=brk, include.lowest=FALSE)

# Add simple features
test$Alone <- (test$SibSp + test$Parch)==0
test$PosFare <- test$Fare>0
train$Alone <- (train$SibSp + train$Parch)==0
train$PosFare <- train$Fare>0
train$Pclass <- as.factor(train$Pclass)
train$Alone <- as.factor(train$Alone)
train$PosFare <- as.factor(train$PosFare)

# Add complex features
extrName <- function(x) {
  str_split(x, ',')[[1]][1]
}
test$Family <- "not a family"
test$Family[(test$SibSp + test$Parch)>=3] <- sapply(test$Name[(test$SibSp + test$Parch)>=3],FUN=extrName)
train$Family <- "not a family"
train$Family[(train$SibSp + train$Parch)>=3] <- sapply(train$Name[(train$SibSp + train$Parch)>=3], FUN=extrName)
train$Family <- as.factor(train$Family)
tab <- table(train$Family,train$Survived)
survived <- row.names(tab)[tab[, 1]==0]
died <- row.names(tab)[tab[, 2]==0]

train$FamSurvival <- "undefined"
train$FamSurvival[train$Family %in% survived] <- "survived"
train$FamSurvival[train$Family %in% died] <- "died"
train$FamSurvival <- as.factor(train$FamSurvival)
test$FamSurvival <- "undefined"
test$FamSurvival[test$Family %in% survived] <- "survived"
test$FamSurvival[test$Family %in% died] <- "died"

extrStatus <- function(x) {
  where <- str_locate_all(x, c("\\,","\\."))
  substr(x, where[[1]][1] + 2, where[[2]][1] - 1)
}
test$Status <- sapply(test$Name, FUN=extrStatus)
train$Status <- sapply(train$Name, FUN=extrStatus)
train$Status <- as.factor(train$Status)

# to force classification for random forest
train$Survived <- as.factor(train$Survived)
maxFare <- max(train$Fare)
maxSibSp <- max(train$SibSp)
maxParch <- max(train$Parch)
train$Fare <- train$Fare/maxFare
train$SibSp <- train$SibSp/maxSibSp
train$Parch <- train$Parch/maxParch
test$Fare <- test$Fare/maxFare
test$SibSp <- test$SibSp/maxSibSp
test$Parch <- test$Parch/maxParch

test$Pclass <- factor(test$Pclass, levels=levels(train$Pclass)) 
test$Sex <- factor(test$Sex, levels=levels(train$Sex)) 
test$Embarked <- factor(test$Embarked, levels=levels(train$Embarked)) 
test$Family[(test$Family %in% levels(train$Family))==FALSE] <- "not a family"
test$Family <- factor(test$Family, levels=levels(train$Family))
test$FamSurvival <- factor(test$FamSurvival, levels=levels(train$FamSurvival))
test$Status[(test$Status %in% levels(train$Status))==FALSE] <- "Lady"  # status 'dona' is not present in the test set
test$Status <- factor(test$Status, levels=levels(train$Status))
test$Age <- factor(test$Age, levels=levels(train$Age))
test$Alone <- factor(test$Alone, levels=levels(train$Alone))
test$PosFare <- factor(test$PosFare, levels=levels(train$PosFare))