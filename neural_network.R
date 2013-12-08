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
test[153, "Cabin"] <- "A11"
test[92, "Cabin"] <- "A11" 
test[144, "Cabin"] <- "A11" 
test[392, "Cabin"] <- "A11" 
test[411, "Cabin"] <- "A11" 

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
test$Pclass <- as.factor(test$Pclass)
train$Alone <- (train$SibSp + train$Parch)==0
train$PosFare <- train$Fare>0
train$Pclass <- as.factor(train$Pclass)

# Add complex features
extrName <- function(x) {
  str_split(x, ',')[[1]][1]
}
test$Family <- "not a family"
test$Family[(test$SibSp + test$Parch)>=3] <- sapply(test$Name[(test$SibSp + test$Parch)>=3],FUN=extrName)
test$Family <- as.factor(test$Family)
train$Family <- "not a family"
train$Family[(train$SibSp + train$Parch)>=3] <- sapply(train$Name[(train$SibSp + train$Parch)>=3], FUN=extrName)
train$Family <- as.factor(train$Family)

extrStatus <- function(x) {
  where <- str_locate_all(x, c("\\,","\\."))
  substr(x, where[[1]][1] + 2, where[[2]][1] - 1)
}
test$Status <- sapply(test$Name, FUN=extrStatus)
#test$Status <- as.factor(test$Status)
train$Status <- sapply(train$Name, FUN=extrStatus)
train$Status <- as.factor(train$Status)

# to force classification for random forest
train$Survived <- as.factor(train$Survived)
maxFare <- max(train$Fare)
train$Fare <- train$Fare/maxFare

trainset <- model.matrix(~ Survived + Pclass + Sex + Fare + Embarked + Alone + PosFare + Status + Age + Family, data=train)
dimnames(trainset)[[2]][1] <- "constant"
dimnames(trainset)[[2]][27]  <- "Statusthe_Countess"
dimnames(trainset)[[2]][28:40] <- paste("age", 1:13, sep="")
dimnames(trainset)[[2]][57] <- "NotAFamily"

net <- neuralnet(Survived1 ~ constant + Pclass2 + Pclass3 + Sexmale + Fare + 
                   EmbarkedC + EmbarkedQ + EmbarkedS + AloneTRUE + PosFareTRUE + StatusCol + 
                   StatusDon + StatusDr + StatusJonkheer + StatusLady + StatusMajor + StatusMaster + 
                   StatusMiss + StatusMlle + StatusMme + StatusMr + StatusMrs + StatusMs + StatusRev + 
                   StatusSir + Statusthe_Countess + age1 + age2 + age3 + age4 + age5 + age6 + age7 + 
                   age8 + age9 + age10 + age11 + age12 + age13 + FamilyAndersson + FamilyAsplund + 
                   FamilyBackstrom + FamilyBaclini + FamilyBecker + FamilyCarter + FamilyDean + FamilyFord + 
                   FamilyFortune + FamilyGoodwin + FamilyHerman + FamilyHocking + FamilyJacobsohn + 
                   FamilyJohnston + FamilyLaroche + FamilyLefebre + NotAFamily + FamilyPalsson + FamilyPanula + 
                   FamilyRenouf + FamilyRice + FamilyRichards + FamilyRyerson + FamilySage + FamilySkoog + FamilyWest, data=trainset, hidden=c(4,2))

net <- neuralnet(Survived1 ~ constant + Pclass2 + Pclass3 + Sexmale + Fare + 
                   EmbarkedC + EmbarkedQ + EmbarkedS + AloneTRUE + PosFareTRUE + StatusCol + 
                   StatusDon + StatusDr + StatusJonkheer + StatusLady + StatusMajor + StatusMaster + 
                   StatusMiss + StatusMlle + StatusMme + StatusMr + StatusMrs + StatusMs + StatusRev + 
                   StatusSir + Statusthe_Countess + age1 + age2 + age3 + age4 + age5 + age6 + age7 + 
                   age8 + age9 + age10 + age11 + age12 + age13 + FamilyAndersson + FamilyAsplund + 
                   FamilyBackstrom + FamilyBaclini + FamilyBecker + FamilyCarter + FamilyDean + FamilyFord + 
                   FamilyFortune + FamilyGoodwin + FamilyHerman + FamilyHocking + FamilyJacobsohn + 
                   FamilyJohnston + FamilyLaroche + FamilyLefebre + NotAFamily + FamilyPalsson + FamilyPanula + 
                   FamilyRenouf + FamilyRice + FamilyRichards + FamilyRyerson + FamilySage + FamilySkoog + FamilyWest, data=trainset, hidden=c(5,3))

test$Fare <- test$Fare/maxFare
testset <- model.matrix(~ Pclass + Sex + Fare + Embarked + Alone + PosFare + Status + Age + Family, data=test)
dimnames(testset)[[2]][1] <- "constant"
dimnames(testset)[[2]][18:30] <- paste("age", 1:13, sep="")
dimnames(testset)[[2]][45] <- "NotAFamily"
missmod <- setdiff(dimnames(trainset)[[2]], dimnames(testset)[[2]])
addmod <- setdiff(dimnames(testset)[[2]], dimnames(trainset)[[2]])
testset <- cbind(testset[, -which(dimnames(testset)[[2]] %in% addmod)], matrix(data=0, nc=length(missmod), nr=nrow(testset)))
dimnames(testset)[[2]][dimnames(testset)[[2]]==""] <- missmod
testset <- testset[, match(dimnames(trainset)[[2]],dimnames(testset)[[2]])]

pred <- compute(net, testset[, -2])$net.result
#threshold <- min(range(pred)) + diff(range(pred))/2
write.csv(data.frame(PassengerId=test$PassengerId, Survived=as.numeric(pred>=0.5)), 
          file='09142013_neural_net_bis.csv',
          quote=FALSE,
          row.names=FALSE)

table(as.numeric(pred>=0.5))

?prediction
net$net.result

pred==0.5


fit = svm(Survived ~ Pclass + Sex + Age + Fare + Embarked + Alone + PosFare + Status + Family, 
          data=train[, c("Survived","Pclass","Sex","Age","Fare","Embarked","Alone","PosFare","Status", "Family")], 
          scale=c(FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE))

train$Alone <- as.factor(train$Alone)
train$PosFare <- as.factor(train$PosFare)
test$Pclass <- factor(test$Pclass, levels=levels(train$Pclass)) 
test$Sex <- factor(test$Sex, levels=levels(train$Sex)) 
test$Embarked <- factor(test$Embarked, levels=levels(train$Embarked)) 
test$Family[(test$Family %in% levels(train$Family))==FALSE] <- "not a family"
test$Family <- factor(test$Family, levels=levels(train$Family))
test$Status[(test$Status %in% levels(train$Status))==FALSE] <- "Lady"
test$Status <- factor(test$Status, levels=levels(train$Status))
test$Age <- factor(test$Age, levels=levels(train$Age))
test$Alone <- factor(test$Alone, levels=levels(train$Alone))
test$PosFare <- factor(test$PosFare, levels=levels(train$PosFare))

pred <- predict(fit, newdata=test[, c("Pclass","Sex","Age","Fare","Embarked","Alone","PosFare","Status","Family")])

pred[diff(as.numeric(names(pred)))==2, ]

write.csv(data.frame(PassengerId=test$PassengerId, Survived=pred), 
          file='09142013_svm.csv',
          quote=FALSE,
          row.names=FALSE)

tab <- table(train$Family,train$Survived)
survived <- row.names(tab)[tab[, 1]==0]
died <- row.names(tab)[tab[, 2]==0]