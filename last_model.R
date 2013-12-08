##################################################### data preparation
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

################################################ cross validation

library(multicore)

nFold <- 10
m <- nrow(train)
seq <- 1:m
v <- vector(mode="numeric", length=nFold)
seqList <- NULL
for(i in 1:nFold) {
  if(length(seq) > round(m/nFold)) indices <- sample(seq, size=round(m/nFold))
  else indices <- seq
  seq <- setdiff(seq, indices)
  seqList[[i]] <- indices
}

################################################### random forest

library(randomForest)

titanic <- function(x, dat, nv, nt) {
  randfor <- randomForest(Survived ~ . , data=dat[-x, ], mtry=nv, ntree=nt)
  pred <- predict(randfor, newdata=dat[x, ])
  check <- pred==dat[x, "Survived"]
  sum(check)/length(pred)  
}

mean(unlist(mclapply(seqList, FUN=titanic, 
                     dat=subset(train, select=-c(PassengerId, Name, Parch, Ticket, Cabin)),
                     nv=3, nt=10000)))

model <- randomForest(Survived ~ . , 
                      data=subset(train, select=-c(PassengerId, Name, Parch, Ticket, Cabin)), 
                      mtry=3, ntree=10000)

pred <- predict(model, 
                newdata=subset(test, select=-c(PassengerId, Name, Parch, Ticket, Cabin)))

write.csv(data.frame(PassengerId=test$PassengerId, Survived=pred), 
          file='09152013_randfor.csv',
          quote=FALSE,
          row.names=FALSE)

################################################### svm

library(e1071)

titanic <- function(x, dat, toscale) {
  fit <- svm(Survived ~ ., data=dat[-x, ], scale=toscale)
  pred <- predict(fit, newdata=dat[x, ])
  check <- pred==dat[x, "Survived"]
  sum(check)/length(pred)  
}

mean(unlist(mclapply(seqList, FUN=titanic, 
                     dat=subset(train, select=-c(PassengerId, Name, Ticket, Cabin)),
                     toscale=FALSE)))

model <- svm(Survived ~ ., 
             data=subset(train, select=-c(PassengerId, Name, Ticket, Cabin)), 
             scale=FALSE)

pred <- predict(model, 
                newdata=subset(test, select=-c(PassengerId, Name, Ticket, Cabin)))

write.csv(data.frame(PassengerId=test$PassengerId, Survived=pred), 
          file='09152013_svm.csv',
          quote=FALSE,
          row.names=FALSE)

################################################### neural network single hidden layers

library(nnet)

titanic <- function(x, dat, s) {
  net <- nnet(Survived ~ ., data=dat[-x, ], size=s)
  pred <- predict(net, newdata=dat[x, ])
  check <- (pred>=0.5)==(as.numeric(dat[x, "Survived"])-1)
  sum(check)/length(pred)  
}

mean(unlist(mclapply(seqList, FUN=titanic, 
                     dat=subset(train, select=-c(PassengerId, Name, Ticket, Cabin)),
                     s=4)))

model <- nnet(Survived ~ ., 
              data=subset(train, select=-c(PassengerId, Name, Ticket, Cabin)), 
              size=4)

pred <- predict(model, 
                newdata=subset(test, select=-c(PassengerId, Name, Ticket, Cabin)))

write.csv(data.frame(PassengerId=test$PassengerId, Survived=as.numeric(pred>=0.5)), 
          file='09152013_nnet.csv',
          quote=FALSE,
          row.names=FALSE)

################################################## neural net multi hidden layers

library(neuralnet)

trainset <- model.matrix(~ Survived + Pclass + Sex + Fare + Embarked + Alone + PosFare + Status + Age + FamSurvival + SibSp + Parch, data=train)
dimnames(trainset)[[2]][1] <- "constant"
dimnames(trainset)[[2]][27]  <- "Statusthe_Countess"
dimnames(trainset)[[2]][28:40] <- paste("age", 1:13, sep="")

titanic <- function(x, dat, layer) {
  net <- neuralnet(Survived1 ~ constant + Pclass2 + Pclass3 + Sexmale + Fare + 
                     EmbarkedC + EmbarkedQ + EmbarkedS + AloneTRUE + PosFareTRUE + StatusCol + 
                     StatusDon + StatusDr + StatusJonkheer + StatusLady + StatusMajor + StatusMaster + 
                     StatusMiss + StatusMlle + StatusMme + StatusMr + StatusMrs + StatusMs + StatusRev + 
                     StatusSir + Statusthe_Countess + age1 + age2 + age3 + age4 + age5 + age6 + age7 + 
                     age8 + age9 + age10 + age11 + age12 + age13 + FamSurvivalsurvived + FamSurvivalundefined +
                     SibSp + Parch, 
                   data=dat[-x, ], hidden=layer)
  pred <- compute(net, dat[x, -2])$net.result
  check <- (pred>=0.5)==dat[x, 2]
  sum(check)/length(pred)
}

mean(unlist(mclapply(seqList, FUN=titanic, dat=trainset, layer=c(4,2))))

a <- mclapply(seqList, FUN=titanic, dat=trainset, layer=c(4,2))


net <- neuralnet(Survived1 ~ constant + Pclass2 + Pclass3 + Sexmale + Fare + 
                   EmbarkedC + EmbarkedQ + EmbarkedS + AloneTRUE + PosFareTRUE + StatusCol + 
                   StatusDon + StatusDr + StatusJonkheer + StatusLady + StatusMajor + StatusMaster + 
                   StatusMiss + StatusMlle + StatusMme + StatusMr + StatusMrs + StatusMs + StatusRev + 
                   StatusSir + Statusthe_Countess + age1 + age2 + age3 + age4 + age5 + age6 + age7 + 
                   age8 + age9 + age10 + age11 + age12 + age13 + FamSurvivalsurvived + FamSurvivalundefined +
                   SibSp + Parch, 
                 data=trainset[-seqList[[1]], ], hidden=c(2,2))
pred <- compute(net, trainset[seqList[[1]], -2])$net.result
check <- (pred>=0.5)==trainset[seqList[[1]], 2]
a1 = sum(check)/length(pred)

net <- neuralnet(Survived1 ~ constant + Pclass2 + Pclass3 + Sexmale + Fare + 
                   EmbarkedC + EmbarkedQ + EmbarkedS + AloneTRUE + PosFareTRUE + StatusCol + 
                   StatusDon + StatusDr + StatusJonkheer + StatusLady + StatusMajor + StatusMaster + 
                   StatusMiss + StatusMlle + StatusMme + StatusMr + StatusMrs + StatusMs + StatusRev + 
                   StatusSir + Statusthe_Countess + age1 + age2 + age3 + age4 + age5 + age6 + age7 + 
                   age8 + age9 + age10 + age11 + age12 + age13 + FamSurvivalsurvived + FamSurvivalundefined +
                   SibSp + Parch, 
                 data=trainset[-seqList[[2]], ], hidden=c(4,2))

pred <- compute(net, trainset[seqList[[2]], -2])$net.result
check <- (pred>=0.5)==trainset[seqList[[2]], 2]
a2 = sum(check)/length(pred)

net <- neuralnet(Survived1 ~ constant + Pclass2 + Pclass3 + Sexmale + Fare + 
                   EmbarkedC + EmbarkedQ + EmbarkedS + AloneTRUE + PosFareTRUE + StatusCol + 
                   StatusDon + StatusDr + StatusJonkheer + StatusLady + StatusMajor + StatusMaster + 
                   StatusMiss + StatusMlle + StatusMme + StatusMr + StatusMrs + StatusMs + StatusRev + 
                   StatusSir + Statusthe_Countess + age1 + age2 + age3 + age4 + age5 + age6 + age7 + 
                   age8 + age9 + age10 + age11 + age12 + age13 + FamSurvivalsurvived + FamSurvivalundefined +
                   SibSp + Parch, 
                 data=trainset[-seqList[[3]], ], hidden=c(4,2))
pred <- compute(net, trainset[seqList[[3]], -2])$net.result
check <- (pred>=0.5)==trainset[seqList[[3]], 2]
a3 = sum(check)/length(pred)

net <- neuralnet(Survived1 ~ constant + Pclass2 + Pclass3 + Sexmale + Fare + 
                   EmbarkedC + EmbarkedQ + EmbarkedS + AloneTRUE + PosFareTRUE + StatusCol + 
                   StatusDon + StatusDr + StatusJonkheer + StatusLady + StatusMajor + StatusMaster + 
                   StatusMiss + StatusMlle + StatusMme + StatusMr + StatusMrs + StatusMs + StatusRev + 
                   StatusSir + Statusthe_Countess + age1 + age2 + age3 + age4 + age5 + age6 + age7 + 
                   age8 + age9 + age10 + age11 + age12 + age13 + FamSurvivalsurvived + FamSurvivalundefined +
                   SibSp + Parch, 
                 data=trainset[-seqList[[4]], ], hidden=c(4,2))
pred <- compute(net, trainset[seqList[[4]], -2])$net.result
check <- (pred>=0.5)==trainset[seqList[[4]], 2]
a4 = sum(check)/length(pred)

net <- neuralnet(Survived1 ~ constant + Pclass2 + Pclass3 + Sexmale + Fare + 
                   EmbarkedC + EmbarkedQ + EmbarkedS + AloneTRUE + PosFareTRUE + StatusCol + 
                   StatusDon + StatusDr + StatusJonkheer + StatusLady + StatusMajor + StatusMaster + 
                   StatusMiss + StatusMlle + StatusMme + StatusMr + StatusMrs + StatusMs + StatusRev + 
                   StatusSir + Statusthe_Countess + age1 + age2 + age3 + age4 + age5 + age6 + age7 + 
                   age8 + age9 + age10 + age11 + age12 + age13 + FamSurvivalsurvived + FamSurvivalundefined +
                   SibSp + Parch, 
                 data=trainset[-seqList[[5]], ], hidden=c(4,2))
pred <- compute(net, trainset[seqList[[5]], -2])$net.result
check <- (pred>=0.5)==trainset[seqList[[5]], 2]
a5 = sum(check)/length(pred)

rm(list="pred")

pred

finalnet_3_2 <- neuralnet(Survived1 ~ constant + Pclass2 + Pclass3 + Sexmale + Fare + 
                            EmbarkedC + EmbarkedQ + EmbarkedS + AloneTRUE + PosFareTRUE + StatusCol + 
                            StatusDon + StatusDr + StatusJonkheer + StatusLady + StatusMajor + StatusMaster + 
                            StatusMiss + StatusMlle + StatusMme + StatusMr + StatusMrs + StatusMs + StatusRev + 
                            StatusSir + Statusthe_Countess + age1 + age2 + age3 + age4 + age5 + age6 + age7 + 
                            age8 + age9 + age10 + age11 + age12 + age13 + FamSurvivalsurvived + FamSurvivalundefined +
                            SibSp + Parch, 
                          data=trainset, hidden=c(3,2))
finalnet_4_2 <- neuralnet(Survived1 ~ constant + Pclass2 + Pclass3 + Sexmale + Fare + 
                            EmbarkedC + EmbarkedQ + EmbarkedS + AloneTRUE + PosFareTRUE + StatusCol + 
                            StatusDon + StatusDr + StatusJonkheer + StatusLady + StatusMajor + StatusMaster + 
                            StatusMiss + StatusMlle + StatusMme + StatusMr + StatusMrs + StatusMs + StatusRev + 
                            StatusSir + Statusthe_Countess + age1 + age2 + age3 + age4 + age5 + age6 + age7 + 
                            age8 + age9 + age10 + age11 + age12 + age13 + FamSurvivalsurvived + FamSurvivalundefined +
                            SibSp + Parch, 
                          data=trainset, hidden=c(4,2))
pred <- compute(finalnet_4_2, trainset[seqList[[3]], -2])$net.result
check <- (pred>=0.5)==trainset[seqList[[3]], 2]
sum(check)/length(pred)



testset <- model.matrix(~ Pclass + Sex + Fare + Embarked + Alone + PosFare + Status + Age + FamSurvival + SibSp + Parch, data=test)
dimnames(testset)[[2]][1] <- "constant"
dimnames(testset)[[2]][26]  <- "Statusthe_Countess"
dimnames(testset)[[2]][27:39] <- paste("age", 1:13, sep="")

pred <- compute(finalnet_3_2, testset)$net.result
write.csv(data.frame(PassengerId=test$PassengerId, Survived=as.numeric(pred>=0.5)), 
          file='09152013_neuralnet_3_2.csv',
          quote=FALSE,
          row.names=FALSE)

pred <- compute(finalnet_4_2, testset)$net.result
write.csv(data.frame(PassengerId=test$PassengerId, Survived=as.numeric(pred>=0.5)), 
          file='09152013_neuralnet_4_2.csv',
          quote=FALSE,
          row.names=FALSE)


######################################################"
X = trainset <- model.matrix(~ Pclass + Sex + Fare + Embarked + Alone + PosFare + Status + Age + FamSurvival + SibSp + Parch, data=train)
Y = as.matrix(as.numeric(train$Survived)-2)
model <- wsvm(X, Y, c.n = rep(1/ length(Y),length(Y)))
pred <- wsvm.predict(X, Y, X[seqList[[i]],], Y[seqList[[i]],], model)
titanic <- function(x, dat, w) {
  X = model.matrix(~ Pclass + Sex + Fare + Embarked + Alone + PosFare + Status + Age + FamSurvival + SibSp + Parch, data=dat)
  Y = as.matrix(as.numeric(dat$Survived)-1)
  model <- wsvm(X[-x, ], Y[-x, ], c.n=w)
  pred <- predict(model, newdata=dat[x, ])
  check <- pred==dat[x, "Survived"]
  sum(check)/length(pred)  
}