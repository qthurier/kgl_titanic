source("data_prep.R")

library(multicore)

nBag <- 50
m <- nrow(train)
bagList <- lapply(1:nBag, function(x) sample(1:m, size=m, replace=TRUE))

######################### RF 

library(randomForest)

titanic <- function(x, dat, test, nv, nt) {
  randfor <- randomForest(Survived ~ . , data=dat[x, ], mtry=nv, ntree=nt)
  pred <- predict(randfor, newdata=test)
  pred <- as.numeric(pred)-1
}

bagging <- mclapply(bagList, FUN=titanic, 
                    dat=subset(train, select=-c(PassengerId, Name, Parch, Ticket, Cabin)),
                    test=subset(test, select=-c(PassengerId, Name, Parch, Ticket, Cabin)),
                    nv=3, nt=10000)

pred <- as.numeric(apply(data.frame(bagging), 1, mean)>=0.5)

write.csv(data.frame(PassengerId=test$PassengerId, Survived=pred), 
          file='09192013_bagging_randfor.csv',
          quote=FALSE,
          row.names=FALSE)

############## SVM

library(e1071)

titanic <- function(x, dat, test, toscale) {
  fit <- svm(Survived ~ ., data=dat[x, ], scale=toscale)
  pred <- predict(fit, newdata=test)
  pred <- as.numeric(pred)-1
}

bagging <- mclapply(bagList, FUN=titanic, 
                    dat=subset(train, select=-c(PassengerId, Name, Ticket, Cabin)),
                    test=subset(test, select=-c(PassengerId, Name, Ticket, Cabin)),
                    toscale=FALSE)

pred <- as.numeric(apply(data.frame(bagging), 1, mean)>=0.5)

write.csv(data.frame(PassengerId=test$PassengerId, Survived=pred), 
          file='09192013_bagging_svm.csv',
          quote=FALSE,
          row.names=FALSE)

##################### nnet

library(nnet)

titanic <- function(x, dat, test, s) {
  net <- nnet(Survived ~ ., data=dat[x, ], size=s)
  pred <- predict(net, newdata=test)
  pred <- as.numeric(pred)
}

bagging <- mclapply(bagList, FUN=titanic, 
                    dat=subset(train, select=-c(PassengerId, Name, Ticket, Cabin)),
                    test=subset(test, select=-c(PassengerId, Name, Ticket, Cabin)),
                    s=4)

pred <- as.numeric(apply(data.frame(bagging), 1, mean)>=0.5)

write.csv(data.frame(PassengerId=test$PassengerId, Survived=pred), 
          file='09192013_bagging_nnet.csv',
          quote=FALSE,
          row.names=FALSE)