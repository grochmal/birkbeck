# prediction trees
library(randomForest)
library(MASS)

old.par <- par(mfrow=c(2,2))

set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston)/2)
names(Boston)
bag.boston <- randomForest(medv ~ . , data=Boston, subset=train, mtry=13, importance=T, ntree=200)

bag.yhat <- predict(bag.boston, newdata=Boston[-train,])
bag.test <- Boston[-train, "medv"]
plot(bag.yhat, bag.test, pch="*", col="cyan")
abline(0,1,col="green")
mean((bag.yhat - bag.test)^2)

#trees <- c( randomForest(medv ~ . , data=Boston, subset=train, mtry=13, importance=T, ntree=10)
#          , randomForest(medv ~ . , data=Boston, subset=train, mtry=13, importance=T, ntree=20)
#          , randomForest(medv ~ . , data=Boston, subset=train, mtry=13, importance=T, ntree=120)
#          , randomForest(medv ~ . , data=Boston, subset=train, mtry=13, importance=T, ntree=150)
#          , randomForest(medv ~ . , data=Boston, subset=train, mtry=13, importance=T, ntree=200)
#          )
#trees[1]
#sapply(seq(1,5), function(x) {
#  tr <- trees[x]
#  typeof(tr)
#  points(predict(tr, newdata=Boston[-train,]), bag.test, col=x)
#  })

f <- function(x) {
  #set.seed(1)
  this.bag  <- randomForest(medv ~ . , data=Boston, subset=train, mtry=2, importance=T, ntree=x)
  this.yhat <- predict(this.bag, newdata=Boston[-train,])
  mean((this.yhat - bag.test)^2)
}

mses <- sapply(seq_len(100), f)
plot(mses, xlab="Number of Bootstrap Data Sets", ylab="Test Means Sum of Squares", type="l")
abline(h=mses[1], lty=2, col="red")

# classification trees
library(ISLR)
H <- ifelse(Carseats$Sales <= 8, "No", "Yes")
C <- data.frame(Carseats, H)
set.seed(2)
train  <- sample(seq_len(nrow(C)), 200)
C.test <- C[-train,]
H.test <- C[-train, "H"]
C.bag  <- randomForest(H ~ . - Sales, data=C, subset=train, mtry=10)
C.yhatr <- predict(C.bag, newdata=C.test, type="response")
C.yhatp <- predict(C.bag, newdata=C.test, type="prob")
C.yhatpr <- ifelse(C.yhatp[,"Yes"] > 0.5, "Yes", "No")
table(C.yhatr, H.test)
table(C.yhatpr, H.test)

f <- function(x) {
  set.seed(2)
  this.bag  <- randomForest(H ~ . - Sales, data=C, subset=train, mtry=10, ntree=x)
  this.yhat <- predict(this.bag, newdata=C[-train,])
  #TODO table difference
}

#mses <- sapply(seq_len(100), f)

f <- function(x) {
  #set.seed(1)
  this.bag  <- randomForest(medv ~ . , data=Boston, subset=train, mtry=x, importance=T)
  this.yhat <- predict(this.bag, newdata=Boston[-train,])
  mean((this.yhat - bag.test)^2)
}

mses <- sapply(seq_len(13), f)
plot(mses)
#abline(h=mses[1], lty=2, col="red")
