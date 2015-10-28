library(ISLR)
library(tree)

H <- ISLR:Hitters
Hi <- na.omit(H)
set.seed(2)

train <- sample(seq(1,nrow(Hi)), 132)
tree.Hi.train <- tree( log(Salary) ~ Hits + Runs + RBI + Walks + Years + PutOuts + AtBat + Assists + Errors
                     , Hi, subset=train)
tree.Hi.train
plot(tree.Hi.train)
text(tree.Hi.train)
cv.Hi <- cv.tree(tree.Hi.train)
#plot(cv.Hi)
plot(cv.Hi$size, cv.Hi$dev, type='b')
prune.Hi <- prune.tree(tree.Hi.train,best=3)
plot(prune.Hi)
#text(prune.Hi)
text(prune.Hi,pretty=0)

# predict stuff
yhat <- predict(prune.Hi, newdata=Hi[-train,])
test.Hi <- Hi[-train, "Salary"]
plot(yhat, log(test.Hi))
abline(0,1)

taberr <- function(x) (x[1,2] + x[2,1]) / sum(x)
tabacc <- function(X) (x[1,1] + x[2,2]) / sum(x)

C <- ISLR::Carseats
High <- ifelse(C$Sales > 8, "Yes", "No")
CH <- data.frame(C, High)

set.seed(2)
train    <- sample(seq(1,nrow(CH)), nrow(CH)/2)
CH.tree  <- tree(High ~ .-Sales, CH, subset=train)
CH.train <- CH[ train,]
CH.test  <- CH[-train,]
#High.train <- High[ train,]
#High.test  <- High[-train,]
#plot(CH.tree)
#text(CH.tree, pretty=6)
CH.cv <- cv.tree(CH.tree,FUN=prune.misclass)
plot(CH.cv$size, CH.cv$dev, type='b')  # best 9
CH.pred.test  <- predict(CH.tree, CH.test,  type="class")
CH.pred.train <- predict(CH.tree, CH.train, type="class")
CH.trtab <- table(CH.pred.train, CH.train$High)
CH.tstab <- table(CH.pred.test,  CH.test$High)

CH.prune <- prune.misclass(CH.tree, best=9)
plot(CH.prune)
text(CH.prune, pretty=6)
CH.pred.test <- predict(CH.prune, CH.test,  type="class")
CH.tstab <- table(CH.pred.test,  CH.test$High)
CH.tstab
taberr(CH.tstab)
