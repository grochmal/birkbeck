library(e1071)

#dev.new()
par(mfrow=c(2,2))
seed.set(1)

x <- matrix(rnorm(40), ncol=2)
y <- c(rep(-1,10), rep(1,10))
x[y==1,] <- x[y==1,] + 1
plot(x, col=(3-y))
dat <- data.frame(x=x, y=as.factor(y))
fit <- svm( y ~ . , data=dat, kernel='linear', cost=10, scale=F)
plot(fit, dat)

fit.tune <- tune(svm, y~ . , data=dat, kernel="linear", range=list(cost=c(0.001, 0.01, 0.1, 1, 10, 100)))
summary(fit.tune)
fit.best <- fit.tune$best.model
summary(fit.best)

nx <- matrix(rnorm(40), ncol=2)
ny <- sample(c(-1,1), 20, rep=T)
nx[ny==1,] <- nx[ny==1,] + 1
ndat <- data.frame(x=nx, y=as.factor(ny))

seed.set(1)
mx <- matrix(rnorm(200*2),ncol=2)
mx[1:100,] <- mx[1:100,]+2
mx[101:150,] <- mx[101:150,]-2
my <- c(rep(1,150),rep(2,50))
mdat <- data.frame(x=mx,y=as.factor(my))

mtr <- sample(200, 100)
mfit <- svm( y~. , data=mdat[mtr,], kernel="radial", gamma=1, cost=1e5)
plot(mfit, mdat[mtr,])
summary(fit)
mfit.tune <- tune( svm, y~. , data=mdat[mtr,], kernel="radial"
                 , ranges=list(cost=c(0.1,1,10,100,1000), gamma=c(0.5,1,2,4,8)))
summary(mfit.tune)
mfit.best <- mfit.tune$best.model
plot(mfit.best, mdat[mtr,])

pred <- predict(mfit.best, newx=mdat[-mtr,])
table(true=mdat[-mtr,"y"], pred=pred)
