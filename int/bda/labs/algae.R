
#setwd("H:")
algae <- read.table( './analysis.txt', header=F, dec='.'
                   , col.names=c( 'season','size','speed','mxPH','mnO2'
                                , 'Cl','NO3','NH4','oPO4','PO4','Chla'
                                ,'a1','a2','a3','a4', 'a5','a6','a7')
                   , na.strings=c('XXXXXXX'))
hist(algae$mxPH)
boxplot(algae$oPO4,ylab='Orthophosphate (oPO4)')
abline(h=mean(algae$oPO4,na.rm=T),lty=2)
rug(jitter(algae$oPO4),side=2)

# outliers
plot(algae$NH4,xlab='')
abline(h=mean(algae$NH4,na.rm=T),lty=1,col="red")
abline(h=mean(algae$NH4,na.rm=T)+sd(algae$NH4,na.rm=T),lty=2,col="blue")
abline(h=median(algae$NH4,na.rm=T),lty=3,col="green")
#identify(algae$NH4)

algae[algae$NH4 >19000,]

# missing data (omit)
library(DMwR)
data(algae)
algae <- read.table( './analysis.txt', header=F, dec='.'
                   , col.names=c( 'season','size','speed','mxPH','mnO2'
                                , 'Cl','NO3','NH4','oPO4','PO4','Chla'
                                ,'a1','a2','a3','a4', 'a5','a6','a7')
                   , na.strings=c('XXXXXXX'))
algae[!complete.cases(algae),]
nrow(algae[!complete.cases(algae),])
algae <- na.omit(algae)

data(algae)
algae <- read.table( './analysis.txt', header=F, dec='.'
                   , col.names=c( 'season','size','speed','mxPH','mnO2'
                                , 'Cl','NO3','NH4','oPO4','PO4','Chla'
                                ,'a1','a2','a3','a4', 'a5','a6','a7')
                   , na.strings=c('XXXXXXX'))
algae[!complete.cases(algae),]
algae <- algae[-c(62,199),]

# missing data (fill them in using mean)
algae[48,'mxPH'] <- mean(algae$mxPH,na.rm=T)

# missing data (fill them in based on correlatioon of numeric values)
cor(algae[,4:18],use="complete.obs")
symnum(cor(algae[,4:18],use="complete.obs"))
lm(PO4 ~ oPO4,data=algae)
fillPO4 <- function(oP) {
  if (is.na(oP)) return(NA)
  else return(42.897 + 1.293 * oP)
}
algae[is.na(algae$PO4),'PO4'] <- sapply(algae[is.na(algae$PO4),'oPO4'],fillPO4)

data(algae)
# many histograms (??)
histogram(~ mxPH | season, data=algae)
#multihist(list(algae[season == 'winter']$mxPH, algae[season == 'summer']$mxPH))

knnImputation(algae,k=10,meth='median')
algae <- knnImputation(algae,k=10) #in DMwR package

# prediction
data(algae)
algae <- algae[-manyNAs(algae), ]
clean.algae <- knnImputation(algae, k = 10)
lm.a1 <- lm(a1 ~ .,data=clean.algae[,1:12]) # here consider a1 with other 11 predictors
summary(lm.a1)  # remove high P(>|t|)s

anova(lm.a1)

# new model
lm2.a1 <- update(lm.a1, . ~ . - season)
summary(lm2.a1)

anova(lm.a1, lm2.a1)  # comparison of models

# automatic model stepping
final.lm <- step(lm.a1)
summary(final.lm)

# tree models might be better
library(tree)
data(algae)
set.seed(2)
train <- sample(1:nrow(algae),nrow(algae)/2)
rt.a1.train <- tree(a1~.,algae[,1:12],subset=train)
plot(rt.a1.train)
text(rt.a1.train,pretty=0)

