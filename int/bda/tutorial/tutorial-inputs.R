## Tyler's Introduction to R - example commands

## Entering data:
# Math:
1 + 1
1 + 1 * 7
(1 + 1) * 7

# Variables:
x <- 1
y = 2
3 -> z
x
y
z
(x + y) * z

# Arrays:
x <- c(0,1,2,3,4)
x
y <- 1:5
y
z <- 1:50
z

# Math on arrays:
x <- c(0,1,2,3,4)
y <- 1:5
z <- 1:50
x + y
x * y
x * z

# Functions:
arc <- function(x) 2*asin(sqrt(x))
arc(0.5)
x <- c(0,1,2,3,4)
x <- x / 10
arc(x)

## Getting help:
help(t.test)
help.search("standard deviation")

## Reading data from files:
myData <- read.table("R_Tutorial_Data.txt", header=TRUE, sep="\t")
myData
plot(myData)

## Selecting Subsets of Data:
myData$Learning
myData$Learning[myData$Group=="A"]
attach(myData)
Learning
Learning[Group=="A"]
Learning[Group!="A"]
Condition[Group=="B"&Learning<0.5]


## Basic parametric inferential statistics 
# Independent sample t-tests:
t.test(Pre2[Group=="A"], Pre2[Group=="B"], paired=FALSE)

# Independent sample t-tests with equal variance assumed:
t.test(Pre2[Group=="A"], Pre2[Group=="B"], paired=FALSE, var.equal=TRUE)

# Independent sample t-tests with equal variance assumed, one-tailed:
t.test(Pre2[Group=="A"], Pre2[Group=="B"], paired=FALSE, var.equal=TRUE, alternative=“greater”)

# Paired sample t-test:
t.test(Pre4[Group=="A"], Pre3[Group=="A"], paired=TRUE)
	# Visualize this test:
	boxplot(Pre4[Group=="A"],Pre3[Group=="A"],col=c("#ffdddd","#ddddff"),names=c("Pre4","Pre3"),main="Group A")

# One sample t-test:
t.test(Learning[Group=="B"], mu=0.5, alternative="greater")
	# Visualize this test:
	boxplot(Learning[Group=="B"],names="Group B",ylab="Learning")
	lines(c(0,2),c(0.5,0.5),col="red")
	points(c(rep(1,length(Learning[Group=="B"]))),Learning[Group=="B"],pch=21,col="blue")

# Correlation
cor.test(Pre1,Learning,method="pearson")
plot(Pre1,Learning)
# for a fancier plot:
	plot(Learning~Pre1, ylim=c(0,1), xlim=c(0,1), ylab="Learning", xlab="Pre1", type="n")
	abline(lm(Learning~Pre1),col="black",lty=2, lwd=2)
	points(Learning[Group=="A"&Condition=="High"]~Pre1[Group=="A"&Condition=="High"], pch=65, col="red", cex=0.9)
	points(Learning[Group=="A"&Condition=="Low"]~Pre1[Group=="A"&Condition=="Low"], pch=65, col="blue", cex=0.9)
	points(Learning[Group=="B"&Condition=="High"]~Pre1[Group=="B"&Condition=="High"], pch=66, col="red", cex=0.9)
	points(Learning[Group=="B"&Condition=="Low"]~Pre1[Group=="B"&Condition=="Low"], pch=66, col="blue", cex=0.9)
	legend(2.5,1.0,
		c("LV Training", "HV Training"),
		pch=c(19),
		col=c("blue","red"),
		bty="y")
	myCor <- cor.test(Pre1, Learning, method="pearson")
	text(0.3,0.8, paste("r = ", format(myCor$estimate,digits=3),", p < ", format(myCor$p.value,digits=3)), cex=0.8)


## Basic nonparametric inferential statistics 

# Check for normal distribution
t.test(Learning[Condition=="High"&Group=="A"],Learning[Condition=="Low"&Group=="A"])
plot(dnorm,-3,3,col="blue",lwd=3,main="The Normal Distribution")
par(mfrow=c(1,2))
hist(Learning[Condition=="High"&Group=="A"])
hist(Learning[Condition=="Low"& Group=="A"])
shapiro.test(Learning[Condition=="High"&Group=="A"])
shapiro.test(Learning[Condition=="Low"&Group=="A"])

# Mann-Whitney U / Wilcoxon Signed Rank Test
wilcox.test(Learning[Condition=="High"&Group=="A"],Learning[Condition=="Low"&Group=="A"],exact=FALSE,paired=FALSE)
x <- matrix(c(length(Learning[Group=="A"&Condition=="High"&Gender=="F"]),length(Learning[Group=="A"&Condition=="Low"&Gender=="F"]),length(Learning[Group=="B"&Condition=="High"&Gender=="F"]),length(Learning[Group=="B"&Condition=="Low"&Gender=="F"])),ncol=2)
x
chisq.test(x)

## Linear Models and ANOVA
# Linear Models with covariates:
myModel <- lm(Learning~Pre1+Pre2+Pre3+Pre4)
par(mfrow=c(2,2))
plot(myModel)
summary(myModel)
step(myModel, direction="backward")

# ANOVA
myANOVA <- aov(Learning~Group*Condition)
summary(myANOVA)
boxplot(Learning~Group*Condition,col=c("#ffdddd","#ddddff"))

# ANOVA with covariates:
myANOVA2 <- aov(Learning~Group*Condition+Gender)
summary(myANOVA2)
boxplot(Learning~Group*Condition+Gender,col=c(rep("pink",4),rep("light blue",4)))

