# Lab: Classification Methods
## The Stock Market Data

rm(list = ls())

###
#install.packages("ISLR2")
library(ISLR2)
?Smarket
names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket, col=Smarket$Direction)
###
cor(Smarket)
cor(Smarket[, -9])
###
attach(Smarket)
plot(Volume)

## Logistic Regression

### Fit the model 
glm.fits <- glm(
  Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
  data = Smarket, family = binomial
)
summary(glm.fits)
###
coef(glm.fits)
summary(glm.fits)$coef
summary(glm.fits)$coef[, 4]

### Fit the probabilities 
glm.probs <- predict(glm.fits, type = "response")
glm.probs[1:10]
###
glm.pred <- rep("Down", 1250)
glm.pred[glm.probs > .5] = "Up"
###
table(glm.pred, Direction)
(507 + 145) / 1250
mean(glm.pred == Direction)

### We may overfit, therefore we split the data into train and test sample 
train <- (Year < 2005)
Smarket.2005 <- Smarket[!train, ]
dim(Smarket.2005)
Direction.2005 <- Direction[!train]
###
glm.fits <- glm(
  Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
  data = Smarket, family = binomial, subset = train
)
glm.probs <- predict(glm.fits, Smarket.2005,
                     type = "response")
### Compute the confusion matrix
glm.pred <- rep("Down", 252)
glm.pred[glm.probs > .5] <- "Up"
data.frame(lda.pred)[1:5,]
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)
mean(glm.pred != Direction.2005)

### Fit the smaller model 
glm.fits <- glm(Direction ~ Lag1 + Lag2, data = Smarket,
                family = binomial, subset = train)
glm.probs <- predict(glm.fits, Smarket.2005,
                     type = "response")
glm.pred <- rep("Down", 252)
glm.pred[glm.probs > .5] <- "Up"
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)

## Linear Discriminant Analysis

###
library(MASS)
lda.fit <- lda(Direction ~ Lag1 + Lag2, data = Smarket,
               subset = train)
lda.fit
plot(lda.fit)

### Predict on the test data frame
lda.pred <- predict(lda.fit, Smarket.2005)
names(lda.pred)
### Compute the confusion matrix
lda.class <- lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class == Direction.2005)
###
sum(lda.pred$posterior[, 1] >= .5)
sum(lda.pred$posterior[, 1] < .5)
###
lda.pred$posterior[1:20, 1]
lda.class[1:20]
###
sum(lda.pred$posterior[, 1] > .9)

## Quadratic Discriminant Analysis

###
qda.fit <- qda(Direction ~ Lag1 + Lag2, data = Smarket,
               subset = train)
qda.fit
###
qda.class <- predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)
mean(qda.class == Direction.2005)

## Naive Bayes

###
library(e1071)
nb.fit <- naiveBayes(Direction ~ Lag1 + Lag2, data = Smarket,
                     subset = train)
nb.fit
###
mean(Lag1[train][Direction[train] == "Down"])
sd(Lag1[train][Direction[train] == "Down"])
###
nb.class <- predict(nb.fit, Smarket.2005)
table(nb.class, Direction.2005)
mean(nb.class == Direction.2005)
###
nb.preds <- predict(nb.fit, Smarket.2005, type = "raw")
nb.preds[1:5, ]

