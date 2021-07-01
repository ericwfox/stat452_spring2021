## backwards elimination -------------------------------

# prepare data
statedata <- data.frame(state.x77, row.names=state.abb)
head(statedata)
dim(statedata)

# scatterplot matrix
pairs(Life.Exp ~ ., data=statedata)

# full model with all predictors
lm1 <- lm(Life.Exp ~ ., data=statedata)
summary(lm1)

lm2 <- update(lm1, ~ . - Area)
summary(lm2)

lm3 <- update(lm2, ~ . - Illiteracy, data=statedata)
summary(lm3)

lm4 <- update(lm3, ~ . - Income, data=statedata)
summary(lm4)

## backwards stepwise selection using AIC -------------------------------
library(ISLR)
head(Hitters)
dim(Hitters)

# remove missing data
Hitters2 <- na.omit(Hitters)
dim(Hitters2)

lm_full <- lm(Salary ~ ., data=Hitters2)
lm2 <- step(lm_full)
summary(lm2)

n <- nrow(Hitters2)
lm3 <- step(lm_full, k=log(n))
summary(lm3)
