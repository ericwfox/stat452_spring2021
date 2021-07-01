# read in data from URL
county_votes <- readRDS(url("https://ericwfox.github.io/data/county_votes16.rds"))
head(county_votes)
dim(county_votes)
table(county_votes$trump_win)
table(county_votes$trump_win) / nrow(county_votes)

# randomly split data into a 70% training and 30% test set
set.seed(999)
n <- nrow(county_votes); n
round(0.7*n)
train_index <- sample(1:n, 2178) 
county_votes_train <- county_votes[train_index, ]
county_votes_test <- county_votes[-train_index, ]

# fit model using training data
glm1 <- glm(trump_win ~ obama_pctvotes, family = "binomial", 
            data=county_votes_train)
summary(glm1)

# make predictions for probabilities on test set
probs1 <- predict(glm1, newdata = county_votes_test, type = "response")

# use 0.5 probability threshold to classify points on test set
# if predicted probability is greater than 0.5 classify as a Trump win (1)
# otherwise, if predicted probability is less than 0.5 classify as Trump lost (0)
preds1 <- ifelse(probs1 > 0.5, 1, 0)

head(data.frame(probs1, preds1), n=15)

# next make the confusion matrix
tb <- table(prediction = preds1,
            actual = county_votes_test$trump_win)
addmargins(tb)

# Accuracy (percent correctly classified)
(125 + 763) / 934
# Sensitivity (percent of Trump wins (1) correctly classified)
763 / 785
# Specificity (percent of Trump losses (0) correctly classified)
125 / 149

# Note: results may change slightly depending on the random number seed or your version of R

# roc curve ----------------------------
library(pROC)

roc_obj <- roc(county_votes_test$trump_win, probs1)

plot(1 - roc_obj$specificities, roc_obj$sensitivities, type="l",
     xlab = "1 - Specificity", ylab = "Sensitivity")

# plot red point corresponding to 0.5 threshold
points(x = 24/149, y = 763/785, col="red", pch=19) 
abline(0, 1, lty=2) # 1-1 line

auc(roc_obj)
