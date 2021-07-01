library(tidyverse)
library(NHANES)

# pre-processing: 
# 1) remove missing data
# 2) recode Diabetes (1=Yes, 0=No)
nhanes2 <- NHANES %>%
  select(Diabetes, BMI) %>%
  na.omit() %>%
  mutate(Diabetes = ifelse(Diabetes == "Yes", 1, 0))

table(nhanes2$Diabetes)
table(nhanes2$Diabetes) / nrow(nhanes2)

ggplot(nhanes2, aes(x = BMI, y = Diabetes)) + geom_point(alpha = 0.15) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se=F) +
  labs(x= "BMI", y = "Probability of Diabetes") + theme_bw()

glm1 <- glm(Diabetes ~ BMI, family = "binomial", data = nhanes2)
summary(glm1)
confint(glm1, level = 0.95)
# for a more compact regression summary
tidy(glm1)

# make prediction
new_x <- data.frame(BMI = 30)
# predict logit
predict(glm1, newdata = new_x) 
# predict probability
predict(glm1, newdata = new_x, type="response") 