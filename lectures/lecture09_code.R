library(tidyverse)
library(NHANES)

# pre-processing: 
# 1) remove missing data
# 2) recode Diabetes (1=Yes, 0=No)
nhanes2 <- NHANES %>%
  select(Diabetes, BMI, Smoke100) %>%
  na.omit() %>%
  mutate(Diabetes = ifelse(Diabetes == "Yes", 1, 0))

ggplot(nhanes2, aes(x = BMI, y = Diabetes, color = Smoke100)) + 
  geom_point(alpha = 0.15) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se=F) +
  ylab("Probability of Diabetes") + theme_bw()

glm2 <- glm(Diabetes ~ BMI + Smoke100, 
            family = "binomial", data = nhanes2)
summary(glm2)
confint(glm2, level = 0.95)
# check coding for Smoke100
contrasts(nhanes2$Smoke100)

# make predictions
new_x <- data.frame(BMI = c(20, 30, 40), Smoke100 = "Yes")
predict(glm2, newdata = new_x, type="response") 

new_x <- data.frame(BMI = c(20, 30, 40), Smoke100 = "No")
predict(glm2, newdata = new_x, type="response")