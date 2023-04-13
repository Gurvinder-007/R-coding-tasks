library(tidyverse)
library(datarium)
marketing = as_tibble(marketing)
observed_sales = marketing$sales
observed_sales

model = lm(sales~youtube, data=marketing)
autoplot(model)


a = model$coefficients[1]
b = model$coefficients[2]
predicted_sales = a + b*marketing$youtube+facebook
predicted_sales

residual_sales = observed_sales - predicted_sales
residual_sales

ggplot(NULL, aes(x=predicted_sales, y=residual_sales)) +
  geom_point()

model = lm(sales~youtube+facebook, data=marketing)
library(broom)
augment(model) %>%
  ggplot(aes(x=.fitted,y=.resid)) +
  geom_point()


library(ggfortify)
model = lm(sales~youtube+facebook,data=marketing)
autoplot(model)

modelA = lm(sales~youtube,data=marketing)
summary(modelA)

cor(marketing$sales,marketing$youtube)