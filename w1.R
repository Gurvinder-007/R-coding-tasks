library(tidyverse)
# Anscombe's quartet (first dataset)
x = anscombe[,1]
y = anscombe[,5]
ggplot(NULL,aes(x=x,y=y)) +
  geom_point(size=4)

a = 2.0
b = 0.7
residuals = y-(a+b*x)
residuals
ggplot(NULL,aes(x=x,y=y)) +
  geom_point(size=4) +
  geom_abline(slope=b,intercept=a)

b = 0.7
a = mean(y)-b*mean(x)
residuals = y-(a+b*x)
ggplot(NULL,aes(x=x,y=y)) +
  geom_point(size=4) +
  geom_abline(intercept=a,slope=b) +
  geom_rect(aes(xmin=x,xmax=x-residuals,
                ymin=y,ymax=y-residuals),
            fill="green",alpha=0.5) +
  coord_fixed() +
  xlim(3,17) +
  ylim(3,12)
SSR = sum(residuals^2)
SSR



library(tidyverse)
loading = c(3,8,10,11,13,16,27,30,35,37,38,44,103,142)
removal = c(4,7, 8, 8,10,11,16,26,21, 9,31,30, 75, 90)
wetland = tibble(loading,removal)
x = loading
y = removal


b = 0.61
a = mean(y)-b*mean(x)
residuals = y-(a+b*x)
ggplot(NULL,aes(x=x,y=y)) +
  geom_point(size=4) +
  geom_abline(intercept=a,slope=b) +
  geom_rect(aes(xmin=x,xmax=x-residuals,
                ymin=y,ymax=y-residuals),
            fill="green",alpha=0.5) +
  coord_fixed() +
  xlim(3,17) +
  ylim(3,12)
SSR = sum(residuals^2)
SSR

x = loading
y = removal
b = sum((x-mean(x))*(y-mean(y)))/sum((x-mean(x))^2)
a = mean(y)-b*mean(x)
b
a





install.packages("datarium")
library(tidyverse)
library(datarium)
marketing = as_tibble(marketing)
marketing

ggplot(data=marketing,aes(x=facebook,y=sales)) +
  geom_point()

x = marketing$facebook
y = marketing$sales
# Model is y = a + b*x
model = lm(y~x)
summary(model)
a = model$coefficients[1]
b = model$coefficients[2]
a
b

cor(x,y)

# Model is sales = a + b*facebook
ggplot(marketing,aes(x=facebook,y=sales)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE)
model = lm(sales~facebook, data=marketing)
summary(model)

library(broom)
tidy(model)

fitted = augment(model)
fitted

ggplot(fitted,aes(x=facebook,y=sales)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE) +
  geom_segment(aes(xend=facebook,yend=.fitted), colour="red", size=0.3)


library(tidyverse)
library(datarium)
marketing = as_tibble(marketing)
# Model is sales = a + b*youtube
ggplot(marketing,aes(x=facebook,y=sales)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE)
model = lm(sales~facebook, data=marketing)
summary(model)