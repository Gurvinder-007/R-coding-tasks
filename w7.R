# Endpoints of Uniform distribution
a = 0 # feel free to change
b = 50 # feel free to change but need a<b
# Sample size
n = 40 # feel free to change but need n>=30
# Random sample
x = runif(n,a,b)
x
# Sample mean
xbar = mean(x)
xbar

a = 0
b = 50
n = 40
x = runif(n,a,b)
xbar = mean(x)
# Sample standard deviation
s = sd(x)
# Magic value
z=2
#confidence interval
lower = xbar - z*s/sqrt(n)
upper = xbar + z*s/sqrt(n)
print(c(lower,upper))


z = 3 # magic value
a = 0
b = 50
n = 40
replications = 100000
outside = rep(FALSE,replications)
for (i in 1:replications)
{
  x = runif(n,a,b)
  xbar = mean(x)
  s = sd(x)
  lower = xbar - z*s/sqrt(n)
  upper = xbar + z*s/sqrt(n)
  print(c(lower,upper))
  truemean = (a+b)/2
  outside[i] = (truemean<lower) | (truemean>upper)
}
summary(outside)
failure = sum(outside)/length(outside)
failure
prob = (1-pnorm(z,0,1))+pnorm(-z,0,1)
prob



alpha = 0.1
qnorm(1-alpha/2,0,1)







install.packages("colorspace")

install.packages("ggplot2") # Install it again
library(ggplot2) # Load the librarie (you have to do this one on each new session)


x = seq(-3.5,3.5,0.01)
ggplot(NULL) +
  geom_line(aes(x,y=dnorm(x,0,1))) +
  geom_line(aes(x,y=dt(x,30)), colour="red") +
  geom_line(aes(x,y=dt(x,8)), colour="green") +
  geom_line(aes(x,y=dt(x,3)), colour="blue")


alpha = 0.05
mu = 25
sigma = 8
n = 5
replications = 100000
outside = rep(FALSE,replications)
for (i in 1:replications)
{
  x = rnorm(n,mu,sigma)
  xbar = mean(x)
  s = sd(x)
  lower = xbar - qt(1-alpha/2,n-1)*s/sqrt(n)
  upper = xbar + qt(1-alpha/2,n-1)*s/sqrt(n)
  print(c(lower,upper))
  truemean = mu
  outside[i] = (truemean<lower) | (truemean>upper)
}
summary(outside)
failure = sum(outside)/length(outside)
failure

library(tidyverse)
state = as_tibble(state.x77)
summary(state)

t.test(state$Murder, mu=8.5)


x = state$Murder
t = (mean(x)-8.5)/(sd(x)/sqrt(length(x)))
t
df = length(x)-1
df
p_value = 2*pt(t,df)
p_value
lower = mean(x)-qt(1-0.05/2,df)*sd(x)/sqrt(length(x))
upper = mean(x)+qt(1-0.05/2,df)*sd(x)/sqrt(length(x))
c(lower,upper)

t.test(state$Income, conf.level=0.90)







