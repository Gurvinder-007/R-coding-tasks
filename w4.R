library(tidyverse)
# Plot of pdf of U(0,360)
a = 0
b = 360
x = -100:450
y = dunif(x,a,b)
qplot(x,y,geom="line")

# If X~U(0,360), find P(45<=X<=90)
punif(315,0,360)-punif(45,0,360)

# Normal distribution in R
mu = 100
sigma = 15
x = (mu-3.5*sigma):(mu+3.5*sigma)
y = dnorm(x,mu,sigma)
qplot(x,y,geom="line")

pnorm(140,mu,sigma)-pnorm(120,mu,sigma)

qnorm(0.99,mu,sigma)