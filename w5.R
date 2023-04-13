library(tidyverse)
# Simulation of 12-sided dice rolls
n = 120
rolls = rdunif(n,1,12)
table(rolls)

mydata = tibble(rolls)
ggplot(mydata,mapping=aes(x=rolls)) +
  geom_bar()

n = 36000
rollsA = rdunif(n,1,6)
rollsB = rdunif(n,1,6)
rolls_sum = rollsA + rollsB
table(rolls_sum)

mydata = tibble(rolls_sum)
ggplot(mydata,mapping=aes(x=rolls_sum)) +
  geom_bar()
