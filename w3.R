library(tidyverse)
library(ISLR)
credit = as_tibble(Credit)
credit = select(credit,Balance,Limit,Income)
summary(credit)

library(GGally)
ggscatmat(select(credit,Balance,Limit,Income))

ggplot(credit, aes(x=Limit,y=Balance)) +
  geom_point() +
  geom_smooth(method="lm",se=FALSE)
model = lm(Balance~Limit, data=credit)
summary(model)
model$coefficients

ggplot(credit, aes(x=Income,y=Balance)) +
  geom_point() +
  geom_smooth(method="lm",se=FALSE)

model = lm(Balance~Rating+Age, data=credit)
summary(model)

library(ggfortify)
autoplot(model)


library(tidyverse)
library(gapminder)
gapminder2007 = gapminder %>%
  filter(year==2007) %>%
  select(country, continent, lifeExp, gdpPercap)

summary(gapminder2007$lifeExp)
ggplot(gapminder2007, aes(x=lifeExp)) +
  geom_histogram(binwidth=5)

model = lm(lifeExp~1, data=gapminder2007)
model

gapminder2007 %>%
  group_by(continent) %>%
  summarise(count=n(),mean=mean(lifeExp))
ggplot(gapminder2007, aes(x=lifeExp)) +
  geom_histogram(binwidth=5) +
  facet_wrap(~continent)

gapminder2007 %>%
  group_by(continent) %>%
  summarise(count=n(),mean=mean(lifeExp-54.8))

model = lm(lifeExp~continent, data=gapminder2007)
model




library(tidyverse)
load(url("http://www.openintro.org/stat/data/evals.RData"))
evals = as_tibble(evals) %>%
  select(score, age, gender)
summary(evals)
ggscatmat(evals, color="gender")


ggplot(evals, aes(x=age,y=score,colour=gender)) +
  geom_jitter() +
  geom_smooth(method="lm", se=FALSE)
female_evals = filter(evals, gender=="female")
female_model = lm(score~age, data=female_evals)
female_model
male_evals = filter(evals, gender=="male")
male_model = lm(score~age, data=male_evals)
male_model

model = lm(score~age+gender, data=evals)
model

model = lm(score~age*gender, data=evals)
model

library(readr)
library(tidyverse)
read_tsv('C:/Users/Gurvinder Nagra/Downloads/cefa1210a38aedab678d354400b605f7birthsmokers.tsv')
birthsmokers = as_tibble(birthsmokers) %>%
  select(gestation,weight,smoking)
summary(birthsmokers)


ggplot(babies, aes(x=gestation,y=weight)) +
  geom_point(aes(colour=smoking)) +
  geom_smooth(method="lm",se=FALSE)
