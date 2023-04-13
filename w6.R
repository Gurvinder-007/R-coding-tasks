library(tidyverse)
state = as_tibble(state.x77)
summary(state)
library(GGally)
ggpairs(state)

cor.test(state$Population, state$Income)

install.packages("psych")
library(psych)
corr.test(state)

children = tribble(
  ~age,~height,~siblings,
  18, 76.1, 1,
  19, 77.0, 2,
  20, 78.1, 3,
  21, 78.2, 2,
  22, 78.8, 0,
  23, 79.7, 1,
  24, 79.9, 5,
  25, 81.1, 0,
  26, 81.2, 1,
  27, 81.8, 4,
  28, 82.8, 1,
  29, 83.5, 5 )

ggplot(children, aes(x=age,y=height)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)
model = lm(height~age, data=children)
summary(model)


# install.packages("faraway") # run only once



library(tidyverse)
library(faraway)
gala = as_tibble(gala)
library(GGally)
ggpairs(gala)
model = lm(Species~Area+Elevation+Nearest+Scruz+Adjacent,data=gala)
summary(model)

corr.test(gala)