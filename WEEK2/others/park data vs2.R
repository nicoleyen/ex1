library(dplyr)

park
str(park)

library(ggplot2)
ggplot(park, aes(x = area, y = price, color = type)) + geom_point() + facet_wrap(~ number)

ggplot(data = park, aes(x = type)) +
  geom_bar(fill = "orange", colour = "black")

ggplot(data = park, aes(x = price)) +
  geom_histogram(fill = "skyblue")
ggplot(data = park, aes(x = area)) +
  geom_histogram(fill = "pink")

ggplot(data = park, aes(x = area, y = price)) +
  geom_point()

ggplot(park, aes(x= type, y= price)) +
  geom_boxplot()

ggplot(park, aes(x= type, y= area)) +
  geom_boxplot()

library(ggplot2)
ggplot(aes(x = area,y = price),data=park)+
  geom_line(aes(color=type))

library(gapminder)
library(dplyr)
park
p1 <- gapminder %>% filter( type   == "坡道平面")

