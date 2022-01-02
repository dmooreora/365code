install.packages("arules")
install.packages("arulesViz")
library("ggplot2")
library(tidyverse)
library(plyr)
library(readr)
library(arules)
library(arulesViz)
library(dplyr)
library(gcookbook)

### 365 Days of Code Day 1.1 ###

ggplot(tg, aes(x=dose, y=length, colour=supp)) +
    geom_line()

str(tg)
summary(tg)

ggplot(tg, aes(x=dose, y=length, linetype=supp)) +
   geom_line()

ggplot(tg, aes(x=dose, y=length)) +
   geom_line()

ggplot(tg, aes(x=dose, y=length, shape=supp)) +
  geom_line() +
  geom_point(size=4)  # Make the points a little larger

ggplot(tg, aes(x=dose, y=length, fill=supp)) +
  geom_line() +
  geom_point(size = 4, shape = 21) # Also use a point with a color fill

ggplot(BOD, aes(x= Time, y=demand)) +
    geom_line(linetype = "dashed", size = 1, colour = "blue")

ggplot(tg, aes(x=dose, y=length, colour=supp)) +
  geom_line() +
  scale_colour_brewer(palette = "Set1")

ggplot(tg, aes(x=dose, y=length, colour=supp)) +
    geom_line(linetype = "dashed") +
    geom_point(shape = 22, size = 3, fill = "white")

ggplot(BOD, aes(x = Time, y = demand)) +
  geom_line() +
  geom_point(size=4, shape=22, colour="darkred", fill="red")

sunspotyear <- data.frame(
     Year = as.numeric(time(sunspot.year)),
     Sunspots = as.numeric(sunspot.year)
)

ggplot(sunspotyear, aes(x = Year, y = Sunspots)) +
  geom_area(colour="black", fill="blue", alpha=.4, size=.2) +
  scale_fill_brewer(palette = "Blues")

ggplot(uspopage, aes(x = Year, y = Thousands, fill = AgeGroup)) +
       geom_area(colour = "black", size = .2, alpha = .4) +
       scale_fill_brewer(palette = "Blues")

ggplot(uspopage, aes(x = Year, y = Thousands, fill = AgeGroup)) +
       geom_area(position = "fill", colour = "black", size = .2, alpha = .4) +
       scale_fill_brewer(palette = "Blues") +
       scale_y_continuous(labels = scales::percent)

climate_mod <- climate %>%
     filter(Source == "Berkeley") %>%
     select(Year, Anomaly10y, Unc10y)

ggplot(climate_mod, aes(x = Year, y = Anomaly10y)) +
geom_line(aes(y=Anomaly10y - Unc10y), colour = "grey50", linetype = "dotted")+
geom_line(aes(y=Anomaly10y + Unc10y), colour = "grey50", linetype = "dotted")+
geom_line()

heightweight %>%
  select(ageYear, heightIn)

ggplot(heightweight, aes(x=ageYear, y=heightIn)) +
   geom_point(shape = 21)

ggplot(heightweight, aes(x=ageYear, y=heightIn)) +
  geom_point(shape = 1.5)

ggplot(heightweight, aes(x = ageYear, y = heightIn, shape = sex, colour = sex)) +
    geom_point() +
    scale_shape_manual(values = c(1,2)) +
    scale_colour_brewer(palette = "Set1")

ggplot(heightweight, aes(x = ageYear, y = heightIn)) +
   geom_point(shape = 3)

ggplot(heightweight, aes(x = ageYear, y = heightIn, shape = sex)) +
   geom_point(size = 3) +
   scale_shape_manual(values = c(1,4))

hw <- heightweight %>%
  mutate(weightgroup = ifelse(weightLb < 100, "< 100", ">= 100"))