install.packages("arules")
install.packages("arulesViz")
install.packages("tidymv")
library("ggplot2")
library(tidyverse)
library(plyr)
library(readr)
library(arules)
library(arulesViz)
library(dplyr)
library(gcookbook)
library(mgcv)
library(tidymv)
install.packages("plotly")
library(plotly)
require("ISLR")
library(ggrepel)
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

ggplot(hw, aes(x = ageYear, y = heightIn, shape = sex, fill = weightgroup))+
  geom_point(size = 2.5) +
  scale_shape_manual(values = c(21, 24)) +
  scale_fill_manual(values = c(NA, "black"), 
  guide = guide_legend(override.aes = list(shape = 21)))

ggplot(heightweight, aes(x = ageYear, y = heightIn, colour = weightLb)) +
  geom_point()

range(heightweight$weightLb)

ggplot(heightweight, aes(x = ageYear, y=heightIn, size=weightLb)) +
  geom_point() +
  scale_size_continuous(range = size_range)

diamonds_sp <- ggplot(diamonds, aes(x = carat, y = price))

diamonds_sp + 
   geom_point()

diamonds_sp +
  stat_bin2d()

diamonds_sp +
  stat_bin2d(bins = 50) +
  scale_fill_gradient(low = "lightblue", high = "red", limits = c(0, 6000))

cw_sp <- ggplot(ChickWeight, aes(x = Time, y = weight))

cw_sp + 
  geom_point()

cw_sp +
  geom_point(position = position_jitter(width = .5, height = 0))

cw_sp + 
  geom_boxplot(aes(group = Time))

hw_sp <- ggplot(heightweight, aes(x = ageYear, y = heightIn))

hw_sp +
  geom_point() +
  stat_smooth(method=lm)

hw_sp +
  geom_point() +
  stat_smooth(method=lm, level=0.99)

hw_sp +
  geom_point() +
  stat_smooth(method = lm, se = FALSE)

hw_sp +
  geom_point(colour = "grey60") +
  stat_smooth(method = loess)

library(MASS)

biopsy_mod <- biopsy %>%
  mutate(classn = recode(class, benign = 0, malignant = 1))

ggplot(biopsy_mod, aes(x = V1, y = classn)) +
  geom_point(
    position = position_jitter(width = 0.3, height = 0.06),
    alpha = 0.4,
    shape = 21,
    size = 1.5
  ) + 
  stat_smooth(method = glm, method.args = list(family = binomial))
  
  
hw_sp <- ggplot(heightweight, aes(x = ageYear, y = heightIn, colour = sex)) +
  geom_point() +
  scale_colour_brewer(palette = "Set1")

hw_sp +
  geom_smooth()

hw_sp +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE)

model <- lm(heightIn ~ ageYear, heightweight)

pred <- predict.glm(model, se.fit = FALSE)
pred_list <- list(x=c(pred))
d1 <- as.data.frame(pred_list)

hw_sp <- ggplot(heightweight, aes(x = ageYear, y = heightIn)) +
   geom_point() +
   geom_line(data = d1)

hw_sp + annotate("text", x=16.5, y=52, label="r^2=0.42")

hw_sp + annotate("text", x=16.5, y=52, label = "r^2 == 0.42", parse = TRUE)
#pred <- predictvals(model, "ageYear", "heightIn")

model <- gam(heightIn ~ ageYear, data=heightweight)

summary(model)

model_p <- predict_gam(model)
model_p

model_p %>%
  ggplot(aes(ageYear,fit)) +
  geom_smooth_ci()

ggplot(faithful, aes(x = eruptions, y = waiting)) +
  geom_point() +
  geom_rug()

ggplot(faithful, aes(x = eruptions, y = waiting)) +
  geom_point() +
  geom_rug(position = "jitter", size = 0.2)



countries_sub <- countries %>%
  filter(Year == 2009 & healthexp > 2000)

countries_sp <- ggplot(countries_sub, aes(x = healthexp, y = infmortality)) +
  geom_point()

countries_sp + 
  annotate("text", x=4350, y = 5.4, label = "Canada") +
  annotate("text", x=7400, y = 6.8, label = "USA")

countries_sp +
  geom_text(aes(label = Name), size = 4)

countries_sp + 
  geom_text_repel(aes(label = Name), size = 3)

countries_sp +
  geom_label_repel(aes(label = Name), size = 3)

c2009 <- countries %>%
  filter(Year == 2009)
 # select("Name", "GDP", "laborrate", "healthexp", "infmortality")

pairs(c2009)

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...){
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, use = "complete.obs"))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste(prefix, txt, sep = "")
  if (missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * (1 + r) / 2)  
}

panel.hist <- function(x, ...){
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5))
  h <- hist(x, plo = FALSE)
  breaks <- h$breaks
  nB <- length(breaks)
  y <- h$counts
  y <-- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "white", ...)
}

pairs(
   c2009,
   upper.panel = panel.cor,
   diag.panel = panel.hist,
   lower.panel = panel.smooth
)
