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

panel.lm <- function(x, y, col = par("col"), bg = NA, pch = par("pch"),
                     cex = 1, col.smooth = "black",...) {
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  abline(stats::lm(y ~ x), col = col.smooth, ...)
}

pairs(
  c2009,
  upper.panel = panel.cor,
  diag.panel = panel.hist,
  lower.panel = panel.smooth,
  pch = "."
)

ggplot(faithful, aes(x = waiting)) +
  geom_histogram()

w <- faithful$waiting

ggplot(NULL, aes(x = w)) +
  geom_histogram()

library(MASS)

ggplot(birthwt, aes(x = bwt)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(smoke ~ .)

birthwt_mod <- birthwt

birthwt_mod$smoke <- recode_factor(birthwt_mod$smoke, 
                                    '0' = 'No Smoke',
                                    '1' = 'Smoke')

ggplot(birthwt_mod, aes(x = bwt)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(smoke ~ .)

ggplot(birthwt_mod, aes(x = bwt, fill = smoke)) +
  geom_histogram(position = "identity", alpha = 0.4)

ggplot(faithful, aes(x = waiting)) +
  geom_density()

ggplot(faithful, aes(x = waiting)) +
  geom_line(stat = "density") +
  expand_limits(y = 0)

w <- faithful$waiting

ggplot(NULL, aes(x = w)) +
  geom_density()

ggplot(faithful, aes(x = waiting)) +
  geom_line(stat = "density") +
  geom_line(stat = "density", adjust = .25, colour = "red") +
  geom_line(stat = "density", adjust = 2, colour = "blue")

ggplot(faithful, aes(x = waiting)) +
  geom_density(fill = "blue", alpha = .2) +
  xlim(35, 105)

ggplot(faithful, aes(x = waiting)) +
  geom_density(fill = "blue", alpha = .2, colour = NA) +
  xlim(35, 105) +
  geom_line(stat = "density")

ggplot(faithful, aes(x = waiting, y = ..density..)) +
  geom_histogram(fill = "cornsilk", colour = "grey60", size = .2) +
  geom_density() +
  xlim(35, 105)

library(MASS)

birthwt_mod <- birthwt %>%
  mutate(smoke = as.factor(smoke))

ggplot(birthwt_mod, aes(x = bwt, colour = smoke)) +
  geom_density()

ggplot(birthwt_mod, aes(x = bwt, fill = smoke)) +
  geom_density(alpha = .3)

ggplot(birthwt_mod, aes(x = bwt)) +
  geom_density() +
  facet_grid(smoke ~ .)

levels(birthwt_mod$smoke)

birthwt_mod$smoke <- recode(birthwt_mod$smoke, '0' = 'No Smoke', '1' = 'Smoke')

ggplot(birthwt_mod, aes(x = bwt)) +
  geom_density() +
  facet_grid(smoke ~ .)

ggplot(birthwt_mod, aes(x = bwt, y = ..density..)) +
  geom_histogram(binwidth = 200, fill = "cornsilk", colour = "grey60", size = .2) +
  geom_density() +
  facet_grid(smoke ~ .)

ggplot(faithful, aes(x=waiting)) +
  geom_freqpoly() 

ggplot(faithful, aes(x = waiting)) +
  geom_freqpoly(binwidth = 4)

binsize <- diff(range(faithful$waiting))/15
ggplot(faithful, aes(x = waiting)) +
  geom_freqpoly(binwidth = binsize)


ggplot(birthwt, aes(x = factor(race), y = bwt)) +
  geom_boxplot()

ggplot(birthwt, aes(x = factor(race), y = bwt)) +
  geom_boxplot(width = .5)

ggplot(birthwt, aes(x = factor(race), y = bwt)) +
  geom_boxplot(outlier.size = 1.5, outlier.shape = 21)

ggplot(birthwt, aes(x = 1, y = bwt)) +
  geom_boxplot() +
  scale_x_continuous(breaks = NULL) +
  theme(axis.title.x = element_blank())

ggplot(birthwt, aes(x = factor(race), y = bwt)) +
  geom_boxplot(notch = TRUE)

ggplot(birthwt, aes(x = factor(race), y = bwt)) +
  geom_boxplot() +
  stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 3, fill = "white")

hw_p <- ggplot(heightweight, aes(x = sex, y = heightIn))

hw_p + geom_violin()

ggplot(heightweight, aes(x = sex, y = heightIn)) +
  geom_boxplot(outlier.colour = NA, width = .4) +
  geom_dotplot(binaxis = "y", binwidth = .5, stackdir = "center", fill=NA)

ggplot(heightweight, aes(x = sex, y = heightIn)) +
  geom_boxplot(aes(x = as.numeric(sex) + .2, group = sex), width = .25) +
  geom_dotplot(aes(x = as.numeric(sex) - .2, group = sex),
               binaxis = "y",
               binwidth = .5,
               stackdir = "center") +
  scale_x_continuous(
    breaks = 1:nlevels(heightweight$sex),
    labels = levels(heightweight$sex)
  )

faithful_p <-ggplot(faithful, aes(x = eruptions, y = waiting)) 

faithful_p + 
  geom_point() +
  stat_density2d()

faithful_p +
  stat_density2d(aes(colour = ..level..))

faithful_p +
  stat_density2d(aes(fill = ..density..), 
                 geom = "raster",
                 contour = FALSE,
                 h = c(.5, 5)
  ) 

p <- ggplot(faithful, aes(x = eruptions, y = waiting)) +
  geom_point()

p + 
  annotate("text", x=3, y=48, label = "Group 1") +
  annotate("text", x=4.5, y=66, label = "Group 2" )

p <- ggplot(data.frame(x = c(-3,3)), aes(x = x)) +
  stat_function(fun = dnorm)

p + annotate("text", x = 2, y = 0.3, parse = TRUE,
             label = "frac(1, sqrt(2 * pi)) * e ^ {-x^2 / 2 }")

p + annotate("text", x = 0, y = 0.05, parse = TRUE, size =4,
             label = "'Function: ' * y==frac(1, sqrt(2*pi)) * e^{-x^2/2}")

hw_plot <- ggplot(heightweight, aes(x = ageYear, y = heightIn,
colour = sex)) +
  geom_point()

hw_plot +
  geom_hline(yintercept = 60) +
  geom_vline(xintercept = 14)

hw_plot + 
  geom_abline(intercept = 37.4, slope = 1.73)

hw_plot <- ggplot(heightweight, aes(x=ageYear, y=heightIn,
                                    colour=sex)) +
                                    geom_point()

hw_plot +
  geom_hline(yintercept = 60) +
  geom_vline(xintercept = 14)

hw_means <- heightweight %>%
  group_by(sex) %>%
  summarise(heightIn = mean(heightIn))

hw_means

hw_plot +
  geom_hline(
    data = hw_means,
    aes(yintercept = heightIn, colour = sex),
    linetype = "dashed",
    size = 1
  )

p <- ggplot(filter(climate, Source == "Berkeley"),
            aes(x = Year, y = Anomaly10y)) +
  geom_line()

p + annotate("segment", x= 1950, xend = 1980, y = -.25, yend = -.25)

library(grid)

p + annotate("segment", x = 1850, xend = 1820, y = -.8, yend = -.95,
             colour = "blue", size = 2, arrow = arrow()) +
  annotate("segment", x = 1950, xend = 1980, y = -.25, yend = -.25,
           arrow = arrow(ends = "both", angle = 90, length = unit(.2, "cm")))

p <- ggplot(filte(client, Source == "Berkeley"),
            aes(x = Year, y = Anomaly10y)) +
  geom_line()

p + annotate("rect", xmin = 1950, xmax = 1980, ymin = -1, ymax = 1,
             alpha = .1, fill = "blue")

pg_mod <- PlantGrowth %>%
  mutate(h1 = recode(group, "ctrl" = "no", "trt1" = "no", "trt2" = "yes"))

ggplot(pg_mod, aes(x = group, y = weight, fill = h1)) +
  geom_boxplot() +
  scale_fill_manual(values = c("grey85","#FFDDCC"), guide = FALSE)
  
ce_mod <- cabbage_exp %>%
  filter(Cultivar == "c39")

ggplot(ce_mod, aes(x = Date, y = Weight)) +
  geom_col(fill = "white", colour = "black") +
  geom_errorbar(aes(ymin = Weight - se, ymax = Weight + se),
  width = .2)

ggplot(ce_mod, aes(x = Date, y = Weight)) +
  geom_line(aes(group = 1)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = Weight -se, ymax = Weight + se),
  width = .2)

ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = Weight - se, ymax = Weight + se), 
                position = "dodge", width = .2)

ggplot(cabbage_exp, aes(x = Date, y = Weight, fill = Cultivar)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = Weight - se, ymax = Weight + se),
                position = position_dodge(0,9, width = .2))

pd <- position_dodge(.3)

ggplot(cabbage_exp, aes(x = Date, y = Weight, colour = Cultivar, group = Cultivar)) +
  geom_errorbar(aes(ymin = Weight - se, ymax = Weight + se),
                width = .2,
                size = 0.25,
                colour = "black",
                position = pd
                ) +
  geom_line(position = pd) +
  geom_point(position = pd, size = 2.5)


mpg_plot <- ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  facet_grid(. ~ drv)

f_labels <- data.frame(drv = c("4", "f", "r"), lbel = c("4wd", "Front", "Rear"))

mpg_plot + 
  geom_text(x = 6, y = 40, aes(label = label), data = f_labels)

mpg_plot +
  annotate("text", x = 6, y = 42, label = "label text")

lm_labels <- function(dat) {
  mod <- lm(hwy ~ displ, data = dat)
  formula <- sprintf("italic(y) == %.2f %.2f * italic(x)", 
                     round(coef(mod)[1], 2), round(coef(mod)[2],2))
r <- cor(dat$displ, dat$hwy)
r2 <- sprintf("italic(R^2) == %.2f", r^2)
data.frame(formula = formula, r2 = r2, stringsAsFactors = FALSE)
}

labels <- mpg %>%
  group_by(drv) %>%
  do(lm_labels(.))

labels

mpg_plot +
  geom_smooth(method = lm, se = FALSE) +
  geom_text(data = labels, aes(label = formula), x = 3, y = 40, parse = TRUE, hjust = 0) +
  geom_text(x = 3, y =35, aes(label = r2), data = labels, parse = TRUE, hjust = 0)

hw_plot <- ggplot(heightweight, aes(x = ageYear, y = heightIn)) +
  geom_point()

hw_plot +
  theme(axis.line = element_line(colour = "black"))

hw_plot +
  theme_bw() +
  theme(panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))

hw_plot +
  theme_bw() +
  theme(
    panel.border = element_black(),
    axis.line = element_line(colour = "black", size = 4)
  )

hw_plot + 
  theme_bw() +
  theme(
    panel.border = element_blank(),
    axis.line = element_line(colour = "black", size = 4, lineend = "square")
  )

ggplot(wind, aes(x = DirCat, fill = SpeedCat)) +
  geom_histogram(binwidth = 15, boundary = -7.5) +
  coord_polar() +
  scale_x_continuous(limits = c(0, 360))

ggplot(wind, aes(x = DirCat, fill = SpeedCat)) +
  geom_histogram(binwidth = 15, boundary = -7.5, colour = "black", size = .25) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_x_continuous(limits = c(0, 360),
                     breaks = seq(0, 360, by = 45),
                     minor_breaks = seq(0, 360, by = 15)) +
  scale_fill_brewer()

ggplot(wind, aes(x = DirCat, fill = SpeedCat)) +
  geom_histogram(binwidth = 15, boundary = -7.5, colour = "black", size = .25) +
  guides(fill = guide_legend(reverse = TRUE)) +
  coord_polar() + 
  scale_x_continuous(limits = c(0, 360),
                     breaks = seq(0, 360, by = 45),
                     minor_breaks = seq(0, 360, by = 15)) +
  scale_fill_brewer()

mdeaths_mod <- data.frame(
  deaths = as.numeric(mdeaths),
  month = as.numeric(cycle(mdeaths))
)

mdeaths_mod

mdeaths_plot <- ggplot(mdeaths_mod, aes(x = month, y = deaths)) +
  geom_line() +
  scale_x_continuous(breaks = 1:12)

mdeaths_plot + coord_polar() +
  ylim(0, max(mdeaths_mod$deaths)) +
  xlim(0, 12)


mdeaths_x <- mdeaths_mod[mdeaths_mod$month==12,]
mdeaths_x$month <- 0
mdeaths_new <- rbind(mdeaths_x, mdeaths_mod)

hw_plot <- ggplot(heightweight, aes(x = ageYear, y = heightIn, colour = sex)) +
  geom_point()

hw_plot + 
  theme(panel.grid.major = element_line(colour = "red"),
        panel.grid.minor = element_line(colour = "red", linetype = "dashed", size = 0.2),
        panel.background = element_rect(fill = "lightblue"),
        panel.border = element_rect(colour = "blue", fill = NA, size = 2)
        )

hw_plot +
  theme(
    legend.background = element_rect(fill = "grey85", colour = "red", size = 1),
    legend.title = element_text(colour = "blue", face = "bold", size = 14),
    legend.text = element_text(colour = "red"),
    legend.key = element_rect(colour = "blue", size = 0.25))

hw_plot + 
ggtitle("Plot title here") +
  theme(
    axis.title.x = element_text(colour = "red", size =14),
    axis.text.x = element_text(colour = "blue"),
    axis.title.y = element_text(colour = "red", size = 14, angle = 90),
    axis.text.y = element_text(colour = "blue"),
    plot.title = element_text(colour = "red", size = 20, face = "bold")
  )

hw_plot +
  facet_grid(sex ~ .) +
  theme(
    strip.background = element_rect(fill = "pink"),
    strip.text.y = element_text(size = 14, angle = -90, face = "bold")
  )
  
mpg_plot <-- ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point()

mpg_plot + facet-gid(drv ~ .)

mpg_plot + facet_grid(. ~ cyl)

mpg_plot + facet_grid(drv ~ cyl)

mpg_plot + facet_wrap( ~ class)

# Colour blind virdis color palette.  Implement for data that is both continous and discrete in nature.

uspopage_plot <- ggplot(uspopage, aes(x = Year, y = Thousands, fill = AgeGroup)) +
  geom_area()

uspopage_plot + 
  scale_fill_viridis_d()

climate_mod <- climate %>%
  filter(Source == "Berkeley") %>%
  mutate(valence = if_else(Anomaly10y >= 0, "pos", "neg"))

ggplot(climate_mod, aes(x = Year, y = Anomaly10y)) +
  geom_area(aes(fill = valence)) +
  geom_line() +
  geom_hline(yintercept = 0)


mcor <- cor(mtcars)

round(mcor, digits = 2)

library(corrplot)
corrplot(mcor)

corrplot(mcor, method = "shade", shade.col = NA, tl.co1 = "black", tl.srt = 45)

co1 <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#477AA"))

corrplot(mcor, method = "shade", shade.co1 = NA, t1.co1 = "black", t1.srt = 45,
         co1 = co1(200), addCoef.co1 = "black", c1.pos = "n", order = "AOE")
