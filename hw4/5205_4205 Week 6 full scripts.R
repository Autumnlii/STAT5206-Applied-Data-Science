

## Slide 7

setwd("~/Desktop/Week6")
diamonds         <- read.csv("diamonds.csv", as.is = T)
diamonds$cut     <- factor(diamonds$cut)
diamonds$color   <- factor(diamonds$color)
diamonds$clarity <- factor(diamonds$clarity)


## Slide 8

set.seed(1)
rows <- dim(diamonds)[1]
diam <- diamonds[sample(1:rows, 1000), ]

## Slide 9

plot(log(diam$carat), log(diam$price), col = diam$cut)
legend("bottomright", legend = levels(diam$cut), 
       fill = 1:length(levels(diam$cut)), cex = .5)


## Slide 12

abline(8, 0, col = "orange", lty = 2)
lm1 <- lm(log(diam$price) ~ log(diam$carat))
abline(lm1)


## Slide 14


cuts        <- levels(diam$cut)
col_counter <- 1

for (i in cuts) {
  this_cut    <- diam$cut == i
  this_data   <- diam[this_cut, ]
  this_lm     <- lm(log(this_data$price) 
                    ~ log(this_data$carat))
  abline(this_lm, col = col_counter)
  col_counter <- col_counter + 1
}


## Slide 18


points(-0.4, 6.8,  pch = "*", col = "purple")


## Slide 20


text(-0.4, 6.8 - .2, "New Diamond", cex = .5)



## Slide 37


pie.chart <- function(data) {
  print("I suck")
}
pie.chart(c("Red","Red","Blue"))


## Slide 41

library(ggplot2)
dim(mpg)
head(mpg, 3)


## Slide 42

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))




## Slide 48


ggplot(data = mpg) + 
  geom_point(mapping = aes(x=displ, y=hwy, color=class))



## Slide 51

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_wrap(~ class, nrow = 2)




## Slide 42


ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ class)




## Slide 55


ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy))


## Slide 58


ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(x = displ, y = hwy))


## Slide 59


ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(x=3, y=30), color = "purple") +
  geom_text(mapping = aes(x=3, y=31, label = "New Point"), size=4) +
  labs(title = "New Plot", x = "Engine Weight", y = "Highway mpg")


## Slide 60

ggplot(data = diamonds) + 
  geom_point(mapping = aes(x = carat, y = price), 
             alpha = 1/10)


## Slide 

