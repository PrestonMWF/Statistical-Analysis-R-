library(dplyr)
library(ggplot2)
library(gridExtra)

test1 <- read.csv("Week1_Test_Sample.csv", sep = "")

result <- test1 %>%
  summarise(sdX = sd(x),
            sdY = sd(y),
            cXY = cor(x, y),
            a = cXY * (sdY/sdX))

test1_lm <- lm(y ~ x, data = test1)

paste("Estimated slope was", round(result$a, 4),
      "while actual slope is", round(test1_lm$coefficients[[2]], 4),
      "; results are the same")

theme_set(
  theme_bw()
)

test1 %>%
  ggplot(aes(x, y)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = F, colour = "red") +
  labs(title = "Test 1 Set Scatterplot",
       subtitle = "x.sd = .2.94 ; y.sd = 1.86 ; cor = .80 ; a = .51")

write.csv(result, "test1_result.csv", row.names = F)

#Exploring cases with different x and y sd
correlation_set <- read.csv("Correlation_Comparison_Project_Data.csv")

#Case 1
correlation_set %>%
  summarise(x.sd = sd(Case1.X),
            y.sd = sd(Case1.Y),
            correlation = cor(Case1.X, Case1.Y),
            R2 = cor(Case1.X, Case1.Y) ^ 2)

case1_lm <- lm(Case1.Y ~ Case1.X, data = correlation_set)

summary(case1_lm)

case1 <- correlation_set %>%
  ggplot(aes(Case1.X,Case1.Y)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = F, colour = "red") +
  labs(title = "x.sd = .33; y.sd = 2.56; cor = .31 ; R2 = .09")

#Case 2
correlation_set %>%
  summarise(x.sd = sd(Case2.X),
            y.sd = sd(Case2.Y),
            correlation = cor(Case2.X, Case2.Y),
            R2 = cor(Case2.X, Case2.Y) ^ 2)

case2_lm <- lm(Case2.Y ~ Case2.X, data = correlation_set)

summary(case2_lm)

case2 <- correlation_set %>%
  ggplot(aes(Case2.X,Case2.Y)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = F, colour = "red") +
  labs(title = "x.sd = .72; y.sd = .89; cor = .27 ; R2 = .08")

#Case 3
correlation_set %>%
  summarise(x.sd = sd(Case3.X),
            y.sd = sd(Case3.Y),
            correlation = cor(Case3.X, Case3.Y),
            R2 = cor(Case3.X, Case3.Y) ^ 2)

case3 <- correlation_set %>%
  ggplot(aes(Case3.X,Case3.Y)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = F, colour = "red") +
  labs(title = "x.sd = .44; y.sd = .32; cor = .30 ; R2 = .09")

#Case 4
correlation_set %>%
  summarise(x.sd = sd(Case4.X),
            y.sd = sd(Case4.Y),
            correlation = cor(Case4.X, Case4.Y),
            R2 = cor(Case4.X, Case4.Y) ^ 2)

case4 <- correlation_set %>%
  ggplot(aes(Case4.X,Case4.Y)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = F, colour = "red") +
  labs(title = "x.sd = 1.01; y.sd = .23; cor = .42 ; R2 = .17")

grid.arrange(case1, case2, case3, case4, nrow = 2)