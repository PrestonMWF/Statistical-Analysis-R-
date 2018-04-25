library(dplyr)
library(ggplot2)
library(ggfortify)
library(reshape2)

regression_data <- read.csv("Week4_StatisticalAnalysis.csv")

head(regression_data)

theme_set(
  theme_bw()
)

regression_data %>%
  ggplot(aes(Input, Output)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = F) +
  labs(title = "Input vs output data for regression set- linear correlation is evident")

linear_model <- lm(Output ~ Input, data = regression_data)

summary(linear_model)$coefficients

#checking residual sigma in different methods
summary(linear_model)$sigma ^ 2

var(linear_model$residuals) * 999/998

sum(linear_model$residuals ^ 2) / linear_model$df.residual

autoplot(linear_model, which = 1)

regression_data <- regression_data %>%
  mutate(original.residuals = linear_model$residuals,
         residual.split = ifelse(original.residuals >= 0, "positive", "negative"))

regression_data %>%
  ggplot(aes(original.residuals)) +
  geom_density(aes(colour = "Empirical"), size = 1.3) +
  stat_function(aes(colour = "Normal"), size = 1.3, fun = dnorm) +
  scale_colour_manual(name = "Density", values = c("royalblue2", "orange")) +
  labs(title = "Normal vs empirical density- residuals do not conform to theoretical bounds")

residual_sequence <- ifelse(
  regression_data$residuals >= 0, 1, 0
)

head(residual_sequence, 10)

regression_data %>%
  ggplot(aes(Input, Output, colour = residual.split)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = F, colour = "royalblue2", size = 1.3) +
  labs(title  = "Input vs output data for regression set with residual split added- regression line highlights divide")

#linear models for both groups
positive_group <- regression_data %>%
  filter(residual.split == "positive")

positive_lm <- lm(Output ~ Input, data = positive_group)

summary(positive_lm)$coefficients

summary(positive_lm)

negative_group <- regression_data %>%
  filter(residual.split == "negative")

negative_lm <- lm(Output ~ Input, data = negative_group)

summary(negative_lm)$coefficients

summary(negative_lm)

#residual comparison
split_residuals <- data.frame(
  split.residuals = positive_lm$residuals
)

split_residuals2 <- data.frame(
  split.residuals = negative_lm$residuals
)

split_residuals <- bind_rows(split_residuals, split_residuals2)

split_residuals <- split_residuals %>%
  mutate(index = 1:1000,
         original.residuals = regression_data$original.residuals)

split_residuals <- melt(split_residuals, 
                        id.vars = "index",
                        value.name = "residuals", 
                        variable.name = "residual.split")

split_residuals %>%
  ggplot(aes(index, residuals, colour = residual.split)) +
  geom_point(alpha = .5) +
  scale_colour_manual(name = "residual split", values = c("red", "lightgray")) +
  geom_hline(yintercept = 1, colour = "darkgray", size = 1.3) +
  geom_hline(yintercept = -1, colour = "darkgray", size = 1.3) +
  labs(title = "Split model approach has much better fit than original as seen by residuals")

#test- with new data
test_data <- read.csv("Week4_Test_Sample.csv", sep = "")

test_lm <- lm(Y ~ X, data = test_data)

summary(test_lm)$coefficients

summary(test_lm)

autoplot(linear_model, which = 1)

test_data <- test_data %>%
  mutate(original.residuals = test_lm$residuals,
         residual.split = ifelse(original.residuals >= 0, "positive", "negative"))

test_data %>%
  ggplot(aes(original.residuals)) +
  geom_density(aes(colour = "Empirical"), size = 1.3) +
  stat_function(aes(colour = "Normal"), size = 1.3, fun = dnorm) +
  scale_colour_manual(name = "Density", values = c("royalblue2", "orange")) +
  labs(title = "Normal vs empirical density- residuals do not conform to theoretical bounds")

residual_sequence <-  data.frame(
  Unscrambled.Selection.Sequence = ifelse(
    test_data$original.residuals >= 0, 1, 0
  )
)

head(residual_sequence, 10)

test_data %>%
  ggplot(aes(X, Y, colour = residual.split)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = F, colour = "royalblue2", size = 1.3) +
  labs(title  = "Input vs output data for regression set with residual split added- regression line highlights divide")

write.csv(residual_sequence, "week4_test_upload.csv", row.names = F)
