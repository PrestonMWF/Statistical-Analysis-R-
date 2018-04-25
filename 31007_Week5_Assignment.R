library(dplyr)
library(ggplot2)
library(ggfortify)
library(reshape2)

regression_data <- read.csv("week5_statistical_analysis.csv", sep = ",")

str(regression_data)

theme_set(
  theme_bw()
)

regression_data %>%
  ggplot(aes(Input, Output)) +
  geom_jitter(colour = "royalblue2", alpha = .5) +
  geom_smooth(method = "lm", se = F, colour = "red") +
  labs(title = "Input vs output data")

linear_model <- lm(Output ~ Input, data = regression_data)

coefficients(linear_model)

regression_data <- regression_data %>%
  mutate(fitted.values = linear_model$fitted.values,
         original.residuals = linear_model$residuals)

summary(linear_model)

autoplot(linear_model, which = 1)

regression_data %>%
  ggplot(aes(original.residuals)) +
  geom_density(aes(colour = "Empirical"), size = 1.3) +
  stat_function(aes(colour = "Normal"), size = 1.3, fun = dnorm) +
  scale_colour_manual(name = "Density", values = c("royalblue2", "orange")) +
  labs(title = "Normal vs empirical density- residuals do not conform to theoretical bounds",
       subtitle = "Tails and central tendencies look incongruent between distributions")

input_threshold <- regression_data$Input >= 5

regression_data <- regression_data %>%
  mutate(steep.output = ifelse(input_threshold & (Output > fitted.values), 
                               Output, NA),
         gentle.output = ifelse(input_threshold & (Output <= fitted.values), 
                                Output, NA))

new_output <- regression_data %>%
  dplyr::select(Input, steep.output, gentle.output)

new_output <- melt(new_output, 
                   id.vars = "Input",
                   variable.name = "output.set",
                   value.name = "output")  
new_output %>%
  ggplot(aes(Input, output, colour = output.set)) +
  geom_jitter(na.rm = T) +
  labs(title = "Split linear model with gentle and steep slopes")

#training new models with split sample output
steep_lm <- lm(steep.output ~ Input, data = regression_data)

gentle_lm <- lm(gentle.output ~ Input, data = regression_data)

summary(steep_lm)

summary(steep_lm)$coefficients

summary(steep_lm)$sigma

summary(gentle_lm)

regression_data %>%
  ggplot(aes(Input, Output)) +
  geom_jitter(colour = "gray", alpha = .5) +
  geom_abline(intercept = gentle_lm$coefficients[1], 
              slope = gentle_lm$coefficients[2], 
              colour = "royalblue2", size = 1.3) +
  geom_abline(intercept = steep_lm$coefficients[1], 
              slope = steep_lm$coefficients[2], 
              colour = "orange", size = 1.3) +
  labs(title = "Input vs output with split linear models")

regression_data <- regression_data %>%
  mutate(steep.distance = abs(
    Output - Input * steep_lm$coefficients[2] - steep_lm$coefficients[1]),
    gentle.distance = abs(
      Output - Input * gentle_lm$coefficients[2] - gentle_lm$coefficients[1]),
    line.closeness = ifelse(steep.distance < gentle.distance, "steep", "gentle"))

regression_data %>%
  ggplot(aes(Input, Output, colour = line.closeness)) +
  geom_jitter() +
  geom_abline(intercept = gentle_lm$coefficients[1], 
              slope = gentle_lm$coefficients[2], 
              colour = "royalblue2", size = 1.3) +
  geom_abline(intercept = steep_lm$coefficients[1], 
              slope = steep_lm$coefficients[2], 
              colour = "orange", size = 1.3) +
  scale_colour_manual(values = c("black", "darkgray")) +
  labs(title = "Input vs output with split linear models",
       subtitle = "Black points closer to blue line and gray points closer to orange")

regression_data %>%
  filter(line.closeness == "steep") %>%
  count() %>%
  summarise(steep.count = n,
            mix.probability = n / nrow(regression_data) * 100)

#results not from same distribution given significance test- probability is not binomial
binom.test(x = 417, n = 1000)

#seperated sample linear models
steep_df <- regression_data %>%
  filter(line.closeness == "steep")

split_steep_lm <- lm(Output ~ Input, data = steep_df)

summary(split_steep_lm)

c(split_steep_R2 = summary(split_steep_lm)$adj.r.squared,
  original_steep_R2 = summary(steep_lm)$adj.r.squared)

gentle_df <- regression_data %>%
  filter(line.closeness == "gentle")

split_gentle_lm <- lm(Output ~ Input, data = gentle_df)

summary(split_gentle_lm)

c(split_gentle_R2 = summary(split_gentle_lm)$adj.r.squared,
  original_gentle_R2 = summary(gentle_lm)$adj.r.squared)

#residual comparison between original and split models
split_residuals <- data.frame(
  split.residuals = split_steep_lm$residuals
)

split_residuals2 <- data.frame(
  split.residuals = split_gentle_lm$residuals
)

split_residuals <- bind_rows(split_residuals, split_residuals2)

split_residuals <- split_residuals %>%
  mutate(Input = regression_data$Input,
         original.residuals = regression_data$original.residuals)

split_residuals <- melt(split_residuals, 
                        id.vars = "Input",
                        value.name = "residuals", 
                        variable.name = "residual.split")

split_residuals %>%
  ggplot(aes(Input, residuals, colour = residual.split)) +
  geom_point(alpha = .5) +
  scale_colour_manual(name = "residual split", values = c("red", "lightgray")) +
  geom_hline(yintercept = 1, colour = "darkgray", size = 1.3) +
  geom_hline(yintercept = -1, colour = "darkgray", size = 1.3) +
  scale_y_continuous(breaks = seq(-4, 4, 1)) +
  labs(title = "Split model approach has much better fit than original as seen by residuals")

split_residuals %>%
  group_by(residual.split) %>%
  summarise(residual.sd = sd(residuals))

#check residual assumptions
split_residuals %>%
  filter(residual.split == "split.residuals") %>%
  ggplot(aes(residuals)) +
  geom_histogram(fill  = "lightgray") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "red", size = 1.3) +
  labs(title = "Residuals from split model- appears normal")

#check normality using ks test
library(fitdistrplus)

residual_test <- split_residuals %>%
  filter(residual.split == "split.residuals") %>%
  dplyr::select(residuals)

residual_param <- fitdistr(residual_test$residuals, "normal")

ks.test(residual_test$residuals, "pnorm",
        residual_param$estimate[1],
        residual_param$estimate[2])

residual_test %>%
  ggplot(aes(sample = residuals)) +
  stat_qq()

#splitting using volatility clustering
regression_data <- regression_data %>%
  mutate(squared.deviations = (Output - mean(Output)) ^ 2)

regression_data %>%
  ggplot(aes(Input, squared.deviations)) +
  geom_jitter(colour = "gray", alpha = ".75") +
  geom_smooth(method = "loess", se = F, size = 1.3) +
  labs(title = "Squared deviations plot (output - avg output squared ^ 2)")

clusteringParabola <- (linear_model$fitted.values - mean(regression_data$Output)) ^ 2

regression_data <- regression_data %>%
  mutate(clustering.parabola = clusteringParabola,
         parabola.groups = ifelse(
           clusteringParabola < regression_data$squared.deviations, "steep", "flat"),
         steep.p.output = ifelse(parabola.groups == "steep", Output, NA),
         flat.p.output = ifelse(parabola.groups == "flat", Output, NA))

#matches assignment code
head(regression_data$parabola.groups, 10)

regression_data %>%
  ggplot(aes(Input, squared.deviations)) +
  geom_jitter(aes(colour = parabola.groups), alpha = ".75") +
  geom_smooth(method = "loess", se = F, size = 1.5, colour = "gray") +
  scale_colour_manual(values = c("royalblue2", "orange")) +
  labs(title = "Squared deviations plot (output - avg output squared ^ 2)",
       subtitle = "Parabola groups formed by splitting points above and below clustering parabola value")

input_mean <- mean(regression_data$Input)

regression_data %>%
  filter(Input < (input_mean - .5) | Input > (input_mean + .5)) %>%
  ggplot(aes(Input, Output)) +
  geom_jitter(aes(colour = parabola.groups), alpha = ".75") +
  scale_colour_manual(values = c("royalblue2", "orange")) +
  labs(title = "Squared deviations plot (output - avg output squared ^ 2)",
       subtitle = "Parabola groups formed by splitting points above and below clustering parabola value")

#training new models with split sample output
p.steep_lm <- lm(steep.p.output ~ Input, data = regression_data)

p.flat_lm <- lm(flat.p.output ~ Input, data = regression_data)

summary(p.steep_lm)

summary(p.steep_lm)$coefficients

summary(p.flat_lm)

regression_data %>%
  ggplot(aes(Input, Output)) +
  geom_jitter(colour = "gray", alpha = .5) +
  geom_abline(intercept = p.flat_lm$coefficients[1], 
              slope = p.flat_lm$coefficients[2], 
              colour = "royalblue2", size = 1.3) +
  geom_abline(intercept = p.steep_lm$coefficients[1], 
              slope = p.steep_lm$coefficients[2], 
              colour = "orange", size = 1.3) +
  labs(title = "Input vs output with parabola clustering split linear models")

#all model comparison
c(parabola.steep = summary(p.steep_lm)$adj.r.squared,
  split.steep = summary(split_steep_lm)$adj.r.squared,
  parabola.flat = summary(p.flat_lm)$adj.r.squared,
  split.flat = summary(split_gentle_lm)$adj.r.squared)

#test data- fit general, flat, and steep lm
test_data <- read.csv("Week5_Test_Sample.csv", sep = "")

str(test_data)

test_data %>%
  ggplot(aes(Input, Output)) +
  geom_jitter(colour = "royalblue2", alpha = .5) +
  geom_smooth(method = "lm", se = F, colour = "red") +
  labs(title = "input vs output data")

GeneralModel <- lm(Output ~ Input, data = test_data)

test_data <- test_data %>%
  mutate(squared.deviations = (Output - mean(Output)) ^ 2)

test_data %>%
  ggplot(aes(Input, squared.deviations)) +
  geom_jitter(colour = "gray", alpha = ".75") +
  geom_smooth(method = "loess", se = F, size = 1.3) +
  labs(title = "Squared deviations plot (output - avg output squared ^ 2)")

clusteringParabola <- (GeneralModel$fitted.values - mean(test_data$Output)) ^ 2

test_data <- test_data %>%
  mutate(clustering.parabola = clusteringParabola,
         parabola.groups = ifelse(
           clusteringParabola < test_data$squared.deviations, "steep", "flat"),
         steep.p.output = ifelse(parabola.groups == "steep", Output, NA),
         flat.p.output = ifelse(parabola.groups == "flat", Output, NA))

mSteep <- lm(steep.p.output ~ Input, data = test_data)

mFlat <- lm(flat.p.output ~ Input, data = test_data)

test_data %>%
  ggplot(aes(Input, Output)) +
  geom_jitter(colour = "gray", alpha = .5) +
  geom_abline(intercept = mSteep$coefficients[1], 
              slope = mSteep$coefficients[2], 
              colour = "royalblue2", size = 1.3) +
  geom_abline(intercept = mFlat$coefficients[1], 
              slope = mFlat$coefficients[2], 
              colour = "orange", size = 1.3) +
  labs(title = "Input vs output with volatility clustering using parabola boundary linear models")

c(general.fit = summary(GeneralModel)$adj.r.squared,
  steep.fit = summary(mSteep)$adj.r.squared,
  flat.fit = summary(mFlat)$adj.r.squared)

#re-train models on split data
input_threshold <- test_data$Input >= 0

test_data <- test_data %>%
  mutate(fitted.values = GeneralModel$fitted.values,
         steep.output = ifelse(input_threshold & (Output > fitted.values), 
                               Output, NA),
         gentle.output = ifelse(input_threshold & (Output <= fitted.values), 
                                Output, NA))

steep_lm <- lm(steep.output ~ Input, data = test_data)

gentle_lm <- lm(gentle.output ~ Input, data = test_data)

test_data <- test_data %>%
  mutate(steep.distance = abs(
    Output - Input * steep_lm$coefficients[2] - steep_lm$coefficients[1]),
    gentle.distance = abs(
      Output - Input * gentle_lm$coefficients[2] - gentle_lm$coefficients[1]),
    line.closeness = ifelse(steep.distance < gentle.distance, "steep", "gentle"))

steep_df <- test_data %>%
  filter(line.closeness == "steep")

split_steep_lm <- lm(Output ~ Input, data = steep_df)

summary(split_steep_lm)

gentle_df <- test_data %>%
  filter(line.closeness == "gentle")

split_gentle_lm <- lm(Output ~ Input, data = gentle_df)

summary(split_gentle_lm)

c(split_gentle_R2 = summary(split_gentle_lm)$adj.r.squared,
  original_gentle_R2 = summary(gentle_lm)$adj.r.squared,
  split_steep_R2 = summary(split_steep_lm)$adj.r.squared,
  original_steep_R2 = summary(steep_lm)$adj.r.squared)

#all test models comparison- parabola better fit
c(parabola.steep = summary(mSteep)$adj.r.squared,
  split.steep = summary(split_steep_lm)$adj.r.squared,
  parabola.flat = summary(mFlat)$adj.r.squared,
  split.flat = summary(split_gentle_lm)$adj.r.squared)

#taking parabola models for assignment ouptut
res <- list(GeneralModel = GeneralModel, mSteep = mSteep, mFlat = mSteep)

saveRDS(res, file = "result.rds")
