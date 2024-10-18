library(lme4)
library(ggeffects)
library(tidyverse)
library(sjPlot)
library(dplyr)
library(scales)
library(ggplot2)
library(lmerTest)

# Constants
PROMPTS_PER_DAY <- 12
PLOTS_DIR <- "outputs/imgs/binomial/"
RESIDUALS_DIR <- paste0(PLOTS_DIR, "residuals/")
dir.create(PLOTS_DIR, showWarnings = TRUE)

# Read the data
# This gets created the binomial_models.ipynb notebook
# Effectively it's just a CSV file with the p_id, day, sex, number of prompts and number of responses in it
# The numbers of prompts and responses are capped at 12
model_df <- read_csv("outputs/data/compliance.csv")

fit_model <- function(formula, data, file_prefix) {
  model <- glmer(formula, data = data, family = binomial)
  capture.output(summary(model), file = paste0(PLOTS_DIR, file_prefix, "_model.txt"))
  return(model)
}

plot_sex_model <- function(model, file_prefix, y_label) {
  df <- ggpredict(model, terms = c("day", "sex"))

  plot <- ggplot(df, aes(x, predicted)) +
    geom_line(aes(linetype = group, color = group)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
    xlab("Study Day") +
    ylab(y_label) +
    scale_fill_manual(values = c("0" = "blue", "1" = "red"), labels = c("0" = "Male", "1" = "Female")) +
    scale_color_manual(values = c("0" = "blue", "1" = "red"), labels = c("0" = "Male", "1" = "Female")) +
    scale_linetype_manual(values = c("0" = "dashed", "1" = "solid"), labels = c("0" = "Male", "1" = "Female")) +
    labs(fill = "", color = "", linetype = "") +
    scale_y_continuous(limits = c(0.0, 1.05), label = percent_format(accuracy = 10), breaks = seq(0, 1, 0.1)) +
    scale_x_continuous(breaks = seq(1, 7, 1))

  ggsave(paste0(PLOTS_DIR, file_prefix, "_model.png"), plot)
}

plot_age_model <- function(model, file_prefix, y_label) {
  df <- ggpredict(model, terms = c("day", "age_group"))

  plot <- ggplot(df, aes(x, predicted)) +
    geom_line(aes(linetype = group, color = group)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
    xlab("Study Day") +
    ylab(y_label) +
    scale_fill_manual(values = c("0" = "blue", "1" = "red"), labels = c("0" = "Children", "1" = "Adolescents")) +
    scale_color_manual(values = c("0" = "blue", "1" = "red"), labels = c("0" = "Children", "1" = "Adolescents")) +
    scale_linetype_manual(values = c("0" = "dashed", "1" = "solid"), labels = c("0" = "Children", "1" = "Adolescents")) +
    labs(fill = "", color = "", linetype = "") +
    scale_y_continuous(limits = c(0.0, 1.05), label = percent_format(accuracy = 10), breaks = seq(0, 1, 0.1)) +
    scale_x_continuous(breaks = seq(1, 7, 1))

  ggsave(paste0(PLOTS_DIR, file_prefix, "_model.png"), plot)
}

plot_day_model <- function(model, file_prefix, y_label) {
  terms <- c("day")

  plot <- plot_model(model, type = "pred", terms = terms, show.rug = FALSE, ci_lvl = 0.95) +
    scale_y_continuous(limits = c(0.0, 1.05), label = percent_format(accuracy = 10), breaks = seq(0, 1, 0.1)) +
    scale_x_continuous(breaks = seq(1, 7, 1)) +
    ggtitle("") +
    xlab("Study Day") +
    ylab(y_label) +
    labs(fill = "", color = "")
  ggsave(paste0(PLOTS_DIR, file_prefix, "_model.png"), plot)
}

# Compliance model
compliance_model <- fit_model(
  cbind(n_responses, PROMPTS_PER_DAY - n_responses) ~ day + (1 + day | p_id),
  model_df,
  "compliance"
)
plot_day_model(compliance_model, "compliance", "Compliance Rate")

# Compliance with sex as a covariate
compliance_sex_model <- fit_model(
  cbind(n_responses, PROMPTS_PER_DAY - n_responses) ~ day * sex + (1 + day | p_id),
  model_df,
  "compliance_sex"
)
plot_sex_model(compliance_sex_model, "compliance_sex", "Compliance Rate")

# Compliance model with age as a covariate
compliance_age_model <- fit_model(
  cbind(n_responses, PROMPTS_PER_DAY - n_responses) ~ day * age_group + (1 + day | p_id),
  model_df,
  "compliance_age"
)
plot_age_model(compliance_age_model, "compliance_age", "Compliance Rate")

# Completion model
completion_model <- fit_model(
  cbind(n_responses, n_prompts - n_responses) ~ day + (1 + day | p_id),
  model_df,
  "completion"
)
plot_day_model(completion_model, "completion", "Completion Rate")

# Completion with sex as a covariate
completion_sex_model <- fit_model(
  cbind(n_responses, n_prompts - n_responses) ~ day * sex + (1 + day | p_id),
  model_df,
  "completion_sex"
)
plot_sex_model(completion_sex_model, "completion_sex", "Completion Rate")

# Completion model with age as a covariate
completion_age_model <- fit_model(
  cbind(n_responses, n_prompts - n_responses) ~ day * age_group + (1 + day | p_id),
  model_df,
  "completion_age"
)
plot_age_model(completion_age_model, "completion_age", "Completion Rate")