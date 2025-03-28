library(lme4)
library(ggeffects)
library(tidyverse)
library(sjPlot)
library(dplyr)
library(scales)
library(ggplot2)
library(lmerTest)

# Constants
PLOTS_DIR <- "outputs/imgs/three_level/"
dir.create(PLOTS_DIR, showWarnings = TRUE)

# Read the data
# This is a CSV that just tells us whether the prompt sent to a participant was responded to or not
model_df <- read_csv("outputs/data/three_level_data.csv")
model_df <- data.frame(model_df)

# Get rid of rows where hour is 8, 21, 22, or 23
model_df <- model_df %>% filter(hour %in% 9:20)

# model_df$hour <- factor(model_df$hour, levels = 9:20, ordered = TRUE)
model_df$period <- factor(
  model_df$period,
  ordered = FALSE,
  levels = c("Morning", "Lunchtime", "Afternoon", "Evening"),
  )

fit_model <- function(formula, data, file_prefix) {
  control <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000))
  model <- glmer(formula, data = data, family = binomial, control = control)
  capture.output(summary(model), file = paste0(PLOTS_DIR, file_prefix, "_model.txt"))
  return(model)
}


plot_base_model <- function(model) {
  plot <- plot_model(model, type = "pred", terms = c("day"), show.rug = FALSE, ci.lvl = 0.95) +
    scale_y_continuous(limits = c(0.0, 1.05), label = percent_format(accuracy = 10), breaks = seq(0, 1, 0.1)) +
    scale_x_continuous(breaks = seq(1, 7, 1)) +
    ggtitle("") +
    xlab("Study Day") +
    ylab("Response Rate") +
    theme_bw() +
    labs(fill = "", color = "")
  ggsave(paste0(PLOTS_DIR, "base_model_day.png"), plot, dpi=300)

  plot <- plot_model(model, type = "pred", terms = c("period"), show.rug = FALSE, ci.lvl = 0.95) +
    scale_y_continuous(limits = c(0.0, 1.05), label = percent_format(accuracy = 10), breaks = seq(0, 1, 0.1)) +
    ggtitle("") +
    xlab("Prompt Time") +
    ylab("Response Rate") +
    theme_bw() +
    labs(fill = "", color = "")
  ggsave(paste0(PLOTS_DIR, "base_model_period.png"), plot, dpi=300)
}


plot_sex_model <- function(model) {
  df_day <- ggpredict(model, terms = c("day", "sex"))
  plot <- ggplot(df_day, aes(x, predicted)) +
    geom_line(aes(linetype = group, color = group)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
    xlab("Study Day") +
    scale_fill_manual(values = c("0" = "blue", "1" = "red"), labels = c("0" = "Male", "1" = "Female")) +
    scale_color_manual(values = c("0" = "blue", "1" = "red"), labels = c("0" = "Male", "1" = "Female")) +
    scale_linetype_manual(values = c("0" = "dashed", "1" = "solid"), labels = c("0" = "Male", "1" = "Female")) +
    labs(fill = "", color = "", linetype = "") +
    theme_bw() +
    scale_y_continuous(limits = c(0.0, 1.05), label = percent_format(accuracy = 10), breaks = seq(0, 1, 0.1)) +
    scale_x_continuous(breaks = seq(1, 7, 1))
  ggsave(paste0(PLOTS_DIR, "sex_model_day.png"), plot, dpi=300)

  df_period <- ggpredict(model, terms = c("period", "sex"))
  plot <- ggplot(df_period, aes(x, predicted)) +
    geom_point(aes(color = group), size = 3) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = group), width = 0.2) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
    xlab("Prompt Time") +
    scale_fill_manual(values = c("0" = "blue", "1" = "red"), labels = c("0" = "Male", "1" = "Female")) +
    scale_color_manual(values = c("0" = "blue", "1" = "red"), labels = c("0" = "Male", "1" = "Female")) +
    scale_linetype_manual(values = c("0" = "dashed", "1" = "solid"), labels = c("0" = "Male", "1" = "Female")) +
    labs(fill = "", color = "", linetype = "") +
    theme_bw() +
    scale_y_continuous(limits = c(0.0, 1.05), label = percent_format(accuracy = 10), breaks = seq(0, 1, 0.1))
  ggsave(paste0(PLOTS_DIR, "sex_model_period.png"), plot, dpi=300)
}


plot_age_model <- function(model) {
  df_day <- ggpredict(model, terms = c("day", "age_group"))
  plot <- ggplot(df_day, aes(x, predicted)) +
    geom_line(aes(linetype = group, color = group)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
    xlab("Study Day") +
    scale_fill_manual(values = c("0" = "blue", "1" = "red"), labels = c("0" = "7-12 years", "1" = "13-18 years")) +
    scale_color_manual(values = c("0" = "blue", "1" = "red"), labels = c("0" = "7-12 years", "1" = "13-18 years")) +
    scale_linetype_manual(values = c("0" = "dashed", "1" = "solid"), labels = c("0" = "7-12 years", "1" = "13-18 years")) +
    labs(fill = "", color = "", linetype = "") +
    theme_bw() +
    scale_y_continuous(limits = c(0.0, 1.05), label = percent_format(accuracy = 10), breaks = seq(0, 1, 0.1)) +
    scale_x_continuous(breaks = seq(1, 7, 1))
  ggsave(paste0(PLOTS_DIR, "age_model_day.png"), plot, dpi=300)

  df_period <- ggpredict(model, terms = c("period", "age_group"))
  plot <- ggplot(df_period, aes(x, predicted)) +
    geom_point(aes(color = group), size = 3) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = group), width = 0.2) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
    xlab("Prompt Time") +
    scale_fill_manual(values = c("0" = "blue", "1" = "red"), labels = c("0" = "7-12 years", "1" = "13-18 years")) +
    scale_color_manual(values = c("0" = "blue", "1" = "red"), labels = c("0" = "7-12 years", "1" = "13-18 years")) +
    scale_linetype_manual(values = c("0" = "dashed", "1" = "solid"), labels = c("0" = "7-12 years", "1" = "13-18 years")) +
    labs(fill = "", color = "", linetype = "") +
    theme_bw() +
    scale_y_continuous(limits = c(0.0, 1.05), label = percent_format(accuracy = 10), breaks = seq(0, 1, 0.1))
  ggsave(paste0(PLOTS_DIR, "age_model_period.png"), plot, dpi=300)
}

# Model without any extra stuff
base_model <- fit_model(
  response ~ day + period + (1 + day | p_id),
  model_df,
  "base"
)
plot_base_model(base_model)

# Model with sex
sex_model <- fit_model(
  response ~ day * sex + period + (1 + day | p_id),
  model_df,
  "sex"
)
plot_sex_model(sex_model)

# Model with age group
age_model <- fit_model(
  response ~ day * age_group + period + (1 + day | p_id),
  model_df,
  "age"
)
plot_age_model(age_model)
