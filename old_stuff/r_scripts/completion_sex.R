library(lme4)
library(tidyverse)
library(sjPlot)
library(dplyr)
library(scales)
library(ggplot2)
library(ggeffects)

# Read the data
model_df <- read_csv("outputs/data/completion.csv")

# Model options
# Sometimes some of the models dont converge unless i increase the number of iterations
# Also bobyqa is faster than the default
control <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))

# Define a model for sex
sex_model <- glmer(entry ~ day * sex + (1 + day | p_id), data = model_df, family = binomial(link = "logit"), control = control)
capture.output(summary(sex_model), file = "outputs/imgs/completion/demographic_models/sex_model.txt")

# Plot the fits
plot <- plot_model(sex_model, type = "pred", terms = c("day", "sex"))
plot_data <- plot$data

# Use revalue to map the default labels to the new labels
plot_data$group_col <- as.character(plot_data$group_col)
plot_data$group_col <- plyr::revalue(plot_data$group_col, list(`0` = "Male", `1` = "Female"))
plot_data$group_col <- as.character(plot_data$group_col)
#
# # Plot the fits
# plot <- ggplot() +
#     scale_y_continuous(limits = c(0.0, 1.0), label = scales::percent_format(accuracy = 1), breaks = seq(0, 1, 0.1)) +
#     scale_x_continuous(breaks = seq(1, 7, 1)) +
#     labs(color = "sex", fill = "sex") +
#     xlab("Day") +
#     ylab("") +
#     ggtitle("Completion Rate")

# Calculate the positive entry rate for each participant on each day
model_df$day <- factor(model_df$day, levels = 1:7)
model_df$sex <- factor(model_df$sex, levels = c(0, 1), labels = c("Male", "Female"))

entry_rates <- model_df %>%
    group_by(day, p_id, sex) %>%
    summarise(positive_rate = sum(entry) / n()) %>%
    ungroup()

entry_rates$day <- factor(entry_rates$day, levels = 1:7)

plot <- ggplot() +
    # Plot the fits
    geom_line(
        data = plot_data,
        aes(x = x, y = predicted, color = group_col),
        show.legend = FALSE,
    ) +
    geom_ribbon(
        data = plot_data,
        aes(x = x, ymin = conf.low, ymax = conf.high, fill = group_col),
        alpha = 0.2,
    ) +

    # Plot the violins
    # geom_violin(
    #     data = entry_rates,
    #     alpha = 0.3,
    #     position = position_dodge(width = 1.0),
    #     aes(x = day, y = positive_rate, fill = sex, group = interaction(day, sex))
    # ) +

    # Formatting etc
    # scale_y_continuous(labels = scales::percent_format(accuracy = 1, breaks = seq(0, 1, 0.1))) +
    scale_x_continuous(breaks = 1:7, labels = 1:7) +
    xlab("Day") +
    ylab("") +
    ggtitle("Completion Rate") +
    labs(fill = "Sex") +
    scale_y_continuous(
        labels = scales::percent_format(accuracy = 1),
        breaks = seq(0, 1, 0.1)
    ) +
    xlab("Day")

ggsave("outputs/imgs/completion/demographic_models/sex_fit.png", plot)
