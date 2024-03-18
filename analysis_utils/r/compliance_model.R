library(lme4)
library(tidyverse)
library(sjPlot)
library(dplyr)
library(scales)
library(ggplot2)

plots_dir <- "outputs/imgs/compliance/"

# Read the data
model_df <- read_csv("outputs/data/compliance.csv")

# Define some models
model <- lmer(compliance_rate ~ day + (1 + day | p_id), data = model_df)

# Find the compliance rate on each day
rate <- model_df %>%
    group_by(day) %>%
    summarise(percentage_yes = mean(compliance_rate) * 100)

plot_and_save <- function(model, filename, title) {
    plot <- plot_model(model, type = "pred", terms = "day", show.rug = FALSE, ci.lvl = 0.95)

    plot <- plot + geom_point(data = rate, aes(x = rate$day, y = rate$percentage_yes / 100), color = "#653D9BC4")
    plot <- plot + scale_y_continuous(limits = c(0.0, 1.1), label = percent_format(accuracy = 10))
    plot <- plot + ggtitle(title)

    ggsave(filename, plot)
}

plot_and_save(model, paste0(plots_dir, "compliance_fit.png"), "Compliance Rate")

plot_individual_pids <- function(model, filename, title) {
    # Create a new data frame with the unique days and p_ids
    newdata <- expand.grid(day = unique(model_df$day), p_id = unique(model_df$p_id))

    # Add the predicted values to the new data frame
    newdata$pred <- predict(model, newdata = newdata, type = "response")

    # Convert p_id to a factor
    newdata$p_id <- as.factor(newdata$p_id)

    # Create the overall plot
    plot <- plot_model(model, type = "pred", terms = "day", show.rug = TRUE)

    # Add the predicted values for each p_id to the plot
    plot <- plot + geom_line(data = newdata, aes(x = day, y = pred, color = p_id), alpha = 0.5) +
        scale_color_manual(values = rep("skyblue", 83)) +
        geom_point(data = rate, aes(x = day, y = percentage_yes / 100), color = "black") +
        scale_y_continuous(limits = c(0.0, 1.0), label = scales::percent_format(accuracy = 10)) +
        theme(legend.position = "none") +
        ggtitle(title)

    ggsave(filename, plot)
}
plot_individual_pids(model, paste0(plots_dir, "compliance_fit_all_pids.png"), "Compliance Rate")
