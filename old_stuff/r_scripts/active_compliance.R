library(lme4)
library(tidyverse)
library(sjPlot)
library(dplyr)
library(scales)
library(ggplot2)


# Install the lmerTest package if it is not already installed
# install.packages("lmerTest")
library(lmerTest)

plots_dir <- "outputs/imgs/active_compliance/"

# Read the data
model_df <- read_csv("outputs/data/active_compliance.csv")

# Define some models
model <- lmer(active_compliance_rate ~ day + (1 + day | p_id), data = model_df)


plot_and_save <- function(model, filename, title) {
  plot <- plot_model(model, type = "pred", terms = "day", show.rug = FALSE, ci.lvl = 0.95)

  plot <- plot + scale_y_continuous(limits = c(-0.05, 1.05), label = percent_format(accuracy = 10), breaks = seq(0, 1, 0.1))
  plot <- plot + scale_x_continuous(breaks = seq(1, 7, 1))
  plot <- plot + ggtitle(title)

  ggsave(filename, plot)
}

plot_and_save(model, paste0(plots_dir, "active_compliance_fit.png"), "Active Participants' Compliance Rate")

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
    plot <- plot + geom_violin(data = newdata, aes(x = factor(day), y = pred, fill = factor(day)), alpha = 0.3) +
        scale_fill_manual(values = rep("skyblue", 7)) +
        # geom_point(data = rate, aes(x = day, y = percentage_yes / 100), color = "black") +
        scale_y_continuous(limits = c(-0.01, 1.01), label = scales::percent_format(accuracy = 10)) +
        scale_x_discrete(breaks = 1:7, labels=1:7) + 
        theme(legend.position = "none") +
        labs(x = "Day", y = "") +
        ggtitle(title)

    ggsave(filename, plot)
}
plot_individual_pids(model, paste0(plots_dir, "active_compliance_fit_all_pids.png"), "Compliance Rate, Active Participants Only")

out_file <- paste0(plots_dir, "compliance_summary.txt")
capture.output(summary(model), file = out_file)
capture.output(anova(model), file = out_file, append = TRUE)
