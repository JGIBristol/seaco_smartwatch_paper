library(lme4)
library(tidyverse)
library(sjPlot)
library(dplyr)
library(scales)
library(ggplot2)


# Install the lmerTest package if it is not already installed
# install.packages("lmerTest")
library(lmerTest)

plots_dir <- "outputs/imgs/compliance/"
residuals_dir <- paste0(plots_dir, "residuals/")

# Read the data
model_df <- read_csv("outputs/data/compliance.csv")

# Define the model
# model <- lmer(compliance_rate ~ day + (1 + day | p_id), data = model_df)

model_df$successes <- pmin(round(model_df$compliance_rate * 12), 12)
model_df$trials <- 12
model <- glmer(cbind(successes, trials - successes) ~ day + (1 + day | p_id), 
               data = model_df, 
               family = binomial)
summary(model)

# Plot the residuals vs the fitted values, to check for homoscedasticity
residuals <- resid(model)
resid_data <- data.frame(
  day = model_df$day,
  actual_vals = model_df$compliance_rate,
  residuals = residuals,
  fitted_vals = fitted(model)
)
resid_plot <- ggplot(resid_data, aes(x = fitted_vals, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title="Residuals vs. Fitted Values",
    x = "Fitted Values",
    y = "Residuals"
)
ggsave(filename = paste0(residuals_dir, "residuals_vs_fitted.png"), plot=resid_plot)

# Plot the residuals vs study day, to check for independence
resid_plot <- ggplot(resid_data, aes(x = day, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title="Residuals vs. Study Day",
    x = "Study Day",
    y = "Residuals"
)
ggsave(filename = paste0(residuals_dir, "residuals_vs_day.png"), plot=resid_plot)

# Check for normality by creating a Q-Q plot of the residuals
png(paste0(residuals_dir, "qq_plot.png"))
qqnorm(residuals)
qqline(residuals, col="red")
dev.off()

# Plot a histogram of residuals just for fun
resid_plot <- ggplot(resid_data, aes(x = residuals)) +
  geom_histogram(binwidth = 0.2, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Residuals",
       x = "Residuals",
       y = "Frequency")
ggsave(filename = paste0(residuals_dir, "residuals_histogram.png"), plot=resid_plot)

# Find the compliance rate on each day
rate <- model_df %>%
    group_by(day) %>%
    summarise(percentage_yes = mean(compliance_rate) * 100)

plot_and_save <- function(model, filename, title) {
    plot <- plot(model, type = "pred", terms = "day", show.rug = FALSE, ci.lvl = 0.95)

    # plot <- plot + geom_point(data = rate, aes(x = rate$day, y = rate$percentage_yes / 100), color = "#653D9BC4")
    plot <- plot + scale_y_continuous(limits = c(0.0, 1.05), label = percent_format(accuracy = 10), breaks = seq(0, 1, 0.1))
    plot <- plot + scale_x_continuous(breaks = seq(1, 7, 1))
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
    plot <- plot(model, type = "pred", terms = "day", show.rug = TRUE)

    # Add the predicted values for each p_id to the plot
    plot <- plot + geom_violin(data = newdata, aes(x = factor(day), y = pred, fill = factor(day)), alpha = 0.3) +
        scale_fill_manual(values = rep("skyblue", 7)) +
        # geom_point(data = rate, aes(x = day, y = percentage_yes / 100), color = "black") +
        scale_y_continuous(limits = c(-0.01, 1.01), label = scales::percent_format(accuracy = 10)) +
        scale_x_discrete(breaks = 1:7, labels = 1:7) +
        theme(legend.position = "none") +
        labs(x = "Day", y = "") +
        ggtitle(title)

    ggsave(filename, plot)
}
plot_individual_pids(model, paste0(plots_dir, "compliance_fit_all_pids.png"), "Compliance Rate")

out_file <- paste0(plots_dir, "compliance_summary.txt")
capture.output(summary(model), file = out_file)
capture.output(anova(model), file = out_file, append = TRUE)
