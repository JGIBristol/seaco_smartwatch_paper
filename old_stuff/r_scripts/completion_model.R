# install.packages("lme4", repos="https://www.stats.bris.ac.uk/R/")
# install.packages("sjPlot", repos="https://www.stats.bris.ac.uk/R/")
# install.packages("effects", repos="https://www.stats.bris.ac.uk/R/")
library(lme4)
library(tidyverse)
library(sjPlot)
library(dplyr)
library(scales)
library(ggplot2)

plots_dir <- "outputs/imgs/completion/compare_models/"

# Read the data
model_df <- read_csv("outputs/data/completion.csv")

# Define some models
fixed_only <- glm(entry ~ day, data = model_df, family = binomial(link = "logit"))
random_intercept <- glmer(entry ~ day + (1 | p_id), data = model_df, family = binomial(link = "logit"))
random_both <- glmer(entry ~ day + (1 + day | p_id), data = model_df, family = binomial(link = "logit"))

# Find what percentage of positive entries there were on each day
percentage_yes <- model_df %>%
    group_by(day) %>%
    summarise(percentage_yes = mean(entry) * 100)

plot_and_save <- function(model, filename, title) {
    plot <- plot_model(model, type = "pred", terms = "day", show.rug = FALSE, ci.lvl = 0.95)

    # plot <- plot + geom_point(data = percentage_yes, aes(x = day, y = percentage_yes / 100), color = "#653D9BC4")
    plot <- plot + scale_y_continuous(limits = c(0.0, 1.0), label = percent_format(accuracy = 10), breaks = seq(0, 1, 0.1))
    plot <- plot + scale_x_continuous(breaks = seq(1, 7, 1))
    plot <- plot + ggtitle(title)

    ggsave(filename, plot)
}

plot_and_save(fixed_only, paste0(plots_dir, "fixed_only_fit.png"), "Fixed Effects Only")
plot_and_save(random_intercept, paste0(plots_dir, "random_intercepts_fit.png"), "Random Intercepts")
plot_and_save(random_both, paste0(plots_dir, "random_intercept_and_slope_fit.png"), "Completion Rate")

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
        # geom_point(data = percentage_yes, aes(x = day, y = percentage_yes / 100), color = "black") +
        scale_y_continuous(limits = c(-0.01, 1.01), label = scales::percent_format(accuracy = 10)) +
        theme(legend.position = "none") +
        scale_x_discrete(breaks = 1:7, labels = 1:7) +
        theme(legend.position = "none") +
        labs(x = "Day", y = "") +
        ggtitle("Completion Rate")

    ggsave(filename, plot)
}
plot_individual_pids(random_intercept, paste0(plots_dir, "random_intercepts_all.png"), "Random Intercepts")
plot_individual_pids(random_both, paste0(plots_dir, "random_pids_fit_all.png"), "Response Rate")


# Plot histograms
random_effects <- ranef(random_both)[1]$p_id
random_effects <- tidyr::pivot_longer(random_effects, everything())
ggplot(random_effects, aes(x = value)) +
    geom_histogram(binwidth = 0.2) +
    facet_wrap(~name, scales = "free") +
    labs(x = "value", y = "Count", title = "Random Effects")

ggsave("random_pid_effects_hists.png")

# Anova - to motivate having both a random intercept and slope
out_file <- paste0(plots_dir, "day_models.txt")
capture.output(summary(fixed_only), file = out_file)
capture.output(summary(random_intercept), file = out_file, append = TRUE)
capture.output(summary(random_both), file = out_file, append = TRUE)
capture.output(anova(random_intercept, random_both), file = out_file, append = TRUE)

aic_values <- data.frame(
    model = c("fixed_only", "random_intercept", "random_both"),
    AIC = c(AIC(fixed_only), AIC(random_intercept), AIC(random_both)),
    BIC = c(BIC(fixed_only), BIC(random_intercept), BIC(random_both))
)

capture.output(aic_values, file = out_file, append = TRUE)

# capture.output(confint(random_both), file = out_file, append = TRUE)
# Find the drop off per day
p_id_values <- unique(model_df$p_id)
day_seq <- seq(min(model_df$day), max(model_df$day), by = 1)

# Initialize a vector to store average differences for each p_id
avg_diffs <- numeric(length(p_id_values))
for (i in seq_along(p_id_values)) {
    prob_seq <- predict(random_both, newdata = data.frame(day = day_seq, p_id = p_id_values[i]), type = "response")
    diff_seq <- diff(prob_seq)
    avg_diffs[i] <- mean(diff_seq)
}

# Take the average of the average differences across all p_id values
avg_diff <- mean(avg_diffs)
print(avg_diff)
print(sd(avg_diffs))
