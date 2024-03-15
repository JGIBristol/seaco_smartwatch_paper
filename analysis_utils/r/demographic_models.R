library(lme4)
library(tidyverse)
library(sjPlot)
library(dplyr)
library(scales)
library(ggplot2)
library(ggeffects)

plot_and_save <- function(model, covariate, filename, legend) {
    sjp <- plot_model(model, type = "pred", terms = c("day", covariate))
    sjp_data <- sjp$data

    # Use revalue to map the default labels to the new labels
    sjp_data$group_col <- as.character(sjp_data$group_col)
    sjp_data$group_col <- plyr::revalue(sjp_data$group_col, legend)
    sjp_data$group_col <- as.character(sjp_data$group_col)

    # Reorder factor levels if covariate is 'weekday'
    if (covariate == "weekday") {
        sjp_data$group_col <- factor(sjp_data$group_col, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
    }

    plot <- ggplot() +
        geom_line(data = sjp_data, aes(x = x, y = predicted, color = group_col)) +
        geom_ribbon(data = sjp_data, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group_col), alpha = 0.1) +
        geom_point(data = percentage_yes, aes(x = day, y = percentage_yes / 100), color = "black") +
        scale_y_continuous(limits = c(0.0, 1.0), label = scales::percent_format(accuracy = 1)) +
        labs(color = covariate, fill = covariate) +
        xlab("Day")

    ggsave(filename, plot)
}

# Read the data
model_df <- read_csv("outputs/data/completion.csv")

# Find what percentage of positive entries there were on each day
percentage_yes <- model_df %>%
    group_by(day) %>%
    summarise(percentage_yes = mean(entry) * 100)

# Model options
# Sometimes some of the models dont converge unless i increase the number of iterations
# Also bobyqa is faster than the default
control <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))

# Define some models
sex_model <- glmer(entry ~ day * sex + (1 + day | p_id), data = model_df, family = binomial(link = "logit"), control = control)
plot_and_save(sex_model, "sex", "outputs/imgs/demographic_models/sex_fit.png", list(`0` = "Male", `1` = "Female"))
capture.output(summary(sex_model), file = "outputs/imgs/demographic_models/sex_model.txt")
# capture.output(confint(sex_model, parm = "beta_")["sex", ], file = "sex_model.txt", append = TRUE)
