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

    plot <- ggplot() +
        geom_line(data = sjp_data, aes(x = x, y = predicted, color = group_col)) +
        geom_ribbon(data = sjp_data, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group_col), alpha = 0.1) +
        geom_point(data = percentage_yes, aes(x = day, y = percentage_yes / 100), color = "black") +
        scale_y_continuous(limits = c(0.0, 1.1), label = scales::percent_format(accuracy = 1)) +
        labs(color = covariate, fill = covariate) +
        xlab("Day")

    ggsave(filename, plot)
}

# Read the data
model_df <- read_csv("outputs/data/compliance.csv")

# Find what percentage of positive entries there were on each day
percentage_yes <- model_df %>%
    group_by(day) %>%
    summarise(percentage_yes = mean(compliance_rate) * 100)

# Model options
# Sometimes some of the models dont converge unless i increase the number of iterations
# Also bobyqa is faster than the default
control <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
sex_model <- lmer(compliance_rate ~ day * respondent_sex + (1 + day | p_id), data = model_df)

plot_and_save(sex_model, "respondent_sex", "outputs/imgs/compliance/demographic_models/sex_fit.png", list(`0` = "Male", `1` = "Female"))

capture.output(summary(sex_model), file = "outputs/imgs/compliance/demographic_models/sex_model.txt")
