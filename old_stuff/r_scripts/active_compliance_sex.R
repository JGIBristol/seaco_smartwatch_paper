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
        geom_line(data = sjp_data, aes(x = x, y = predicted, color = group_col), show.legend = FALSE) +
        geom_ribbon(data = sjp_data, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group_col), alpha = 0.1) +
        # geom_point(data = percentage_yes, aes(x = day, y = percentage_yes / 100), color = "black") +
        scale_y_continuous(limits = c(0.0, 1.05), label = percent_format(accuracy = 10), breaks = seq(0, 1, 0.1)) +
        scale_x_continuous(breaks = seq(1, 7, 1)) +
        labs(color = covariate, fill = covariate) +
        xlab("Day") +
        ggtitle("Compliance Rate, Active Participants") +
        ylab("") +
        labs(fill = "Sex")

    ggsave(filename, plot)
}

# Read the data
model_df <- read_csv("outputs/data/active_compliance.csv")

# Model options
# Sometimes some of the models dont converge unless i increase the number of iterations
# Also bobyqa is faster than the default
control <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
sex_model <- lmer(active_compliance_rate ~ day * sex + (1 + day | p_id), data = model_df)

plot_and_save(sex_model, "sex", "outputs/imgs/active_compliance/demographic_models/active_sex_fit.png", list(`0` = "Male", `1` = "Female"))

capture.output(summary(sex_model), file = "outputs/imgs/active_compliance/demographic_models/active_sex_model.txt")
