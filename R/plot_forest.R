##########################
# aim ^- forest plot
##########################

library(finalfit)
library(dplyr)
library(ggplot2)

data(colon_s)
colon_s %>% glimpse()

explanatory = c("age.factor",
                "sex.factor",
                "obstruct.factor",
                "perfor.factor")
dependent = "Surv(time, status)"

colon_s %>%
  hr_plot(dependent, explanatory, dependent_label = "Survival")

colon_s %>%
  hr_plot(
    dependent,
    explanatory,
    dependent_label = "Survival",
    table_text_size = 4,
    title_text_size = 14,
    plot_opts = list(xlab("HR, 95% CI"), theme(axis.title = element_text(size =
                                                                           12)))
  )

##############################################
# recreate with tidyverse
##############################################

library(survival)
fit <- coxph(Surv(time, status) ~ age.factor + sex.factor + obstruct.factor + perfor.factor, data = colon_s)
tidy_fit <- fit %>% broom::tidy(exponentiate = TRUE)
tidy_fit %>%
  ggplot(aes(x = term, y= estimate, ymin = conf.low, ymax = conf.high)) +
  geom_linerange() +
  geom_point() +
  coord_flip()

