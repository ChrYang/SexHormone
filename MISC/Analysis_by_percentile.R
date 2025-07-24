library(ggplot2)
library(tidyverse)
library(ggpubr)

df_input <- df_unfil1 # the data frame with individual level data in wide format

df_input$X_percentile <- ntile(df_input$Free_Testosterone) # groupping Free T by percentile

#creating summary statistics table
df_perc <- data.frame(percentile = 1:100,
                      beta = rep(NA,100),
                      upper_CI = rep(NA,100),
                      lower_CI = rep(NA,100),
                      p_val = rep(NA,100))

#Analysing the effect size and CI of acetate by percentile.
for (i in 1:100){
  model <- summary(lm(SBP ~ Acetate + Acetone + Urine_potassium + Urine_sodium + BMI + Age + Medicated + TG_HDLC,df_input %>% filter(X_percentile == i)))
  beta <- model$coefficients[2,1]
  stde <- model$coefficients[2,2]
  df_perc[i,2] <- beta
  df_perc[i,3] <- beta + 1.96*stde
  df_perc[i,4] <- beta - 1.96*stde
  df_perc[i,5] <- model$coefficients[2,4]
}

#plotting
ggplot(df_perc, aes(x = percentile, y = beta, ymax=upper_CI,ymin=lower_CI)) +
  stat_smooth(method="loess",se = T, size = 0.5) + geom_hline(yintercept = 0) +  # CI
  labs(title = "", x = "Free Testosterone (percentiles)", y = "Adjusted Effect of Acetate on SBP") + theme_classic()

ggplot(df_perc, aes(x = percentile, y = beta, ymax=upper_CI,ymin=lower_CI)) +
  stat_smooth(method="lm") + geom_hline(yintercept = 0) +  # CI
  labs(title = "", x = "Free Testosterone (percentiles)", y = "Adjusted Effect of Acetate on SBP") + theme_classic()

ggplot(df_perc, aes(x = percentile, y = beta, ymax=upper_CI,ymin=lower_CI)) +
  stat_smooth(method="lm",formula = y ~ x + I(x^2)) + geom_hline(yintercept = 0) +  # CI
  labs(title = "", x = "Free Testosterone (percentiles)", y = "Adjusted Effect of Acetate on SBP") + theme_classic()
