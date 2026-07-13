library(tidyverse)
library(ggplot2)
library(patchwork)

prt_dir <- "~" #put your directory here
outdir <- paste0(prt_dir,"/Acetate_EM_Interactions/")

tte_fil2pm <- tte_fil2m %>% filter(Menopaused == 1)

whole_cox <- coxph(Surv(MACE_onset2,MACE_event2) ~ Acetate_SD + Early_menopause + Regular_drinker + Age_SD + BMI_SD
                   + Urine_sodium_SD + Urine_potassium_SD + CVD_Medicine +  Current_HRT + Previous_HRT
                   + Acetate_SD:Early_menopause + Acetate_SD:Regular_drinker + Acetate_SD:Age_SD + Acetate_SD:BMI_SD
                   + Acetate_SD:Urine_sodium_SD + Acetate_SD:Urine_potassium_SD
                   + Acetate_SD:CVD_Medicine + Acetate_SD:Current_HRT + Acetate_SD:Previous_HRT
                   + Early_menopause:Regular_drinker + Early_menopause:Age_SD + Early_menopause:BMI_SD
                   + Early_menopause:Urine_sodium_SD + Early_menopause:Urine_potassium_SD + Early_menopause:CVD_Medicine
                   + Early_menopause:Current_HRT + Early_menopause:Previous_HRT,
                   data = tte_fil2m,x=TRUE)

postm_cox <- coxph(Surv(MACE_onset2,MACE_event2) ~ Acetate_SD + Early_menopause + Regular_drinker + Age_SD + BMI_SD
                   + Urine_sodium_SD + Urine_potassium_SD + CVD_Medicine +  Current_HRT + Previous_HRT
                   + Acetate_SD:Early_menopause + Acetate_SD:Regular_drinker + Acetate_SD:Age_SD + Acetate_SD:BMI_SD
                   + Acetate_SD:Urine_sodium_SD + Acetate_SD:Urine_potassium_SD
                   + Acetate_SD:CVD_Medicine + Acetate_SD:Current_HRT + Acetate_SD:Previous_HRT
                   + Early_menopause:Regular_drinker + Early_menopause:Age_SD + Early_menopause:BMI_SD
                   + Early_menopause:Urine_sodium_SD + Early_menopause:Urine_potassium_SD + Early_menopause:CVD_Medicine
                   + Early_menopause:Current_HRT + Early_menopause:Previous_HRT,
                   data = tte_fil2pm,x=TRUE)

whole_cox <- CoxphToDF(summary(whole_cox))
postm_cox <- CoxphToDF(summary(postm_cox))

print(whole_cox)
print(postm_cox)

df_i <- as.data.frame(matrix(NA,nrow=2,ncol=6))
rownames(df_i) <-  c("Whole","Post-menopausal")
colnames(df_i) <- c("beta_interaction","se_interaction","HR","ll","ul","p")

df_i["Whole",] <- as.numeric(whole_cox["Acetate_SD:Early_menopause1",c("coef","se(coef)","exp(coef)","lower .95","upper .95","Pr(>|z|)")]) #beta, se, and p
df_i["Post-menopausal",] <- as.numeric(postm_cox["Acetate_SD:Early_menopause1",c("coef","se(coef)","exp(coef)","lower .95","upper .95","Pr(>|z|)")]) #beta, se, and p


df_i <- df_i %>%
  mutate(
    label = sprintf("%.3f[%.3f,%.3f]", HR, ll, ul),
    Significance = p < 0.05
  )


df_i$Significance <- factor(df_i$Significance, levels = c(TRUE, FALSE))
df_i$Name <- rownames(df_i)

p1 <- ggplot(df_i, aes(x = HR, y = Name)) +
  geom_point(aes(color = HR < 1), 
             position = position_dodge(width = 0.5)) +
  geom_errorbarh(aes(xmin = ll, xmax = ul, 
                     linetype = Significance,
                     color = HR < 1),
                 height = 0.2,
                 position = position_dodge(width = 0.5)) +
  geom_text(aes(x = HR, label = sprintf("p = %.3f", p)),
            vjust = -1,   # push text slightly right
            size = 5) + 
  geom_vline(xintercept = 1, linetype = "dashed") + 
  labs(
    x = "Early Menopause × Acetate Interaction (per 1 SD, 95% CI)",
    y = "Cohort",
    color = ""
  ) + 
  theme_classic2() +
  scale_linetype_manual(
    values = c("TRUE" = "solid", "FALSE" = "dashed")
  ) +
  scale_color_manual(
    values = c("TRUE" = "blue3", "FALSE" = "red3"),
    labels = c("FALSE" = "HR ≥ 1", "TRUE" = "HR < 1")
  ) +
  theme(
    legend.position = "bottom",
    axis.text = element_text(size = 13),
    axis.title = element_text(size = 13)
  ) +
  scale_x_continuous(trans = "log") + 
  xlim(0.7, 1)



p2 <- ggplot(df_i, aes(x = 0, y = Name, label = label)) +
  geom_text(
    position = position_dodge(0.5),
    hjust = 0,
    size = 5
  ) +
  xlim(0, 1) +  # dummy axis
  
  labs(title = "Interaction") + scale_color_manual(
    values = c("All" = "red3", "Non-regular drinkers" = "blue3")
  ) +
  
  theme_void() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0, size = 15),
  )

pdf(paste0(outdir,"/Interaction_Plotting.pdf"),width=8,height=6)
print(p1+p2)
dev.off()

print(p1+p2)


write.csv(whole_cox,paste0(outdir,"/Whole_Cohort_Interaction_Model.csv"))
write.csv(postm_cox,paste0(outdir,"/Postmenopausal_Cohort_Interaction_Model.csv"))