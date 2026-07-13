library(patchwork)
library(ggplot2)
library(patchwork)

outdir <- paste0(prt_dir,"/percentile/")

tte_fil2pm <- tte_fil2m %>% filter(Menopaused == 1)

for (cohort in c("Postmenopausal","Whole")){
  
  ###Setting###
  
  hr_breaks <- c(0.5, 0.8, 1, 1.2, 1.5, 2)
  hr_limits <- c(0.5, 2)
  
  phvar <- "Early_menopause"
  x <- "Acetate"
  
  ###
  xlbl <- paste0(x," (percentiles)")
  ylbl <- paste0("HR of ",gsub("_"," ",tolower(phvar)))
  
  filename <- paste0(outdir,"/",phvar,"_",cohort,"Acetate_percentiles")
  csv_filename <- paste0(filename,".csv")
  pdf_filename <- paste0(filename,".pdf")
  
  if (cohort == "Whole"){
    df_input <- tte_fil2m
  }
  if (cohort == "Postmenopausal"){
    df_input <- tte_fil2pm
  }
  
  pred_nm <- paste0(x,"_SD")
  
  df_input$pred_percentile <- ntile(df_input[,pred_nm],100)
  df_input$X_test <- df_input[,phvar]
  
  
  df_perc <- data.frame(percentile = 1:100,
                        beta = rep(NA,100),
                        upper_CI = rep(NA,100),
                        lower_CI = rep(NA,100),
                        p_val = rep(NA,100))
  
  for (i in 1:100){
    cox_model <- coxph(Surv(MACE_onset2,MACE_event2) ~ X_test + Regular_drinker + Age_SD + BMI_SD + 
                         Urine_sodium_SD + Urine_potassium_SD + CVD_Medicine + Current_HRT + Previous_HRT,data = df_input %>% filter(pred_percentile == i),x=TRUE)
    
    cox_model <- CoxphToDF(summary(cox_model))
    
    beta <- cox_model["X_test1","coef"]
    se <- cox_model["X_test1","se(coef)"]
    
    df_perc[i,2] <- beta
    df_perc[i,3] <- beta + 1.96*se
    df_perc[i,4] <- beta - 1.96*se
    df_perc[i,5] <- cox_model["X_test","Pr(>|z|)"]
  }
  
  write.csv(df_perc,row.names=F,csv_filename)
  
  p1 <- ggplot(df_perc, aes(x = percentile, y = beta, ymax=upper_CI,ymin=lower_CI)) +
    stat_smooth(method="loess",se = T, size = 0.5) + geom_hline(yintercept = 0,linetype = "dashed") +  # CI
    labs(title = "LOESS", x = xlbl, y = ylbl) + 
    theme_classic() + 
    scale_y_continuous(
      trans = "exp",
      breaks = log(hr_breaks),
      labels = hr_breaks
    ) + coord_cartesian(ylim = log(hr_limits))
  
  
  
  p2 <- ggplot(df_perc, aes(x = percentile, y = beta, ymax=upper_CI,ymin=lower_CI)) +
    stat_smooth(method="lm") + geom_hline(yintercept = 0,linetype = "dashed") +  # CI
    labs(title = "GLM", x = xlbl, y = ylbl) + 
    theme_classic() + 
    scale_y_continuous(
      trans = "exp",
      breaks = log(hr_breaks),
      labels = hr_breaks
    ) + coord_cartesian(ylim = log(hr_limits))
  
  p3 <- ggplot(df_perc, aes(x = percentile, y = beta, ymax=upper_CI,ymin=lower_CI)) +
    stat_smooth(method="lm",formula = y ~ x + I(x^2)) + geom_hline(yintercept = 0,linetype = "dashed") +  # CI
    labs(title = "GLM - Quadratic", x = xlbl, y = ylbl) + 
    theme_classic() + 
    scale_y_continuous(
      trans = "exp",
      breaks = log(hr_breaks),
      labels = hr_breaks
    ) + coord_cartesian(ylim = log(hr_limits))
  
  p_comb <- p1 + p2 + p3
  
  print(p_comb)
  
  pdf(pdf_filename,width = 12,height = 4)
  
  print(p_comb)
  
  dev.off()
  
}