library(tidyverse)
library(ggplot2)
library(patchwork)

dir1 <- "~" # put your directory here

tte_fil2pm <- tte_fil2m %>% filter(Menopaused == 1)

for (coh in c("Whole","Postmenopausal")){
  
  flnm <- paste0(dir1,"/",
                 coh,"_","MACE_percentage_among_acetate_percentile")
  
  if (coh == "Whole"){
    tte_input <- tte_fil2m
    ylim_plot <- c(2.5,4.5)
    
  }
  
  if (coh == "Postmenopausal"){
    tte_input <- tte_fil2pm
    ylim_plot <- c(3,5)
  }
  

  df_perc <- data.frame(percentile = 1:100,
                        MACE_incidence = rep(NA,100))
  
  for (i in 1:100){
    tte_ext <- tte_input[tte_input$Acetate_percentile == i,]
    df_perc[i,2] <- table(tte_ext$MACE_event2)[[2]]/nrow(tte_ext)
  }
  
  
  df_perc$MACE_percentage <- df_perc$MACE_incidence*100
  
  
  
  
  p1 <- ggplot(df_perc, aes(x = percentile, y = MACE_percentage)) + 
    stat_smooth(method="loess",se = T, size = 0.5,color = "#7809C8") + 
    labs(title = "LOESS", x = "Acetate percentile", y = "Percentage of 10-year MACE") + 
    theme_classic() + 
    coord_cartesian(ylim = ylim_plot)
  
  p2 <- ggplot(df_perc, aes(x = percentile, y = MACE_percentage)) + 
    stat_smooth(method="lm",se = T, size = 0.5,color = "#7809C8") + 
    labs(title = "GLM", x = "Acetate percentile", y = "Percentage of 10-year MACE") + 
    theme_classic() + 
    coord_cartesian(ylim = ylim_plot)
  
  p3 <- ggplot(df_perc, aes(x = percentile, y = MACE_percentage)) + 
    stat_smooth(method="lm",formula = y ~ x + I(x^2),se = T, size = 0.5,color = "#7809C8") + 
    labs(title = "GLM - Quadratic", x = "Acetate percentile", y = "Percentage of 10-year MACE") + 
    theme_classic() + 
    coord_cartesian(ylim = ylim_plot)
  
  print(p1+p2+p3)
  
  write.csv(df_perc,paste0(flnm,".csv"),row.names=F)
  
  pdf(paste0(flnm,".pdf"),width = 12,height = 4)
  print(p1+p2+p3)
  dev.off()
  
}








