#testosterone SHBG nmol/L ALB g/L
FT <- function(TT,SHBG,ALB){
  Kalb <- 3.6*10^4
  Kshbg <- 10^9
  N <- 1 + Kalb*ALB/69000
  a <- N*Kshbg
  b <- N + Kshbg*(SHBG - TT)/10^9
  c <- -TT/10^9
  FT <- (-b + sqrt(b^2 - 4*a*c))/(2*a)*10^9
  FT
}

df1$Free_Testosterone <- mapply(FT,df1$Testosterone,df1$SHBG,df1$Albumin)
df1$Free_Testosterone <- df1$Free_Testosterone*1000 # converting free T from nmol/L to pmol/L. 
