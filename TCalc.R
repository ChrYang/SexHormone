#' Free / Bioavailable Testosterone Calculation
#'
#' @param Testosterone The serum total testosterone level.
#' @param SHBG The serum SHBG level.
#' @param ALB The serum albumin level.
#' @param ALB_default If the level of serum albumin were somehow unknown, it could be set to 43g/L by defaulty when selecting this parameter as true.
#' @param Testosterone_unit The unit of total testosterone, it must be ng/dL, ng/mL, nmol/dL, nmol/mL, or nmol/L (case insensitive).
#' @param SHBG_unit The unit of serum SHBG, it must be ng/dL, ng/mL, nmol/dL, nmol/mL, or nmol/L (case insensitive).
#' @param ALB_unit The unit of albumin, it must be g/L ,g/dL, g/mL, mol/dL, mol/mL, or mol/L (case insensitive).
#' @param Type It could only be "FreeT" to calculate free testosterone or "BioavailableT" to calculate bioavailable testosterone.
#' @return Free or bioavialble testosterone in nomol/L, depending on the value of the parameter "Type"
#' @export


TCalc <- function(Testosterone,SHBG,ALB=43,ALB_default=FALSE,
                  Testosterone_unit = "nmol/L",
                  SHBG_unit = "nmol/L",
                  ALB_unit = "g/L",
                  Type = "FreeT"
){
  Testosterone_unit2 <- tolower(Testosterone_unit)

  if (!Type %in% c("FreeT","BioavailableT")) {
    stop("This function could only calculate free testosterone (FreeT) and bioavailable testosterone (BioavailableT)!")
  }

  if (!ALB_default %in% c(TRUE,FALSE)) {
    stop("ALB_default parameter could only be either TRUE or FALSE!")
  }

  if (!Testosterone_unit2 %in% c("ng/dl","ng/ml","nmol/dl","nmol/ml","nmol/l")) {
    stop("`The unit of testosterone must be ng/dL, ng/mL, nmol/dL, nmol/mL, or nmol/L (case insensitive)!")
  }

  #Converting the unit of testosterone to nmol/L

  if (Testosterone_unit2 %in% c("ng/dl","ng/ml")){
    if (Testosterone_unit2 == "ng/ml"){
      Testosterone_cov <- Testosterone*100 # 1 ng/mL = 100 ng/dL
    }
    if (Testosterone_unit2 == "ng/dl"){
      Testosterone_cov <- Testosterone # 1 ng/mL = 100 ng/dL
    }
    Testosterone2 <- Testosterone_cov*0.0347 # 1ng/dL testosterone = 0.0347nmol/L
  }

  if (Testosterone_unit2 %in% c("nmol/dl","nmol/ml","nmol/l")){
    if (Testosterone_unit2 == "nmol/dl"){
      Testosterone2 <- Testosterone*10 # 1 nmol/dL = 10 nmol/L
    }
    if (Testosterone_unit2 == "nmol/ml"){
      Testosterone2 <- Testosterone*1000 # 1 nmol/dL = 10 nmol/L
    }
    if (Testosterone_unit2 == "nmol/l"){
      Testosterone2 <- Testosterone
    }
  }


  SHBG_unit2 <- tolower(SHBG_unit)
  if (!SHBG_unit2 %in% c("ng/dl","ng/ml","nmol/dl","nmol/ml","nmol/l")) {
    stop("`The unit of SHBG must be ng/dL, ng/mL, nmol/dL, nmol/mL, or nmol/L (case insensitive)!")
  }

  #Converting the unit of SHBG to nmol/L
  if (SHBG_unit2 %in% c("ug/dl","ug/ml")){
    if (SHBG_unit2 == "ug/ml"){
      SHBG_cov <- SHBG*100 # 1 ug/mL = 100 ug/dL
    }
    if (SHBG_unit2 == "ug/dl"){
      SHBG_cov <- SHBG
    }
    SHBG2 <- SHBG_cov*0.1053 # 1ug/dL SHBG = 0.1053nmol/L
  }

  if (SHBG_unit2 %in% c("nmol/dl","nmol/ml","nmol/l")){
    if (SHBG_unit2 == "nmol/dl"){
      SHBG2 <- SHBG*10 # 1 nmol/dL = 10 nmol/L
    }
    if (SHBG_unit2 == "nmol/ml"){
      SHBG2 <- SHBG*1000 # 1 nmol/dL = 10 nmol/L
    }
    if (SHBG_unit2 == "nmol/l"){
      SHBG2 <- SHBG
    }
  }

  if (ALB_default != TRUE){
    ALB_unit2 <- tolower(ALB_unit)
    if (!ALB_unit2 %in% c("g/l","g/dl","g/ml","mol/dl","mmol/ml","mmol/l")) {
      stop("`The unit of albumin must be g/L, g/dL, g/mL, mol/dL, mol/mL, or mol/L (case insensitive)!")
    }

    #converting to g/L
    if (ALB_unit2 %in% c("g/dl","g/l","g/ml")){
      if (ALB_unit2 == "g/ml"){
        ALB2 <- ALB*1000 # 1 g/mL = 1000 g/l
      }
      if (ALB_unit2 == "g/dl"){
        ALB2 <- ALB*10 # 1 g/dL = 10 g/L
      }
      if (ALB_unit2 == "g/l"){
        ALB2 <- ALB
      }
    }

    #converting to mmol/L, then to g/L
    if (ALB_unit2 %in% c("mmol/dl","mmol/ml","mmol/l")){
      if (ALB_unit2 == "mmol/dl"){
        ALB_cov <- ALB*10 # 1 mmol/dL = 10 mmol/L
      }
      if (ALB_unit2 == "mmol/ml"){
        ALB_cov <- ALB*1000 # 1 mmol/mL = 1000 mol/L
      }
      ALB2 <- ALB_cov*66.46 # 1mmol/L = 66.46 g/L Albumin
    }
  }
  else if (ALB_default == TRUE){
    ALB2 = 43 # setting Albumin = 4.3 g/dL = 43 g/L if specific value were not provided
  }

  Kalb <- 3.6*10^4
  Kshbg <- 10^9
  N <- 1 + Kalb*ALB2/69000
  a <- N*Kshbg
  b <- N + Kshbg*(SHBG2 - Testosterone2)/10^9
  c <- -Testosterone2/10^9
  ft <- (-b + sqrt(b^2 - 4*a*c))/(2*a)*10^9
  if (Type == "FreeT") {
    return(ft)
  }
  if (Type == "BioavailableT"){
    N <- 1 + Kalb*ALB2/69000
    cbat <- N*ft # Calculated bioavailable testosterone
    return(cbat)
  }



}



