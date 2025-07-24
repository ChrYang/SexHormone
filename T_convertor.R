
#A brief function for Unit converting for testosterone.
#EG. ng_dl <- T_convertor(testosterone_nmol_l,"nmol/l","ng/dl")

T_convertor <- function(Testosterone,
                        Input_unit,
                        Output_unit
){
  Input_unit2 <- tolower(Input_unit)
  Output_unit2 <- tolower(Output_unit)
  if (!Input_unit %in% c("ng/dl","ng/ml","nmol/dl","nmol/ml","nmol/l")) {
    stop("`The unit of input data must be ng/dL, ng/mL, nmol/dL, nmol/mL, nmol/L (case insensitive)!")
  }
  if (!Output_unit %in% c("ng/dl","ng/ml","nmol/dl","nmol/ml","nmol/l")) {
    stop("`The unit of output data must be one from ng/dL, ng/mL, nmol/dL, nmol/mL, nmol/L (case insensitive)!")
  }
  if (Input_unit2 %in% c("ng/dl","ng/ml")){
    if (Input_unit2 == "ng/ml"){
      Testosterone_mass <- Testosterone*100 # 1 ng/mL = 100 ng/dL
    }
    if (Input_unit2 == "ng/dl"){
      Testosterone_mass <- Testosterone # 1 ng/mL = 100 ng/dL
    }
    Testosterone_mol <- Testosterone_mass*0.0347 # 1ng/dL testosterone = 0.0347nmol/L
  }
  
  else if (Input_unit2 %in% c("nmol/dl","nmol/ml","nmol/l")){
    if (Input_unit2 == "nmol/dl"){
      Testosterone_mol <- Testosterone*10 # 1 nmol/dL = 10 nmol/L
    }
    if (Input_unit2 == "nmol/ml"){
      Testosterone_mol <- Testosterone*1000 # 1 nmol/dL = 10 nmol/L
    }
    if (Input_unit2 == "nmol/l"){
      Testosterone_mol <- Testosterone
    }
    Testosterone_mass <- Testosterone*28.842 # 1 nmol/L = 28.842 ng/dL
  }
  
  if(Output_unit2 == "ng/dl"){
    out <- Testosterone_mass
  }
  if(Output_unit2 == "ng/ml"){
    out <- Testosterone_mass/100 # 1ng/dL = 0.01 ng/mL
  }
  if(Output_unit2 == "nmol/l"){
    out <- Testosterone_mol
  }
  
  if(Output_unit2 == "nmol/dl"){
    out <- Testosterone_mol/10 # 1nmol/L = 0.1nmol/dL
  }
  
  if(Output_unit2 == "nmol/ml"){
    out <- Testosterone_mol/1000 # 1nmol/L = 0.001nmol/mL
  }
  return(out)
}