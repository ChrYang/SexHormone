### Usage Example of free testosterone calculator
You could copy and paste the function in TCal.R to convert total testosterone into bioaviable testosterone or free testosterone in R.<br/>
For instance:<br/>

```r
FreeT <- TCal(
  Testosterone = df$T,
  SHBG = df$SHBG,
  ALB = df$ALB,
  ALB_deafult = FALSE,
  Testosterone_unit = "nmol/L",
  SHBG_unit = "nmol/L",
  ALB_unit = "g/L",
  type = "FreeT"
)
```

#### Instruction for Parameters Needed

- **Testosterone**: The plasma total testosterone level.  
- **SHBG**: The plasma SHBG level.  
- **ALB**: The plasma albumin level.  
- **ALB_default**: If the level of serum albumin is unknown, it can be set to 43 g/L by default when this parameter is selected as true.  
- **Testosterone_unit**: The unit of total testosterone. Must be one of: `ng/dL`, `ng/mL`, `nmol/dL`, `nmol/mL`, or `nmol/L` (case insensitive).  
- **SHBG_unit**: The unit of serum SHBG. Must be one of: `ng/dL`, `ng/mL`, `nmol/dL`, `nmol/mL`, or `nmol/L` (case insensitive).  
- **ALB_unit**: The unit of albumin. Must be one of: `g/L`, `g/dL`, `g/mL`, `mol/dL`, `mol/mL`, or `mol/L` (case insensitive).  
- **Type**: Must be either `"FreeT"` to calculate free testosterone or `"BioavailableT"` to calculate bioavailable testosterone.


The output is Free or bioavialble testosterone in nomol/L, depending on the value of the parameter "Type".<br/>

### Usage Example of testosterone unit convertor
You could copy and paste the function in T_convertor.R to convert testosterone unit:<br/>
For instance:<br/>

```r
ng_dl <- T_convertor(testosterone_nmol_l,"nmol/l","ng/dl")
```

The command above could help you to convert the a vector with testosterone level in `nmol/L` to an output vector in `ng/dL`.
