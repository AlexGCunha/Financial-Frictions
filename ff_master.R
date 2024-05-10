#------------------------------------------------------------------------------------------------
#This code will run other codes
#------------------------------------------------------------------------------------------------
repository = "http://artifactory.bcnet.bcb.gov.br/artifactory/cran-remote/"
if (!require("dplyr")) install.packages("splitstackshape", repos=repository)
if (!require("haven")) install.packages("splitstackshape", repos=repository)
if (!require("readxl")) install.packages("splitstackshape", repos=repository)
if (!require("arrow")) install.packages("splitstackshape", repos=repository)
if (!require("stringr")) install.packages("splitstackshape", repos=repository)
if (!require("tibble")) install.packages("splitstackshape", repos=repository)
if (!require("tidyr")) install.packages("splitstackshape", repos=repository)
if (!require("ggplot2")) install.packages("splitstackshape", repos=repository)
if (!require("readr")) install.packages("splitstackshape", repos=repository)
if (!require("purrr")) install.packages("splitstackshape", repos=repository)
if (!require("MatchIt")) install.packages("splitstackshape", repos=repository)

library(tidyverse)
library(haven)
library(readxl)
library(arrow)


data_path = "Z:/Bernardus/Cunha_Santos_Doornik/Dta_files"
scr_path = "Z:/DATA/Dta_files/SCR"
output_path = "Z:/Bernardus/Cunha_Santos_Doornik/Output_check"
codes_path = "Z:/Bernardus/Cunha_Santos_Doornik/Codes"
rais_path = "Z:/DATA/Dta_files/RAIS"

date = "20240402"

# #This code will work on SCR data to create an anual dataframe
#Generates samples and filters only selected variables
# setwd(codes_path)
# flname = paste0("prep_ff_year_env_20240408.R")
# source(flname)
# ff_year()

#
# #This code will work on SCR data to create an anually dataframe by firm
# setwd(codes_path)
# flname = paste0("prep_ff_firm_env_20240402.R")
# source(flname)
# ff_firm()
#
#
# #This code will work on RAIS data and unify with other dataframes
# setwd(codes_path)
# flname = paste0("prep_ff_rais_env_20240402.R")
# source(flname)
# ff_rais()


#This code will  run some descriptive analysis
setwd(codes_path)
flname = paste0("ff_descriptive_analysis_20240510.R")
source(flname)
ff_descriptive()


# #This code will  run the regressions
# setwd(codes_path)
# flname = paste0("ff_regs_env_",date,".R")
# source(flname)
# ff_regs()