ff_regs <- function(){
  #------------------------------------------------------------------------------------------------
  #This code will run the regressions
  #------------------------------------------------------------------------------------------------
  library(arrow)
  library(haven)
  library(lfe)
  
  options(file.download.method="wininet")
  
  if (!require("dplyr")) install.packages("splitstackshape", repos="http://artifactory.bcnet.bcb.gov.br/artifactory/cran-remote/")
  if (!require("haven")) install.packages("splitstackshape", repos="http://artifactory.bcnet.bcb.gov.br/artifactory/cran-remote/")
  if (!require("readxl")) install.packages("splitstackshape", repos="http://artifactory.bcnet.bcb.gov.br/artifactory/cran-remote/")
  if (!require("arrow")) install.packages("splitstackshape", repos="http://artifactory.bcnet.bcb.gov.br/artifactory/cran-remote/")
  if (!require("stringr")) install.packages("splitstackshape", repos="http://artifactory.bcnet.bcb.gov.br/artifactory/cran-remote/")
  if (!require("tibble")) install.packages("splitstackshape", repos="http://artifactory.bcnet.bcb.gov.br/artifactory/cran-remote/")
  if (!require("tidyr")) install.packages("splitstackshape", repos="http://artifactory.bcnet.bcb.gov.br/artifactory/cran-remote/")
  if (!require("ggplot2")) install.packages("splitstackshape", repos="http://artifactory.bcnet.bcb.gov.br/artifactory/cran-remote/")
  if (!require("readr")) install.packages("splitstackshape", repos="http://artifactory.bcnet.bcb.gov.br/artifactory/cran-remote/")
  if (!require("purrr")) install.packages("splitstackshape", repos="http://artifactory.bcnet.bcb.gov.br/artifactory/cran-remote/")
  if (!require("lfe")) install.packages("splitstackshape", repos="http://artifactory.bcnet.bcb.gov.br/artifactory/cran-remote/")
  if (!require("lmtest")) install.packages("splitstackshape", repos="http://artifactory.bcnet.bcb.gov.br/artifactory/cran-remote/")
  if (!require("fixest")) install.packages("splitstackshape", repos="http://artifactory.bcnet.bcb.gov.br/artifactory/cran-remote/")
  
  
  
  rm(list=ls())
  data_path = "Z:/Bernardus/Cunha_Santos_Doornik/Dta_files"
  rais_path = "Z:/DATA/Dta_files/RAIS"
  output_path = "Z:/Bernardus/Cunha_Santos_Doornik/Output_check"
  
  setwd(data_path)
  df = read_parquet('fin_fric_dataset.parquet')
  date = Sys.Date()
  
  
  ##Create a list of variables
  #type of loan
  vars_loant <- colnames(df[,grepl("loant",names(df))])
  vars_loant <- paste(vars_loant,collapse="+")
  #Type of resource
  vars_loanr <- colnames(df[,grepl("loanr",names(df))])
  vars_loanr <- paste(vars_loanr,collapse="+")
  #Other Varaiables
  vars_other <- c("maturity", "ex_currency", "npl_180","lag_npl","lead_npl","rel_duration", "n_banks","dist_min")
  vars_other <- paste(vars_other,collapse="+")
  
  
  
  
  ############################################################
  #Regressions
  ############################################################
  #Without inspections
  m1 <- felm(lspread ~ maturity + ex_currency + npl_180 + lag_npl + lead_npl + 
               rel_duration + n_banks + dist_min + loanr_private + #loanr_bndes + 
               loanr_regional + loanr_other + loant_advance + loant_without_spurpose + 
               loant_receiable + loant_with_spurpose + loant_rural #+ loant_other  
             # loant_limit + loant_exports + loant_imports + loant_intervention +
             # loant_imob + loant_tvm + loant_infra + loant_leasing + loant_intfin +
             # loant_coobligations + loant_credits_released + loant_linked +
             # loant_linked2 + loant_riskretention
             |
               sector+uf+ano+sect_state_time,
             data = df)
  
  setwd(output_path)
  filename = paste0("without_insp_",date,".txt")
  sink(file = filename)
  print(summary(m1))
  sink()
  
  #With inspections from 2005-2008 over population
  m2 <- felm(lspread ~ maturity + ex_currency + npl_180 + lag_npl + lead_npl + 
               rel_duration + n_banks + dist_min +inspect_period_p +loanr_private + #loanr_bndes + 
               loanr_regional + loanr_other + loant_advance + loant_without_spurpose + 
               loant_receiable + loant_with_spurpose + loant_rural #+ loant_other  
             # loant_limit + loant_exports + loant_imports + loant_intervention +
             # loant_imob + loant_tvm + loant_infra + loant_leasing + loant_intfin +
             # loant_coobligations + loant_credits_released + loant_linked +
             # loant_linked2 + loant_riskretention
             |
               sector+uf+ano+sect_state_time,
             data = df)
  
  setwd(output_path)
  filename = paste0("with_insp_per_pop_",date,".txt")
  sink(file = filename)
  print(summary(m2))
  sink()
  
  #With inspections from 2005-2008
  m3 <- felm(lspread ~ maturity + ex_currency + npl_180 + lag_npl + lead_npl + 
               rel_duration + n_banks + dist_min +inspections_period +loanr_private + #loanr_bndes + 
               loanr_regional + loanr_other + loant_advance + loant_without_spurpose + 
               loant_receiable + loant_with_spurpose + loant_rural #+ loant_other  
             # loant_limit + loant_exports + loant_imports + loant_intervention +
             # loant_imob + loant_tvm + loant_infra + loant_leasing + loant_intfin +
             # loant_coobligations + loant_credits_released + loant_linked +
             # loant_linked2 + loant_riskretention
             |
               sector+uf+ano+sect_state_time,
             data = df)
  
  setwd(output_path)
  filename = paste0("with_insp_per_",date,".txt")
  sink(file = filename)
  print(summary(m3))
  sink()
  
  #With inspections by year and over population
  m4 <- felm(lspread ~ maturity + ex_currency + npl_180 + lag_npl + lead_npl + 
               rel_duration + n_banks + dist_min +inspect_p +loanr_private + #loanr_bndes + 
               loanr_regional + loanr_other + loant_advance + loant_without_spurpose + 
               loant_receiable + loant_with_spurpose + loant_rural #+ loant_other  
             # loant_limit + loant_exports + loant_imports + loant_intervention +
             # loant_imob + loant_tvm + loant_infra + loant_leasing + loant_intfin +
             # loant_coobligations + loant_credits_released + loant_linked +
             # loant_linked2 + loant_riskretention
             |
               sector+uf+ano+sect_state_time,
             data = df)
  
  setwd(output_path)
  filename = paste0("with_insp_pop_",date,".txt")
  sink(file = filename)
  print(summary(m4))
  sink()
  
  
  #With inspections at the firm level
  m5 <- felm(lspread ~ maturity + ex_currency + npl_180 + lag_npl + lead_npl + 
               rel_duration + n_banks + dist_min +inspected +loanr_private + #loanr_bndes + 
               loanr_regional + loanr_other + loant_advance + loant_without_spurpose + 
               loant_receiable + loant_with_spurpose + loant_rural #+ loant_other  
             # loant_limit + loant_exports + loant_imports + loant_intervention +
             # loant_imob + loant_tvm + loant_infra + loant_leasing + loant_intfin +
             # loant_coobligations + loant_credits_released + loant_linked +
             # loant_linked2 + loant_riskretention
             |
               sector+uf+ano+sect_state_time,
             data = df)
  
  setwd(output_path)
  filename = paste0("with_inspected_",date,".txt")
  sink(file = filename)
  print(summary(m5))
  sink()
  
  
  #With inspections at the firm level, dummy after inspection
  m6 <- felm(lspread ~ maturity + ex_currency + npl_180 + lag_npl + lead_npl + 
               rel_duration + n_banks + dist_min +after_inspection +loanr_private + #loanr_bndes + 
               loanr_regional + loanr_other + loant_advance + loant_without_spurpose + 
               loant_receiable + loant_with_spurpose + loant_rural #+ loant_other  
             # loant_limit + loant_exports + loant_imports + loant_intervention +
             # loant_imob + loant_tvm + loant_infra + loant_leasing + loant_intfin +
             # loant_coobligations + loant_credits_released + loant_linked +
             # loant_linked2 + loant_riskretention
             |
               sector+uf+ano+sect_state_time,
             data = df)
  
  setwd(output_path)
  filename = paste0("with_inspected_after_",date,".txt")
  sink(file = filename)
  print(summary(m6))
  sink()
  
}