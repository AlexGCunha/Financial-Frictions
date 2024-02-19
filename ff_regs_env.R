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


############################################################
#Additional modifications in the database
############################################################

setwd(data_path)
df_1<-read_parquet("fin_fric_dataset.parquet")

#Create a "balanced panel". Will be important for lags and leads
dates = unique(df_1$ano)
firms = unique(df_1$cnpj8)
df <- expand.grid(ano=dates, cnpj8=firms)

df <- df %>% left_join(df_1, by=c("ano","cnpj8"), na_matches="never")
rm(df_1)

print(summary(df))

#Create lag and lead npl
df <- df %>% 
  arrange(ano, cnpj8) %>% 
  group_by(cnpj8) %>% 
  mutate(lag_npl = lag(npl_180, n=1L)) %>% 
  mutate(lead_npl = lead(npl_180)) %>% 
  ungroup()


#Create Additional Variables
df <- df %>% 
  filter(spread>0) %>% 
  mutate(lspread = log(1+spread)) %>% 
  mutate(uf = substr(as.character(munic_ibge),1,2)) %>% 
  mutate(sect_state_time = paste0(sector, uf, ano))


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


#merge with distance DF
setwd(data_path)
df_dist <- read_parquet("Distances_munic.parquet") %>% 
  rename(munic_ibge = munic) %>% 
  mutate(munic_ibge = substr(as.character(munic_ibge),1,6)) %>% select(c(1,2)) %>% 
  mutate(munic_ibge = as.numeric(munic_ibge))

df <- df %>% 
  left_join(df_dist, by="munic_ibge", na_matches = "never")

rm(df_dist)
gc()

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
sink(file = "without_insp.txt")
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
sink(file = "with_insp_per_pop.txt")
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
sink(file = "with_insp_per.txt")
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
sink(file = "with_insp_pop.txt")
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
sink(file = "with_inspected.txt")
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
sink(file = "with_inspected_after.txt")
print(summary(m6))
sink()

