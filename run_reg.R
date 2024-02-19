#------------------------------------------------------------------------------------------------
#This code will run the regressions

#PRECISO MUDAR AO TROCAR OS ARQUIVOS:
#- as partes com apagar
#Os paths em setwd()
#------------------------------------------------------------------------------------------------



library(tidyverse)
library(haven)
library(lfe)
library(lmtest) 
library(fixest)
library(arrow)


rm(list=ls())
scr_path = "C:/Users/xande/OneDrive/Documentos/Doutorado/RA/Financial_Frictions/Data/SCR"
rais_path = "C:/Users/xande/OneDrive/Documentos/Doutorado/RA/Financial_Frictions/Data/RAIS"
other_path = "C:/Users/xande/OneDrive/Documentos/Doutorado/RA/Financial_Frictions/Data/Other"
output_path = "C:/Users/xande/OneDrive/Documentos/Doutorado/RA/Financial_Frictions/Output"


############################################################
#Additional modifications in the database
############################################################

setwd(scr_path)
df_1<-read_dta("fin_fric_dataset.dta")

######APAGAR#######
df_1 <- df_1 %>% 
  rename(cnpj8=cnpj14)

df_1 <- df_1 %>% 
  mutate(spread = case_when(spread <0 ~ -spread,
                            spread <1 ~ spread,
                            T ~ spread)) %>% 
  mutate(lspread = log(spread))

df_1 <- df_1 %>% 
  mutate(uf = sample(1:2, nrow(df_1),T)) %>% 
  filter(loan_outstanding >0)

######NAO PRECISA MAIS APAGAR#######

#Create a "balanced panel". Will be important for lags and leads
dates = unique(df_1$ano)
firms = unique(df_1$cnpj8)
df <- expand.grid(ano=dates, cnpj8=firms)

df <- df %>% left_join(df_1, by=c("ano","cnpj8"), na_matches="never")
rm(df_1)

#Create lag and lead npl
df <- df %>% 
  arrange(ano, cnpj8) %>% 
  group_by(ano, cnpj8) %>% 
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
setwd(other_path)
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


#First Regression
X_vars = paste(vars_other,vars_loanr,vars_loant,sep="+")
formula <- as.formula(paste("lspread~",X_vars,sep=""))


m1 <- felm(lspread ~ maturity + ex_currency + npl_180 + lag_npl + lead_npl + 
             rel_duration + n_banks + dist_min + loanr_private + loanr_bndes + 
             loanr_regional + loanr_other + loant_advance + loant_without_spurpose + 
             loant_receiable + loant_with_spurpose + loant_rural + loant_other + 
             loant_limit + loant_exports + loant_imports + loant_intervention + 
             loant_imob + loant_tvm + loant_infra + loant_leasing + loant_intfin + 
             loant_coobligations + loant_credits_released + loant_linked + 
             loant_linked2 + loant_riskretention|
             sector+uf+ano+sect_state_time,
           data = df)

print(summary(m1))

