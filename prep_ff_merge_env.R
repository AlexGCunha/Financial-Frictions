#------------------------------------------------------------------------------------------------
#This code will work on RAIS data and unify with other dataframes
#- Merge Rais, SCR and census data
#------------------------------------------------------------------------------------------------


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


library(tidyverse)
library(haven)
library(readxl) 
library(arrow)



data_path = "Z:/Bernardus/Cunha_Santos_Doornik/Dta_files"
rais_path = "Z:/DATA/Dta_files/RAIS"
output_path = "Z:/Bernardus/Cunha_Santos_Doornik/Output_check"


setwd(data_path)
df <- read_parquet("rais_worked.parquet")

############################################################
#Read Census data for characteristics on municipalities
############################################################
setwd(data_path)
df_census <- read_dta('data_munic_census_2010.dta') %>%
  rename(munic_ibge=munic) %>%
  mutate(munic_ibge =as.character(munic_ibge)) %>% 
  mutate(munic_ibge = as.integer(substr(munic_ibge,1,6)))




############################################################
#Merging different datasets
############################################################
#Merge RAIS and census
df<-df %>% 
  left_join(df_census,by='munic_ibge')

rm(df_census)
gc()

#Merge with SCR
setwd(data_path)
df_scr <- read_parquet("SCR_full.parquet")
df<- df %>% 
  left_join(df_scr,by=c('cnpj14','ano'))


#Saving
setwd(data_path)
write_parquet(df,'fin_fric_dataset.parquet')
rm(list=ls())
gc()

