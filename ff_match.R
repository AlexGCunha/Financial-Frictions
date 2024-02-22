#------------------------------------------------------------------------------------------------
# This code will  create the matched sample
#- It needs prep_ff_rais
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
if (!require("MatchIt")) install.packages("splitstackshape", repos="http://artifactory.bcnet.bcb.gov.br/artifactory/cran-remote/")


library(tidyverse)
library(haven)
library(readxl) 
library(arrow)
library(MatchIt)

df = read_parquet('fin_fric_dataset.parquet')

#Some modifications
df = df %>% 
  #calculate how many times a firm was inspected
  group_by(cnpj8) %>% 
  mutate(n_inspected = unique(year_decision),
         n_inspected = ifelse(is.na(n_inspected),0,n_inspected)) %>% 
  ungroup() %>% 
  #filter companies that were inspected at most once
  filter(n_inspected <=1) %>% 
  #take the year the company was first inspected
  group_by(cnpj_8) %>% 
  mutate(inspected = max(c(0,inspected), na.rm = T),
         year_decision = max(c(0,year_decision), na.rm = T)) %>% 
  #filter
    




