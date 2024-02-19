#------------------------------------------------------------------------------------------------
#This code will work on RAIS data and unify with other dataframes
#- But first it will bind all the year scr dfs we've done before
#- This is equivalent to the previous "prep_rais_env", but expliciting that it is for financial frictions (ff)
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


############################################################
#Merge previous files and select only companies to look at
############################################################
years <- c('2005','2006','2007','2008','2009','2010','2011','2012','2013','2014','2015','2016')
years <- c('2005','2006','2007','2008')

aux_count = 0
for (y in years){
  setwd(data_path)
  archive <- paste("SCR_firm_year_",y,".parquet",sep="")
  aux <- read_parquet(archive)
  if (aux_count==0){
    df_scr <- aux
    aux_count <-1
  } else {
    df_scr <- rbind(df_scr, aux)
  }
}

write_parquet(df_scr,"SCR_full.parquet")
rm(aux, df_scr)
gc()

#Define companies to look at
setwd(data_path)
df_emp <- read_parquet("SCR_full.parquet")
companies <- unique(df_emp$cnpj8)
rm(df_emp)
gc()
print("1")


############################################################
#Create an useful mode function to be used later on
############################################################

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

############################################################
#Work with RAIS to create firm variables
############################################################
years <- c('2005','2006','2007','2008')
aux_count = 0

for (y in years){
  setwd(rais_path)
  archive <- paste("RAIS_",y,".dta",sep='')
  df_r <- read_dta(archive)
  
  df_r <- df_r %>%
    # mutate(cnpj8 = as.character(cnpj8)) %>% 
    # mutate_at(c("cnpj8"),~gsub(",","",.)) %>% 
    # mutate_at(c("cnpj8"),~gsub(".","",.)) %>%
    # mutate_at(c("cnpj8"),~gsub("-","",.)) %>%
    # mutate_at(c("cnpj8"),~gsub("/","",.)) %>%
    mutate(cnpj8 = as.numeric(cnpj8)) %>% 
    #Filter only companies that appear in the SCR code
    filter(cnpj8%in%companies) %>%
    #Filter private employees
    filter(!(emp_type%in%c(30,31,35))) %>% 
    #Create a variable of total formal employees
    mutate(wage_contr=str_replace(wage_contr,",",".")) %>% 
    mutate_at(c('municipality',"wage_contr","ind_cnae95"),~as.integer(.)) %>% 
    group_by(cnpj8) %>% 
    summarize(comp_employees = length(unique(cpf)),
              wagebill = sum(wage_contr),
              sector = first(ind_cnae95),
              munic_ibge = Mode(municipality)) %>% 
    mutate(ano = as.integer(y))
  
  if (aux_count==0){
    df <- df_r
    aux_count <- aux_count + 1
    print(y)
  }
  else{
    df <- rbind(df,df_r)
    aux_count <- aux_count+1
    print(y)
  }
}


#Saving rais dataset
setwd(data_path)
write_parquet(df,"rais_worked.parquet")
rm(df, df_r)
gc()





setwd(data_path)
df <- read_parquet("rais_worked.parquet")

############################################################
#Merging different datasets
############################################################
#Census Data
setwd(data_path)
df_census <- read_dta('data_munic_census_2010.dta') %>%
  rename(munic_ibge=munic) %>%
  mutate(munic_ibge =as.character(munic_ibge)) %>% 
  mutate(munic_ibge = as.integer(substr(munic_ibge,1,6)))



#Merge RAIS and census
df<-df %>% 
  left_join(df_census,by='munic_ibge')

rm(df_census)
gc()

#Merge with SCR
setwd(data_path)
df_scr <- read_parquet("SCR_full.parquet")
df<- df %>% 
  left_join(df_scr,by=c('cnpj8','ano'), na_matches="never")


#Merge with Inspections data (municipality level)
df_ins <-read_parquet("inspections_munic.parquet")
df <- df %>% 
  left_join(df_ins, by=c("munic_ibge","ano"))
rm(df_ins)
gc()

#Inspections over population
df <- df %>% 
  mutate(inspect_p = inspections/pop) %>% 
  mutate(inspect_period_p = inspections_period/pop)


#Inspections data (at the firm level)
setwd(data_path)
df_aux = read_parquet("inspected_firms.parquet")

df = df %>% 
  left_join(df_aux, by = "cnpj8", na_matches = "never") %>% 
  mutate(inspected = ifelse(is.na(inspected),0,inspected),
         after_inspection = ifelse(inspected ==1 & year_decision >= ano,1,0))



#Saving
print(summary(df))
setwd(data_path)
write_parquet(df,'fin_fric_dataset.parquet')
rm(list=ls())
gc()

