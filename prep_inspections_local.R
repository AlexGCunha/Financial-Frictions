#------------------------------------------------------------------------------------------------
#This code will generate a panel of inspections across municipalities
#------------------------------------------------------------------------------------------------

rm(list=ls())
library(tidyverse)
library(haven)
library(readxl)
library(arrow)

scr_path = "C:/Users/xande/OneDrive/Documentos/Doutorado/RA/Financial_Frictions/Data/SCR"
rais_path = "C:/Users/xande/OneDrive/Documentos/Doutorado/RA/Financial_Frictions/Data/RAIS"
other_path = "C:/Users/xande/OneDrive/Documentos/Doutorado/RA/Financial_Frictions/Data/Other"


setwd(other_path)
df <- read_stata("inspections.dta")

#Select only columns we want
df <- df %>%
  rename(munic_ibge = cod_municpio) %>% 
  mutate(munic_ibge = as.integer(substr(munic_ibge,1,6))) %>% 
  select(!c(cod_uf, uf,nome_uf,municpio, cod_micro, nome_micro, '_merge'
            ,c(starts_with("rfa")),c(starts_with("rfb"))
            )
         )

#Create a panel of inspections
df <- df %>% 
  pivot_longer(1: (ncol(df)-1), values_to="inspections") %>% 
  mutate(ano = substr(name, nchar(name)-3,nchar(name))) %>% 
  mutate(ano = as.integer(ano)) %>% 
  select(!name)

#Create a variable of inspections in the period
df <- df %>% 
  group_by(munic_ibge) %>% 
  mutate(inspections_period = sum(inspections[ano <=2008 & ano >=2005])) %>% 
  ungroup()

#Save
write_parquet(df,"inspections_munic.parquet")
rm(list=ls())
  
