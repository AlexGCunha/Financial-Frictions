#------------------------------------------------------------------------------------------------
#This code will work on RAIS data and unify with other dataframes
#The outuput is a yearlly dataframe by firm
#------------------------------------------------------------------------------------------------


##############################################################################################
#DÚVIDAS
#

##############################################################################################


##############################################################################################
#OUTROS
# Falta pedir a RAIS estabelecimentos, para variaveis como cidade e idade
# Estou usando wagw_contr pq wage_month estava estranho
#3) Tenho que corrigir os anos para rodar os loops
#4) Preciso apagar os fake
##############################################################################################



##############################################################################################
#Falta fazer
#Age

##############################################################################################




#install.packages("tidyverse")
#install.packages("haven")
#install.packages("readxl")


library(tidyverse)
library(haven)
library(readxl) 




scr_path = "C:/Users/xande/OneDrive/Documentos/Doutorado/RA/Financial_Frictions/Data/SCR"
rais_path = "C:/Users/xande/OneDrive/Documentos/Doutorado/RA/Financial_Frictions/Data/RAIS"
other_path = "C:/Users/xande/OneDrive/Documentos/Doutorado/RA/Financial_Frictions/Data/Other"
output_path = "C:/Users/xande/OneDrive/Documentos/Doutorado/RA/Financial_Frictions/Output"

# data_path = "Z:/Bernardus/Cunha_Santos_Doornik/Dta_files"
# rais_path = "Z:/DATA/Dta_files/RAIS"
# output_path = "Z:/Bernardus/Cunha_Santos_Doornik/Output_check"


############################################################
#Define only the companies we want to look at
############################################################
setwd(scr_path)
df_emp <- read_dta("SCR_full.dta")
companies <- unique(df_emp$cnpj14)
rm(df_emp)


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
years <- c('2011','2012')
aux_count = 0

for (y in years){
  setwd(rais_path)
  archive <- paste("RAIS_",y,".dta",sep='')
  df_r <- read_dta(archive)
  
  df_r <- df_r %>% 
    #Filter only companies that appear in the SCR code
    filter(cnpj14%in%companies) %>%
    #Filter private employees
    filter(!(emp_type%in%c(30,31,35))) %>% 
    #Create a variable of total formal employees
    mutate(wage_contr=str_replace(wage_contr,",",".")) %>% 
    mutate_at(c('municipality',"wage_contr","ind_cnae20","cnpj14"),~as.integer(.)) %>% 
    group_by(cnpj14) %>% 
    summarize(comp_employees = length(unique(cpf)),
              wagebill = sum(wage_contr),
              sector = first(ind_cnae20),
              munic_ibge = Mode(municipality)) %>% 
    mutate(ano = as.integer(y))
  
  if (aux_count==0){
    df <- df_r
    aux_count <- aux_count + 1
  }
  else{
    df <- rbind(df,df_r)
    aux_count <- aux_count+1
  }
}

rm(df_r)
gc()



############################################################
#Read Census data for characteristics on municipalities
############################################################
setwd(other_path)
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
setwd(scr_path)
df_scr <- read_dta("SCR_full.dta")
df<- df %>% 
  left_join(df_scr,by=c('cnpj14','ano'))


#Saving
write_dta(df,'fin_fric_dataset.dta')
rm(df,df_scr)

df<- read_dta("fin_fric_dataset.dta")
