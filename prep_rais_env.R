#------------------------------------------------------------------------------------------------
#This code will work on RAIS data and unify with other dataframes
#- But first it will bind all the year scr dfs we've done before
#------------------------------------------------------------------------------------------------


install.packages("tidyverse")
install.packages("haven")
install.packages("readxl")


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
aux_count = 0
for (y in years){
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
companies <- unique(df_emp$cnpj14)
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
years <- c('2005','2006','2007','2008','2009','2010','2011','2012','2013','2014','2015','2016')
aux_count = 0

for (y in years){
  setwd(rais_path)
  archive <- paste("RAIS_",y,".dta",sep='')
  df_r <- read_dta(archive)
  
  df_r <- df_r %>%
    mutate(cnpj14 = as.character(cnpj14)) %>% 
    mutate_at(c("cnpj14"),~gsub(",","",.)) %>% 
    mutate_at(c("cnpj14"),~gsub(".","",.)) %>%
    mutate_at(c("cnpj14"),~gsub("-","",.)) %>%
    mutate_at(c("cnpj14"),~gsub("/","",.)) %>%
    mutate(cnpj14 = as.numeric(cnpj14)) %>% 
    #Filter only companies that appear in the SCR code
    filter(cnpj14%in%companies) %>%
    #Filter private employees
    filter(!(emp_type%in%c(30,31,35))) %>% 
    #Create a variable of total formal employees
    mutate(wage_contr=str_replace(wage_contr,",",".")) %>% 
    mutate_at(c('municipality',"wage_contr","ind_cnae20"),~as.integer(.)) %>% 
    group_by(cnpj14) %>% 
    summarize(comp_employees = length(unique(cpf)),
              wagebill = sum(wage_contr),
              sector = first(ind_cnae20),
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
rm(df)
gc()

rm(df_r)
gc()


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
print("3")

#Merge with SCR
setwd(data_path)
df_scr <- read_parquet("SCR_full.parquet")
df<- df %>% 
  left_join(df_scr,by=c('cnpj14','ano'))


#Saving
setwd(data_path)
write_parquet(df,'fin_fric_dataset.parquet')
rm(df,df_scr)
gc()