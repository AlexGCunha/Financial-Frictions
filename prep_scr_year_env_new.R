#------------------------------------------------------------------------------------------------
#This code will work on SCR data to create an anual dataframe
#------------------------------------------------------------------------------------------------

# install.packages("tidyverse")
# install.packages("haven")
# install.packages("readxl")


library(tidyverse)
library(haven)
library(readxl)




data_path = "Z:/Bernardus/Cunha_Santos_Doornik/Dta_files"
scr_path = "Z:/DATA/Dta_files/SCR"
output_path = "Z:/Bernardus/Cunha_Santos_Doornik/Output_check"


############################################################
#AGGREGATE MONTHLY DATA INTO YEARLY DATAFRAMES
############################################################
years <- c('2005','2006','2007','2008','2009','2010','2011','2012','2013','2014','2015','2016')
months <- c('01','02','03','04','05','06','07','08','09','10','11','12')


for (y in years){
  count_aux = 0
  setwd(scr_path)
  for (m in months){
    archive <- paste("SCR_",y,m,".dta",sep="")
    df <- read_dta(archive,
                   col_select = c(time_id, loan_id, bank_id, firm_id,
                                  loan_outstanding, loan_type, loan_rating, loan_base_rate, loan_index_rate,loan_resource,
                                  loan_start_date,loan_end_date, firm_start_date,firm_bank_start_date, loan_currency,
                                  firm_nature, firm_control,firm_industry, collateral_type)
                   )
    
    #select only the contracts that started in the given year:
    df<- df %>% 
      #Some modifications in date variables
      mutate_at(c("loan_start_date","loan_end_date","firm_bank_start_date"),as.Date) %>% 
      mutate_at(c("loan_start_date","loan_end_date","firm_bank_start_date"),~case_when(.<=as.Date("1jan1901",format='%d%b%Y')~as.Date(NA),TRUE~.)) %>% #input nan for dates befor 1901
      mutate(loan_start_year = format(loan_start_date, "%Y")) %>% 
      #filter only contracts that started in this year
      filter(loan_start_year==y)
    
    
    
    if(count_aux==0){
      df_a <- df
      count_aux <- count_aux + 1
    }
    else{
      df_a <- rbind(df_a, df)
      count_aux <- count_aux + 1
    }
  }
  #Saving year dataframe
  setwd(data_path)
  archive_name <- paste("SCR_",y,".dta",sep="")
  write_dta(df_a,archive_name)
  print(y)
  
  
}

rm(df,df_a)
gc()