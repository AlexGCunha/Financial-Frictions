#------------------------------------------------------------------------------------------------
#This code will work on SCR data to create an NPL Dataframe
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
#CREATE NPL DATAFRAME
############################################################
#The goal is to create a panel of npl by firm by month
count_aux = 0

years <- c('2005','2006','2007','2008','2009','2010','2011','2012','2013','2014','2015','2016')
months <- c('01','02','03','04','05','06','07','08','09','10','11','12')
setwd(scr_path)
for (y in years){
  for(m in months){
    archive <- paste("SCR_",y,m,".dta",sep="")
    df <- read_dta(archive)
    df<-df %>% 
      select(c(time_id, firm_id, loan_arrears_90_180_days,loan_arrears_90_days,loan_arrears_over_180_days)) %>%
      # #Create fake values for time_id DELETE THIS!!!!!
      # mutate(time_id = paste(y,m,sep="")) %>% 
      # #from now on i dont have to delete
      #Filter only when the time_id is of the correspondent month
      filter(time_id ==paste(y,m,sep="")) %>% 
      group_by(firm_id) %>% 
      summarize(npl_90 = sum(loan_arrears_90_days),
                npl_180 = sum(loan_arrears_90_180_days),
                npl_over_180 = sum(loan_arrears_over_180_days)) %>% 
      mutate(time_id = paste(y,m,sep=""))
    
    if (count_aux ==0){
      df_a <- df
      count_aux = count_aux+1
    }
    else{
      df_a <- rbind(df_a, df)
      count_aux <- count_aux + 1
    }
    
    
  }
  print(y)
}
setwd(data_path)
archive_name <- "SCR_NPL.dta"
write_dta(df_a,archive_name)
rm(df,df_a)
gc()