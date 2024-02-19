#------------------------------------------------------------------------------------------------
#This code will work on SCR data to create an anually dataframe by firm
#------------------------------------------------------------------------------------------------


##############################################################################################
#D?VIDAS
#4) De que ponto de vista eu estou olhando a base?
# - Mostra, em cada mes, todos os emprestimos ativos e alguns inativos, entao os emprestimos //
# reaparecem na base - para n?s ? melhor olhara primeira apari??o de cada empr?stimo
# - Muitos loan_outstanding = 0 - depois de um tempo as empresas vao pagando o emprestimo, e isso ? abatido \\
# do loan_outstanding
#3) Non-performing loans 
# - estao a nivel da operacao
# - Verificar quest?o de taxas com o Marcelo
#6) Por que valor do  colateral ? uma info ruim?
# - Algumas opera??es aparecem como com colateral, mas as empresas n botam o valor.//
# Alem disso, valor do colateral n ? atualizado ao longo do tempo algumasvezes (n tao importante)
#7) QUe outras metricas de risco usar: 
# - Garantia
# - Origem dos recursos
# - Rating (caracteristica da firma? FE)
#6) Emprestimo denominado em outra moeda,? Na base tem loan_currency, mas a maioria tem valor -2 (estou supoden ser real), mas n achei na descric??o uma confirma??o disso 
#8) Creditos  permitidos pelo bc, estou usando os mesmos que vc usou no sue paper decredit unions, mas n sei se faz sentido
#9) Estou dividindo os recursos entre nao direcionados (recursos proprios), direcionados por bndes, direcionados por fundos regionais e direcionados por outros
# e o que sao esses considerados como default no recurso usado?
#10) O que ? earmarkerd rate? S?o taxas de juros direcionadas

##############################################################################################


##############################################################################################
#OUTROS
#2) APAGAR A PARTE DE GERAR TIME ID FALSOS (tanto na secao de criar planilha de npl qt na de agregar por ano-firma)
#3) Tenho que corrigir os anos para rodar os loops
#4) Preciso apagar os fake
#5) Muitos base_rate =0 e spread negativo
##############################################################################################



##############################################################################################
#Falta fazer
#- um codigo para unificar
##############################################################################################

  



#install.packages("tidyverse")
#install.packages("haven")
#install.packages("readxl")


library(tidyverse)
library(haven)
library(readxl)




set.seed(1234)
scr_path = "C:/Users/xande/OneDrive/Documentos/Doutorado/RA/Financial_Frictions/Data/SCR"
rais_path = "C:/Users/xande/OneDrive/Documentos/Doutorado/RA/Financial_Frictions/Data/RAIS"
other_path = "C:/Users/xande/OneDrive/Documentos/Doutorado/RA/Financial_Frictions/Data/Other"
output_path = "C:/Users/xande/OneDrive/Documentos/Doutorado/RA/Financial_Frictions/Output"


# data_path = "Z:/Bernardus/Cunha_Santos_Doornik/Dta_files"
# scr_path = "Z:/DATA/Dta_files/SCR"
# output_path = "Z:/Bernardus/Cunha_Santos_Doornik/Output_check"


############################################################
#CREATE NPL DATAFRAME
############################################################
#The goal is to create a panel of npl by firm by month
count_aux = 0

years <- c('2011')
months <- c('01','02','03','04','05','06','07','08','09','10','11','12')
setwd(scr_path)
for (y in years){
  for(m in months){
    archive <- paste("SCR_FAKE_",y,m,".dta",sep="")
    df <- read_dta(archive)
    df<-df %>% 
      select(c(time_id, firm_id, loan_arrears_90_180_days,loan_arrears_90_days,loan_arrears_over_180_days)) %>%
      #Create fake values for time_id DELETE THIS!!!!!
      mutate(time_id = paste(y,m,sep="")) %>% 
      #from now on i dont have to delete
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
      print(y)
    }
    else{
      df_a <- rbind(df_a, df)
      count_aux <- count_aux + 1
      print(y)
      
    }
      
    
  }
}

archive_name <- "SCR_NPL.dta"
write_dta(df_a,archive_name)
rm(df,df_a)
gc()
print("1")


############################################################
#AGGREGATE MONTHLY DATA INTO YEARLY DATAFRAMES
############################################################


setwd(scr_path)
for (y in years){
  count_aux = 0
  for (m in months){
    archive <- paste("SCR_FAKE_",y,m,".dta",sep="")
    df <- read_dta(archive, 
                   col_select = c(time_id, loan_id, bank_id, firm_id,loan_losses,
                                  loan_outstanding, loan_type, loan_rating, loan_base_rate, loan_index_rate,loan_resource,
                                  loan_start_date,loan_end_date, firm_start_date,firm_bank_start_date, loan_currency,
                                  firm_size, firm_nature, firm_control,firm_industry, firm_location, collateral_type
                   ))
    #Filter only prefixed contracts
    df<- df %>% 
      filter(loan_index_rate==11)
    
    
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
      print(y)
    }
    else{
      df_a <- rbind(df_a, df)
      count_aux <- count_aux + 1
      print(y)
    }
  }
  #Saving year dataframe
  archive_name <- paste("SCR_",y,".dta",sep="")
  write_dta(df_a,archive_name)
  
  
}

rm(df,df_a)
gc()
print("2")

############################################################
#CONSTRUCT VARIABLES AND AGGREGATE YEARLY BY FIRM
############################################################
count_aux = 0

for (y in years){
  setwd(scr_path)
  archive <- paste("SCR_",y,".dta",sep="")
  df <- read_dta(archive)
  
  #CONSTRUCT AND SELECT VARIABLES
  ############################################################
  df<- df %>% 
    group_by(loan_id, bank_id) %>% 
    #Take only the first line per loan
    summarize_all(first) %>% 
    ungroup() %>% 
    #create some variables to help in the filtering process
    mutate(non_private_1= ifelse(firm_control!=1,1,0)) %>% #non private companies by firm control
    mutate(non_private_2 = ifelse(firm_nature <=201,1,0)) %>% #non private companies by firm nature
    mutate(fin_ind = ifelse(firm_industry>=6400&firm_industry<=6900,1,0)) %>% #financial institutions
    mutate(int_loans = ifelse(loan_type==1401,1,0)) %>% #Firms with interbank loans
    #Now we group by firms and exclude the ones with either one of the previous characteristics 
    group_by(firm_id) %>% 
    mutate(drop_firm = sum(non_private_1,non_private_2,fin_ind,int_loans)) %>% 
    ungroup() %>% 
    filter(drop_firm==0) %>% #Exclude all firms with at least one of the previous characteristics
    select(-c('non_private_1','non_private_2','fin_ind','int_loans')) %>%  #drop auxiliary columns
    filter(loan_type<500|(loan_type>800&loan_type<900)|(loan_type>1300&loan_type<1400)|(loan_type>1900&loan_type<2000)) %>% #filter only loan types allowed by the BC
    #Create Maturity variable
    mutate(maturity = loan_end_date-loan_start_date) %>%  #Maturity
    mutate(maturity = ifelse(maturity<1|maturity>12775,NaN,maturity)) #drop impossible contracts
  
  
  
  
  ##########Create fake values for time ID (DELETE THIS!!!!!)
  month = sample(c('01','02','03','04','05','06','07','08','09','10','11','12'),nrow(df),replace=TRUE)
  year = rep("2011",nrow(df))
  fake_date = paste(year,month,sep='')
  df <- df %>% 
    mutate(time_id = as.integer(fake_date))
  ########## Fom here down I dont have do erase
  
  
  df <- df %>% 
    #Create year, quarter and month columns as well as a date format time id
    mutate(time_id = as.character(time_id)) %>% 
    mutate(time_id_y = substr(time_id,1,4)) %>% 
    mutate(time_id_m = substr(time_id, 5,6)) %>% 
    mutate_at(c('time_id','time_id_y','time_id_m'),as.integer) %>% 
    mutate(time_id_q = ceiling(time_id_m/3)) %>%
    mutate(time_id_ymd = paste(time_id,30,sep='')) %>%
    mutate(time_id_ymd = ifelse(time_id_m%in%c('01','03','05','07','08','10','12'),paste(time_id,31,sep=''),time_id_ymd)) %>%
    mutate(time_id_ymd = ifelse(time_id_m%in%c('02'),paste(time_id,28,sep=''),time_id_ymd)) %>%
    mutate(time_id_ymd = as.Date(time_id_ymd,format='%Y%m%d')) %>% 
    #Filter rows where the relationship start date is before the firm start date
    filter(firm_start_date<=firm_bank_start_date) %>% 
    #Create firm-bank relationship time measure
    group_by(bank_id, firm_id) %>% 
    mutate(mean_start = mean(firm_bank_start_date, na.rm=T)) %>% 
    ungroup() %>% 
    #Filter dates different from 1901 (problematic)
    filter(format(loan_start_date,"%Y")!="1901") %>% 
    filter(format(firm_bank_start_date,"%Y")!="1901") %>% 
    mutate(rel_duration = as.integer(loan_start_date-mean_start))
  
  
  #Dummies for loan types
  ############################################################
  df<-df %>% 
    mutate(loant_advance = ifelse(loan_type>=101&loan_type<=101,1,0)) %>% #Advance cash
    mutate(loant_without_spurpose = ifelse(loan_type>=201&loan_type<=299,1,0)) %>% # Loan withou a specific purpose
    mutate(loant_receiable = ifelse(loan_type>=301&loan_type<=399,1,0)) %>% # Discounted receivables
    mutate(loant_with_spurpose = ifelse(loan_type>=400&loan_type<=499,1,0)) %>% # With Specific purpose
    mutate(loant_rural = ifelse(loan_type>=800&loan_type<=899,1,0)) %>% # Rural loans
    mutate(loant_other = ifelse(loan_type>=1300&loan_type<=1399,1,0)) %>% # Other types of loans
    mutate(loant_limit = ifelse(loan_type>=1900&loan_type<=1999,1,0)) %>% # Some limits (cheque especial, credit card, aquisition of goods...)
    mutate(loant_exports = ifelse(loan_type>=501&loan_type<=599,1,0)) %>% #
    mutate(loant_imports = ifelse(loan_type>=601&loan_type<=699,1,0)) %>% #
    mutate(loant_intervention = ifelse(loan_type>=701&loan_type<=799,1,0)) %>% #
    mutate(loant_imob = ifelse(loan_type>=901&loan_type<=999,1,0)) %>% #
    mutate(loant_tvm = ifelse(loan_type>=1001&loan_type<=1099,1,0)) %>% #
    mutate(loant_infra = ifelse(loan_type>=1101&loan_type<=1199,1,0)) %>% #
    mutate(loant_leasing = ifelse(loan_type>=1201&loan_type<=1299,1,0)) %>% #
    mutate(loant_intfin = ifelse(loan_type>=1401&loan_type<=1499,1,0)) %>% #
    mutate(loant_coobligations = ifelse(loan_type>=1501&loan_type<=1599,1,0)) %>% #
    mutate(loant_credits_released = ifelse(loan_type>=1601&loan_type<=1699,1,0)) %>% #
    mutate(loant_linked = ifelse(loan_type>=1701&loan_type<=1799,1,0)) %>% #
    mutate(loant_linked2 = ifelse(loan_type>=1801&loan_type<=1899,1,0)) %>% #
    mutate(loant_riskretention = ifelse(loan_type>=2001&loan_type<=2099,1,0)) #
    
  
  
  #Dummies for loan resorces
  ############################################################
  df<-df %>% 
    mutate(loanr_private = ifelse(loan_resource<200,1,0)) %>% #dummy for loans with private resources
    mutate(loanr_bndes = ifelse(loan_resource==202,1,0)) %>% # BNDES
    mutate(loanr_regional = ifelse(loan_resource>=204&loan_resource<=207,1,0)) %>% # Dummy for fco, fne, fno and other state funds
    mutate(loanr_other = ifelse(loan_resource==201|loan_resource==203|(loan_resource>=208&loan_resource<300),1,0)) %>% #dummy for other
    #Dummy for collateral
    mutate(collateral= ifelse(collateral_type >0,1,0)) %>%  #dummy for any kind of collateral
    #Dummy for external currency
    mutate(ex_currency = ifelse(loan_currency !=-2,1,0)) %>%  #dummy for loans in external currency
    #Number of banks
    group_by(firm_id) %>% 
    mutate(n_banks = length(unique(bank_id))) %>% 
    ungroup()
  
  
  
  #Ratings
  ############################################################
  df<-df %>% 
    mutate(rating = case_when(loan_rating =="AA"~10,
                              loan_rating =="A"~9,
                              loan_rating =="B"~8,
                              loan_rating =="C"~7,
                              loan_rating =="D"~6,
                              loan_rating =="E"~5,
                              loan_rating =="F"~4,
                              loan_rating =="G"~3,
                              loan_rating =="H"~2))
  
  
  
  #Merge with NPL database
  ############################################################
  df_npl <- read_dta("SCR_NPL.dta")
  df_npl <- df_npl %>% 
    mutate(time_id = as.integer(time_id))
  
  df<- df %>%
    left_join(df_npl, by=c('firm_id','time_id'))
  
  rm(df_npl)
  gc()
  
  
  #Merge rates database
  ############################################################
  setwd(other_path)
  df_rates <- read_excel('cdi.xlsx',sheet='month')
  df_rates<- df_rates %>% 
    mutate(time_id = as.integer(time_id))
  
  df<-df %>% 
    left_join(df_rates,by='time_id')
  rm(df_rates)
  gc()
  
  
  
  #Filter non 0 base_rate and create spreads
  ############################################################
  df<- df %>% 
    filter(loan_base_rate !=0) %>% 
    mutate(spread = loan_base_rate-taxa)
  
  
  
  #AGGREGATE BY FIRM
  ############################################################
  df<- df %>%
    #Create loan weights by firm as the share of each loan on the total ammount borrowed by the firm in the year
    group_by(firm_id) %>% 
    mutate(firm_tot_loan = sum(loan_outstanding)) %>% 
    ungroup() %>% 
    mutate(weight = loan_outstanding/firm_tot_loan) %>%
    mutate(weight = replace(weight, is.na(weight),0))
  
  ##Create a list of variables we want to aggregate
  #type of loan
  vars_loant <- colnames(df[,grepl("loant",names(df))])
  #Type of resource
  vars_loanr <- colnames(df[,grepl("loanr",names(df))])
  #Other
  vars_other <- c('loan_outstanding', 'ex_currency', 'n_banks','rel_duration','maturity','rel_duration',
                  'npl_90','npl_180','npl_over_180','spread','loan_base_rate','rating')
  #total
  vars_total <- c(vars_other, vars_loanr,vars_loant)
  
  
  
  
  #now, grouping by firm, we can create the annual variables
  df<- df %>% 
    group_by(firm_id) %>% 
    summarize_at(vars_total, ~sum(.*weight)) %>% 
    mutate(ano=as.integer(y)) %>% 
    rename(cnpj14=firm_id)
  
  
  if (count_aux ==0){
    df_a <- df
    count_aux = count_aux+1
    print(y)
  }
  else{
    df_a <- rbind(df_a, df)
    count_aux <- count_aux + 1
    print(y)
    
  }
}

setwd(scr_path)
write_dta(df_a,"SCR_full.dta")
rm(df,df_a)
gc()
print("3")



