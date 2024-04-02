ff_firm <- function(){
  #------------------------------------------------------------------------------------------------
  #This code will work on SCR data to create an anually dataframe by firm
  #- it needs prep_ff_year and prep_scr_npl
  #------------------------------------------------------------------------------------------------
  
  options(file.download.method="wininet")
  
  repository = "http://artifactory.bcnet.bcb.gov.br/artifactory/cran-remote/"
  if (!require("dplyr")) install.packages("splitstackshape", repos=repository)
  if (!require("haven")) install.packages("splitstackshape", repos=repository)
  if (!require("readxl")) install.packages("splitstackshape", repos=repository)
  if (!require("arrow")) install.packages("splitstackshape", repos=repository)
  
  data_path = "Z:/Bernardus/Cunha_Santos_Doornik/Dta_files"
  scr_path = "Z:/DATA/Dta_files/SCR"
  output_path = "Z:/Bernardus/Cunha_Santos_Doornik/Output_check"
  
  
  ############################################################
  #CONSTRUCT VARIABLES AND AGGREGATE YEARLY BY FIRM
  ############################################################
  years <- c('2005','2006','2007','2008','2009','2010','2011','2012','2013','2014','2015','2016')
  # years <- c('2005','2006','2007','2008')
  
  count_aux = 0
  
  for (y in years){
    setwd(data_path)
    archive <- paste("SCR_",y,".parquet",sep="")
    df <- read_parquet(archive)
    
    #Filter only prefixed contracts
    df<- df %>% 
      filter(loan_index_rate==11)
    
    #CONSTRUCT AND SELECT VARIABLES
    ############################################################
    df<- df %>% 
      group_by(loan_id, bank_id) %>% 
      #Take only the first line per loan
      summarize_all(first) %>% 
      ungroup() %>% 
      #create some variables to help in the filtering process
      #non private companies by firm control
      mutate(non_private_1= ifelse(firm_control!=1,1,0)) %>% 
      #non private companies by firm nature
      mutate(non_private_2 = ifelse(firm_nature <=201,1,0)) %>% 
      #financial institutions
      mutate(fin_ind = ifelse(firm_industry>=6400&firm_industry<=6900,1,0)) %>%
      #Firms with interbank loans
      mutate(int_loans = ifelse(loan_type==1401,1,0)) %>%
      #Now we group by firms and exclude the ones with either one of the previous characteristics 
      group_by(firm_id) %>% 
      mutate(drop_firm = sum(non_private_1,non_private_2,fin_ind,int_loans)) %>% 
      ungroup() %>% 
      #Exclude all firms with at least one of the previous characteristics
      filter(drop_firm==0) %>% 
      #drop auxiliary columns
      select(-c('non_private_1','non_private_2','fin_ind','int_loans')) %>%  
      #filter only loan types allowed by the BC
      filter(loan_type<500|(loan_type>800&loan_type<900)|
               (loan_type>1300&loan_type<1400)|(loan_type>1900&loan_type<2000)
      ) %>% 
      #Create Maturity variable
      #Maturity
      mutate(maturity = loan_end_date-loan_start_date) %>%  
      #drop impossible contracts
      mutate(maturity = ifelse(maturity<1|maturity>12775,NaN,maturity)) 
    
    
    
    df <- df %>% 
      #Create year, quarter and month columns as well as a date format time id
      mutate(time_id = as.character(time_id)) %>% 
      mutate(time_id_y = substr(time_id,1,4)) %>% 
      mutate(time_id_m = substr(time_id, 5,6)) %>% 
      mutate_at(c('time_id','time_id_y','time_id_m'),as.integer) %>% 
      mutate(time_id_q = ceiling(time_id_m/3)) %>%
      mutate(time_id_ymd = paste(time_id,30,sep='')) %>%
      mutate(time_id_ymd = ifelse(time_id_m%in%c('01','03','05','07','08','10',
                                                 '12'),paste(time_id,31,sep=''),
                                  time_id_ymd)) %>%
      mutate(time_id_ymd = ifelse(time_id_m%in%c('02'),paste(time_id,28,sep=''),
                                  time_id_ymd)) %>%
      mutate(time_id_ymd = as.Date(time_id_ymd,format='%Y%m%d')) %>% 
      #Filter rows where the relationship start date is before the firm start date
      #filter(firm_start_date<=firm_bank_start_date) %>% 
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
      #Advance cash
      mutate(loant_advance = ifelse(loan_type>=101&loan_type<=101,1,0)) %>%
      # Loan withou a specific purpose
      mutate(loant_without_spurpose = ifelse(loan_type>=201&loan_type<=299,1,0)
      ) %>% 
      # Discounted receivables
      mutate(loant_receiable = ifelse(loan_type>=301&loan_type<=399,1,0)) %>%
      # With Specific purpose
      mutate(loant_with_spurpose = ifelse(loan_type>=400&loan_type<=499,1,0)) %>% 
      # Rural loans
      mutate(loant_rural = ifelse(loan_type>=800&loan_type<=899,1,0)) %>%
      # Other types of loans
      mutate(loant_other = ifelse(loan_type>=1300&loan_type<=1399,1,0)) %>% 
      # Some limits (cheque especial, credit card, aquisition of goods...)
      mutate(loant_limit = ifelse(loan_type>=1900&loan_type<=1999,1,0)) %>% 
      mutate(loant_exports = ifelse(loan_type>=501&loan_type<=599,1,0)) %>% 
      mutate(loant_imports = ifelse(loan_type>=601&loan_type<=699,1,0)) %>% 
      mutate(loant_intervention = ifelse(loan_type>=701&loan_type<=799,1,0)) %>% 
      mutate(loant_imob = ifelse(loan_type>=901&loan_type<=999,1,0)) %>% 
      mutate(loant_tvm = ifelse(loan_type>=1001&loan_type<=1099,1,0)) %>% 
      mutate(loant_infra = ifelse(loan_type>=1101&loan_type<=1199,1,0)) %>%
      mutate(loant_leasing = ifelse(loan_type>=1201&loan_type<=1299,1,0)) %>% 
      mutate(loant_intfin = ifelse(loan_type>=1401&loan_type<=1499,1,0)) %>% 
      mutate(loant_coobligations = ifelse(loan_type>=1501&loan_type<=1599,
                                          1,0)) %>% 
      mutate(loant_credits_released = ifelse(loan_type>=1601&loan_type<=1699,
                                             1,0)) %>% #
      mutate(loant_linked = ifelse(loan_type>=1701&loan_type<=1799,1,0)) %>% 
      mutate(loant_linked2 = ifelse(loan_type>=1801&loan_type<=1899,1,0)) %>% 
      mutate(loant_riskretention = ifelse(loan_type>=2001&loan_type<=2099,1,0)) 
    
    
    
    #Dummies for loan resorces
    ############################################################
    df<-df %>% 
      #dummy for loans with private resources
      mutate(loanr_private = ifelse(loan_resource<200,1,0)) %>% 
      # BNDES
      mutate(loanr_bndes = ifelse(loan_resource==202,1,0)) %>% 
      mutate(loanr_regional = ifelse(loan_resource>=204&loan_resource<=207,1,0)) %>% # Dummy for fco, fne, fno and other state funds
      mutate(loanr_other = ifelse(loan_resource==201|loan_resource==203|(loan_resource>=208&loan_resource<300),1,0)) %>% #dummy for other
      #Dummy for collateral
      #dummy for any kind of collateral
      mutate(collateral= ifelse(collateral_type >0,1,0)) %>%  
      #Dummy for external currency
      mutate(ex_currency = ifelse(loan_currency !=-2,1,0)) %>%
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
    setwd(data_path)
    df_npl <- read_dta("SCR_NPL.dta")
    df_npl <- df_npl %>% 
      mutate(time_id = as.integer(time_id))
    
    df<- df %>%
      left_join(df_npl, by=c('firm_id','time_id'))
    
    rm(df_npl)
    gc()
    
    
    #Merge rates database
    ############################################################
    setwd(data_path)
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
    vars_other <- c('loan_outstanding', 'ex_currency', 'n_banks',
                    'rel_duration','maturity','rel_duration',
                    'npl_90','npl_180','npl_over_180','spread',
                    'loan_base_rate','rating')
    #total
    vars_total <- c(vars_other, vars_loanr,vars_loant)
    
    
    
    
    #now, grouping by firm, we can create the annual variables
    df<- df %>% 
      group_by(firm_id) %>% 
      summarize_at(vars_total, ~sum(.*weight)) %>% 
      ungroup() %>% 
      mutate(ano=as.integer(y)) %>% 
      rename(cnpj8=firm_id) %>% 
      # mutate(cnpj8 = as.character(cnpj8)) %>% 
      # mutate_at(c("cnpj8"),~gsub(",","",.)) %>% 
      # mutate_at(c("cnpj8"),~gsub(".","",.)) %>%
      # mutate_at(c("cnpj8"),~gsub("-","",.)) %>%
      # mutate_at(c("cnpj8"),~gsub("/","",.)) %>%
      mutate(cnpj8 = as.numeric(cnpj8))
    
    
    #Saving firm-year dataframe
    summary(df)
    archive <- paste("SCR_firm_year_",y,".parquet",sep="")
    setwd(data_path)
    write_parquet(df,archive)
    rm(df)
    gc()
    
    
    print(y)
  }
  
  
}