
library(tidyverse)
library(haven)
library(readxl) 
library(arrow)


scr_path = "C:/Users/xande/OneDrive/Documentos/Doutorado/RA/Financial_Frictions/Data/SCR"
rais_path = "C:/Users/xande/OneDrive/Documentos/Doutorado/RA/Financial_Frictions/Data/RAIS"
other_path = "C:/Users/xande/OneDrive/Documentos/Doutorado/RA/Financial_Frictions/Data/Other"
output_path = "C:/Users/xande/OneDrive/Documentos/Doutorado/RA/Financial_Frictions/Output"

setwd(other_path)

df <- read_parquet("census_pes_2010.parquet")


df<- df %>% 
  #Only 15-65 age
  filter(age >=15 & age <=65) %>%
  select(worked_ref_week,npaid1,npaid2,npaid3,tried_job,position,weight,munic) %>% 
  #Define employed
  mutate(employed = ifelse(worked_ref_week==1|npaid1==1|npaid2 == 1| npaid3==1,1,0)) %>% 
  mutate(employed = replace(employed,is.na(employed),0)) %>% 
  #Define PEA
  mutate(in_pea = ifelse(employed==1|tried_job==1,1,0)) %>% 
  mutate(in_pea = replace(in_pea,is.na(in_pea),0)) %>%
  #Filter only in PEA
  filter(in_pea==1) %>% 
  #Define formal as employed with a signed booklet
  mutate(formal = ifelse(position==1&employed==1,1,0)) %>% 
  mutate(formal = replace(formal,is.na(formal),0)) %>%
  #Define informal as employed without a signed booklet
  mutate(informal = ifelse(position==3&employed==1,1,0)) %>% 
  mutate(informal = replace(informal,is.na(informal),0)) %>%
  #Aggregating by municipality
  group_by(munic) %>% 
  summarize(pea=sum(weight),
            formal=sum(weight*formal),
            informal=sum(weight*informal),
            employed = sum(weight*employed)) %>% 
  mutate_at(c('munic','pea','formal','informal','employed'),~as.integer(.))

df<-df %>% 
  #Creating other variables
  mutate(private_emp = formal+informal) %>% 
  mutate(inf_rate = informal/private_emp) %>% 
  mutate(inf_rate_all = informal/employed) %>% 
  mutate(emp_rate = employed/pea)


##Create population DF
df2 <- read_parquet("census_pes_2010.parquet") %>% 
  group_by(munic) %>% 
  summarize(pop=sum(weight)) %>%
  mutate_all(~as.integer(.)) %>% 
  inner_join(df,by='munic')


setwd(other_path)
write_dta(df2,'data_munic_census_2010.dta')

rm(df,df2)

