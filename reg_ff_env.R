#------------------------------------------------------------------------------------------------
#This code will run the regressions
#------------------------------------------------------------------------------------------------



library(tidyverse)
library(haven)
library(plm)
library(lmtest) 



data_path = "Z:/Bernardus/Cunha_Santos_Doornik/Dta_files"
rais_path = "Z:/DATA/Dta_files/RAIS"
output_path = "Z:/Bernardus/Cunha_Santos_Doornik/Output_check"


############################################################
#Additional modifications in the database
############################################################

setwd(data_path)
df<-read_dta("fin_fric_dataset.dta")

#Transform into a panel dataframe
df <- pdata.frame(df,index=c("cnpj14","ano"))

##Create a list of variables
#type of loan
vars_loant <- colnames(df[,grepl("loant",names(df))])
vars_loant <- paste(vars_loant,collapse="+")
#Type of resource
vars_loanr <- colnames(df[,grepl("loanr",names(df))])
vars_loanr <- paste(vars_loanr,collapse="+")
#Other Varaiables
vars_other <- c("maturity", "ex_currency", "npl_180","rel_duration", "n_banks")
vars_other <- paste(vars_other,collapse="+")

############################################################
#Regressions
############################################################


#First Regression
X_vars = paste(vars_other,"lag(npl_180)+lead(npl_180)+factor(ind_cnae20)",vars_loanr,vars_loant,sep="+")
formula = paste("y~",X_vars,sep="")

model1 <- plm(spread ~ treat_after+treat_after_enf+share_woman+share_white+share_higheduc,
              data=df_r,
              index=c("date","mmc"),
              model="within",
              effect="time")


#Save outputs of the models
setwd(output_path)
sink("model1.txt")
print(coeftest(model1, vcov = vcovHC, type = "HC1"))
sink()
