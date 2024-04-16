ff_descriptive <- function(){
  #------------------------------------------------------------------------------------------------
  # This code will  run some descriptive analysis
  #- It needs prep_ff_rais
  #------------------------------------------------------------------------------------------------
  
  
  options(file.download.method="wininet")
  repository = "http://artifactory.bcnet.bcb.gov.br/artifactory/cran-remote/"
  if (!require("dplyr")) install.packages("splitstackshape", repos= repository)
  if (!require("haven")) install.packages("splitstackshape", repos=repository)
  if (!require("readxl")) install.packages("splitstackshape", repos=repository)
  if (!require("arrow")) install.packages("splitstackshape", repos=repository)
  if (!require("stringr")) install.packages("splitstackshape", repos=repository)
  if (!require("tibble")) install.packages("splitstackshape", repos=repository)
  if (!require("tidyr")) install.packages("splitstackshape", repos=repository)
  if (!require("ggplot2")) install.packages("splitstackshape", repos=repository)
  if (!require("readr")) install.packages("splitstackshape", repos=repository)
  if (!require("purrr")) install.packages("splitstackshape", repos=repository)
  #if (!require("MatchIt")) install.packages("splitstackshape", repos=repository)
  

  library(tidyverse)
  library(haven)
  library(readxl) 
  library(arrow)
  
  
  data_path = "Z:/Bernardus/Cunha_Santos_Doornik/Dta_files"
  rais_path = "Z:/DATA/Dta_files/RAIS"
  output_path = "Z:/Bernardus/Cunha_Santos_Doornik/Output_check"
  
  #data_path ="../Data/SCR"
  #output_path ="../../Output"
  
  setwd(data_path)
  # df = read_stata('fin_fric_dataset.dta') %>% 
  #   mutate(ano = as.character(ano))
  
  df = read_parquet('fin_fric_dataset.parquet') %>% 
    mutate(ano = as.character(ano))
  date = Sys.Date()
  
  #Spread Histograms
  p1 = ggplot(df, aes(spread))+
    geom_histogram()+
    facet_wrap(~ano)+theme_minimal()+
    labs(y = "", x = "Spread")
  
  setwd(output_path)
  filename = paste0("histogram_spreads_",date,".png")
  ggsave(filename, plot = p1)
  
  ##############
  #Spreads by year
  ##############
  agg = df %>% 
    group_by(ano) %>% 
    summarise(spreads = weighted.mean(spread, loan_outstanding),
              sd_spreads = sd(spread)) %>% 
    ungroup()
  
  #Average agg spreads
  p2 = ggplot(agg, aes(x = ano, y = spreads))+
    geom_point()+geom_line()+
    theme_minimal()
  
  filename = paste0("time_series_spreads_",date,".png")
  ggsave(filename, plot = p2)
  
  #Standard deviation of agg spreads, per year
  p3 = ggplot(agg, aes(x = ano, y = sd_spreads))+
    geom_point()+geom_line()+
    theme_minimal()+
    labs(y = "SD spreads")
  
  filename = paste0("time_series_sd_spreads_", date, ".png")
  ggsave(filename, plot = p3)
  
  ##############
  #Spreads by year and firm size
  ##############
  #Create a column with indicators for quantile of firm employees
  quantiles = quantile(df$comp_employees, probs = c(0.25, 0.5, 0.75,1))
  df = df %>% 
    mutate(quant_size = cut(comp_employees, quantiles),
           quant_size = as.character(quant_size))
  
  #fake values for testing, delete
  # df = df %>% mutate(quant_size = sample(c(1:4), nrow(df), replace = T)) %>% 
  #   mutate(quant_size = as.character(quant_size))
  
  agg = df %>% 
    group_by(ano, quant_size) %>% 
    summarise(spreads = weighted.mean(spread, loan_outstanding),
              sd_spreads = sd(spread)) %>% 
    ungroup() %>% 
    arrange(ano, quant_size)
  
  #Average agg spreads per year and firm size
  p4 = ggplot(agg, aes(x = ano, y = spreads,
                       color = quant_size, group = quant_size))+
    geom_point()+geom_line()+
    theme_minimal()+
    labs(title = "Avg Spreads by year and firm size quartile",
         color = "N of employees")+
    scale_color_discrete(labels = as.character(quantiles))
  
  filename = paste0("time_series_spreads_size_", date,".png")
  ggsave(filename, plot = p4)
  
  #Standard deviation of agg spreads, per year and firm size
  p5 = ggplot(agg, aes(x = ano, y = sd_spreads,
                       color = quant_size, group = quant_size))+
    geom_point()+geom_line()+
    theme_minimal()+
    labs(title = "SD Spreads by year and firm size quartile",
         color = "N of employees")+
    scale_color_discrete(labels = as.character(quantiles))
  
  filename = paste0("time_series_sd_spreads_size_", date, ".png")
  ggsave(filename, plot = p5)
  
  
  ##############
  #Spreads by year and enforcement level
  ##############
  #Fake values, delete
  # df = df %>% mutate(mid_dist = sample(c(1,0), nrow(df), replace = T))
  
  df = df %>% 
    mutate(enforcement = ifelse(mid_dist == 1, "Low", "High"))
  
  agg = df %>% 
    group_by(ano, enforcement) %>% 
    summarise(spreads = weighted.mean(spread, loan_outstanding),
              sd_spreads = sd(spread)) %>% 
    ungroup() %>% 
    arrange(ano, enforcement)
  
  #Average agg spreads per year and enforcement level
  p6 = ggplot(agg, aes(x = ano, y = spreads,
                       color = enforcement, group = enforcement))+
    geom_point()+geom_line()+
    theme_minimal()+
    labs(title = "Avg Spreads by year and enforcement level",
         color = "Enforcement")
  
  filename = paste0("time_series_spreads_enf_",date, ".png")
  ggsave(filename, plot = p6)
  
  #Standard deviation of agg spreads, per year and enforcement level
  p7 = ggplot(agg, aes(x = ano, y = sd_spreads,
                       color = enforcement, group = enforcement))+
    geom_point()+geom_line()+
    theme_minimal()+
    labs(title = "SD Spreads by year and enforcement level",
         color = "Enforcement")
  
  filename = paste0("time_series_sd_spreads_enf_", date, ".png")
  ggsave(filename, plot = p7)
  
  
  ##############
  #Spreads by year and rating
  ##############
  df = df %>% 
    mutate(rating_round = round(rating, digits = 0))
  
  agg = df %>% 
    group_by(ano, rating_round) %>% 
    summarise(spreads = weighted.mean(spread, loan_outstanding),
              sd_spreads = sd(spread)) %>% 
    ungroup() %>% 
    arrange(ano, rating_round) %>% 
    mutate(rating_round = as.character(rating_round))
  
  #Average agg spreads per year and rating
  p8 = ggplot(agg, aes(x = ano, y = spreads,
                       color = rating_round, group = rating_round))+
    geom_point()+geom_line()+
    theme_minimal()+
    labs(title = "Avg Spreads by year and rating",
         color = "Rating")
  
  filename = paste0("time_series_spreads_rate_", date, ".png")
  ggsave(filename, plot = p8)
  
  #Standard deviation of agg spreads, per year and rating
  p9 = ggplot(agg, aes(x = ano, y = sd_spreads,
                       color = rating_round, group = rating_round))+
    geom_point()+geom_line()+
    theme_minimal()+
    labs(title = "SD Spreads by year and rating",
         color = "Rating")
  
  filename = paste0("time_series_sd_spreads_rate_", date, ".png")
  ggsave(filename, plot = p9)
  
  
  
}