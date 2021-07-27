library(dplyr)
library(tidyverse)
library(vtable)#for summary statistics
library(stringr)
library(ggplot2) #for plots
library(plotly)

rm(list = ls())

############Descriptive Statistics 

#Read in clean datasets
suicide <- read.csv("clean_data/suicides_wide.csv") %>%  
    as_tibble() %>% 
    select(kreis,kreisnummer,suicide_total = total)

smr_deprivation <- read.csv("clean_data/Processed_wide.csv") %>% 
    select(-(X)) %>% 
    as_tibble() %>% 
    dplyr::rename (
        kreisnummer = Kennziffer, 
        kreis = Kreis, 
        unemployment = ZX_1, 
        employees_residence_technical_university_degree = ZX_2, 
        employment_rate = ZX_3, 
        gross_wage_and_salary= ZX_4,
        net_household_income = ZX_5, 
        school_leavers_without_qualification = ZX_6,
        debtor_rate = ZX_7, 
        tax_revenues = ZX_8) 

citizens <-  read.csv("clean_data/citizens_wide.csv") %>%  as_tibble() %>% 
    select(kreis, kreisnummer, population_kreis = total)

#Merge all data together
full_data <- citizens %>%  
    select(-kreis) %>%  
    left_join(suicide, by= "kreisnummer") %>% 
    select(-kreis) %>%  
    left_join(smr_deprivation, by = "kreisnummer")
    

######## Descriptive Statistics ######
###Summary Statistics-> Outputs a latexcode to put into overleaf

full_data %>%  select(-kreisnummer) %>% 
    relocate(SMR, .before = kreis) %>% 
    relocate(DI, .after = SMR) %>% 
    rename('Age-standardized suicide rate (per 100,000)' = SMR, 
           'Employee residence with technical university degree' = employees_residence_technical_university_degree, 
           'Net household income' = net_household_income, 
           'Deprivation index' = DI, 
           'Number of suicides 2017' = suicide_total, 
           'Total population 2017' = population_kreis, 
           'Tax revenues' = tax_revenues, 
           'Debtor rate' = debtor_rate, 
           'Gross wage and salary' = gross_wage_and_salary, 
           'School leavers without qualification' = school_leavers_without_qualification,
           'Unemployment' = unemployment,
           'Employment rate' = employment_rate) %>% 
   st( summ=c(     'mean(x)',
                    'median(x)',
                    'sd(x)',
                    'min(x)',
                    'max(x)'), 
                    out = 'latex', 
                    file = 'summary.tx'
                    )

###########Plots
#1. Plot: Regression line between deprivation index and suicide deaths per 100.000 
full_data %>%  
    select(DI, SMR, population_kreis, kreis) %>% 
    ggplot(aes(x = DI, y = SMR, size = population_kreis))+
    stat_smooth(geom = "line",
                method ="lm", 
                alpha = 0.3,
                se = FALSE)+
    geom_point(aes(size = population_kreis), alpha = .7, color = "pink") +
    geom_text(aes(label=kreis),alpha = 0.8) +
  #  scale_color_manual(values = lacroix_palette(n=6, name="PeachPear"))+
    scale_size_continuous(breaks = seq(50000, 1150000, 100000), name = "Population of Kreis")+
    labs(x = "Deprivation Index", 
         y = "Age-standardized suicide rate (per 100,000)")+
    theme(legend.position = "vertical") %>% 
  ggplotly()

#2. Plot: Quantiles of Deprivation Index and Suicide Deaths per 100,000

g <- full_data %>%  
  mutate(quantile_rank = ntile(desc(DI),5)) %>%  
  mutate(quantile_rank = as.factor(quantile_rank))


g %>%  
  group_by(quantile_rank) %>%  
  ggplot(aes(x = quantile_rank, y = SMR))+
  geom_boxplot(aes(color = quantile_rank))+
  theme_minimal()+
  labs(x = "Deprivation Index Quantiles, 1 = Least deprived, 5 = Most deprived", 
       y = "Age-standardized suicide rate (per 100,000)")+
  theme(legend.position = "none")
  




