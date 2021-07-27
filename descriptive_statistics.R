library(dplyr)
library(tidyverse)
library(psych) #for summary statistics 
library(plyr)
library(stringr)
rm(list = ls())

############Descriptive Statistics 

#Read in clean datasets
suicide <- read.csv("clean_data/suicides_wide.csv") %>%  
    as_tibble() 

deprivation <- read.csv("clean_data/GISD_wide.csv") %>%
    as_tibble() %>% 
    dplyr::rename (kreis_number = Kennziffer, 
            kreis = Raumeinheit,
            unemployment = ZX_1, 
            employees_residence_technical_university_degree = ZX_2, 
            employment_rate = ZX_3, 
            gross_wage_and_salary= ZX_4,
            net_household_income = ZX_5, 
            school_leavers_without_qualification = ZX_6,
            debtor_rate = ZX_7, 
            tax_revenues = ZX_8) %>% 
    select(-c((starts_with("X_"))))

citizens <-  read.csv("clean_data/citizens_wide.csv") %>%  as_tibble()


######## Descriptive Statistics ######
###Summary Statistics 

#summarize population statistics 
pop_2017 <- citizens %>%  
    mutate(insgesamt = as.numeric(insgesamt)) %>%  
    select(insgesamt) %>%  
    mutate(mean = mean(insgesamt), 
           sd = sd(insgesamt), min = min(insgesamt), 
           max = max(insgesamt)) %>% slice_tail() %>% 
    select(-insgesamt) %>% as.matrix() 

#summarize suicide statistics:
suicide_summary <- suicide %>% filter(kreis != "Niedersachsen") %>% 
    select(kreis, total) %>% 
    mutate(mean = mean(total), 
           sd = sd(total), 
           max = max(total), 
           min = min(total)) %>% 
    select(-kreis ,-total) %>% 
    slice_tail() %>% 
    as.matrix()

#summarize deprivation statistics
deprivation_summary <- deprivation %>% 
    select(-c(kreis, kreis_number)) %>% 
    mutate_all(as.numeric) %>% 
    describe(fast = TRUE) %>% as.matrix() %>% 
    subset(select = -c(se, n,vars, range))

full_summary <- rbind(suicide_summary,pop_2017)
rownames(full_summary) <- c("Number of Suicides 2017", "Total Population 2017")
#full_summary <- rbind(full_summary, deprivation_summary)   #This changes the values - Don't know how to fix now



###########Plots
#1. Plot: Regression line between deprivation index and suicide deaths per 100.000 
summary(suicides) 
summary(deprivation)
# calculate deprivation index as in paper: "The standardized Z-scores for the eight indicators were summed up
# to create the deprivation index." 
deprivation <- deprivation %>%  
    mutate(deprivation = rowSums(.[3:10])) %>% 
    arrange(desc(deprivation))

f <- deprivation %>%  
    mutate(employees_residence_technical_university_degree = employees_residence_technical_university_degree*(-1),
           employment_rate = employment_rate*(-1),
           gross_wage_and_salary = gross_wage_and_salary*(-1),
           net_household_income = net_household_income*(-1),
           tax_revenues = tax_revenues*(-1)) %>% 
    mutate(deprivation = rowSums(.[3:10])) %>% 
    arrange(desc(deprivation))

colnames(deprivation)
