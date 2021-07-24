library(dplyr)
library(tidyverse)
library(psych) #for summary statistics 


#Read in clean dataset
suicide <- read.csv("clean_data/suicides_wide.csv") %>%  
    as_tibble() %>% 
    subset(variable != "Männlich" & variable != "Weiblich")   

deprivation <- readr::read_csv2("raw_data/deprivation.csv") %>%
    as_tibble() %>% 
    dplyr::rename (kreis_number = Kennziffer, 
            kreis = Raumeinheit,
            kreis_type = Aggregat,
            unemployment_rate = Arbeitslosenquote, 
            employment_rate= Beschäftigtenquote, 
            net_household_income = Haushaltseinkommen, 
            gross_wage_and_salary= Bruttoverdienst, 
            employees_without_academic_degree = `Quote Beschäftigte ohne Berufsabschluss`, 
            employees_with_academic_degree = `Quote Beschäftigte mit Berufsabschluss`, 
            debtor_rate = Schuldnerquote, 
            school_leavers_with_qualification = `Schulabgänger mit Hochschulreife`, 
            school_leavers_without_qualification = `Schulabgänger ohne Abschluss`, 
            tax_revenues = Einkommensteuer, 
            doctor_per_inhabitant = `Ärzte je  Einwohner`, 
            male_long_unemployment_rate = `Männliche Langzeitarbeitslose`)
deprivation <- deprivation[-c(1),]

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
    select(-c(kreis, kreis_number, kreis_type)) %>% 
    mutate_all(as.numeric) %>% 
    describe(fast = TRUE) %>% as.matrix() %>% 
    subset(select = -c(se, n,vars, range))

full_summary <- rbind(suicide_summary,pop_2017)
rownames(full_summary) <- c("Number of Suicides 2017", "Total Population 2017")
#full_summary <- rbind(full_summary, deprivation_summary)   #This changes the values - Don't know how to fix now


##########Find out distribution for dataset    
#following this example: https://stats.stackexchange.com/questions/132652/how-to-determine-which-distribution-fits-my-data-best
library(fitdistrplus)
library(logspline)
hist(deprivation$unemployment_rate)
hist(deprivation$net_household_income)
hist(deprivation$gross_wage_and_salary)
hist(deprivation$employees_without_academic_degree, bins = 45)
hist(deprivation$employees_with_academic_degree)
hist(deprivation$debtor_rate)
hist(deprivation$school_leavers_with_qualification)
hist(deprivation$school_leavers_without_qualification)

descdist(deprivation$unemployment_rate, discrete = FALSE)
#normal, uniform 

fit.norm <- fitdist(deprivation$unemployment_rate, "norm")
plot(fit.norm)

descdist(deprivation$net_household_income, discrete = FALSE)




#### Simulate Data
# citizens_wide: Suicide mortality for each age-group 
# suicides_wide: Population per kreis and age-group
# deprivation: index per kreis 
