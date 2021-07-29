#Read in packages 
library(dplyr)
library(stringr)
rm(list = ls())

###############READ IN DATA AND CLEAN #################
#We have 3 datasets
#1. Deprivation Indices: GISD_NDS_Kreise_2017_final.csv in raw_data --> GISD_wide.csv in clean_data
#2. Citizens data (population of each kreis) Einwohner_NDS_Kreise_2017.csv in raw_data --> citizens_wide in clean_data
#3. Suicide numbers of each kreis: TODESURS78_NDS_Kreise_2017_final.csv --> clean_data/suicides_wide.csv
#and --> processed_wide including age-standardized suicide mortality rates 

####### 1. Deprivation index data: GISD #####
GISD <- readr::read_csv2("raw_data/GISD_NDS_Kreise_2017_final.csv") %>%
    as_tibble() 
    #mutate(kreis = str_replace(Raumeinheit, ", Stadt", "")) %>% #make new variable where Stadt is deleted 

#Clean & Standardize Indices
GISD <- GISD[-1,-3]

# rename indicators
varname <- c('X') # define a list of varying "varname"
n <- c(8) # there are 8 indicators in total
names(GISD)[3:ncol(GISD)] <- unlist(mapply(function(x,y) paste(x, seq(1,y), sep="_"), varname, n))
# X_1 = 'Arbeitslosigkeit', engl.: Unemployment
# X_2 = 'Beschäftigte am Wohnort mit akademischen Anschluss' (!= Beschäftigte am Wohnort mit Fach-(Hochschulabschluss)), engl.: Employees at place of residence with a technical (university) degree
# X_3 = 'Beschäftigtenquote'
# X_4 = 'Bruttoverdienst' (!= Bruttolohn und -gehalt), engl.: Gross wage and salary
# X_5 = 'Haushaltseinkommen' (!= Haushaltsnettoeinkommen), engl.: Net household income
# X_6 = 'Schulabgänger ohne Abschluss', engl.: School leavers without qualifications
# X_7 = 'Schuldnerquote', engl.: Debtor rate
# X_8 = 'Einkommenssteuer' (!= Steuereinnahmen), engl. Tax revenues
##### Z-Transformation of GISD data #####
# Z-Standardization fo all indicators; transformation of the variables of a distribution for better comparability of the different value ranges
# Z-score = (X - µ)/ sqrt(var)
# GISD <- scale(GISD.select[, X_1:X_8], center = TRUE, scale = TRUE)
# Z-standardization and saving as ZX_i
GISD$ZX_1 <- scale(GISD$X_1)
GISD$ZX_2 <- scale(GISD$X_2)
GISD$ZX_3 <- scale(GISD$X_3)
GISD$ZX_4 <- scale(GISD$X_4)
GISD$ZX_5 <- scale(GISD$X_5)
GISD$ZX_6 <- scale(GISD$X_6)
GISD$ZX_7 <- scale(GISD$X_7)
GISD$ZX_8 <- scale(GISD$X_8)
# Strip first characters of 'Kennziffer' in order to merge later
GISD$Kennziffer<-substring(GISD$Kennziffer, 3) 

GISD <- GISD %>% as_tibble(GISD) %>%
    select(Kennziffer, Kreis = Raumeinheit, ZX_1, ZX_2, ZX_3, ZX_4, ZX_5, ZX_6, ZX_7, ZX_8)

#Change signs of variables, 
# because logically, a high employment rate does not contribute to more deprivation,
# but a high debtor rate does. So we put all indicators on the same interpretation scale

# employees_residence_technical_university_degree = ZX_2
# employment_rate = ZX_3
# gross_wage_and_salary = ZX_4
# net_household_income = ZX_5
# tax_revenues = ZX_8

GISD <- GISD %>%  
    mutate(ZX_2 = ZX_2*(-1),
           ZX_3 = ZX_3*(-1), 
           ZX_4 = ZX_4*(-1), 
           ZX_5 = ZX_5*(-1),
           ZX_8 = ZX_8*(-1)) %>% 
    mutate(DI = rowSums(.[3:10])) %>% 
    arrange(desc(DI)) #Overall Deprivation Index per Kreis = DI

write.csv(GISD, "clean_data/GISD_wide.csv")


############### 2. Citizen data ###############
### 2nd: Citizen numbers on 'Kreis'-level

cdata <- readr::read_csv2("raw_data/Einwohner_NDS_Kreise_2017.csv")
cdata <- cdata[-c(5356:5361),]
ind_nas <- which(rowSums(is.na(cdata)) == 4)
cdata <- cdata[-ind_nas, ]
data <- data.frame()
for (j in 1:51) {
    
    indx <- 103 * (j-1) + 1
    
    transp <- t(cdata[indx:(indx+102), ])
    transp[,1] <- transp[1,1]
    transp <- transp[-1,]
    
    data <- rbind(data, transp)
}
columnnames <-  t(cdata[1:(1+102), ])[1,]
columnnames[1] <- "Kreis"
colnames(data) <- columnnames
colnames(data)
data$variable = str_remove_all( rownames(data), "[0-9]+")
sapply(data, class)
# Output: All values are saved as factors --> change them to numeric!!!
data <- as_tibble(data) %>%
    rename_all( ~ tolower(.) %>% str_remove_all(" ") %>% str_replace("-","_")) %>% 
    rename_at(vars(3:103), ~ paste0("age_", .)) %>%
    mutate_all(as.character) %>%
    mutate_at(vars(3:103), ~ na_if(. , "-") %>% as.numeric()) %>%
    mutate(kreisnummer = str_extract(kreis, "[0-9]+")) %>% ##########################
mutate(kreis = str_remove(kreis, "[0-9]+ +")) %>%
    mutate(age_under_20 = select(., age_0_1:age_19_20) %>% rowSums(na.rm = TRUE)) %>%
    mutate(age_20_30 = select(., age_20_21:age_29_30) %>% rowSums(na.rm = TRUE)) %>%
    mutate(age_30_40 = select(., age_30_31:age_39_40) %>% rowSums(na.rm = TRUE)) %>%
    mutate(age_40_50 = select(., age_40_41:age_49_50) %>% rowSums(na.rm = TRUE)) %>%
    mutate(age_50_60 = select(., age_50_51:age_59_60) %>% rowSums(na.rm = TRUE)) %>%
    mutate(age_60_70 = select(., age_60_61:age_69_70) %>% rowSums(na.rm = TRUE)) %>%
    mutate(age_70_80 = select(., age_70_71:age_79_80) %>% rowSums(na.rm = TRUE)) %>%
    mutate(age_over_80 = select(., age_80_81:age_100undälter) %>% rowSums(na.rm = TRUE)) %>%
    mutate(total = select(., c(age_under_20, age_20_30, age_30_40, age_40_50, age_50_60, age_60_70, age_70_80, age_over_80)) %>% rowSums(na.rm = TRUE)) %>%
    select(kreis, kreisnummer, total, age_under_20:age_over_80, variable)
data <- filter(data, variable == "Insgesamt")
# Removing row 'Niedersachsen' and the column 'variable'
data <- data[-1, -12]
# Removing rows 'Braunschweig', 'Hannover', 'Hannover,Landeshauptstadt', 'Lüneburg' (appears two times, with the same name), 'Weser-Ems'
# Removing rows manually/by row number due to the double entry of 'Lüneburg'
data <- data[-c(1, 12, 14, 21, 33), ]
# Substitute entries 'NA' by value 0
data[is.na(data)] = 0
data$total <- as.numeric(data$total)
citizens <- data
write.csv(citizens, "clean_data/citizens_wide.csv", row.names = F)


############### 3. suicide rates per kreis ##################
#suicide rates with age groups data
#make variable kreis: line 1: 12 -> take 

### 3rd: Suicide numbers on ‘Kreis’-Level
#rm(list = ls())
d <- readr::read_csv2("raw_data/TODESURS78_NDS_Kreise_2017_final.csv", na = "-")
column_names <- pull( d[2:12, 1])
nkreise = nrow(d)/12
data <- data.frame()
for(i in 1:nkreise){
    ind = 12 * (i-1)  + 1
    transposed = t(d[ind:(ind+11),])
    transposed[,1] = transposed[1,1]
    transposed = transposed[-1,]
    data = rbind(data, transposed)
}
colnames(data) <- c("Kreis", column_names)
data$variable = str_remove_all( rownames(data), "[0-9]+")
data <- as_tibble(data)  %>%
    rename_all( ~ tolower(.) %>% str_remove_all(" ") %>% str_replace("-","_")) %>%
    mutate_at(vars(3:12), as.numeric) %>%
    rename_at(vars(4:12), ~ paste0("age_", .)) %>%
    mutate(kreisnummer = str_extract(kreis, "[0-9]+")) %>%
    mutate(kreis = str_remove(kreis, "[0-9]+ +")) %>%
    mutate(age_under_20 = u.1jahr + age_1_15 + age_15_20) %>%
    mutate(age_over_80 = age_80u.ä.) %>%
    mutate(total = select(., c(age_under_20, age_20_30, age_30_40, age_40_50, age_50_60, age_60_70, age_70_80, age_over_80)) %>% rowSums(na.rm = TRUE)) %>%
    select(kreis, kreisnummer, total, age_under_20, age_20_30, age_30_40, age_40_50, age_50_60, age_60_70, age_70_80, age_over_80, variable)
data <- filter(data, variable == "Insgesamt")
# Removing row ‘Niedersachsen’ and the column ‘variable’
data <- data[-1, -12]
# Removing rows ‘Braunschweig’, ‘Hannover’, ‘Hannover,Landeshauptstadt’, ‘Lüneburg’ (appears two times, with the same name), ‘Weser-Ems’
# Removing rows manually/by row number due to the double entry of ‘Lüneburg’
data <- data[-c(1, 12, 14, 21, 33), ]
# data

suicides <- data
# suicides
write.csv(suicides, "clean_data/suicides_wide.csv", row.names = F)




##### Calculation of age-standardized suicide mortality rate (SMR) #####
# The following is calculated for each 'Kreis' - Why? In order to also take into account the age-distribution of a population
# 1st step: age-specific suicide rate (age specific SR)
# age-specific SR =  (age-specific no. of suicides / age-specific no. of population) x 100.000 (inhabitants)

asSR <- citizens
i <- 4
while(i <= 11) {
    asSR[,i] <- (suicides[,i] / citizens[,i]) * 100000
    i <- i + 1
}
write.csv(asSR, "clean_data/asSR_wide.csv")
# 2nd step: Calculation of age-specific proportions of population
# age-specific proportion of population = age-specific no. of population / total no. of population
asProp <- citizens
i <- 4
while(i <= 11) {
    asProp[,i] <- (citizens[,i] / citizens[,3])
    i <- i + 1
}
write.csv(asProp, "clean_data/asSR_wide.csv")

# 3rd step: age-standardized suicide mortality rate (SMR)
# age-standardized SMR = sum(age-specific SR's x age-specific proportion of population)
asSMR <- citizens
i <- 4
while(i <= 11) {
    asSMR[,i] <- (asSR[,i] * asProp[,i])
    i <- i + 1
}

asSMR <- as_tibble(asSMR) %>%
    mutate(total = select(., c(age_under_20, age_20_30, age_30_40, age_40_50, age_50_60, age_60_70, age_70_80, age_over_80)) %>% 
               rowSums(na.rm = TRUE)) %>% 
    select(Kennziffer = kreisnummer, SMR = total) %>% 
    arrange(desc(SMR))


write.csv(asSMR, "clean_data/asSMR_wide.csv")

##### Merging processed GISD and SMR #####
Processed <-
    left_join(GISD, asSMR, by="Kennziffer") %>% 
    arrange(desc(SMR))

write.csv(Processed, "clean_data/Processed_wide.csv")
