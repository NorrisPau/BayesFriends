#Read in packages 
library(dplyr)
library(stringr)
rm(list = ls())

###############READ IN DATA AND CLEAN #################

#1. deprivation index data
deprivation <- readr::read_csv2("raw_data/deprivation.csv") %>%
    as_tibble() 
    #mutate(kreis = str_replace(Raumeinheit, ", Stadt", "")) %>% #make new variable where Stadt is deleted 

#2. citizen data
cdata <- readr::read_csv2("raw_data/Einwohner_NDS_Kreise_2017.csv")
cdata <- cdata[-c(5356:5361),]
ind_nas <- which(rowSums(is.na(cdata)) == 4)
cdata <- cdata[-ind_nas, ]



data <- data.frame()
for (j in 1:51) {
    
    indx <- 103 * (j-1) + 1
    
    #print(indx:(indx+102))
    
    transp <- t(cdata[indx:(indx+102), ])
    transp[,1] <- transp[1,1]
    transp <- transp[-1,]
    
    data <- rbind(data, transp)
    
}

columnnames <-  t(cdata[1:(1+102), ])[1,]
columnnames[1] <- "Kreis"
colnames(data) <- columnnames
data$variable = str_remove_all( rownames(data), "[0-9]+")

data <- as_tibble(data)
citizens <- data %>% 
    rename_all( ~ tolower(.) %>% str_remove_all(" ") %>% str_replace("-","_")) %>% 
    rename_at(vars(3:103), ~ paste0("age_", .)) %>% 
    mutate_at(vars(3:103), ~ na_if(. , "-") %>% as.numeric()) %>% 
    mutate(kreisnummer = str_extract(kreis, "[0-9]+"), 
           kreis = str_remove(kreis, "[0-9]+ +")) %>% 
    select(kreis, kreisnummer, variable, everything())

write.csv(citizens, "clean_data/citizens_wide.csv", row.names = F)


#3. suicide rates per kreis    
#suicide rates with age groups data
#make variable kreis: 
#line 1: 12 -> take 

rm(list = ls())

d <- readr::read_csv2("raw_data/TODESURS78_NDS_Kreise_2017.csv", na = "-")
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
data <- as_tibble(data)
suicide <- data %>% 
    rename_all( ~ tolower(.) %>% str_remove_all(" ") %>% str_replace("-","_")) %>% 
    mutate_at(vars(3:12), as.numeric) %>% 
    rename_at(vars(4:12), ~ paste0("age_", .)) %>% 
    mutate(kreisnummer = str_extract(kreis, "[0-9]+"), 
           kreis = str_remove(kreis, "[0-9]+ +"), 
           age_under_20 = u.1jahr + age_1_15 + age_15_20) %>% 
    rename(age_80_other = age_80u.ä.) %>%  
    select(kreis, kreisnummer, total = insgesamt, age_under_20, age_20_30:age_80_other, variable)

summary(suicide$age_under_20)
class(suicide$age_under_20)

write.csv(suicide, "clean_data/suicides_wide.csv", row.names = F)

# 1 zeile in 3er paket:  insgesamt pro altergruppe
# 2. zeile in 3er paket: männlich pro altergruppe
# 3. zeile in 3er paket: weiblich pro altergruppe

