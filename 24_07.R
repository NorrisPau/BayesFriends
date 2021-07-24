#Niklas 24/07/2021

library(dplyr)

### 3rd: Suicide numbers on ‘Kreis’-Level
 rm(list = ls())
d <- readr::read_csv2("raw_data/TODESURS78_NDS_Kreise_2017_12_05.csv", na = "-")
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