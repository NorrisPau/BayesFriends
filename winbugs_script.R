############WINBUGS##############

#Reading in Niklas cleaned datasets
gisd_wide <- readr::read_csv("clean_data/Data_Niklas/GISD_wide.csv")
processed_wide <- readr::read_csv("clean_data/Data_Niklas/Processed_wide.csv")
landesgrenzen <- read.shape("raw_data/NDS_Landesgrenze.shp")

###Prepare Data for WINBUGS ##########
#Merge smr with deprivation indexes 
smr_data <- processed_wide %>%  
    select(kreisnummer, kreis, SMR) 

depr_small <- gisd_wide %>%  
    select(kreisnummer = Kennziffer, kreis = Raumeinheit, ZX_1:ZX_8) %>% 
    mutate(kreisnummer = str_remove(kreisnummer, "03")) %>% 
    mutate(kreisnummer = as.numeric(kreisnummer))

depr_smr <- inner_join(depr_small, smr_data, by = "kreisnummer") %>% 
    select(-kreis.y) %>% 
    rename(kreis = kreis.x)



#Make dataset to list
#depr_smr_list <- as.list(depr_smr)

#make list to txt file
#capture.output(depr_smr_list, file = "depr_smr_list.txt")
#smr <- depr_smr_list[[11]] #extract smr from list


#Start with first X (which is unemployment): 
#X1 <- depr_smr_list[[3]]
#N <- length(smr)



#Run WINBUGS
library(R2WinBUGS)


#set priors for parameters 
list( mu=c(0,0), prec=c(1,1))

#There is still an error in this code 
for (i in 1:N) {
    smr[i] ~ dnorm(mu[i], tau)
    mu[i] <- alpha + beta * X1[i] #+ u[i]
}
u[1:m] ~ car.normal(adj[], weights[], num[], tau.u)
for (k in 1:sumNumNeigh) {
    weights[k] <- 1
}
alpha ~ dflat()
tau.u ~ dgamma(0.05, 0.005)
tau ~ dgamma(0.05, 0.005)
sigma <- sqrt(1/tau)
beta ~ dnorm(0.00000E+00, 1.00000E-05)


#(where: smr = age-standardized suicide mortality rate,
#   X= each of eight indicators that compose of the deprivation index)
