
library(tidyverse)

#Read the data into R.
Entrep_data <- read.csv("C:/Users/David/Documents/STA 660/Entrepreneurship Study/EData.csv")

#Convert NAs in firm age to zeroes for removal
Entrep_data$firm_age[is.na(Entrep_data$firm_age)] <- 0

#Combine the three country columns into one and then remove them.
Entrep_data <- Entrep_data %>%
  #Mod.V.Plan: 0=Bizmod, 1=Bizplan
  #Aftercare: 0=No, 1=Yes
  mutate("Country"=Country.2+2*Country.3,
         "Mod.v.Plan"=bizplan+bizplanafter, 
         "Aftercare"=bizmodafter+bizplanafter) %>%
  filter(!ent_age==0) %>%
  filter(!basesales...disguised==0) %>%
  filter(!firm_age==0) %>%
  select(c(1:10,15:18,22:36)) 

#This variable should have been numeric, but it was in somme improper form. Some other variables 
#might need to be changed in this way, but I'm not entirely sure. Firm age, in particular, is the
#one I'm thinking of.
Entrep_data$month.since.Jan.2013..i.e..month.01.is.Jan.2013..02.is.Feb.2013..etc.. <- as.numeric(Entrep_data$month.since.Jan.2013..i.e..month.01.is.Jan.2013..02.is.Feb.2013..etc..)



Entrep_data$data_ent <- NULL #all 1
Entrep_data$orig_ent <- NULL #all 0
Entrep_data$urban <- NULL #all 1
Entrep_data$cycle <- NULL #all 2

for(u in c(2:18,20:27)){
Entrep_data[u] <- as.numeric(Entrep_data[u])
}

min(Entrep_data$firm_age) #-1 isn't a valid age, what does this mean?

#different number of observations per firm
ff <- Entrep_data$firmid
ff <- sort(ff)
firmid_rep <- c()
tt <- 0
ss <- 0
for(gg in 1:length(ff)){
  tt <- ff[gg]
  if(tt==ss)
    firmid_rep[length(firmid_rep)] <- firmid_rep[length(firmid_rep)]+1
  else firmid_rep<-c(firmid_rep,1)
  ss <- tt
}



save(Entrep_data,file="Entrep_data.Rdata")
str(Entrep_data)