#################################################################################
# GRSC Data Cleaning Script
# 1. Gets all state data in similar format
# 2. Produces csv of data to be used for mapping 
#   and data to be used for Length Frequency Distributions (LFD)
# L. Denson April 2025
#
################################################################################

# Load packages ####
library(readxl)
library(tidyverse)

# set working directory ####
setwd("C:\\Users\\latreese.denson\\Desktop\\SEDAR_98_Models\\DataWorkshop\\GRSC_LFD_Exploration")

# read in raw data sets ####
# one workbook from FL, AL, MS and TX, one workbook from LA
sites <- read_excel("Red Snapper length summary_Gulf of Mexico_LDEdit.xlsx", sheet = "Site Coordinants_FL")
FL_Data <- read_excel("Red Snapper length summary_Gulf of Mexico_LDEdit.xlsx", sheet = "Data_Region_FL")
AL_MS_TX_Data <- read_excel("Red Snapper length summary_Gulf of Mexico_LDEdit.xlsx", sheet = "Data_Regions_AL_MS_TX")
LA_Data <-read_excel("LGL_RedSnapper_Louisiana.xlsx", sheet = "Length_Age")

# conversions MTL to FL in cm ###
alpha = 0.138
beta = 0.926

# Data Clean Up by State and Stock ID ####
# Florida - East ####
sites_east1 = sites%>%dplyr::filter(Longitude >= -85,Latitude <= 29)    # separating points into SID regions by box
sites_east2 = sites%>%dplyr::filter(Longitude >= -85, Latitude<28)      # separating points into SID regions by box
sites_east = merge(sites_east1,sites_east2,all=TRUE)                    # put points back together
east_dat = FL_Data[FL_Data$`Reef_Name`%in%sites_east$`Reef_Name`,]      # only take data where the reef names are in the list of sites in the SID region
east_dat = merge(east_dat,sites_east,all.x = TRUE)                      # need the additional information 

FL_East_clean = east_dat%>%dplyr::filter(!is.na(east_dat$`Length_count`))%>% # remove records where fish are not measured
  separate("Sample Date", into = c("year", "month", "day"), sep = "-")%>% # change date format
  mutate(Region = "Florida")%>%mutate(HabitatType = if_else(str_detect(`Habitat_Type`, "AR"), "AR", "NR"))%>%
  mutate(DepthZone="Unknown")%>%rename(RS_measured = `Length_count`)%>% # add columns and simplify info to match other datasets below
  select(-Habitat_Type)                                                 # remove excess columns

# Wide version of data for Mapping
FL_East_Map = FL_East_clean%>%
  select(year,Latitude,Longitude,Region,HabitatType,DepthZone,RS_measured)%>%
  mutate(SID = "East",Reg_SID = "FL_E", Gear = "ROV")

# Long version of data for LFD
FL_East_LFD =FL_East_clean%>%
  pivot_longer(cols = matches("^L[0-9]+$"),names_to = "L_bin",names_prefix = "L", # pivot longer is needed for ggplot histogram
  values_to = "TL_mm",values_drop_na = TRUE)%>%
  select(year,Region,HabitatType,DepthZone,L_bin,TL_mm)%>%              # limiting the number of columns we see
  mutate(SID = "East",Reg_SID = "FL_E", Gear = "ROV")%>%                # add columns for comparison with other data sources
  mutate(FL_cm=alpha+beta*(round(TL_mm)/10))%>%
  select(-TL_mm, -L_bin)


# Florida - Central ####
sites_central1 = sites%>%dplyr::filter(Longitude >= -85,Latitude >= 29)         # separate central SID sites 
sites_central2 = sites%>%dplyr::filter(Longitude < -85, Latitude>28)            # separate central SID sites
sites_central = merge(sites_central1,sites_central2,all=TRUE)                   # same process as Florida - east 
central_dat = FL_Data[FL_Data$`Reef_Name` %in%sites_central$`Reef_Name`,]
central_dat = merge(central_dat,sites_central,all.x = TRUE)
central_dat_clean = central_dat%>%dplyr::filter(!is.na(central_dat$`Length_count`))%>%
  separate("Sample Date", into = c("year", "month", "day"), sep = "-")%>%
  mutate(Region = "Florida")

FL_Central_clean = central_dat_clean%>%
  mutate(HabitatType = if_else(str_detect(`Habitat_Type`, "AR"), "AR", "NR"))%>%
  mutate(DepthZone="Unknown", Region="Florida")%>%
  rename(RS_measured = `Length_count`)%>%select(-Habitat_Type)

# Wide version of data for Mapping
FL_Central_Map = FL_Central_clean%>%
  select(year,Latitude,Longitude,Region,HabitatType,DepthZone,RS_measured)%>%
  mutate(SID = "Central",Reg_SID = "FL_C", Gear = "ROV")

# Long version of data for LFD
FL_Central_LFD =FL_Central_clean%>%
  pivot_longer(cols = matches("^L[0-9]+$"),names_to = "L_bin",names_prefix = "L", # pivot longer is needed for ggplot histogram
               values_to = "TL_mm",values_drop_na = TRUE)%>%
  select(year,Region,HabitatType,DepthZone,L_bin,TL_mm)%>%              # limiting the number of columns we see
  mutate(SID = "Central",Reg_SID = "FL_C", Gear = "ROV")%>%                # add columns for comparison with other data sources
  mutate(FL_cm=alpha+beta*(round(TL_mm)/10))%>%
  select(-TL_mm,-L_bin)


# Alabama - Central ####
AL_Sites = AL_MS_TX_Data %>%filter(Region=="Alabama",
                                   YearDeployment>=2018, 
                                   YearDeployment<=2019,
                                   !is.na(FL_mm))%>%                        # remove NA and data outside of GRSC study years
  select(Region,Latitude,Longitude)%>%count(Region,Latitude,Longitude)%>%rename(RS_measured = n)

AL_descript =AL_MS_TX_Data%>%filter(Region=="Alabama",YearDeployment>=2018, YearDeployment<=2019,!is.na(FL_mm))%>%
  select(YearDeployment,Latitude,Longitude, DepthZone, HabitatType, Method)%>%distinct()%>%
  mutate(HabitatType = if_else(str_detect(`HabitatType`, "Artificial"), "AR", 
                               if_else(str_detect(`HabitatType`, "Natural"), "NR", 
                                       if_else(str_detect(`HabitatType`, "Unconsolidated"),"UCB",
                                               if_else(str_detect(`HabitatType`, "Unidentified"),"UknS","NS")))))

AL_sites_clean = AL_Sites%>%
  inner_join(AL_descript, by = c("Latitude", "Longitude"))                  # bring together descriptive data of sites

AL_Central_clean =  AL_MS_TX_Data %>%filter(Region=="Alabama",YearDeployment>=2018, YearDeployment<=2019,!is.na(FL_mm))%>%
  select(YearDeployment,MonthDeployment,DayDeployment,Latitude,Longitude, DepthZone, HabitatType, Method,FL_mm)%>%
  mutate(HabitatType = if_else(str_detect(`HabitatType`, "Artificial"), "AR", 
                               if_else(str_detect(`HabitatType`, "Natural"), "NR", 
                                       if_else(str_detect(`HabitatType`, "Unconsolidated"),"UCB",
                                               if_else(str_detect(`HabitatType`, "Unidentified"),"UknS","NS")))))

# Wide version of data for Mapping
AL_Central_Map = AL_sites_clean %>%
  mutate(SID = "Central",Reg_SID = "AL_C")%>%
  rename("Gear"="Method","year"="YearDeployment")%>%
  select(year,Latitude,Longitude,Region,HabitatType,DepthZone,RS_measured,SID,Reg_SID,Gear)
 
  
# Long version of data for LFD
AL_Central_LFD = AL_Central_clean%>%select(YearDeployment,HabitatType,DepthZone,Method,FL_mm)%>%
  mutate(Region ="Alabama", SID ="Central",Reg_SID ="AL_C")%>%
  mutate(FL_cm = FL_mm/10)%>%
  rename("Gear"="Method","year"="YearDeployment")%>%
  select(-FL_mm)
  
#Mississippi - Central ####
MS_Sites = AL_MS_TX_Data %>%filter(Region=="Mississippi",
                                   YearDeployment>=2018, 
                                   YearDeployment<=2019,
                                   !is.na(FL_mm))%>%
  select(Region,Latitude,Longitude)%>%
  count(Region,Latitude,Longitude)%>%
  rename(RS_measured = n)

MS_descript =AL_MS_TX_Data%>%filter(Region=="Mississippi",
                                    YearDeployment>=2018, 
                                    YearDeployment<=2019,
                                    !is.na(FL_mm))%>%
  select(YearDeployment,Latitude,Longitude, DepthZone, HabitatType, Method)%>%
  distinct()%>%
  mutate(HabitatType = if_else(str_detect(`HabitatType`, "Artificial"), "AR", if_else(str_detect(`HabitatType`, "Natural"), "NR", if_else(str_detect(`HabitatType`, "Unconsolidated"),"UCB",if_else(str_detect(`HabitatType`, "Unidentified"),"UknS","NS")))))

MS_sites_clean = MS_Sites%>%
  inner_join(MS_descript, by = c("Latitude", "Longitude"))

MS_Central_clean =  AL_MS_TX_Data %>%filter(Region=="Mississippi",
                                            YearDeployment>=2018, 
                                            YearDeployment<=2019,
                                            !is.na(FL_mm))%>%
  select(YearDeployment,MonthDeployment,DayDeployment,
         Latitude,Longitude, DepthZone, 
         HabitatType, Method,FL_mm)%>%
  mutate(HabitatType = if_else(str_detect(`HabitatType`, "Artificial"), "AR", 
                               if_else(str_detect(`HabitatType`, "Natural"), "NR", 
                                       if_else(str_detect(`HabitatType`, "Unconsolidated"),"UCB",
                                               if_else(str_detect(`HabitatType`, "Unidentified"),"UknS","NS")))))

# Wide version of data for Mapping
MS_Central_Map = MS_sites_clean %>%
  mutate(SID = "Central",Reg_SID = "MS_C")%>%
  rename("Gear"="Method","year"="YearDeployment")%>%
  select(year,Latitude,Longitude,Region,HabitatType,DepthZone,RS_measured,SID,Reg_SID,Gear)

# Long version of data for LFD
MS_Central_LFD = MS_Central_clean%>%select(YearDeployment,HabitatType,
                                           DepthZone,Method,FL_mm)%>%
  mutate(Region ="Mississippi", SID ="Central",Reg_SID ="MS_C")%>%
  mutate(FL_cm = FL_mm/10)%>%
  rename("Gear"="Method","year"="YearDeployment")%>%
  select(-FL_mm)

# Louisiana - West ####
# how many fish measured at each site
LA_sites = LA_Data%>%count(siteNo,Lat, Lon)%>%rename(RS_measured = n)%>%
  mutate(Region = "Louisiana")

LA_descript =LA_Data%>%
  select(Year,Lat,Lon, depthZone, siteType)%>%distinct()%>%mutate(Method = "HL")%>%
  mutate(HabitatType = if_else(str_detect(siteType, "Artificial"), "AR", 
                               if_else(str_detect(siteType, "Natural"), "NR", 
                                       if_else(str_detect(siteType, "UCB"),"UCB",
                                               if_else(str_detect(siteType, "Platform"),"PF","PC")))))

LA_sites_clean = LA_sites%>%
  inner_join(LA_descript, by = c("Lat", "Lon"))%>%select(-siteType)

LA_Central_clean =  LA_Data %>%select(Year,Lat,Lon, depthZone, siteType,fork_length_mm)%>%
  mutate(Gear = "HL",Region="Lousiana")%>%
  mutate(HabitatType = if_else(str_detect(siteType, "Artificial"), "AR", 
                               if_else(str_detect(siteType, "Natural"), "NR", 
                                       if_else(str_detect(siteType, "UCB"),"UCB",
                                               if_else(str_detect(siteType, "Platform"),"PF","PC")))))%>%
  select(-siteType)

# Wide version of data for Mapping
LA_West_Map = LA_sites_clean %>%
  mutate(SID = "West",Reg_SID = "LA_W",Gear="HL")%>%
  rename("year"="Year", "Longitude" ="Lon","Latitude"="Lat","DepthZone"="depthZone")%>%
  select(year,Latitude,Longitude,Region,HabitatType,
         DepthZone,RS_measured,SID,Reg_SID,Gear)
  
# Long version of data for LFD
LA_West_LFD = LA_Central_clean%>%
  mutate(SID ="West",Reg_SID ="LA_W")%>%
  mutate(FL_cm = fork_length_mm/10)%>%
  rename("year"="Year","DepthZone"="depthZone")%>%
  select(year,HabitatType,DepthZone,Gear,Region,SID,Reg_SID,FL_cm)
  

# Texas - West ####
TX_Sites = AL_MS_TX_Data %>%filter(Region=="Texas",YearDeployment>=2018,
                                   YearDeployment<=2019,!is.na(FL_mm),
                                   DataSource=="TAMUCC")%>%
  select(Region,Latitude,Longitude)%>%
  count(Region,Latitude,Longitude)%>%rename(RS_measured = n)

TX_descript =AL_MS_TX_Data%>%filter(Region=="Texas",YearDeployment>=2018, 
                                    YearDeployment<=2019,!is.na(FL_mm),
                                    DataSource=="TAMUCC")%>%
  select(YearDeployment,Latitude,Longitude, DepthZone, HabitatType, Method)%>%
  distinct()%>%
  mutate(HabitatType = if_else(str_detect(`HabitatType`, "Artificial"), "AR","NR" ))

TX_sites_clean = TX_Sites%>%
  inner_join(TX_descript, by = c("Latitude", "Longitude"))

TX_West_clean =  AL_MS_TX_Data %>%filter(Region=="Texas",
                                         YearDeployment>=2018, 
                                         YearDeployment<=2019,
                                         !is.na(FL_mm),
                                         DataSource=="TAMUCC")%>%
  select(YearDeployment,MonthDeployment,DayDeployment,
         Latitude,Longitude, DepthZone, HabitatType, Method,FL_mm)%>%
  mutate(HabitatType = if_else(str_detect(`HabitatType`, "Artificial"), "AR","NR" ))

# Wide version of data for Mapping
TX_West_Map = TX_sites_clean %>%
  mutate(SID = "West",Reg_SID = "TX_W")%>%
  rename("Gear"="Method","year"="YearDeployment")%>%
  select(year,Latitude,Longitude,Region,HabitatType,DepthZone,RS_measured,SID,Reg_SID,Gear)

# Long version of data for LFD
TX_West_LFD = TX_West_clean%>%select(YearDeployment,HabitatType,DepthZone,Method,FL_mm)%>%
  mutate(Region ="Texas", SID ="West",Reg_SID ="TX_W")%>%
  mutate(FL_cm = FL_mm/10)%>%
  rename("Gear"="Method","year"="YearDeployment")%>%
  select(-FL_mm)

# Combine data for mapping and tables ####
GRSC_Site_Dat = rbind(FL_Central_Map,FL_East_Map,AL_Central_Map,
                      MS_Central_Map,LA_West_Map,TX_West_Map)
write.csv(GRSC_Site_Dat,"GRSC_Data_for_Mapping.csv",row.names = FALSE)

# combine data for LFD ####
GRSC_LFD_Dat = rbind(FL_Central_LFD,FL_East_LFD,AL_Central_LFD,
                     MS_Central_LFD,LA_West_LFD,TX_West_LFD)
write.csv(GRSC_LFD_Dat,"GRSC_Data_for_LFD.csv",row.names = FALSE)
