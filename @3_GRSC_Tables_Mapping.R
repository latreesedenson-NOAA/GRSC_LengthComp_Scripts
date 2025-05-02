#################################################################################
# GRSC Data LFD creation Script
# 1. Reads in  GRSC_Data_for_Mapping.csv created by @1_GRSC_DataCleaning.R
# 2. Produces tables of total measured by state, gear, year and habitat type
# 3. Produces map of measured coverage  
# L. Denson April 2025
#
################################################################################

# Load packages ####
library(readxl)
library(tidyverse)
library(maps)
library(ggplot2)
library(sf)
library(rnaturalearth)

# set working directory ####
setwd("C:\\Users\\latreese.denson\\Desktop\\SEDAR_98_Models\\DataWorkshop\\GRSC_LFD_Exploration")

# read in clean data ####
GRSC_Map_Dat <- read.csv("GRSC_Data_for_Mapping.csv")
GRSC_Table <-read.csv("@GRSC_NumberTable.csv")

# Tables of measured fish by SID ####
SID_list=unique(GRSC_Map_Dat$SID)
for(i in 1:length(SID_list)){
Data_summary = GRSC_Map_Dat%>%
  filter(SID == SID_list[i])%>%
  group_by(Reg_SID,year,Gear,HabitatType) %>%             
  summarize(RS_meas_total = sum(RS_measured)) 
write.csv(Data_summary,paste0("GRSC_",SID_list[i],"_Data_Summary.csv"))
} # end loop

# Table Code for proportions ####
Data_summary2 = GRSC_Map_Dat%>%
  group_by(Region,HabitatType) %>%             
  summarize(RS_meas_total = sum(RS_measured))%>%group_by(Region,HabitatType)%>%summarise_all(sum)%>%
  pivot_wider(names_from = HabitatType,values_from = RS_meas_total)%>%
  rowwise() %>% 
  mutate(Total = sum(c_across(where(is.numeric)), na.rm = T))%>%
  mutate(LCper_NR = (NR/Total)*100, LCper_AR = (AR/Total)*100, LCper_UCB = (UCB/Total)*100)
write.csv(Data_summary2,"LengthComp_HabitatProportions.csv",row.names = FALSE)

GRSC_tab = GRSC_Table%>%pivot_wider(names_from = HabitatType,values_from = Number)%>%
  mutate(RSCper_NR = (NR/Total)*100, RSCper_AR = (AR/Total)*100, RSCper_UCB = (UCB/Total)*100)
write.csv(GRSC_tab,"RSC_HabitatProportions.csv",row.names = FALSE)

# Compare length comp proportions to abundance proportions ####
LCdata = Data_summary2%>%select(Region,LCper_NR,LCper_AR,LCper_UCB)
RSCdata = GRSC_tab%>%select(Region,RSCper_NR,RSCper_AR,RSCper_UCB)
PerDat = merge(LCdata,RSCdata)
write.csv(PerDat,"LengthCompvsRSC_HabitatProportions.csv",row.names = FALSE)

# Set up maps ####
my_sf <- st_read(file.path(getwd(),"GOMShape", "GSHHS_f_GOM.shp"))

worldmap <- ne_countries(scale = 'medium', type = 'map_units',
                         returnclass = 'sf')

GULF = st_crop(worldmap, xmin = -98, xmax = -81,
               ymin = 24, ymax = 31)

GULF_C = st_crop(worldmap, xmin = -90, xmax = -84,
                 ymin = 28, ymax = 31)

E_GULF = st_crop(worldmap, xmin = -90, xmax = -80,
                 ymin = 24, ymax = 31)

W_GULF = st_crop(worldmap, xmin = -98, xmax = -90,
                 ymin = 24, ymax = 31)

# kncoast <- data.frame(orig_lat=12.7571, 
#                       orig_lon=74.87461,
#                       dest_lat=15,
#                       dest_lon=74.87461) 
# 
# kncoast_bbox <- kncoast %>% st_as_sf(coords = c("lon", "lat"), crs=4326) %>%
#   st_bbox() %>% st_as_sfc()


# Entire Gulf habitat type
ggplot() + geom_sf(data = GULF) + theme_bw()+
  geom_point(data=GRSC_Map_Dat, aes(x=Longitude, y=Latitude,col = HabitatType))
ggsave("Gulf_HabType.jpg", plot = last_plot(), device = NULL, path = NULL, 
       width = 7, height = 5, units = "in", dpi = 300)

# Entire Gulf by year and habitat type
ggplot() + geom_sf(data = GULF) + theme_bw()+facet_grid(~year)+
  geom_point(data=GRSC_Map_Dat, aes(x=Longitude, y=Latitude,col = HabitatType))
ggsave("Gulf_Year_HabType.jpg", plot = last_plot(), device = NULL, path = NULL, 
       width = 7, height = 5, units = "in", dpi = 300)

# Entire Gulf by Year, gear and habitat type
ggplot() + geom_sf(data = GULF) + theme_bw()+facet_grid(Gear~year)+
  geom_point(data=GRSC_Map_Dat, aes(x=Longitude, y=Latitude,col = HabitatType))
ggsave("Gulf_Year_Gear_HabType.jpg", plot = last_plot(), device = NULL, path = NULL, 
       width = 7, height = 5, units = "in", dpi = 300)

# East
E_GRSC_Map_Dat = GRSC_Map_Dat%>%filter(SID=="East")
ggplot() + geom_sf(data = E_GULF) + theme_bw()+facet_grid(Gear~year)+
  geom_point(data=E_GRSC_Map_Dat, aes(x=Longitude, y=Latitude,col = HabitatType))
ggsave("East_Year_Gear_HabType.jpg", plot = last_plot(), device = NULL, path = NULL, 
       width = 7, height = 5, units = "in", dpi = 300)

# Cental
C_GRSC_Map_Dat = GRSC_Map_Dat%>%filter(SID=="Central")
ggplot() + geom_sf(data = GULF_C) + theme_bw()+facet_grid(Gear~year)+
  geom_point(data=C_GRSC_Map_Dat, aes(x=Longitude, y=Latitude,col = HabitatType))
ggsave("Central_Year_Gear_HabType.jpg", plot = last_plot(), device = NULL, path = NULL, 
       width = 7, height = 5, units = "in", dpi = 300)

# West
W_GRSC_Map_Dat = GRSC_Map_Dat%>%filter(SID=="West")
ggplot() + geom_sf(data = W_GULF) + theme_bw()+facet_grid(Gear~year)+
  geom_point(data=W_GRSC_Map_Dat, aes(x=Longitude, y=Latitude,col = HabitatType))
ggsave("West_Year_Gear_HabType.jpg", plot = last_plot(), device = NULL, path = NULL, 
       width = 7, height = 5, units = "in", dpi = 300)




