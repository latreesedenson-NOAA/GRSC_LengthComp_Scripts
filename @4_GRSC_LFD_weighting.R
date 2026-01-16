#################################################################################
# GRSC Data Weighting Script
# Script uses methods determined from internal presentation on 5/6/25
# East Solution:
#   Combine years and use as length comp for 2018 or just drop 2019.
#   No weighting needed because data are from the same gear and habitat. 
#
# West Solution: 
#   Combine all comp across years and states. Use West SID index proportions by 
#   habitat to weight the combined comp. 
#   Makes a lot of assumptions but realistically all that can be accomplished with available data.
#   June 2025  Notified that TX only used ROV data for abundance not VLL, all TX length data is now removed

# Central Solution: 
#   Scale the ROV and the VLL line gear to have equal weight and then reweight the
#   combined data (ROV, BLL, VLL) using the habitat proportion data from the Central SID 
# 
# L. Denson April 2025
# updated Janurary 2026
################################################################################
library(tidyverse)

# Set up data frame ####
min_size=10
len_plus=115
bin_size =5
min_year = 2018 
max_year = 2018
bins <- seq(min_size,len_plus,bin_size)

full_set <- crossing(YEAR = seq(from = min_year, to = max_year, by = 1),
                     lbin = seq(from = min_size, to = len_plus, by = bin_size), 
                     N = 0) %>%
  pivot_wider(id_cols = YEAR, names_from=lbin, values_from = N)

comp_names = c("YEAR","ln_fish", names(full_set)[-1])

# Read in data ####
LFD_data = read.csv("GRSC_Data_for_LFD.csv")
RSC_Proportions =read.csv("@GRSC_NumberTable.csv") 
Alt_RSC_Proportions =read.csv("@GRSC_NumberTable_UCBNRCombo.csv") 

# East - no weighting needed ####
East_lfd_data = LFD_data%>%filter(SID=="East")
counts = table(factor(cut(East_lfd_data$FL_cm, bins)))
East_lfd = as.data.frame(counts/sum(counts))%>%pivot_wider(names_from = Var1,values_from = Freq)

east_n <- LFD_data%>%
  filter(SID == "East") %>% select(-year)%>%
  summarise(ln_fish = n())

east_lengths = LFD_data%>%
  filter(SID == "East", HabitatType!="PC") %>% select(-year)%>%
  mutate(lbin = floor(FL_cm / bin_size)*bin_size)%>%
  mutate(ln_fish = n()) %>%
  group_by(lbin)%>% # by length bin in a certain habitat type
 reframe(freq = n() / unique(ln_fish))%>% # should be all length bins for that habitat type
  mutate(ln_fish=as.numeric(east_n),YEAR = 2018)

east_lfd_wide = east_lengths%>%pivot_wider(id_cols = c(YEAR,ln_fish),
                                           names_from = lbin, 
                                           values_from = freq, 
                                           values_fill = list(freq = 0)) %>%
  left_join(full_set) %>%
  select(all_of(comp_names))%>%
  replace(is.na(.), 0) %>%
  mutate(SID = "East", Type = "Nominal")


#View(east_lfd_wide)

# West ####
# West Nominal ####
west_n <- LFD_data%>%
  filter(SID == "West", HabitatType!="PC") %>% select(-year)%>%
  filter(Region !="Texas",Gear!="VLL") %>%
  summarise(ln_fish = n())

West_lengths = LFD_data%>%
  filter(SID == "West", HabitatType!="PC") %>% select(-year)%>%
  filter(Region !="Texas",Gear!="VLL") %>%
  mutate(lbin = floor(FL_cm / bin_size)*bin_size)%>%
  mutate(ln_fish = n()) %>%
  group_by(lbin)%>% # by length bin in a certain habitat type
  summarise(freq = n() / unique(ln_fish))%>% # should be all length bins for that habitat type
  mutate(ln_fish=as.numeric(west_n),YEAR = 2018)

west_lfd_wide = West_lengths%>%pivot_wider(id_cols = c(YEAR,ln_fish),
                                           names_from = lbin, 
                                           values_from = freq, 
                                           values_fill = list(freq = 0)) %>%
  left_join(full_set) %>%
  select(comp_names) %>%
  replace(is.na(.), 0)%>%
  mutate(SID = "West", Type = "Nominal")

#View(west_lfd_wide)

# West Weighted ####
West_RSC = RSC_Proportions%>%filter(Region=="Texas"|Region =="Louisiana")%>%group_by(HabitatType)%>%
  summarize(RSC_nfish=sum((Number)))%>%filter(HabitatType!="Total")%>%
  mutate(prop_RSC=RSC_nfish/sum(RSC_nfish))

comps <-  LFD_data %>%
  filter(SID == "West", HabitatType!="PC") %>% 
  filter(Region !="Texas",Gear!="VLL") %>%
  mutate(lbin = floor(FL_cm / bin_size)*bin_size)%>%
  group_by(HabitatType) %>%
  mutate(ln_fish_reg = n()) %>%
  ungroup() %>%
  group_by(HabitatType, ln_fish_reg, lbin)%>% # by length bin in a certain habitat type
  summarise(freq = n() / unique(ln_fish_reg)) %>% # should be all length bins for that habitat type
  ungroup() %>%
  left_join(West_RSC ,by="HabitatType") %>% # join the habitat proportions for the west to the long length data
  mutate(lfdw_reg = freq*prop_RSC)
  
#View(comps)

  comps_n <- comps %>%
        group_by(HabitatType, ln_fish_reg) %>%
    summarise() %>%
    ungroup()%>%
    mutate(ln_fish = sum(ln_fish_reg))

#View(comps_n)

west_lfdw_wide <- left_join(comps,comps_n) %>%
    group_by(ln_fish_reg, lbin) %>%
  mutate(lfdw1 = sum(lfdw_reg)) %>%
  ungroup() %>%
  mutate(RSC_rep=sum(lfdw1, na.rm = TRUE)) %>%  #proportion of landings represented in final weighted comps # good I think
  ungroup() %>%
    group_by( RSC_rep,ln_fish, lbin) %>%
  summarise(
    lfdw2=sum(lfdw1, na.rm=TRUE) ) %>%
  mutate(
    lfdw=lfdw2/RSC_rep) %>%
  mutate(YEAR = 2018)%>%
  pivot_wider(id_cols = c(YEAR,ln_fish), 
              names_from = lbin, 
              values_from = lfdw, 
              values_fill = list(lfdw = 0)) %>%
  left_join(full_set) %>%
  select(comp_names) %>%
  replace(is.na(.), 0)%>%
  mutate(SID = "West", Type = "Weighted")

#View(west_lfdw_wide)

#Central ####
# Central Nominal ####
Central_n <- LFD_data%>%
  filter(SID == "Central",year == 2018, HabitatType!="UknS",HabitatType!="NS")%>% 
  select(-year)%>%
  summarise(ln_fish = n())

Central_lengths = LFD_data%>%
  filter(SID == "Central",year == 2018, HabitatType!="UknS",HabitatType!="NS")%>% 
  select(-year)%>%
  mutate(lbin = floor(FL_cm / bin_size)*bin_size)%>%
  mutate(ln_fish = n()) %>%
  group_by(lbin)%>% # by length bin in a certain habitat type
  summarise(freq = n() / unique(ln_fish))%>% # should be all length bins for that habitat type
  mutate(ln_fish=as.numeric(Central_n),YEAR = 2018)

Central_lfd_wide = Central_lengths%>%pivot_wider(id_cols = c(YEAR,ln_fish),
                                           names_from = lbin, 
                                           values_from = freq, 
                                           values_fill = list(freq = 0)) %>%
  left_join(full_set) %>%
  select(comp_names) %>%
  replace(is.na(.), 0)%>%
  mutate(SID = "Central", Type = "Nominal") 

#View(Central_lfd_wide)

# Central Weighted ####
Central_RSC = RSC_Proportions%>%filter(Region=="Alabama")%>% # Alabama includes FL numbers now
  group_by(HabitatType)%>%
  summarize(RSC_nfish=sum((Number)))%>%filter(HabitatType!="Total")%>%
  mutate(prop_RSC=RSC_nfish/sum(RSC_nfish))

comps_central <-  LFD_data %>%
  filter(SID == "Central",year == 2018, HabitatType!="UknS",HabitatType!="NS") %>% 
  mutate(lbin = floor(FL_cm / bin_size)*bin_size)%>%
  group_by(HabitatType) %>%
  mutate(ln_fish_reg = n()) %>%
  ungroup() %>%
  group_by(HabitatType, ln_fish_reg, lbin)%>% # by length bin in a certain habitat type
  summarise(freq = n() / unique(ln_fish_reg)) %>% # should be all length bins for that habitat type
  ungroup() %>%
  left_join(Central_RSC ,by="HabitatType") %>% # join the habitat proportions for the west to the long length data
  mutate(lfdw_reg = freq*prop_RSC)

#View(comps)

comps_n <- comps_central %>%
  group_by(HabitatType, ln_fish_reg) %>%
  summarise() %>%
  ungroup()%>%
  mutate(ln_fish = sum(ln_fish_reg))

#View(comps_n)

Central_lfdw_wide <- left_join(comps_central,comps_n) %>%
  group_by(ln_fish_reg, lbin) %>%
  mutate(lfdw1 = sum(lfdw_reg)) %>%
  ungroup() %>%
  mutate(RSC_rep=sum(lfdw1, na.rm = TRUE)) %>%  #proportion of landings represented in final weighted comps # good I think
  ungroup() %>%
  group_by( RSC_rep,ln_fish, lbin) %>%
  summarise(
    lfdw2=sum(lfdw1, na.rm=TRUE) ) %>%
  mutate(
    lfdw=lfdw2/RSC_rep) %>%
  mutate(YEAR = 2018)%>%
  pivot_wider(id_cols = c(YEAR,ln_fish), 
              names_from = lbin, 
              values_from = lfdw, 
              values_fill = list(lfdw = 0)) %>%
  left_join(full_set) %>%
  select(comp_names) %>%
  replace(is.na(.), 0)%>%
  mutate(SID = "Central", Type = "Weighted")

#View(Central_lfdw_wide)
# Central ALT Weighting ####
Alt_Central_RSC = Alt_RSC_Proportions%>%filter(Region=="Alabama")%>% # Alabama includes FL but NR now includes FL UCB and NR
  group_by(HabitatType)%>%
  summarize(RSC_nfish=sum((Number)))%>%filter(HabitatType!="Total")%>%
  mutate(prop_RSC=RSC_nfish/sum(RSC_nfish))

alt_comps_central <-  LFD_data %>%
  filter(SID == "Central",year == 2018, HabitatType!="UknS",HabitatType!="NS") %>% 
  mutate(lbin = floor(FL_cm / bin_size)*bin_size)%>%
  group_by(HabitatType) %>%
  mutate(ln_fish_reg = n()) %>%
  ungroup() %>%
  group_by(HabitatType, ln_fish_reg, lbin)%>% # by length bin in a certain habitat type
  summarise(freq = n() / unique(ln_fish_reg)) %>% # should be all length bins for that habitat type
  ungroup() %>%
  left_join(Alt_Central_RSC ,by="HabitatType") %>% # join the habitat proportions for the west to the long length data
  mutate(lfdw_reg = freq*prop_RSC)

#View(comps)

alt_comps_n <- alt_comps_central %>%
  group_by(HabitatType, ln_fish_reg) %>%
  summarise() %>%
  ungroup()%>%
  mutate(ln_fish = sum(ln_fish_reg))

#View(comps_n)

Alt_Central_lfdw_wide <- left_join(alt_comps_central,alt_comps_n) %>%
  group_by(ln_fish_reg, lbin) %>%
  mutate(lfdw1 = sum(lfdw_reg)) %>%
  ungroup() %>%
  mutate(RSC_rep=sum(lfdw1, na.rm = TRUE)) %>%  #proportion of landings represented in final weighted comps # good I think
  ungroup() %>%
  group_by( RSC_rep,ln_fish, lbin) %>%
  summarise(
    lfdw2=sum(lfdw1, na.rm=TRUE) ) %>%
  mutate(
    lfdw=lfdw2/RSC_rep) %>%
  mutate(YEAR = 2018)%>%
  pivot_wider(id_cols = c(YEAR,ln_fish), 
              names_from = lbin, 
              values_from = lfdw, 
              values_fill = list(lfdw = 0)) %>%
  left_join(full_set) %>%
  select(comp_names) %>%
  replace(is.na(.), 0)%>%
  mutate(SID = "Central", Type = "Alt_Weighted")

# Final Comps ####
Comps_Final = rbind(Central_lfd_wide,Alt_Central_lfdw_wide,Central_lfdw_wide,east_lfd_wide,west_lfd_wide,west_lfdw_wide)

write.csv(Comps_Final,"GRSC_weightedandNominal_LengthComps_updated2026.csv")

#Plotting ####
comps_final_long = Comps_Final%>%
  pivot_longer(cols = paste0(seq(min_size,len_plus,bin_size)),
               names_to = "lbin",
               values_to = "prop") 

SID.list =c("East", "West", "Central")

for (i in 1:length(SID.list)){
 # i=3
data = comps_final_long%>%filter(SID==SID.list[i],Type!="Alt_Weighted")%>%mutate(lbin=as.numeric(lbin))
n.dat=unique(data$ln_fish)
ggplot(data,aes(x=lbin,y=prop,fill=Type))+
  geom_bar(stat = "identity",position = "dodge")+
  scale_x_continuous(breaks = seq(0,120,by=20),limits=c(0,120))+
  labs(x="Fork Length (cm)",y="Proportion at size",fill="Composition Type",
       title= paste0(SID.list[i], " unadjusted n = ",n.dat))+
  theme_minimal()

Filename=paste0(SID.list[i],"_Final_LFD_updated2026.jpg")
ggsave(Filename,width = 5, height = 2.5, dpi = 300, units = "in")
}
