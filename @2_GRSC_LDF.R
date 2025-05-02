#################################################################################
# GRSC Data LFD creation Script
# 1. Reads in  GRSC_Data_for_LFD.csv created by @1_GRSC_DataCleaning.R
# 2. Produces histograms of length by state, gear, year and habitat type
# L. Denson April 2025
#
################################################################################

# Load packages ####
library(tidyverse)
library(ggplot2)

# set working directory ####
setwd("C:\\Users\\latreese.denson\\Desktop\\SEDAR_98_Models\\DataWorkshop\\GRSC_LFD_Exploration")

# read in clean data ####
GRSC_LFD_Dat <- read.csv("GRSC_Data_for_LFD.csv")

# make combinations
year_list = c("2018","2019","2020")
gear_list = c("ROV", "VLL","HL","BLL")
Region_list = unique(GRSC_LFD_Dat$Reg_SID)
habitat_list = c("AR","NR","UCB","PF","PC") # only 6+15 from NS in AL, UknS 45 
SID_list = unique(GRSC_LFD_Dat$SID)
options = as.data.frame(expand.grid(year_list,gear_list,Region_list,habitat_list))

# empty dataframe for LFDs
LFD.datafame =data.frame()

for(i in 1:dim(options)[1]){
  #i=2 # test
  print(i)
  data = GRSC_LFD_Dat%>% filter(year==options[i,1],Gear==options[i,2],Reg_SID==options[i,3],HabitatType==options[i,4])
  
  if(length(data!=0)){
    med_val = median(data$FL_cm)
    graph_title = paste(options[i,3],options[i,1],options[i,2],options[i,4])
    sum_measured = dim(data)[1]
    
    #Histogram of LFDs
    ggplot(data,aes(x=FL_cm))+
      geom_histogram(aes(y=after_stat(width * density)),color="black", fill="white",breaks =seq(10,115, by =5))+
      labs(y = "Proportion",x ="FL_cm",title= paste0(graph_title, " n = ",sum_measured))+
      geom_vline(xintercept = med_val, color = "red", linetype= "dashed", linewidth = 1)+
      annotate("text", x = med_val+20,y = 1, label =paste("median size = ", round(med_val),"cm"), color = "red")+
      theme_minimal()
    
    Filename = paste0(paste(c(options[i,3],options[i,1],options[i,2],options[i,4]),collapse = "_"),".jpg")
    ggsave(Filename,width = 5, height = 2.5, dpi = 300, units = "in")
    
    # # Table of LFDs
    # bins <- seq(10,115,5)
    # counts = table(factor(cut(data$FL_cm, bins)))
    # LFD = as.data.frame(counts/sum(counts))%>%pivot_wider(names_from = Var1,values_from = Freq)
    # 
    # tempdat=cbind(options[i,],LFD)
    # LFD.datafame=rbind(LFD.datafame,tempdat)
  }# end if
  
}# end loop through combinations

# write.csv(LFD.datafame,"GRSC_Various_LFD.csv")

# Loop for SID year and gear
options2 = as.data.frame(expand.grid(year_list,gear_list,SID_list))

for(i in 1:dim(options2)[1]){
  #i=29 # test
  print(options2[i,])
  data = GRSC_LFD_Dat%>% filter(year==options2[i,1],Gear==options2[i,2],SID==options2[i,3])
  
  if(length(data!=0)){
    med_val = median(data$FL_cm)
    graph_title = paste(options2[i,3],options2[i,1],options2[i,2])
    sum_measured = dim(data)[1]
    
    #Histogram of LFDs
    ggplot(data,aes(x=FL_cm))+
      geom_histogram(aes(y=after_stat(width * density)),color="black", fill="white",breaks =seq(10,115, by =5))+
      labs(y = "Proportion",x ="FL_cm",title= paste0(graph_title, " n = ",sum_measured))+
      geom_vline(xintercept = med_val, color = "red", linetype= "dashed", linewidth = 1)+
      annotate("text", x = med_val+20,y = 1, label =paste("median size = ", round(med_val),"cm"), color = "red")+
      theme_minimal()
    
    Filename = paste0(paste(c(options2[i,3],options2[i,1],options2[i,2]),collapse = "_"),".jpg")
    ggsave(Filename,width = 5, height = 2.5, dpi = 300, units = "in")
    
    # # Table of LFDs
    # bins <- seq(10,115,5)
    # counts = table(factor(cut(data$FL_cm, bins)))
    # LFD = as.data.frame(counts/sum(counts))%>%pivot_wider(names_from = Var1,values_from = Freq)
    # 
    # tempdat=cbind(options[i,],LFD)
    # LFD.datafame=rbind(LFD.datafame,tempdat)
  }# end if
  
}# end loop through combinations

# SID - Year~Habitat ####
for(i in 1:length(SID_list)){
  #i = 3
  Data = GRSC_LFD_Dat%>%filter(SID ==SID_list[i])%>%mutate(Gear=as.factor(Gear))
  Year_mean = Data%>%group_by(year,HabitatType)%>%
    summarize(mean_FL_cm = median(FL_cm),
              count = n())
  if(SID_list[i]!="East"){ 
    Mod1 = lm(FL_cm~year+Region+HabitatType+Gear+HabitatType:Gear,data = Data)
    anova(Mod1)%>%as.data.frame() %>% write.csv(file =paste0(SID_list[i],"_LFD_ANOVA.csv"))
  }else{
    #only one gear type
    Mod1 = lm(FL_cm~year,data = Data)
    anova(Mod1)%>%as.data.frame() %>% write.csv(file =paste0(SID_list[i],"_LFD_ANOVA.csv"))
  }
  
  ggplot(Data,aes(x=FL_cm))+
    geom_histogram(aes(y=after_stat(width * density)),color="black", fill="white",breaks =seq(10,115, by =5))+
    facet_grid(year~HabitatType)+
    geom_text(aes(x = 60, y = .5, label = paste0("med=",round(mean_FL_cm,2)," n=",count)), data = Year_mean)
  labs(y = "Proportion",x ="FL_cm")+theme_minimal()  
  
  Filename = paste0(SID_list[i],"_LFD_Year_Habitat.jpg")
  ggsave(Filename,width = 8, height = 4, dpi = 300, units = "in")
  
}

# SID - Year~Gear ####
for(i in 1:length(SID_list)){
  #i = 2
  Data = GRSC_LFD_Dat%>%filter(SID ==SID_list[i])
  Year_mean = Data%>%group_by(year,Gear)%>%
    summarize(mean_FL_cm = median(FL_cm),
              count = n())
  
  ggplot(Data,aes(x=FL_cm))+
    geom_histogram(aes(y=after_stat(width * density)),color="black", fill="white",breaks =seq(10,115, by =5))+
    facet_grid(Gear~year)+
    geom_text(aes(x = 70, y = .5, label = paste0("med =",round(mean_FL_cm,2)," n=",count)), data = Year_mean)
  labs(y = "Proportion",x ="FL_cm")+theme_minimal()  
  
  Filename = paste0(SID_list[i],"_LFD_Year_Gear.jpg")
  ggsave(Filename,width = 8, height = 4, dpi = 300, units = "in")
  
}

# year~SID ####
Year_SID_mean = GRSC_LFD_Dat%>%group_by(year,SID)%>%
  summarize(mean_FL_cm = median(FL_cm),
            count = n())
Mod1 = lm(FL_cm~year+Region+HabitatType+SID+Gear+HabitatType:Gear,data = GRSC_LFD_Dat)
anova(Mod1)%>%as.data.frame() %>% write.csv(file ="LFD_ANOVA.csv")

ggplot(GRSC_LFD_Dat,aes(x=FL_cm))+
  geom_histogram(aes(y=after_stat(width * density)),color="black", fill="white",breaks =seq(10,115, by =5))+
  facet_grid(year~SID)+
  geom_text(aes(x = 60, y = .5, label = paste0("med=",round(mean_FL_cm,2)," n=",count)), data = Year_SID_mean)
  labs(y = "Proportion",x ="FL_cm")+theme_minimal()
  ggsave("LFD_Year_SID.jpg",width = 8, height = 4, dpi = 300, units = "in")

  # Just SID ####
  Year_SID_mean = GRSC_LFD_Dat%>%group_by(SID)%>%
    summarize(mean_FL_cm = median(FL_cm),
              count = n())
  ggplot(GRSC_LFD_Dat,aes(x=FL_cm))+
    geom_histogram(aes(y=after_stat(width * density)),color="black", fill="white",breaks =seq(10,115, by =5))+
    facet_grid(~SID)+
    geom_text(aes(x = 60, y = .5, label = paste0("med=",round(mean_FL_cm,2)," n=",count)), data = Year_SID_mean)
  labs(y = "Proportion",x ="FL_cm")+theme_minimal()
  ggsave("LFD_SID.jpg",width = 8, height = 4, dpi = 300, units = "in")  
  
  
# # Code to compare two histograms on one plot - High level ####
# LC_Comp_HistFunc_SID = function(year1 =2018, year2=2019, SID1 ="Central",SID2="Central"){
#   
# Comp.data1 = GRSC_LFD_Dat%>%filter(year==year1,SID==SID1)
# Comp.data2 = GRSC_LFD_Dat%>%filter(year==year2,SID==SID2)
# 
# 
# med_val1 = median(Comp.data1$FL_cm)
# med_val2 = median(Comp.data2$FL_cm)
# graph_title = paste(year1,year2)
# 
# #Histogram of LFDs
# ggplot()+
#   geom_histogram(aes(x = Comp.data1$FL_cm,y=after_stat(count/sum(count)), fill = "Comp.data1"),breaks =seq(10,115, by =5),alpha = 0.5) +
#   geom_histogram(aes(x = Comp.data2$FL_cm,y=after_stat(count/sum(count)), fill = "Comp.data2"),breaks =seq(10,115, by =5),alpha = 0.5) +
#   labs(y = "Proportion",x ="FL_cm",title= graph_title)+
#   geom_vline(xintercept = med_val1, color = "red", linetype= "dashed", linewidth = 1)+
#   annotate("text", x = med_val1+40,y = 1, label =paste("median size1 = ", round(med_val1),"cm"), color = "red")+
#   geom_vline(xintercept = med_val2, color = "red", linetype= "dashed", linewidth = 1)+
#   annotate("text", x = med_val2+40,y = .75, label =paste("median size2 = ", round(med_val2),"cm"), color = "red")+
#   theme_minimal()
# 
# #Filename = paste0(paste(c(options2[i,3],options2[i,1],options2[i,2]),collapse = "_"),".jpg")
# #ggsave(Filename,width = 5, height = 2.5, dpi = 300, units = "in")
# } # end function
# 
# LC_Comp_HistFunc_SID()

## Code to compare two histograms on one plot - Lower level (adding gear and habitat type) ####
  