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
      geom_histogram(aes(y=after_stat(count/sum(count))),color="black", fill="white",breaks =seq(10,115, by =5))+
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
  #i=28 # test
  print(options2[i,])
  data = GRSC_LFD_Dat%>% filter(year==options2[i,1],Gear==options2[i,2],SID==options2[i,3])
  
  if(length(data!=0)){
    med_val = median(data$FL_cm)
    graph_title = paste(options2[i,3],options2[i,1],options2[i,2])
    sum_measured = dim(data)[1]
    
    #Histogram of LFDs
    ggplot(data,aes(x=FL_cm))+
      geom_histogram(aes(y=after_stat(count/sum(count))),color="black", fill="white",breaks =seq(10,115, by =5))+
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
