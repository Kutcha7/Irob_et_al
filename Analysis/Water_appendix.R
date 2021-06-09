# ==============================================
# ------- Model run, input & output manipulation
# ==============================================
# Irob et al., 2021 ---------------------------
# Author of R script: Katja Irob (irob.k@fu-berlin.de)
# ==============================================

rm(list=ls()) # clears working environment 


library(tidyr)
library(dplyr)
options(dplyr.width = Inf) #enables head() to display all coloums
library(ggplot2)
library(grid)
library(gridExtra)
library(reshape2)
library(tidyverse)
library(scales)
library(plotly)


paths <- here::here("Data/Results/Appendix/")

#### reading in all outputfiles returned as one dataframe 
readfiles<- function(path=paths) {
  files<- list.files(path = here::here("Data/Results/Appendix/"), pattern="yearly", full.names = T)
  
  
  outputfiles<-lapply(files, function(x) {
    read.table(file=x, header=T, sep="\t", skipNul = TRUE) 
  })
  
  # extracting scenario and climrep from filename
  scenarios<-as.list(gsub(".*EH_\\s*|_.*", "", files))
  climrep<-as.list(gsub(".*climrep-\\s*|_.*", "", files))
  
  
  PFTs<-Map(cbind, outputfiles, scenario=scenarios, climrep = climrep) # adding extra column with scenario name 
  PFTs<-do.call("rbind", PFTs) # merging list into df
  
  # T/ET --
  
  PFTs$wateravailability <- PFTs$AnnualtranspirationL1/(PFTs$Annualevaporation+PFTs$AnnualtranspirationL1)
  PFTs$wateravailability2 <- PFTs$AnnualtranspirationL2/(PFTs$Annualevaporation+PFTs$AnnualtranspirationL2)
  
  PFTs$ET <- (PFTs$Annualevaporation+PFTs$AnnualtranspirationL1)
  
  PFTs$totalCover<-(PFTs$meanGtotalcover+PFTs$meanAtotalcover+PFTs$meanStotalcover)
  
  PFTs<-select(PFTs,contains("Cover"), c("year", "Annualevaporation", "AnnualtranspirationL1" ,  "scenario", "climrep", "ML1", "ML2", "ET", "wateravailability", "wateravailability2", "Annualevaporation", "totalCover"))
  PFTs<-select(PFTs,starts_with("mean"), c("year", "ML1", "ML2", "scenario", "climrep", "ET", "Annualevaporation", "AnnualtranspirationL1" ,"wateravailability", "wateravailability2", "Annualevaporation", "totalCover"))
  
  
  no<-c("meanRCover")
  PFTs<-PFTs[, !names(PFTs) %in% no, drop=F ] # drop =F means that it should be a df not a list
  
  return(PFTs)
}


PFTcoverall<-readfiles()


### plots -----------------------------------

  cover<-PFTcoverall[, c("wateravailability", "ML1", "ML2", "year", "wateravailability2", "scenario", "totalCover", "climrep", "meanGtotalcover", "meanAtotalcover", "meanStotalcover")]
  cover <- cover %>% filter(year > 79) 
  cover<-melt(cover, id.vars=c( "scenario", "wateravailability", "wateravailability2", "ML1", "ML2", "totalCover", "climrep"))
  
  cover$totalCover<-cover$totalCover*100
  cover$wateravailability<-cover$wateravailability*100
  cover$wateravailability2<-cover$wateravailability2*100
  cover$ML1 <- cover$ML1*100
  cover$ML2 <- cover$ML2*100
  
  cover <- cover %>% 
    rename(
      PFT = variable, 
      cover =value
    )
  cover$type <- ifelse(grepl("(meanGtotalcover)", cover$PFT),"Perennial", ifelse(grepl("(meanStotalcover)", cover$PFT),"Shrub", "Annual"))
  
  
  cover$scenario<-as.character(cover$scenario)
  cover$scenario[cover$scenario=="SR50graze"] <- 'Cattle very low'
  cover$scenario[cover$scenario=="SR10graze"] <- 'Cattle very high'
  cover$scenario[cover$scenario=="SR30graze"] <- 'Cattle medium'
  #
  cover$scenario[cover$scenario=="SR10browse"] <- 'Wildlife very high'
  cover$scenario[cover$scenario=="SR30browse"] <- 'Wildlife medium'
  cover$scenario[cover$scenario=="SR50browse"] <- 'Wildlife very low'
  
  cover$scenario<- factor(cover$scenario, levels=c('Cattle very low', 'Wildlife very low', "Cattle medium", "Wildlife medium",  'Cattle very high', 'Wildlife very high' ))
  
  
  # make plot ---
  
  cols <-c("darksalmon", "darkseagreen1", "chocolate1", "darkseagreen3", "brown4",    "darkgreen")
  
  WaterVals_mean <- cover %>% group_by(scenario, climrep) %>% summarise_at(vars(wateravailability, totalCover, ML1), funs(mean, sd))
  
  # T/ET --
  Scatterplot_TET<-ggplot(WaterVals_mean, aes(x=totalCover_mean, y=wateravailability_mean, color=scenario, shape=scenario)) + 
    geom_point()+
    geom_smooth(method=lm, se=T) +
    ylab(bquote("T/(ET) L1 \n [%]"))+
    xlab("\nTotal cover [%]")+
    scale_color_manual(values = c(cols))+
    theme(axis.text.x = element_text(size=14),
          axis.text.y = element_text(size=14),
          axis.title.y= element_text(size=15),
          axis.title.x=element_text(size=15),
          strip.text.x = element_text(size = 18),
          strip.text.y = element_text(size = 14),
          legend.text=element_text(size=16),
          legend.direction = "horizontal", legend.position = "bottom", legend.title = element_blank(), 
          legend.background = element_blank(),
          panel.grid.major = element_line(size = 0.2, linetype = 'solid', colour = "gray"),
          panel.background = element_blank()) 
  Scatterplot_TET
  
  ggsave(Scatterplot_TET, file="T_ET_cover_Appendix.png", width = 32,
         height = 16,
         units = "cm", dpi=450)
  
  # soil moisture --
  Scatterplot_ML1<-ggplot(WaterVals_mean, aes(x=totalCover_mean, y=ML1_mean, color=scenario, shape=scenario)) + 
    geom_point()+
    geom_smooth(method=lm, se=T) +
    ylab(bquote("Soil moisture L1\n [Vol %] "))+
    xlab("\nTotal cover [%]")+
    scale_color_manual(values = c(cols))+
    theme(axis.text.x = element_text(size=14),
          axis.text.y = element_text(size=14),
          axis.title.y= element_text(size=15),
          axis.title.x=element_text(size=15),
          strip.text.x = element_text(size = 18),
          strip.text.y = element_text(size = 14),
          legend.text=element_text(size=16),
          legend.direction = "horizontal", legend.position = "top", legend.title = element_blank(), 
          legend.background = element_blank(),
          panel.grid.major = element_line(size = 0.2, linetype = 'solid', colour = "gray"),
          panel.background = element_blank()) 
  Scatterplot_ML1
  
  