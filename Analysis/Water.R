# ==============================================
# ------- Model output analysis: water properties
# ==============================================
# Irob et al., 2021 ---------------------------
# Author of R script: Katja Irob (irob.k@fu-berlin.de)
# ==============================================

rm(list=ls()) # clears working environment 


library(tidyverse)
options(dplyr.width = Inf) #enables head() to display all coloums
library(grid)
library(gridExtra)
library(reshape2)
library(scales)
library(plotly)

####reading in all outputfiles returned as one dataframe 
readfiles<- function() {
  files<- list.files(path = here::here("Data/Results/"), pattern="yearly", full.names = T)
  
  outputfiles<-lapply(files, function(x) {
    read.table(file=x, header=T, skipNul = TRUE) 
  })
  
  scenarios<-as.list(gsub(".*EH_\\s*|_.*", "", files))
  climrep<-as.list(gsub(".*climrep-\\s*|_.*", "", files))
  
  
  PFTs<-Map(cbind, outputfiles, scenario=scenarios, climrep = climrep) # adding extra column with scenario name 
  PFTs<-do.call("rbind", PFTs) # merging list into df
  
  #####evaporation########
  
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


## Calculate corelations, means etc -------

cover<-PFTcoverall[, c("wateravailability", "ML1", "ML2", "year", "wateravailability2", "scenario", "totalCover", "climrep", "meanGtotalcover", "meanAtotalcover", "meanStotalcover")]
cover <- cover %>% filter(year > 79) 

cover$totalCover<-cover$totalCover*100
cover$wateravailability<-cover$wateravailability*100
cover$ML1 <- cover$ML1*100

# test for normal distribution
shapiro.test(cover$wateravailability) # p < 0.05

cor.test(cover$totalCover, cover$ML1, method="spearman")
# rho = 0.58
cor.test(cover$totalCover, cover$wateravailability, method="spearman")
# rho = 0.86

## LMs -----------------

# T/ET ------

lm1 <- lm(wateravailability ~ totalCover + scenario + totalCover:scenario, data = cover)
lm0 <- lm(wateravailability ~ 1, data = cover)

summary(lm1)

plot(lm1) # check assumptions

anova(lm1, lm0) # compare model with null model 
# p < 0.01

# effect size cohen's d

library(effsize)
cover$landuse <- ifelse(grepl("(browse)", cover$scenario),"Wildlife","Cattle")

cohen.d(cover$wateravailability, cover$landuse)
# d = 2.73

# Soil moisture --------

LM1 <- lm(ML1 ~ totalCover + scenario + totalCover:scenario, data = cover)
LM0 <- lm(ML1 ~ 1, data = cover)

# welch-anova for unequal variances -- 

library(rstatix)

cover %>% group_by(scenario) %>%
  welch_anova_test(ML1 ~ totalCover)

oneway.test(ML1 ~ scenario,
            data=cover,
            var.equal=FALSE)

summary(LM1)

plot(LM1) # check assumptions, distribution of residuls etc

anova(lm1, lm0) # compare with null model -> p < 0.01


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
  
  # rename scenarios
  cover$scenario<-as.character(cover$scenario)
  cover$scenario[cover$scenario=="SR40graze"] <- 'Cattle low'
  cover$scenario[cover$scenario=="SR20graze"] <- 'Cattle high'
  #
  cover$scenario[cover$scenario=="SR20browse"] <- 'Wildlife high'
  cover$scenario[cover$scenario=="SR40browse"] <- 'Wildlife low'
  
  # order by scenario 
  cover$scenario<- factor(cover$scenario, levels=c('Cattle low', 'Wildlife low',  'Cattle high', 'Wildlife high' ))
  
  
  # make plot ---
  
  cols <-c("coral", "cyan", "coral4",  "seagreen") # set colours for scenario
  
  # calculate water properties and cover by climrep 
  WaterVals_mean <- cover %>% group_by(scenario, climrep) %>% summarise_at(vars(wateravailability, totalCover, ML1), funs(mean, sd))
  
  # T/ET --
  Scatterplot<-ggplot(WaterVals_mean, aes(x=totalCover_mean, y=wateravailability_mean, color=scenario, shape=scenario)) + 
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
  Scatterplot
  
  # save plot 
  ggsave(Scatterplot, file="T_ET_cover.png", width = 32,
         height = 16,
         units = "cm", dpi=450)
  
  # soil moisutre --
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
  
  
  
  # other water properties --
  ## ET, EP and TP between scenarios -------------------------------------
  
  cover<-PFTcoverall[, c("year", "ET", "Annualevaporation", "AnnualtranspirationL1" , "scenario", "totalCover", "climrep", "meanGtotalcover", "meanAtotalcover", "meanStotalcover")]
  cover <- cover %>% filter(year > 79) 
  cover<-melt(cover, id.vars=c("year", "scenario", "ET", "Annualevaporation", "AnnualtranspirationL1" , "totalCover", "climrep"))
  
  cover$totalCover<-cover$totalCover*100
  
  cover <- cover %>% 
    rename(
      PFT = variable, 
      cover =value
    )
  cover$type <- ifelse(grepl("(meanGtotalcover)", cover$PFT),"Perennial", ifelse(grepl("(meanStotalcover)", cover$PFT),"Shrub", "Annual"))
  
  
  cover$scenario<-as.character(cover$scenario)
  cover$scenario[cover$scenario=="SR40graze"] <- 'Cattle low'
  cover$scenario[cover$scenario=="SR20graze"] <- 'Cattle high'
  #
  cover$scenario[cover$scenario=="SR20browse"] <- 'Wildlife high'
  cover$scenario[cover$scenario=="SR40browse"] <- 'Wildlife low'
 
  cover$scenario<- factor(cover$scenario, levels=c('Cattle low', 'Wildlife low',  'Cattle high', 'Wildlife high' ))
  
  
  # make plot ---
  
  cols <-c("coral", "cyan", "coral4",  "seagreen")
  
  WaterVals_mean <- cover %>% group_by(scenario, climrep) %>% summarise_at(vars(ET, totalCover), funs(mean, sd))
  
  # ET --
  
  Scatterplot_ET<-ggplot(WaterVals_mean, aes(x=totalCover_mean, y=ET_mean, color=scenario, shape=scenario)) + 
    geom_point()+
    geom_smooth(method=lm, se=T) +
    ylab(bquote("ET L1 \n"))+
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
  Scatterplot_ET
  
  
  ## evaporation --- 
  
  WaterVals_mean <- cover %>% group_by(scenario, climrep) %>% summarise_at(vars(Annualevaporation, AnnualtranspirationL1, totalCover), funs(mean, sd))
  
  Scatterplot_EP<-ggplot(WaterVals_mean, aes(x=totalCover_mean, y=Annualevaporation_mean, color=scenario, shape=scenario)) + 
    geom_point()+
    geom_smooth(method=lm, se=T) +
    ylab(bquote("Evaporation \n"))+
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
  Scatterplo_EP
  
  # transpiration --
  Scatterplot_TP<-ggplot(WaterVals_mean, aes(x=totalCover_mean, y=AnnualtranspirationL1_mean, color=scenario, shape=scenario)) + 
    geom_point()+
    geom_smooth(method=lm, se=T) +
    ylab(bquote("Transpiration \n"))+
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
  Scatterplot_TP
  
 