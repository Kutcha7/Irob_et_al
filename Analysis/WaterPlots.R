# ==============================================
# ------- Model run, input & output manipulation
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
library(viridis)
library(scales)
library(plotly)



setwd("~/Documents/Strategy/Results/Setup January/20-40/Feb16/")
path <- ("~/Documents/Strategy/Results/Setup January/20-40/Feb16/")

####reading in all outputfiles returned as one dataframe 
readfiles<- function(path="~/Documents/Strategy/Results/Setup January/20-40/Feb16/") {
  files<-list.files(path= path, pattern="yearly")
  
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

shapiro.test(cover$wateravailability)

cor.test(cover$totalCover, cover$ML1, method="spearman")
cor.test(cover$totalCover, cover$wateravailability, method="spearman")


## LMs -----

# T/ET --
lm1 <- lm(wateravailability ~ totalCover + scenario + totalCover:scenario, data = cover)
lm0 <- lm(wateravailability ~ 1, data = cover)

summary(lm1)

plot(lm1)

anova(lm1, lm0)

# effect size cohen's d

library(effsize)
cover$landuse <- ifelse(grepl("(browse)", cover$scenario),"Wildlife","Cattle")


cohen.d(cover$wateravailability, cover$landuse)

# Soil moisture --

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

plot(LM1)

anova(lm1, lm0)


### plots -----------------------------------




makeWaterplot <- function(PFTcoverall) {
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
  cover$scenario[cover$scenario=="SR40graze"] <- 'Cattle low'
  cover$scenario[cover$scenario=="SR20graze"] <- 'Cattle high'
  #
  cover$scenario[cover$scenario=="SR20browse"] <- 'Wildlife high'
  cover$scenario[cover$scenario=="SR40browse"] <- 'Wildlife low'
  
  #cover$scenario<- factor(cover$scenario, levels=c( 'Wildlife high', 'Cattle high',  'Wildlife low', 'Cattle low'))
  
  cover$scenario<- factor(cover$scenario, levels=c('Cattle low', 'Wildlife low',  'Cattle high', 'Wildlife high' ))
  
  #cols <-c(  "seagreen", "coral4", "cyan",  "coral"),
  
  # make plot ---
  
  cols <-c("coral", "cyan", "coral4",  "seagreen")
  
  WaterVals_mean <- cover %>% group_by(scenario, climrep) %>% summarise_at(vars(wateravailability, totalCover, ML1), funs(mean, sd))
  
  #WaterVals_mean <- cover %>% group_by(scenario) %>% summarise_at(vars(wateravailability, totalCover, ML1), funs(mean, sd))
  
  
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
  
  ggsave(Scatterplot, file="T_ET_cover.png", width = 32,
         height = 16,
         units = "cm", dpi=450)
  
  
  Scatterplot_ML1<-ggplot(WaterVals_mean, aes(x=totalCover_mean, y=ML1_mean, color=scenario, shape=scenario)) + 
    geom_point()+
    geom_smooth(method=lm, se=T) +
    ylab(bquote("Soil moisture L1\n [Vol %] "))+
    xlab("\nTotal cover [%]")+
    #ylim(40, 100) +
    #xlim(40, 110) +
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
  
  
  legend <- get_legend(Scatterplot_ML1)
  
  ML1_mean <- cover %>% group_by(scenario) %>% summarise_at(vars(ML1), funs(mean, sd))
  
  
  ML1plot<-ggplot(ML1_mean,  aes(y = mean, x=scenario, fill=scenario)) +
    geom_bar(stat="identity", 
             position=position_dodge()) +
    geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                  position=position_dodge(.9)) +
    # ylim(0, 100) +
    ylab(bquote("Soil moisture L1\n [Vol%]"))+
    xlab("\nScenario")+
    scale_fill_manual(values = cols)+
    theme(axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          axis.title.y= element_text(size=15),
          axis.title.x=element_text(size=15),
          legend.text=element_text(size=16),
          legend.direction = "horizontal", legend.position = "none", legend.title = element_blank(), 
          # legend.background = element_blank(),
          # legend.spacing.x = unit(0.3, 'cm'),
          panel.grid.major = element_line(size = 0.2, linetype = 'solid', colour = "gray"),
          panel.background = element_blank()) 
  ML1plot
  
  
  Scatterplot <- Scatterplot + theme(legend.position="none")
  Scatterplot_ML1 <- Scatterplot_ML1+ theme(legend.position="none")
  
  waterplots <- plot_grid(Scatterplot, ML1plot,
                          ncol=2,  nrow=1, 
                          rel_widths=c(7, 7, 7, 7), 
                          align ="h", axis =bt, 
                          labels=c("a", "b")
  )
  
  
  library(cowplot)
  
  waterplots <- plot_grid(Scatterplot, Scatterplot_ML1,
                         ncol=2,  nrow=1, 
                         rel_widths=c(7, 7, 7, 7), 
                         align ="h", axis =bt
  )
  
  water_legend<- plot_grid(waterplots, legend, nrow=2, rel_heights = c(1, 0.1))
  water_legend             
  
  
  ggsave(water_legend, file="waterplots_comb_1264.png", width = 32,
         height = 18,
         units = "cm", dpi=450)
  
  
  ## ET between scenarios -------------------------------------
  
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
  
  #cover$scenario<- factor(cover$scenario, levels=c( 'Wildlife high', 'Cattle high',  'Wildlife low', 'Cattle low'))
  
  cover$scenario<- factor(cover$scenario, levels=c('Cattle low', 'Wildlife low',  'Cattle high', 'Wildlife high' ))
  
  #cols <-c(  "seagreen", "coral4", "cyan",  "coral")
  
  # make plot ---
  
  cols <-c("coral", "cyan", "coral4",  "seagreen")
  
  WaterVals_mean <- cover %>% group_by(scenario, climrep) %>% summarise_at(vars(ET, totalCover), funs(mean, sd))
  
  Scatterplot<-ggplot(WaterVals_mean, aes(x=totalCover_mean, y=ET_mean, color=scenario, shape=scenario)) + 
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
  Scatterplot
  
  
  ## evaporation --- 
  
  WaterVals_mean <- cover %>% group_by(scenario, climrep) %>% summarise_at(vars(Annualevaporation, AnnualtranspirationL1, totalCover), funs(mean, sd))
  
  Scatterplot<-ggplot(WaterVals_mean, aes(x=totalCover_mean, y=Annualevaporation_mean, color=scenario, shape=scenario)) + 
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
  Scatterplot
  
  Scatterplot<-ggplot(WaterVals_mean, aes(x=totalCover_mean, y=AnnualtranspirationL1_mean, color=scenario, shape=scenario)) + 
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
  Scatterplot
  
  
  
  
  ET_mean <- cover %>% group_by(scenario) %>% summarise_at(vars(ET), funs(mean, sd))
  
  
  barplot<-ggplot(ET_mean,  aes(y = mean, x=scenario, fill=scenario)) +
    geom_bar(stat="identity", 
             position=position_dodge()) +
    geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                  position=position_dodge(.9)) +
   # ylim(0, 100) +
    ylab(bquote("ET"))+
    xlab("\nScenario")+
    scale_fill_manual(values = cols)+
    theme(axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          axis.title.y= element_text(size=15),
          axis.title.x=element_text(size=15),
          legend.text=element_text(size=16),
          legend.direction = "horizontal", legend.position = "none", legend.title = element_blank(), 
          # legend.background = element_blank(),
          # legend.spacing.x = unit(0.3, 'cm'),
          panel.grid.major = element_line(size = 0.2, linetype = 'solid', colour = "gray"),
          panel.background = element_blank()) 
  barplot
  
  ###############################################################
  ####### WITHOUT MEANS -----------------------------------------
  
  
  Scatterplot<-ggplot(cover, aes(x=totalCover, y=wateravailability, color=scenario, shape=scenario)) + 
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
          legend.direction = "horizontal", legend.position = "top", legend.title = element_blank(), 
          legend.background = element_blank(),
          panel.grid.major = element_line(size = 0.2, linetype = 'solid', colour = "gray"),
          panel.background = element_blank()) 
  Scatterplot
  
  
  
  ScatterplotL2<-ggplot(cover, aes(x=totalCover, y=wateravailability2, color=scenario, shape=scenario)) + 
    geom_point()+
    geom_smooth(method=lm, se=T) +
    ylab(bquote("T/(ET) L2\n [%]"))+
    xlab("\nTotal cover [%]")+
    ylim(0, 100) +
    #xlim(40, 110) +
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
  ScatterplotL2
  
  
  
  Scatterplot_ML1<-ggplot(cover, aes(x=totalCover, y=ML1, color=scenario, shape=scenario)) + 
    geom_point()+
    geom_smooth(method=lm, se=T) +
    ylab(bquote("Soil moisture L1 [Vol %] "))+
    xlab("\nTotal cover [%]")+
    #ylim(40, 100) +
    #xlim(40, 110) +
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
  
  boxplot_ML1<-ggplot(cover, aes(x=scenario, y=ML1, color=scenario, shape=scenario)) + 
    geom_boxplot()+
    #geom_jitter(position=position_jitter(0.2))+
    #ylab(bquote("Soil moisture L1 [Vol %] "))+
    #xlab("\nTotal cover [%]")+
    #ylim(40, 100) +
    #xlim(40, 110) +
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
  boxplot_ML1
  
  ### Barplot ---------
  library(data.table)
  waterL1 <- as.data.table(cover)
  waterL1_agg <- waterL1[,.(mean(ML1), sd(ML1)), by = .(scenario)]
  
  waterL1_agg <- waterL1_agg %>% 
    rename(
      mean = V1, 
      sd = V2
    )
  
  water_avail1<-ggplot(waterL1_agg, aes(y = mean, x = scenario, fill=scenario)) + 
    geom_bar(stat="identity", color="lightgray") +
    geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2)+
    #ylim(0, 18) +
    ylab(bquote("Soil moisture L1 [Vol %]"))+
    xlab("\nIncreasing landuse intensity")+
    geom_vline(xintercept = c(3.5))+
    scale_fill_manual(values = c(cols))+
    theme(axis.text.x = element_text(size=14, angle= 90),
          axis.text.y = element_text(size=14),
          axis.title.y= element_text(size=15),
          axis.title.x=element_text(size=15),
          strip.text.x = element_text(size = 18),
          strip.text.y = element_text(size = 14),
          #legend.text=element_text(size=16),
          legend.direction = "horizontal", legend.position = "none", legend.title = element_blank(), 
          legend.background = element_blank(),
          panel.grid.major = element_line(size = 0.2, linetype = 'solid', colour = "gray"),
          panel.background = element_blank()) 
  water_avail1
  
  
  
  Scatterplot_ML2<-ggplot(cover, aes(x=totalCover, y=ML2, color=scenario, shape=scenario)) + 
    geom_point()+
    geom_smooth(method=lm, se=T) +
    ylab(bquote("Soil moisture L2 [Vol %] "))+
    xlab("\nTotal cover [%]")+
    #ylim(40, 100) +
    #xlim(40, 110) +
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
  Scatterplot_ML2
  
  boxplot_ML2<-ggplot(cover, aes(x=scenario, y=ML2, color=scenario, shape=scenario)) + 
    geom_boxplot()+
    #geom_jitter(position=position_jitter(0.2))+
    ylab("Soil moisture L2")+
    xlab("Scenario")+
    #ylim(40, 100) +
    #xlim(40, 110) +
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
  boxplot_ML2
 
  
  ydensity<-ggplot(cover, aes(wateravailability, fill=scenario)) + 
    geom_density(alpha=.5) + 
    ylab(bquote("Density"))+
    xlab("\nT/(ET)\n [%]")+
    scale_fill_manual(values = c(cols))+
    theme(axis.text.x = element_text(size=14),
          axis.text.y = element_text(size=14),
          axis.title.y= element_text(size=15),
          axis.title.x=element_text(size=15),
          strip.text.x = element_text(size = 18),
          strip.text.y = element_text(size = 14),
          #legend.text=element_text(size=16),
          legend.direction = "horizontal", legend.position = "none", legend.title = element_blank(), 
          legend.background = element_blank(),
          panel.grid.major = element_line(size = 0.2, linetype = 'solid', colour = "gray"),
          panel.background = element_blank()) 
  ydensity
  
  
  ML1density<-ggplot(cover, aes(ML1, fill=scenario)) + 
    geom_density(alpha=.5) + 
    ylab(bquote("Density"))+
    xlab("\nSoil moisture [Vol %]")+
    scale_fill_manual(values = c(cols))+
    theme(axis.text.x = element_text(size=14),
          axis.text.y = element_text(size=14),
          axis.title.y= element_text(size=15),
          axis.title.x=element_text(size=15),
          strip.text.x = element_text(size = 18),
          strip.text.y = element_text(size = 14),
          #legend.text=element_text(size=16),
          legend.direction = "horizontal", legend.position = "none", legend.title = element_blank(), 
          legend.background = element_blank(),
          panel.grid.major = element_line(size = 0.2, linetype = 'solid', colour = "gray"),
          panel.background = element_blank()) 
  ML1density
  
  
  ML2density<-ggplot(cover, aes(ML2, fill=scenario)) + 
    geom_density(alpha=.5) + 
    ylab(bquote("Density"))+
    xlab("\nSoil moisture [Vol %]")+
    scale_fill_manual(values = c(cols))+
    theme(axis.text.x = element_text(size=14),
          axis.text.y = element_text(size=14),
          axis.title.y= element_text(size=15),
          axis.title.x=element_text(size=15),
          strip.text.x = element_text(size = 18),
          strip.text.y = element_text(size = 14),
          #legend.text=element_text(size=16),
          legend.direction = "horizontal", legend.position = "none", legend.title = element_blank(), 
          legend.background = element_blank(),
          panel.grid.major = element_line(size = 0.2, linetype = 'solid', colour = "gray"),
          panel.background = element_blank()) 
  ML2density
  
  
  
  
  
  
  
  # create blank placeholder 
  
  
  blankPlot <- ggplot()+geom_blank(aes(1,1))+
    theme(plot.background = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(), 
          axis.text.y = element_blank(),
          axis.ticks = element_blank()
    )
  
  # Arrange ggplot2 with adapted height and width for each row and column :
  
  library("gridExtra")
  arranged_densplot<-grid.arrange(xdensity, blankPlot, Scatterplot, ydensity, 
                                  ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4))
  
  
  
  ggsave(arranged_densplot, file="densplot_TET_cover_arranged_browsegr.png", width = 32,
         height = 16,
         units = "cm", dpi=350)
  
  return(arranged_densplot)
  
} 

makeDensityplot(PFTcoverall)



cover<-PFTcoverall[, c("wateravailability", "ML1", "ML2", "year", "wateravailability2", "scenario", "totalCover")]
cover <- cover %>% filter(year> 79) 
#cover<-melt(cover, id.vars=c( "wateravailability2", "ML1", "ML2"))

cover<-PFTcoverall[, c("wateravailability", "scenario", "totalCover")]

list_df <- split(cover, cover$scenario)
list2env(split(cover, cover$scenario), envir = .GlobalEnv)

cover$perc_change_cattle<-(SR20graze$wateravailability - SR40graze$wateravailability)/SR40graze$wateravailability
cover$perc_change_wild<-(SR20browse$wateravailability - SR40browse$wateravailability)/SR40browse$wateravailability


cover$totalCover<-cover$totalCover*100
cover$wateravailability<-cover$wateravailability*100
cover$wateravailability2<-cover$wateravailability2*100

cover <- cover %>% 
  rename(
    PFT = variable, 
    cover =value
  )
cover$type <- ifelse(grepl("(meanGtotalcover)", cover$PFT),"Perennial", ifelse(grepl("(meanStotalcover)", cover$PFT),"Shrub", "Annual"))


cover$scenario<-as.character(cover$scenario)
cover$scenario[cover$scenario=="SR40graze"] <- "Cattle_low"
cover$scenario[cover$scenario=="SR20graze"] <- "Cattle_high"
# 
cover$scenario[cover$scenario=="SR20browse"] <- "Wildlife_high"
cover$scenario[cover$scenario=="SR40browse"] <- "Wildlife_low"

cover$scenario<- factor(cover$scenario, levels=c("Cattle_low", "Wildlife_low",  "Cattle_high", "Wildlife_high" ))


# make plot ---

cols <-c("coral", "cyan", "coral4",  "seagreen")

Scatterplot<-ggplot(cover, aes(x=totalCover, y=wateravailability, color=scenario, shape=scenario)) + 
  geom_point()+
  geom_smooth(method=lm, se=T) +
  ylab(bquote("T/(ET)\n [%]"))+
  xlab("\nTotal cover [%]")+
  #ylim(40, 100) +
  #xlim(40, 110) +
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
Scatterplot



#### SOIL MOISTURE 

cover<-PFTcoverall[, c("wateravailability", "ML1", "ML2", "year", "wateravailability2", "scenario", "totalCover")]
cover <- cover %>% filter(year == 99) 


cover$totalCover<-cover$totalCover*100
cover$wateravailability<-cover$ML1*100

cover$scenario<-as.character(cover$scenario)
cover$scenario[cover$scenario=="SR40graze"] <- "Cattle_low"
cover$scenario[cover$scenario=="SR20graze"] <- "Cattle_high"
# 
cover$scenario[cover$scenario=="SR20browse"] <- "Wildlife_high"
cover$scenario[cover$scenario=="SR40browse"] <- "Wildlife_low"

cover$scenario<- factor(cover$scenario, levels=c("Cattle_low", "Wildlife_low",  "Cattle_high", "Wildlife_high" ))


# make plot ---

cols <-c("coral", "cyan", "coral4",  "seagreen")

ScatterplotML1<-ggplot(cover, aes(x=totalCover, y=ML1, color=scenario, shape=scenario)) + 
  geom_point()+
  geom_smooth(method=lm, se=T) +
  ylab(bquote("Soil moisture L1 [Vol %]\n Year = 99"))+
  xlab("\nTotal cover [%]")+
  #ylim(40, 100) +
  #xlim(40, 110) +
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
ScatterplotML1

