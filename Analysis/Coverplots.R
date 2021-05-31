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


# functions for summarising data
se <- function(x) sd(x) / sqrt(length(x))
se.min <- function(x) (mean(x)) - se(x)
se.max <- function(x) (mean(x)) + se(x)


setwd("~/Documents/Strategy/Results/Setup January/20-40/Feb16/")
path <- ("~/Documents/Strategy/Results/Setup January/20-40/Feb16/")

####reading in all outputfiles returned as one dataframe 
readfiles<- function(path="~/Documents/Strategy/Results/Setup January/20-40/Feb16/") {
  files<-list.files(path= path, pattern="yearly")
  
  outputfiles<-lapply(files, function(x) {
    read.table(file=x, header=T, sep="\t", skipNul = TRUE) 
  })
  
  #scenarios<-as.list(gsub(".*yearly_\\s*|_.*", "", files))
  scenarios<-as.list(gsub(".*EH_\\s*|_.*", "", files))
  climrep<-as.list(gsub(".*climrep-\\s*|_.*", "", files))
  #PFTs<-Map(cbind, outputfiles, scenario=scenarios) # adding extra column with scenario name 
  
  PFTs<-Map(cbind, outputfiles, scenario=scenarios, climrep = climrep) # adding extra column with scenario name 
  PFTs<-do.call("rbind", PFTs) # merging list into df
  
  
  PFTs<-select(PFTs,contains("Cover"), c("year", "Richness", "ShannonDiv", "Evenness", "scenario", "climrep"))
  PFTs<-select(PFTs,starts_with("mean"), c("year", "Richness", "ShannonDiv", "Evenness", "scenario", "climrep"))
  
  
  
  
  no<-c("meanRCover")
  PFTs<-PFTs[, !names(PFTs) %in% no, drop=F ] # drop =F means that it should be a df not a list
  
  
  
  return(PFTs)
}

PFTcoverall<-readfiles()

makeMeanCover <- function(PFTcoverall) {
  
  
  cover<-select(PFTcoverall,contains("Cover"), c("year", "scenario", "Richness", "ShannonDiv", "Evenness", "climrep"))
  cover<-select(cover,starts_with("mean"), c("year", "scenario", "Richness", "ShannonDiv", "Evenness"))
  
  cover<-cover[, !names(cover) %in% c("meanGtotalcover", "meanAtotalcover", "meanStotalcover"), drop=F ]
  
  cover <- cover %>% filter(year> 79) 
  
  cover<-melt(cover, id.vars=c("year", "scenario", "Richness", "ShannonDiv", "Evenness"))
  
  
  # take mean over all scenarios and climreps, here we want the mean cover of the last 20 years of simulation per scenario for each sub-PFT !!!
  
  cover<-aggregate(list(cover = cover$value, Richness=cover$Richness, diversity = cover$ShannonDiv), by = list(PFT = cover$variable, scenario=cover$scenario), FUN=mean)
  
  
  cover$type <- ifelse(grepl("(meanGCover)", cover$PFT),"Perennial", ifelse(grepl("(meanSCover)", cover$PFT),"Shrub", "Annual"))
  
  ## BE CAREFUL WITH RENAMING< AFTER >80 years there might not be all PFTs represented, not sure though if this really is the problem 
  # rename PFTs
  # cover$PFT<-as.character(cover$PFT) # this is important otherwise it will error
  # cover$PFT[cover$PFT=="meanACover0"]<-"Base_A"
  # 
  # cover$PFT[cover$PFT=="meanGCover0"]<-"Base"
  # cover$PFT[cover$PFT=="meanGCover1"]<-"Cb"
  # cover$PFT[cover$PFT=="meanGCover2"]<-"Cp"
  # cover$PFT[cover$PFT=="meanGCover3"]<-"Pr"
  # cover$PFT[cover$PFT=="meanGCover4"]<-"Pb"
  # cover$PFT[cover$PFT=="meanGCover5"]<-"Rb"
  # cover$PFT[cover$PFT=="meanGCover6"]<-"Rp"
  # cover$PFT[cover$PFT=="meanGCover7"]<-"Bp"
  # cover$PFT[cover$PFT=="meanGCover8"]<-"Bd"
  # 
  # 
  # 
  # cover$PFT[cover$PFT=="meanSCover0"]<-"Base"
  # cover$PFT[cover$PFT=="meanSCover1"]<-"Cb"
  # cover$PFT[cover$PFT=="meanSCover2"]<-"Rd"
  # cover$PFT[cover$PFT=="meanSCover3"]<-"Rc"
  # cover$PFT[cover$PFT=="meanSCover4"]<-"Bd"
  # cover$PFT[cover$PFT=="meanSCover5"]<-"Bc"
  # cover$PFT[cover$PFT=="meanSCover6"]<-"Bp"
  # cover$PFT[cover$PFT=="meanSCover7"]<-"Dc"
  # cover$PFT[cover$PFT=="meanSCover8"]<-"Db"
  # cover$PFT[cover$PFT=="meanSCover9"]<-"Dr"
  # cover$PFT[cover$PFT=="meanSCover10"]<-"Mb"
  # 
  # Cover  in percentage instead of 0-1  
  cover$cover<-cover$cover*100
  
  return(cover)
}

meanCover <- makeMeanCover(PFTcoverall)


# ===================================================
# ------- Plotting cover over time for all scenarios
# ===================================================

plotCoverOverTime<- function(PFTcoverall) {
  
  cover<-PFTcoverall[, c("year", "meanGtotalcover", "meanStotalcover", "meanAtotalcover", "scenario")]
  
  cover<-melt(cover, id.vars=c("year", "scenario"))
  
  cover$value <- cover$value*100
  
  cover$type <- ifelse(grepl("(meanGtotalcover)", cover$variable),"Perennial", ifelse(grepl("(meanStotalcover)", cover$variable),"Shrub", "Annual"))
  
  cover <-cover %>% group_by(scenario, year, type) %>% summarise_at(vars(value), funs(mean, sd))
  
  
  
  # cover$scenario<-as.character(cover$scenario)
  # cover$scenario[cover$scenario=="SR40graze"] <- "Cattle_low"
  # cover$scenario[cover$scenario=="SR20graze"] <- "Cattle_high"
  # #
  # cover$scenario[cover$scenario=="SR20browse"] <- "Wildlife_high"
  # cover$scenario[cover$scenario=="SR40browse"] <- "Wildlife_low"
  # 
  # cover$scenario<- factor(cover$scenario, levels=c("Cattle_low", "Wildlife_low",  "Cattle_high", "Wildlife_high" ))
  # scenario_list <-unique(cover$scenario)


  cover$scenario<-as.character(cover$scenario)
  cover$scenario[cover$scenario=="SR40graze"] <- 'Cattle low'
  cover$scenario[cover$scenario=="SR20graze"] <- 'Cattle high'
  #
  cover$scenario[cover$scenario=="SR20browse"] <- 'Wildlife high'
  cover$scenario[cover$scenario=="SR40browse"] <- 'Wildlife low'

  cover$scenario<- factor(cover$scenario, levels=c('Cattle low', 'Wildlife low',  'Cattle high', 'Wildlife high' ))

  
  cols<-c( "gold1", "seagreen", "coral")
  
  scenario_list <-unique(cover$scenario)
  
  # library()
  # 
  # cover_ren <-cover  %>%
  #   dplyr::mutate(scenario = dplyr::recode_factor(scenario,
  #                                                 "Cattle_low" = "Cattle low",
  #                                                 "Wildlife_high" = "Wildlife high",
  #                                                 "Cattle_high" = "Cattle high",
  #                                                 "Wildlife_low" = "Wildlife low"
  #   )) 
  #   
  # install.packages("forcats")
  # library(forcats)
  # cover_ren <- cover
  #   cover_ren$scenario <- fct_recode(cover_ren$scenario,
  #                                                 "Cattle low" = "Cattle_low",
  #                                                 "Wildlife high" = "Wildlife_high",
  #                                                 "Cattle high" = "Cattle_high",
  #                                                 "Wildlife low" = "Wildlife_low"
  #   )
  # 
  # scenario_list <-unique(cover_ren$scenario)  
  #   
  plot_list = list()
  
  for(i in 1:length(scenario_list)) {
  
    plot <- ggplot(subset(cover,scenario==scenario_list[i]), 
                aes(x=year, y=mean, colour=type)) + 
   geom_ribbon(aes(x= year, ymin = mean-sd, ymax = mean+sd), size=0.5,  fill= "lightgrey", alpha=0.5) +
    geom_line(size=1.2) +
    ylim(0, 100) +
    xlab("Years")+
    ylab(bquote("Cover [%]"))+
    scale_colour_manual(values=cols)+
    ggtitle(paste(scenario_list[i]))+
    theme_set(theme_minimal())+
    theme(axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          axis.title.y= element_text(size=14),
          axis.title.x=element_text(size=14),
          legend.text=element_text(size=12),
          plot.title = element_text(size = 16, face = "bold"), 
          legend.direction = "horizontal", legend.position = "none", legend.title = element_blank(), 
          legend.background = element_blank(),
          panel.grid.major = element_line(size = 0.2, linetype = 'solid', colour = "gray"),
          panel.background = element_blank())  +
    guides(col=guide_legend(nrow=1,byrow=TRUE))
  
    plotname <- paste0(gsub(" ", "_", scenario_list[i]), "_line")
    
    plot_list[[plotname]] <- plot
  
  
  }
  

plot_list$Cattle_low_line

#### Barplot of last 20 years ------------

cover<-meanCover

cover$PFT<- factor(cover$PFT, levels=c("meanACover0", "meanSCover0", "meanSCover1","meanSCover2", "meanSCover3", "meanSCover4","meanSCover5", "meanSCover6", "meanSCover7", "meanSCover8", "meanSCover9", "meanSCover10", "meanGCover0", "meanGCover1", "meanGCover2", "meanGCover3", "meanGCover4", "meanGCover5", "meanGCover6", "meanGCover7", "meanGCover8"))


cover$scenario<-as.character(cover$scenario)
cover$scenario[cover$scenario=="SR40graze"] <- "Cattle_low"
cover$scenario[cover$scenario=="SR20graze"] <- "Cattle_high"
# 
cover$scenario[cover$scenario=="SR20browse"] <- "Wildlife_high"
cover$scenario[cover$scenario=="SR40browse"] <- "Wildlife_low"

cover$scenario<- factor(cover$scenario, levels=c("Cattle_low", "Wildlife_low",  "Cattle_high", "Wildlife_high" ))
scenario_list <-unique(cover$scenario)

barplot_list = list()




for(i in 1:length(scenario_list)) {
  
  cover$type <- factor(cover$type, levels=c( "Shrub", "Perennial", "Annual"))
  cols<-c( "coral", "seagreen", "gold1")
    survival20<-ggplot(subset(cover, scenario==scenario_list[i]),
                       aes(y = cover, x=scenario, fill=type)) +
      geom_col(color="whitesmoke", lwd=0.28)+
      ylim(0, 100) +
      ylab(bquote("Mean cover"))+
      scale_fill_manual(values = cols)+
      theme_set(theme_minimal())+
      theme(axis.text.x = element_blank(),
            axis.text.y = element_text(size=10),
            axis.title.y= element_blank(),
            #axis.title.x=element_text(size=15),
            axis.title.x=element_blank(),
            #legend.text=element_text(size=16),
            legend.direction = "horizontal", legend.position = "none", legend.title = element_blank(), 
            legend.background = element_blank(),
            panel.grid.major = element_line(size = 0.2, linetype = 'solid', colour = "gray"),
            panel.background = element_blank()) 
          #ggtitle(paste(scenario_list[i], sep=''))
      
      barplot_list[[i]] <- survival20
}    

scen_list <- c("Cattle_low", "Wildlife_low",  "Cattle_high", "Wildlife_high" )

names(barplot_list) <- print(paste0(scenario_list,"_bar"))

barplot_list$Wildlife_high_bar
  
  # Arrange ggplot2 with adapted height and width for each row and column :
library(dplyr)

cattle_low_bar<-ggplot(subset(cover, scenario %in% "Cattle_low"),
                   aes(y = cover, x=scenario=="Cattle_low", fill=type)) +
  geom_col(color="whitesmoke", lwd=0.3)+
  ylim(0, 100) +
  ylab(bquote("Mean cover"))+
  #xlab("\nIncreasing landuse intensity")+
  scale_fill_manual(values = cols)+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size=12),
        axis.title.y= element_blank(),
        #axis.title.x=element_text(size=15),
        axis.title.x=element_blank(),
        legend.text=element_text(size=16),
        legend.direction = "horizontal", legend.position = "bottom", legend.title = element_blank(), 
        legend.background = element_blank(),
        legend.spacing.x = unit(0.3, 'cm'),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid', colour = "gray"),
        panel.background = element_blank()) 
  cattle_low_bar 
  
  get_legend<-function(cattle_low_bar){
    tmp <- ggplot_gtable(ggplot_build(cattle_low_bar))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
  }
  
  legend <- get_legend(cattle_low_bar)
  
  library("gridExtra")
  #blank <- grid.rect(gp=gpar(col="white"))
  # 
  # library(ggpubr)  
  # 
  #   cover_arranged <- grid.arrange(arrangeGrob(plot_list$Cattle_low_line,  barplot_list$Cattle_low_bar, plot_list$Wildlife_low_line, barplot_list$Wildlife_low_bar,  
  #                plot_list$Cattle_high_line, barplot_list$Cattle_high_bar, plot_list$Wildlife_high_line, barplot_list$Wildlife_high_bar,
  #                
  #                ncol=4,  nrow=2, 
  #                widths=c(4, 1.5, 4, 1.5), 
  #                heights=c(8, 8)), 
  #                legend, nrow =2, heights=c(10, 1)) 
    
    library(cowplot)
    
    coverplots <- plot_grid(plot_list$Cattle_low_line,  barplot_list$Cattle_low_bar, plot_list$Wildlife_low_line, barplot_list$Wildlife_low_bar,  
              plot_list$Cattle_high_line, barplot_list$Cattle_high_bar, plot_list$Wildlife_high_line, barplot_list$Wildlife_high_bar,
              ncol=4,  nrow=2, 
              rel_widths=c(4, 1.5, 4, 1.5), 
              align ="h", axis =bt
               )
  
    
   cover_legend<- plot_grid(coverplots, legend, nrow=2, rel_heights = c(1, 0.1)) 
  
   cover_legend
    
  
  
  ggsave(cover_legend, file="cover_comb_172.png", width = 32,
         height = 20,
         units = "cm", dpi=350)

  
  return(cover_legend)
  
  

}

plotCoverOverTime(PFTcoverall)


## STATS ------------------
cover<-PFTcoverall[, c("year", "meanGtotalcover", "meanStotalcover", "meanAtotalcover", "scenario", "Richness", "ShannonDiv")]

cover$TotalCover <- cover$meanGtotalcover + cover$meanStotalcover + cover$meanAtotalcover

cover<-melt(cover, id.vars=c("year", "scenario", "TotalCover"))

cover$value <- cover$value*100

cover$type <- ifelse(grepl("(meanGtotalcover)", cover$variable),"Perennial", ifelse(grepl("(meanStotalcover)", cover$variable),"Shrub", "Annual"))
meancover <-cover %>% group_by(scenario,type) %>% summarise_at(vars(value), funs(mean, sd))
mediancover <-cover %>% group_by(scenario) %>% summarise_at(vars(value), funs(median))


cover$intensity <- ifelse(grepl("(SR20)", cover$scenario),"high","low")
cover$landuse <- ifelse(grepl("(browse)", cover$scenario),"Wildlife","Cattle")

cover <- cover %>% filter(year> 79) 


if(!require(rcompanion)){install.packages("rcompanion")}
if(!require(FSA)){install.packages("FSA")}
library(rcompanion)
library(FSA)

scheirerRayHare(TotalCover ~ landuse + intensity, data = cover)

cover$landuse = factor(cover$landuse, levels = c("Wildlife", "Cattle"))
cover$intensity = factor(cover$intensity, levels = c("low", "high"))
# order by median from high to low 
cover$scenario <- factor(cover$scenario, levels =c("SR40browse", "SR20browse", "SR20graze", "SR40graze"))


DT <- dunnTest(TotalCover ~ scenario,  data = cover, method="bh")
DT

PT = DT$res

cldList(P.adj ~ Comparison,
        data = PT,
        threshold = 0.05)

## Effect size --------- 

epsilonSquared(x = cover$TotalCover, g = cover$landuse)


## Composition -------------


cover<-select(PFTcoverall,contains("Cover"), c("year", "scenario"))
cover<-select(cover,starts_with("mean"), c("year", "scenario"))

cover<-cover[, !names(cover) %in% c("meanGtotalcover", "meanAtotalcover", "meanStotalcover"), drop=F ]

cover <- cover %>% filter(year> 79) 

cover<-melt(cover, id.vars=c("year", "scenario"))

cover <- cover %>% 
  rename(
    PFT = variable, 
    cover =value
  )
cover$landuse <- ifelse(grepl("(browse)", cover$scenario),"Wildlife","Cattle")
cover$type <- ifelse(grepl("(meanGCover)", cover$PFT),"Perennial", ifelse(grepl("(meanSCover)", cover$PFT),"Shrub", "Annual"))



# rename PFTs
cover$PFT<-as.character(cover$PFT) # this is important otherwise it will error


cover$PFT[cover$PFT=="meanGCover7"]<-"Perennial_Bp"
cover$PFT[cover$PFT=="meanGCover8"]<-"Perennial_Bd"
cover$PFT[cover$PFT=="meanGCover1"]<-"Perennial_Cb"
cover$PFT[cover$PFT=="meanGCover2"]<-"Perennial_Cp"
cover$PFT[cover$PFT=="meanGCover3"]<-"Perennial_Pr"
cover$PFT[cover$PFT=="meanGCover4"]<-"Perennial_Pb"
cover$PFT[cover$PFT=="meanGCover5"]<-"Perennial_Rb"
cover$PFT[cover$PFT=="meanGCover6"]<-"Perennial_Rp"
cover$PFT[cover$PFT=="meanGCover0"]<-"Perennial_Base"


cover$PFT[cover$PFT=="meanSCover4"]<-"Shrub_Bd"
cover$PFT[cover$PFT=="meanSCover6"]<-"Shrub_Bp"
cover$PFT[cover$PFT=="meanSCover5"]<-"Shrub_Bc"
cover$PFT[cover$PFT=="meanSCover1"]<-"Shrub_Cb"
cover$PFT[cover$PFT=="meanSCover8"]<-"Shrub_Db"
cover$PFT[cover$PFT=="meanSCover9"]<-"Shrub_Dr"
cover$PFT[cover$PFT=="meanSCover7"]<-"Shrub_Dc"
cover$PFT[cover$PFT=="meanSCover10"]<-"Shrub_Mb"
cover$PFT[cover$PFT=="meanSCover2"]<-"Shrub_Rd"
cover$PFT[cover$PFT=="meanSCover3"]<-"Shrub_Rc"
cover$PFT[cover$PFT=="meanSCover0"]<-"Shrub_Base"

cover$PFT[cover$PFT=="meanACover0"]<-"Annual_Base"



scheirerRayHare(cover ~ PFT + landuse, data = cover)

DT <- dunnTest(cover ~ PFT,  data = cover, method="bh")
DT

PT = DT$res

cldList(P.adj ~ Comparison,
        data = PT,
        threshold = 0.05)


## Effect size --------- 

epsilonSquared(x = cover$cover, g = cover$landuse)


#### #### #### #### #### #### #### #### #### #### 
#### FD Matrices --------------------------------
#### #### #### #### #### #### #### #### #### #### 

# create scenario matrix ------

cover$PFT <-factor(cover$PFT, levels=c("Perennial_Bp",   "Perennial_Bd",   "Perennial_Cb" ,  "Perennial_Cp",   "Perennial_Pr"  ,
                   "Perennial_Pb" ,  "Perennial_Rb" ,  "Perennial_Rp" ,  "Perennial_Base", "Shrub_Bd",      
                   "Shrub_Bp" ,      "Shrub_Bc"    ,   "Shrub_Cb"    ,   "Shrub_Db"    ,   "Shrub_Dr"      ,
                   "Shrub_Dc"    ,   "Shrub_Mb"   ,    "Shrub_Rd"   ,    "Shrub_Rc"    ,   "Shrub_Base"  ,  
                   "Annual_Base" ))

meancover <-cover %>% group_by(scenario,landuse, PFT) %>% summarise_at(vars(cover), funs(mean))

meancover$cover <- meancover$cover*100


Wildlife <- meancover[meancover$landuse=="Wildlife", ]
Cattle <- meancover[meancover$landuse=="Cattle", ]

Wildlife<-Wildlife[, -2]
Cattle<-Cattle[, -2]

Wild_mat <- spread(Wildlife, PFT, cover)

Wild_Mat <- data.matrix(Wild_mat, rownames.force=NA)
Wild_Mat <- Wild_Mat[, -1]
rownames(Wild_Mat) <- Wild_mat$scenario


Wild_mat2 <- replace(Wild_Mat, Wild_Mat < 0.02, 0 )

Wild_mat2_updated <- Wild_mat2[, -c("Shrub_Bd", "Shrub_Bc", "Shrub_Cb", "Shrub_Dr", "Shrub_Mb", "Shrub_Base")]

Wild_mat2_updated <- Wild_mat2[, -c(10:13, 15, 17, 20)]


write.table(Wild_Mat, file="wildlife_matrix.txt", sep="\t", row.names=T)

# read in trait matrix -------

# wildlife --
wild_trait <- read.table(file="trait_matrix_wildlife.txt", header=T)

wild_trait_up <- wild_trait[-c(10:13, 15, 17, 20), ]

library(FD)

wild_FD <- dbFD(wild_trait, Wild_Mat, messages =T)

wild_FD2 <- dbFD(wild_trait_up, Wild_mat2_updated, messages =T)

wild_clust <- dbFD(wild_trait_up, Wild_mat2_updated, corr = "cailliez",
                   calc.FGR = TRUE, clust.type = "ward" )


# cattle -- 

cattle_mat <- spread(Cattle, PFT, cover)

Cattle_Mat <- data.matrix(cattle_mat, rownames.force=NA)
Cattle_Mat <- Cattle_Mat[, -1]
rownames(Cattle_Mat) <- cattle_mat$scenario

Cattle_Mat2 <- replace(Cattle_Mat, Cattle_Mat < 2.0 , 0 )

Cattle_Mat2 <- Cattle_Mat2[, -c(1:3, 6:7, 20)]

write.table(Cattle_Mat, file="cattle_matrix.txt", sep="\t", row.names=T, quote=F)


cattle_trait <- read.table(file="trait_matrix_cattle.txt", header=T)


cattle_trait <- cattle_trait[ -c(1:3, 6:7, 20), ]

cattle_FD <- dbFD(cattle_trait, Cattle_Mat, messages =T)

cattle_FD2 <- dbFD(cattle_trait, Cattle_Mat2, messages =T)

clust <- dbFD(cattle_trait, Cattle_Mat2, corr = "cailliez",
              calc.FGR = TRUE, clust.type = "ward")

clust2 <- dbFD(cattle_trait, Cattle_Mat2, corr = "cailliez",
              calc.FGR = TRUE, clust.type = "kmeans", km.sup.gr = 4)



