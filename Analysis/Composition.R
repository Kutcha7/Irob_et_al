# ==============================================
# ------- Model output anlaysis: composition
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
library(gtable)
library(cowplot)
library(data.table)
library(cowplot)

# setting working directory --
paths <- here::here("Data/Results/")

####reading in all outputfiles returned as one dataframe 
readfiles<- function(path=paths) {
  files<- list.files(path = here::here("Data/Results/"), pattern="yearly", full.names = T)
  
  outputfiles<-lapply(files, function(x) {
    read.table(file=x, header=T, skipNul = TRUE) 
  })
  
  
  scenarios<-as.list(gsub(".*EH_\\s*|_.*", "", files))
  climrep<-as.list(gsub(".*climrep-\\s*|_.*", "", files))
  
  PFTs<-Map(cbind, outputfiles, scenario=scenarios, climrep = climrep) # adding extra column with scenario name 
  PFTs<-do.call("rbind", PFTs) # merging list into df
  
  
  PFTs<-select(PFTs,contains("Cover"), c("year", "scenario", "climrep")) # extracting only parameters of interest 
  PFTs<-select(PFTs,starts_with("mean"), c("year", "scenario", "climrep"))
  
  
  no<-c("meanRCover")
  PFTs<-PFTs[, !names(PFTs) %in% no, drop=F ] # drop =F means that it should be a df not a list
  
  
  return(PFTs)
}

PFTcoverall<-readfiles()

# bringing the df in the right format -------------- 

cover<-select(PFTcoverall,contains("Cover"), c("year", "scenario", "climrep"))
cover<-select(cover,starts_with("mean"), c("year", "scenario"))

cover<-cover[, !names(cover) %in% c("meanGtotalcover", "meanAtotalcover", "meanStotalcover"), drop=F ]

cover <- cover %>% filter(year> 79) # only include last 20 years of simulation 

cover<-melt(cover, id.vars=c("year", "scenario"))

cover <- cover %>% # rename melted columns
  rename(
    PFT = variable, 
    cover =value
  )


cover$type <- ifelse(grepl("(meanGCover)", cover$PFT),"Perennial", ifelse(grepl("(meanSCover)", cover$PFT),"Shrub", "Annual"))

cover <- cover[!cover$type=="Annual", ] # remove annuals

cover$cover<-cover$cover*100 # percentage

# rename scenarios 
cover$scenario<-as.character(cover$scenario)
cover$scenario[cover$scenario=="SR40graze"] <- paste("Cattle low")
cover$scenario[cover$scenario=="SR20graze"] <- paste("Cattle high")
# 
cover$scenario[cover$scenario=="SR40browse"] <- paste("Wildlife low")
cover$scenario[cover$scenario=="SR20browse"] <- paste ("Wildlife high")

cover$scenario = factor(cover$scenario, levels=c('Cattle high','Cattle low','Wildlife high','Wildlife low'))


# rename PFTs
cover$PFT<-as.character(cover$PFT) # this is important otherwise it will error

cover$PFT[cover$PFT=="meanGCover0"]<-"Base"
cover$PFT[cover$PFT=="meanGCover1"]<-"Cb"
cover$PFT[cover$PFT=="meanGCover2"]<-"Cp"
cover$PFT[cover$PFT=="meanGCover3"]<-"Pr"
cover$PFT[cover$PFT=="meanGCover4"]<-"Pb"
cover$PFT[cover$PFT=="meanGCover5"]<-"Rb"
cover$PFT[cover$PFT=="meanGCover6"]<-"Rp"
cover$PFT[cover$PFT=="meanGCover7"]<-"Bp"
cover$PFT[cover$PFT=="meanGCover8"]<-"Bd"



cover$PFT[cover$PFT=="meanSCover0"]<-"Base"
cover$PFT[cover$PFT=="meanSCover1"]<-"Cb"
cover$PFT[cover$PFT=="meanSCover2"]<-"Rd"
cover$PFT[cover$PFT=="meanSCover3"]<-"Rc"
cover$PFT[cover$PFT=="meanSCover4"]<-"Bd"
cover$PFT[cover$PFT=="meanSCover5"]<-"Bc"
cover$PFT[cover$PFT=="meanSCover6"]<-"Bp"
cover$PFT[cover$PFT=="meanSCover7"]<-"Dc"
cover$PFT[cover$PFT=="meanSCover8"]<-"Db"
cover$PFT[cover$PFT=="meanSCover9"]<-"Dr"
cover$PFT[cover$PFT=="meanSCover10"]<-"Mb"

namesShrubs <- c("Base", "Base", "Cb", "Cb", "Rd", "Rd", "Rc", "Rc", "Bd", "Bd",  "Bc", "Bc", "Bp", "Bp",  "Dc", "Dc", "Db", "Db", "Dr", "Dr", "Mb", "Mb")

namesPer <- c("Base", "Base")

## separate by perennials and shrubs -----------

# shrubs ---------------------------------
shrubs <- as.data.table(cover[cover$type=="Shrub", ])

shrubs_agg <- shrubs %>% group_by(PFT, scenario, type ) %>% summarise_at(vars(cover), funs(mean, max, min, sd))


shrubs_agg$treatment <- shrubs_agg$scenario
shrubs_agg$treatment <- as.character(shrubs_agg$treatment )
shrubs_agg$treatment [shrubs_agg$treatment =="Cattle low"] <- "Cattle"
shrubs_agg$treatment [shrubs_agg$treatment =="Cattle high"] <- "Cattle"
#
shrubs_agg$treatment [shrubs_agg$treatment =="Wildlife low"] <- "Wildlife"
shrubs_agg$treatment [shrubs_agg$treatment =="Wildlife high"] <- "Wildlife"

shrubs_agg$scenario<- factor(shrubs_agg$scenario, levels=c("Cattle high",  "Cattle low", "Wildlife high", "Wildlife low"  ))


shrubs_agg$treatment <- as.factor(shrubs_agg$treatment )


shrubs_agg$effect <- shrubs_agg$scenario
shrubs_agg$effect <- as.character(shrubs_agg$effect )
shrubs_agg$effect [shrubs_agg$effect =="Cattle low"] <- "low"
shrubs_agg$effect [shrubs_agg$effect =="Cattle high"] <- "high"
# 
shrubs_agg$effect [shrubs_agg$effect =="Wildlife high"] <- "low"
shrubs_agg$effect [shrubs_agg$effect =="Wildlife low"] <- "high"

shrubs_agg$effect <- as.factor(shrubs_agg$effect )

shrubs_agg$effect <- factor(shrubs_agg$effect, levels=c("low", "high") )


shrubs_agg$PFT2 <-  paste(shrubs_agg$PFT, shrubs_agg$effect) 

namesShrubs <- c("Base", "Base", "Cb", "Cb", "Rd", "Rd", "Rc", "Rc", "Bd", "Bd",  "Bc", "Bc", "Bp", "Bp",  "Dc", "Dc", "Db", "Db", "Dr", "Dr", "Mb", "Mb")
namesShrubs <- c("Base", "", "Cb", "", "Rd", "", "Rc", "", "Bd", "",  "Bc", "", "Bp", "",  "Dc", "", "Db", "", "Dr", "", "Mb", "")

shrubs_agg$treatment <- factor(shrubs_agg$treatment, levels=c("Cattle", "Wildlife") )


shrubs_by_strat_bytreatment_swap<-ggplot(shrubs_agg, aes(y = mean, x = PFT, fill=scenario)) + 
  geom_bar(stat="identity", color="lightgray", width=0.7, position=position_dodge(width=0.7)) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2, position=position_dodge(0.8))+
  ylab(bquote("\nMean cover [%]"))+
  xlab("\nStrategy")+
  ggtitle("Shrubs") +
  coord_flip()+
  scale_y_continuous(limits = c(0, 10), breaks=c(0,  5, 10)) +
  scale_fill_manual(breaks = c("Cattle low", "Cattle high", "Wildlife low", "Wildlife high"), values = c(  "coral", "coral4", "cyan",  "seagreen"))+
  facet_wrap(.~treatment, ncol=4)+
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=13, face = "bold"),
        axis.title.y= element_text(size=14),
        axis.title.x=element_text(size=14),
        strip.text.x = element_text(size = 16),
        strip.text.y = element_text(size = 16),
        legend.text=element_text(size=14),
        plot.title = element_text(size = 16, face = "bold"), 
        legend.direction = "horizontal", legend.position = "bottom", legend.title = element_blank(), 
        legend.spacing.x = unit(0.3, 'cm'),
        legend.background = element_blank(),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid', colour = "gray"),
        panel.background = element_blank()) 

shrubs_by_strat_bytreatment_swap

# perennials -----------------------
perennials<-cover[cover$type=="Perennial", ]

perennials_agg <- perennials %>% group_by(PFT, scenario, type ) %>% summarise_at(vars(cover), funs(mean, max, min, sd))

perennials_agg$scenario<- factor(perennials_agg$scenario, levels=c("Cattle high",  "Cattle low", "Wildlife high", "Wildlife low"  ))
perennials_agg$treatment <- perennials_agg$scenario
perennials_agg$treatment <- as.character(perennials_agg$treatment )
perennials_agg$treatment [perennials_agg$treatment =="Cattle low"] <- "Cattle"
perennials_agg$treatment [perennials_agg$treatment =="Cattle high"] <- "Cattle"
# 
perennials_agg$treatment [perennials_agg$treatment =="Wildlife low"] <- "Wildlife"
perennials_agg$treatment [perennials_agg$treatment =="Wildlife high"] <- "Wildlife"

perennials_agg$treatment <- as.factor(perennials_agg$treatment )


perennials_agg$effect <- perennials_agg$scenario
perennials_agg$effect <- as.character(perennials_agg$effect )
perennials_agg$effect [perennials_agg$effect =="Cattle low"] <- "low"
perennials_agg$effect [perennials_agg$effect =="Cattle high"] <- "high"
# 
perennials_agg$effect [perennials_agg$effect =="Wildlife high"] <- "low"
perennials_agg$effect [perennials_agg$effect =="Wildlife low"] <- "high"

perennials_agg$effect <- as.factor(perennials_agg$effect )


perennials_agg$PFT2 <-  paste(perennials_agg$PFT, perennials_agg$effect) 

namesPer <- c("Base","", "Cb","", "Cp","", "Pr","", "Pb", "", "Rb", "",  "Rp","",  "Bp", "", "Bd", "")

perennials_by_strat_bytreatment_swap<-ggplot(perennials_agg, aes(y = mean, x = PFT, fill=scenario)) + 
  geom_bar(stat="identity", color="lightgray", width=0.7, position=position_dodge(width=0.7)) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2, position=position_dodge(0.8))+
  ylab(bquote("\nMean cover [%]"))+
  xlab("\nStrategy")+
  ggtitle("Perennials") +
  coord_flip()+
  scale_y_continuous(limits = c(0, 32), breaks=c(0, 5, 10, 15, 20, 25, 30)) + 
  scale_fill_manual(breaks = c("Cattle low", "Cattle high", "Wildlife low", "Wildlife high"), values = c(  "coral", "coral4", "cyan",  "seagreen"))+
  facet_wrap(.~treatment, ncol=4)+
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=13, face = "bold"),
        axis.title.y= element_text(size=14),
        axis.title.x=element_text(size=14),
        strip.text.x = element_text(size = 16),
        strip.text.y = element_text(size = 16),
        legend.text=element_text(size=14),
        plot.title = element_text(size = 16, face = "bold"), 
        legend.direction = "horizontal", legend.position = "bottom", legend.title = element_blank(), 
        legend.spacing.x = unit(0.3, 'cm'),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid', colour = "gray"),
        panel.background = element_blank()) 
perennials_by_strat_bytreatment_swap


## make universal legend with scenarios in right order -----

perennials_agg$scenario<- factor(perennials_agg$scenario, levels=c("Cattle low" , "Wildlife low", "Cattle high", "Wildlife high" ))

perennials_by_strat_legend<-ggplot(perennials_agg, aes(y = mean, x = PFT, fill=scenario)) + 
  geom_bar(stat="identity", color="lightgray", width=0.9, position=position_dodge(width=0.7)) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2, position=position_dodge(.7))+
  ylab(bquote("\nMean cover [%]"))+
  xlab("\nStrategy")+
  ggtitle("Perennials") +
  coord_flip()+
  scale_y_continuous(limits = c(0, 13), breaks=c(0, 5, 10)) + 
  scale_fill_manual(breaks = c("Cattle low",  "Wildlife low", "Cattle high", "Wildlife high"), values = c( "coral", "cyan","coral4",  "seagreen"))+
  facet_wrap(.~treatment, ncol=4)+
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=13, face = "bold"),
        axis.title.y= element_text(size=14),
        axis.title.x=element_text(size=14),
        strip.text.x = element_text(size = 16),
        strip.text.y = element_text(size = 16),
        legend.text=element_text(size=14),
        plot.title = element_text(size = 16, face = "bold"), 
        legend.direction = "horizontal", legend.position = "bottom", legend.title = element_blank(), 
        legend.spacing.x = unit(0.3, 'cm'),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid', colour = "gray"),
        panel.background = element_blank()) 
perennials_by_strat_legend

legend <- get_legend(perennials_by_strat_bytreatment_swap)


# combine all plots together -----------------

perennials_by_strat_bytreatment_swap <- perennials_by_strat_bytreatment_swap + theme(legend.position="none")
shrubs_by_strat_bytreatment_swap <- shrubs_by_strat_bytreatment_swap + theme(legend.position="none")



compplots <- plot_grid(perennials_by_strat_bytreatment_swap, shrubs_by_strat_bytreatment_swap,
                        ncol=2,  nrow=1, 
                        rel_widths=c(7, 7, 7, 7), 
                        align ="h", axis =bt, 
                       labels=c("a", "b")
)

comp_legend<- plot_grid(compplots, legend, nrow=2, rel_heights = c(1, 0.1))
comp_legend             


ggsave(comp_legend, file="composition_comb_76.png", width = 32,
       height = 20,
       units = "cm", dpi=500)


##################################################
## Composition stats -----------------------------
##################################################

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

# non-parametric Scheirer-Ray-Hay-Test ---------------------

# total comp
scheirerRayHare(cover ~ PFT + landuse, data = cover)
# H = 19778.7, p = 0
DT <- dunnTest(cover ~ PFT,  data = cover, method="bh")
DT

PT = DT$res

cldList(P.adj ~ Comparison,
        data = PT,
        threshold = 0.05)

# perennials --

scheirerRayHare(cover ~ PFT + scenario, data = perennials)
# 2388.4, p = 0

DT <- dunnTest(cover ~ PFT,  data = perennials, method="bh")

# shrubs --

scheirerRayHare(cover ~ PFT + scenario, data = shrubs)
# 3163.7, p = 0

DT <- dunnTest(cover ~ PFT,  data = shrubs, method="bh")


## Effect size --------- 

# order by median from high to low 
cover$scenario <- factor(cover$scenario, levels =c("SR40browse", "SR20browse", "SR20graze", "SR40graze"))

epsilonSquared(x = cover$cover, g = cover$scenario)

# perennials --- 
epsilonSquared(x = perennials$cover, g = perennials$scenario)

# shrubs --- 
epsilonSquared(x = shrubs$cover, g = shrubs$scenario)

