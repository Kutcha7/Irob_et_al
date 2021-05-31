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
library(gtable)
library(cowplot)
library(data.table)



setwd("~/Documents/Strategy/Results/Setup January/20-40/Feb16/Appendix/")
path <- ("~/Documents/Strategy/Results/Setup January/20-40/Feb16/Appendix/")

####reading in all outputfiles returned as one dataframe 
readfiles<- function(path="~/Documents/Strategy/Results/Setup January/20-40/Feb16/Appendix/") {
  files<-list.files(path= path, pattern="yearly")
  
  outputfiles<-lapply(files, function(x) {
    read.table(file=x, header=T, skipNul = TRUE) 
  })
  
  
  scenarios<-as.list(gsub(".*EH_\\s*|_.*", "", files))
  climrep<-as.list(gsub(".*climrep-\\s*|_.*", "", files))
  
  PFTs<-Map(cbind, outputfiles, scenario=scenarios, climrep = climrep) # adding extra column with scenario name 
  PFTs<-do.call("rbind", PFTs) # merging list into df
  
  
  PFTs<-select(PFTs,contains("Cover"), c("year", "Richness", "ShannonDiv", "Evenness", "scenario", "climrep"))
  PFTs<-select(PFTs,starts_with("mean"), c("year", "Richness", "ShannonDiv", "Evenness", "scenario", "climrep"))
  
  
  
  
  no<-c("meanRCover")
  PFTs<-PFTs[, !names(PFTs) %in% no, drop=F ] # drop =F means that it should be a df not a list
  
  
  
  return(PFTs)
}

PFTcoverall<-readfiles()




cover<-select(PFTcoverall,contains("Cover"), c("year", "scenario", "Richness", "ShannonDiv", "Evenness", "climrep"))
cover<-select(cover,starts_with("mean"), c("year", "scenario", "Richness", "ShannonDiv", "Evenness"))

cover<-cover[, !names(cover) %in% c("meanGtotalcover", "meanAtotalcover", "meanStotalcover"), drop=F ]

cover <- cover %>% filter(year> 79) 

cover<-melt(cover, id.vars=c("year", "scenario", "Richness", "ShannonDiv", "Evenness"))

cover <- cover %>% 
  rename(
    PFT = variable, 
    cover =value
  )


cover$type <- ifelse(grepl("(meanGCover)", cover$PFT),"Perennial", ifelse(grepl("(meanSCover)", cover$PFT),"Shrub", "Annual"))

cover <- cover[!cover$type=="Annual", ]

cover$cover<-cover$cover*100


# rename scenarios 
cover$scenario<-as.character(cover$scenario)
cover$scenario[cover$scenario=="SR50graze"] <- 'Cattle very low'
cover$scenario[cover$scenario=="SR10graze"] <- 'Cattle very high'
cover$scenario[cover$scenario=="SR30graze"] <- 'Cattle medium'
#
cover$scenario[cover$scenario=="SR10browse"] <- 'Wildlife very high'
cover$scenario[cover$scenario=="SR30browse"] <- 'Wildlife medium'
cover$scenario[cover$scenario=="SR50browse"] <- 'Wildlife very low'


cover$scenario<- factor(cover$scenario, levels=c('Cattle very low', 'Wildlife very low', "Cattle medium", "Wildlife medium",  'Cattle very high', 'Wildlife very high' ))



# rename PFTs
cover$PFT<-as.character(cover$PFT) # this is important otherwise it will error
#cover$PFT[cover$PFT=="meanACover0"]<-"Base_A"

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
# perennials ----
perennials<-cover[cover$type=="Perennial", ]

library(dplyr)

#cover_surv <- cover %>% dplyr::filter(cover > 1)

perennials_agg <- perennials %>% group_by(PFT, scenario, type ) %>% summarise_at(vars(cover), funs(mean, max, min, sd))

shrubs <- as.data.table(cover[cover$type=="Shrub", ])

shrubs_agg <- shrubs %>% group_by(PFT, scenario, type ) %>% summarise_at(vars(cover), funs(mean, max, min, sd))




shrubs_agg$treatment <- shrubs_agg$scenario
shrubs_agg$treatment <- as.character(shrubs_agg$treatment )
shrubs_agg$treatment [shrubs_agg$treatment =="Cattle very low"] <- "Cattle"
shrubs_agg$treatment [shrubs_agg$treatment =="Cattle medium"] <- "Cattle"
shrubs_agg$treatment [shrubs_agg$treatment =="Cattle very high"] <- "Cattle"
#
shrubs_agg$treatment [shrubs_agg$treatment =="Wildlife very low"] <- "Wildlife"
shrubs_agg$treatment [shrubs_agg$treatment =="Wildlife medium"] <- "Wildlife"
shrubs_agg$treatment [shrubs_agg$treatment =="Wildlife very high"] <- "Wildlife"

shrubs_agg$scenario<- factor(shrubs_agg$scenario, levels=c("Cattle very high",  "Cattle medium", "Cattle very low", "Wildlife very high", "Wildlife medium", "Wildlife very low"  ))


shrubs_agg$treatment <- as.factor(shrubs_agg$treatment )


shrubs_agg$effect <- shrubs_agg$scenario
shrubs_agg$effect <- as.character(shrubs_agg$effect )
shrubs_agg$effect [shrubs_agg$effect =="Cattle very low"] <- "low"
shrubs_agg$effect [shrubs_agg$effect =="Cattle medium"] <- "medium"
shrubs_agg$effect [shrubs_agg$effect =="Cattle very high"] <- "very high"
# 
shrubs_agg$effect [shrubs_agg$effect =="Wildlife very low"] <- "low"
shrubs_agg$effect [shrubs_agg$effect =="Wildlife medium"] <- "medium"
shrubs_agg$effect [shrubs_agg$effect =="Wildlife very high"] <- "high"

shrubs_agg$effect <- as.factor(shrubs_agg$effect )

shrubs_agg$effect <- factor(shrubs_agg$effect, levels=c("low", "medium", "high") )


shrubs_agg$PFT2 <-  paste(shrubs_agg$PFT, shrubs_agg$effect) 

namesShrubs <- c("Base", "Base", "Cb", "Cb", "Rd", "Rd", "Rc", "Rc", "Bd", "Bd",  "Bc", "Bc", "Bp", "Bp",  "Dc", "Dc", "Db", "Db", "Dr", "Dr", "Mb", "Mb")
namesShrubs <- c("Base", "", "Cb", "", "Rd", "", "Rc", "", "Bd", "",  "Bc", "", "Bp", "",  "Dc", "", "Db", "", "Dr", "", "Mb", "")

shrubs_agg$treatment <- factor(shrubs_agg$treatment, levels=c("Cattle", "Wildlife") )

cols <-c("darksalmon", "darkseagreen1", "chocolate1", "darkseagreen3", "brown4",    "darkgreen")

shrubs_by_strat_bytreatment_swap<-ggplot(shrubs_agg, aes(y = mean, x = PFT, fill=scenario)) + 
  geom_bar(stat="identity", color="lightgray", width=0.7, position=position_dodge(width=0.7)) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2, position=position_dodge(0.8))+
  ylab(bquote("\nMean cover [%]"))+
  xlab("\nStrategy")+
  ggtitle("Shrubs") +
  coord_flip()+
  #scale_x_reverse() +
  scale_y_continuous(limits = c(0, 10), breaks=c(0,  5, 10)) +
  #scale_fill_discrete(breaks = rev(levels(shrubs_agg$scenario)), col=c( "coral",  "coral4",  "cyan", "seagreen"))+
  scale_fill_manual(breaks = c('Cattle very low', "Cattle medium",  'Cattle very high',  'Wildlife very low', "Wildlife medium", 'Wildlife very high' ), values = c("darksalmon", "chocolate1", "brown4",  "darkseagreen1", "darkseagreen3",  "darkgreen"  ))+ # "coral", "coral4", "cyan",  "seagreen"
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

  

perennials_agg$scenario<- factor(perennials_agg$scenario, levels=c("Cattle very high",  "Cattle medium", "Cattle very low", "Wildlife very high", "Wildlife medium", "Wildlife very low"  ))


perennials_agg$treatment <- perennials_agg$scenario
perennials_agg$treatment <- as.character(perennials_agg$treatment )
perennials_agg$treatment [perennials_agg$treatment =="Cattle very low"] <- "Cattle"
perennials_agg$treatment [perennials_agg$treatment =="Cattle medium"] <- "Cattle"
perennials_agg$treatment [perennials_agg$treatment =="Cattle very high"] <- "Cattle"
#
perennials_agg$treatment [perennials_agg$treatment =="Wildlife very low"] <- "Wildlife"
perennials_agg$treatment [perennials_agg$treatment =="Wildlife medium"] <- "Wildlife"
perennials_agg$treatment [perennials_agg$treatment =="Wildlife very high"] <- "Wildlife"


perennials_agg$effect <- perennials_agg$scenario
perennials_agg$effect <- as.character(perennials_agg$effect )
perennials_agg$effect [perennials_agg$effect =="Cattle very low"] <- "low"
perennials_agg$effect [perennials_agg$effect =="Cattle medium"] <- "medium"
perennials_agg$effect [perennials_agg$effect =="Cattle very high"] <- "high"
# 
perennials_agg$effect [perennials_agg$effect =="Wildlife very low"] <- "low"
perennials_agg$effect [perennials_agg$effect =="Wildlife medium"] <- "medium"
perennials_agg$effect [perennials_agg$effect =="Wildlife very high"] <- "high"


perennials_agg$treatment <- as.factor(perennials_agg$treatment )

perennials_agg$effect <- as.factor(perennials_agg$effect )


perennials_agg$PFT2 <-  paste(perennials_agg$PFT, perennials_agg$effect) 



# namesPer <- c("Base", "Base", "Cb","Cb", "Cp", "Cp", "Pr","Pr", "Pb", "Pb", "Rb", "Rb", "Rp", "Rp", "Bp", "Bp", "Bd", "Bd")
namesPer <- c("Base","", "Cb","", "Cp","", "Pr","", "Pb", "", "Rb", "",  "Rp","",  "Bp", "", "Bd", "")

perennials_by_strat_bytreatment_swap<-ggplot(perennials_agg, aes(y = mean, x = PFT, fill=scenario)) + 
  geom_bar(stat="identity", color="lightgray", width=0.7, position=position_dodge(width=0.7)) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2, position=position_dodge(0.8))+
  # geom_bar(stat="identity", color="lightgray", width=0.9, position=position_dodge(width=0.7)) +
  # geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2, position=position_dodge(.7))+
  ylab(bquote("\nMean cover [%]"))+
  xlab("\nStrategy")+
  ggtitle("Perennials") +
  #scale_x_discrete(labels=namesPer)+
  coord_flip()+
  #scale_x_reverse() +
  scale_y_continuous(limits = c(0, 32), breaks=c(0, 5, 10, 15, 20, 25, 30)) + 
  scale_fill_manual(breaks = c('Cattle very low', "Cattle medium",  'Cattle very high',  'Wildlife very low', "Wildlife medium", 'Wildlife very high' ), values = c("darksalmon", "chocolate1", "brown4",  "darkseagreen1", "darkseagreen3",  "darkgreen"  ))+ # "coral", "coral4", "cyan",  "seagreen"
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


## make universal legend ---

perennials_agg$scenario<- factor(perennials_agg$scenario, levels=c("Cattle very low" , "Wildlife very low", "Cattle medium", "Wildlife medium" ,"Cattle very high", "Wildlife very high" ))


perennials_by_strat_legend<-ggplot(perennials_agg, aes(y = mean, x = PFT, fill=scenario)) + 
  #geom_col(color="lightgray", lwd=0.03)+
  geom_bar(stat="identity", color="lightgray", width=0.9, position=position_dodge(width=0.7)) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2, position=position_dodge(.7))+
  ylab(bquote("\nMean cover [%]"))+
  xlab("\nStrategy")+
  ggtitle("Perennials") +
  #scale_x_discrete(labels=namesPer)+
  coord_flip()+
  scale_y_continuous(limits = c(0, 13), breaks=c(0, 5, 10)) + 
  scale_fill_manual(breaks = c("Cattle very low" , "Wildlife very low", "Cattle medium", "Wildlife medium" ,"Cattle very high", "Wildlife very high"), values = c( "coral4",  "coral",  "seagreen", "cyan"))+
  scale_fill_manual(values = cols)+
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


perennials_by_strat_bytreatment_swap <- perennials_by_strat_bytreatment_swap + theme(legend.position="none")
shrubs_by_strat_bytreatment_swap <- shrubs_by_strat_bytreatment_swap + theme(legend.position="none")


library(cowplot)

compplots <- plot_grid(perennials_by_strat_bytreatment_swap, shrubs_by_strat_bytreatment_swap,
                        ncol=2,  nrow=1, 
                        rel_widths=c(7, 7, 7, 7), 
                        align ="h", axis =bt, 
                       labels=c("A", "B")
)

comp_legend<- plot_grid(compplots, legend, nrow=2, rel_heights = c(1, 0.1))
comp_legend             

ggsave(comp_legend, file="composition_comb_115.png", width = 32,
       height = 20,
       units = "cm", dpi=500)
