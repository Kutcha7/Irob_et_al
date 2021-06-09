# ==============================================
# ------- Model run, input & output manipulation
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

source(here::here("R/Utility.R"))

PFTcoverall<-readfiles(path = "Data/Results/Appendix")

# bringing the df in the right format -------------- 

cover<-select(PFTcoverall,contains("Cover"), c("year", "scenario", "climrep"))
cover<-select(cover,starts_with("mean"), c("year", "scenario"))

cover<-cover[, !names(cover) %in% c("meanGtotalcover", "meanAtotalcover", "meanStotalcover"), drop=F ]

cover <- cover %>% filter(year> 79) # only include last 20 years of simulation 

cover<-reshape2::melt(cover, id.vars=c("year", "scenario"))

cover <- cover %>% # rename melted columns
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

cover <- renamePFT_grass(cover)
cover <- renamePFT_shrub(cover)

namesShrubs <- c("Base", "Base", "Cb", "Cb", "Rd", "Rd", "Rc", "Rc", "Bd", "Bd",  "Bc", "Bc", "Bp", "Bp",  "Dc", "Dc", "Db", "Db", "Dr", "Dr", "Mb", "Mb")

namesPer <- c("Base", "Base")
## separate by perennials and shrubs -----------

# shrubs ---------------------------------
shrubs <- as.data.table(cover[cover$type=="Shrub", ])

shrubs_agg <- shrubs %>% group_by(PFT, scenario, type ) %>% summarise_at(vars(cover), funs(mean, sd))


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

namesShrubs <- c("Base", "", "Cb", "", "Rd", "", "Rc", "", "Bd", "",  "Bc", "", "Bp", "",  "Dc", "", "Db", "", "Dr", "", "Mb", "")

shrubs_agg$treatment <- factor(shrubs_agg$treatment, levels=c("Cattle", "Wildlife") )


# make plot -- 
shrubs_by_strat_bytreatment_swap<-ggplot(shrubs_agg, aes(y = mean, x = PFT, fill=scenario)) + 
  geom_bar(stat="identity", color="lightgray", width=0.7, position=position_dodge(width=0.7)) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2, position=position_dodge(0.8))+
  ylab(bquote("\nMean cover [%]"))+
  xlab("\nStrategy")+
  ggtitle("Shrubs") +
  coord_flip()+
  scale_y_continuous(limits = c(0, 10), breaks=c(0,  5, 10)) +
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


# perennials -----------------------
perennials<-cover[cover$type=="Perennial", ]

perennials_agg <- perennials %>% group_by(PFT, scenario, type ) %>% summarise_at(vars(cover), funs(mean, max, min, sd))

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

# renaming strategy types for plot
namesPer <- c("Base","", "Cb","", "Cp","", "Pr","", "Pb", "", "Rb", "",  "Rp","",  "Bp", "", "Bd", "")


# make plot --
perennials_by_strat_bytreatment_swap<-ggplot(perennials_agg, aes(y = mean, x = PFT, fill=scenario)) + 
  geom_bar(stat="identity", color="lightgray", width=0.7, position=position_dodge(width=0.7)) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2, position=position_dodge(0.8))+
ylab(bquote("\nMean cover [%]"))+
  xlab("\nStrategy")+
  ggtitle("Perennials") +
  coord_flip()+
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
  geom_bar(stat="identity", color="lightgray", width=0.9, position=position_dodge(width=0.7)) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2, position=position_dodge(.7))+
  ylab(bquote("\nMean cover [%]"))+
  xlab("\nStrategy")+
  ggtitle("Perennials") +
  coord_flip()+
  scale_y_continuous(limits = c(0, 13), breaks=c(0, 5, 10)) + 
  scale_fill_manual(breaks = c('Cattle very low', 'Wildlife very low', "Cattle medium",  "Wildlife medium", 'Cattle very high', 'Wildlife very high' ), values = c("darksalmon", "darkseagreen1", "chocolate1", "darkseagreen3", "brown4", "darkgreen" ))+
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
                        align ="h", axis ="bt", 
                       labels=c("A", "B")
)

comp_legend<- plot_grid(compplots, legend, nrow=2, rel_heights = c(1, 0.1))
comp_legend             

# ggsave(comp_legend, file="composition_comb_115.png", width = 32,
#        height = 20,
#        units = "cm", dpi=500)
