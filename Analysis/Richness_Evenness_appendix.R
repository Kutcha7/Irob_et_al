# ==============================================
# ------- Model output analysis: biodiversity
# ==============================================
# Irob et al., 2021 ---------------------------
# Author of R script: Katja Irob (irob.k@fu-berlin.de)
## CALC RICHNESS AND EVENNESS MANUALLY ########
## SET THRESHOLD TO DIFFERENT 2.5% #####
## MAKE PLOTS ###########################

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
library(vegan)

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
  
  
  PFTs<-select(PFTs,contains("Cover"), c("year", "Richness", "ShannonDiv", "Evenness", "scenario", "climrep"))
  PFTs<-select(PFTs,starts_with("mean"), c("year", "Richness", "ShannonDiv", "Evenness", "scenario", "climrep"))
  
  
  
  
  no<-c("meanRCover")
  PFTs<-PFTs[, !names(PFTs) %in% no, drop=F ] # drop =F means that it should be a df not a list
  
  
  
  return(PFTs)
}

PFTcoverall<-readfiles()


# ==============================================
# ------- Calc mean Richness of last 20 years for all scenarios
# ==============================================

cover<-select(PFTcoverall,contains("Cover"), c("year", "scenario", "climrep"))
cover<-select(cover,starts_with("mean"), c("year", "scenario", "climrep"))

cover<-cover[, !names(cover) %in% c("meanGtotalcover", "meanAtotalcover", "meanStotalcover"), drop=F ]

cover <- cover %>% filter(year> 79) 

cover<-melt(cover, id.vars=c("year", "scenario", "climrep"))

cover <- cover %>% 
  rename(
    PFT = variable, 
    cover =value
  )

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



cover$landuse <- ifelse(grepl("(browse)", cover$scenario),"Wildlife","Cattle")
cover$type <- ifelse(grepl("(meanGCover)", cover$PFT),"Perennial", ifelse(grepl("(meanSCover)", cover$PFT),"Shrub", "Annual"))

cover$cover <- cover$cover*100

cover$Richness <- ifelse(cover$cover > 2.5, 1, 0)

sumperyearrich <-cover %>% group_by(scenario,  type, year, climrep) %>% summarise_at(vars(Richness), funs(sumrich=sum)) # only consider mean of last 20 years of simulation (and all climreps)

sumperclimrep <- sumperyearrich %>% group_by(scenario,  type, climrep) %>% summarise_at(vars(sumrich), funs(speciesRich=mean, sd)) # only consider mean of last 20 years of simulation (and all climreps)

meanrich20total <- sumperyearrich %>% group_by(scenario, type) %>% summarise_at(vars(sumrich), funs(speciesRich= mean, sd)) # only consider mean of last 20 years of simulation (and all climreps)



## PLOT BIODIV --------------------------------------

richtotal <- meanrich20total %>% group_by(scenario) %>% summarise_at(vars(speciesRich, sd), funs(sum)) # only consider mean of last 20 years of simulation (a

richtotal$type <- "Total"

richtotal <- richtotal[, c(1,4,2,3)]


cols<-c( "gold1", "seagreen", "coral", "black")

meanrichtotal<-bind_rows(meanrich20total, richtotal)

richplot <-ggplot(meanrichtotal,  aes(y = speciesRich, ymin=speciesRich-sd, ymax=speciesRich+sd, x=scenario, color=type)) +
  geom_point(size=2) +
  ylim(0, 20) +
  ylab(bquote("Richness"))+
  xlab("\nLand use type")+
  scale_color_manual(values = cols)+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=12),
        axis.title.y= element_text(size=15),
        axis.title.x=element_text(size=15),
        legend.text=element_text(size=15),
        legend.direction = "horizontal", legend.position = "bottom", legend.title = element_blank(), 
         legend.background = element_blank(),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid', colour = "gray"),
        panel.background = element_blank()) 
richplot

get_legend<-function(richplot){
  tmp <- ggplot_gtable(ggplot_build(richplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(richplot)

# jitter and pointrange ---------------

richness<-ggplot(sumperclimrep, aes(x= scenario, y=speciesRich)) +
  geom_jitter(aes(color=type), position = position_jitter(0.2), alpha=0.4) + 
  geom_point(aes(color=type), size = 5,  data=meanrich20total)+ 
  geom_pointrange(aes(ymin=speciesRich-sd, ymax=speciesRich+sd), size= 1, data = richtotal) +
  ylab(bquote("Richness"))+
  xlab("\nLand use type")+
  scale_color_manual(values = c( "gold1", "seagreen", "coral"))+
  theme(axis.text.x = element_text(size=12, angle=90, hjust=1),
        axis.text.y = element_text(size=14),
        axis.title.y= element_text(size=15),
        axis.title.x=element_text(size=15),
        legend.text=element_text(size=16),
        legend.direction = "horizontal", legend.position = "bottom", legend.title = element_blank(), 
        legend.background = element_blank(),
        axis.line = element_line (colour = "gray"), 
        panel.grid.major = element_line(size = 0.2, linetype = 'solid', colour = "gray"),
        panel.background = element_blank()) 
richness

### CALCULATE EVENNESS ----------------------------------

div <- cover[, c(1:4, 7:8)]

sumperyear <- div %>% group_by(scenario,  type, year, climrep) %>% summarise_at(vars(Richness), funs(Richness=sum)) # only consider mean of last 20 years of simulation (and all climreps)

# calc species richness --------------

# ---- by climrep and type

richperclimreptype <- sumperyear  %>% group_by(scenario,  type,  climrep) %>% summarise_at(vars(Richness), funs(speciesRich=mean)) # only consider mean of last 20 years of simulation (and all climreps)

evenspreadtype<- richperclimreptype %>% spread(type, speciesRich, convert = T)

richperclimrepscen <- richperclimreptype  %>% group_by(scenario, climrep) %>% summarise_at(vars(speciesRich), funs(speciesRich=sum)) # only consider mean of last 20 years of simulation (and all climreps)

S <- richperclimrepscen$speciesRich

evennessdf <- evenspreadtype[1:2]

# step for calculating evenesss ---------
evennessdf$Total<-diversity(evenspreadtype[-c(1:2)], index="simpson")/log(S)


# PLOT EVENNESS ---------------------------------------

eventotal <- evennessdf%>% group_by(scenario) %>% summarise_at(vars(Total), funs(Total=mean, median)) # only consider mean of last 20 years of simulation (a
evensd <- evennessdf%>% group_by(scenario) %>% summarise_at(vars(Total), funs(sd=sd))

eventotal$sd <-evensd$sd

Evenness<-ggplot(evennessdf, aes(x= scenario, y=Total)) +
  geom_jitter(position = position_jitter(0.2), alpha=0.4) + 
  geom_pointrange(aes(ymin=Total-sd, ymax=Total+sd), size= 1, data = eventotal) +
  ylab(bquote("Evenness \nPilou's J"))+
  xlab("\nLand use type")+
  theme(axis.text.x = element_text(size=12, angle=90, hjust=1),
        axis.text.y = element_text(size=14),
        axis.title.y= element_text(size=15),
        axis.title.x=element_text(size=15), 
        legend.text=element_text(size=16),
        legend.direction = "horizontal", legend.position = "none", legend.title = element_blank(), 
        legend.background = element_blank(),
        axis.line = element_line (colour = "gray"), 
        panel.grid.major = element_line(size = 0.2, linetype = 'solid', colour = "gray"),
        panel.background = element_blank()) 
Evenness

## combine plots ------

library(cowplot)

richness <-richness + theme(legend.position="none") # remove legend 

# combine plot
richevenplots <- plot_grid(richness, Evenness,
                           ncol=2,  nrow=1, 
                           rel_widths=c(12, 12), 
                           align ="h", axis= bt,
                           labels =c("A", "B")
)

biodivricheven_legend<- plot_grid(richevenplots, legend, nrow=2, rel_heights = c(1, 0.1))
biodivricheven_legend             


ggsave(biodivricheven_legend, file="richness_evenness_appendix_76.png", width = 32,
       height = 18,
       units = "cm", dpi=500)

