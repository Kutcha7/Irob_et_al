# ==============================================
# ------- Model output anlaysis: cover
# ==============================================
# Irob et al., 2021 ---------------------------
# Author of R script: Katja Irob (irob.k@fu-berlin.de)

rm(list=ls()) # clears working environment 


library(tidyverse)
options(dplyr.width = Inf) #enables head() to display all coloums
library(grid)
library(gridExtra)
library(reshape2)
library(scales)
library(cowplot)

source(here::here("R/Utility.R"))

# Read results
PFTcoverall <- readfiles(path = "Data/Results/Appendix")

makeMeanCover <- function(PFTcoverall) {
  
  
  cover<-select(PFTcoverall,contains("Cover"), c("year", "scenario", "climrep"))
  cover<-select(cover,starts_with("mean"), c("year", "scenario"))
  
  cover<-cover[, !names(cover) %in% c("meanGtotalcover", "meanAtotalcover", "meanStotalcover"), drop=F ]
  
  cover <- cover %>% filter(year> 79) 
  
  cover<-melt(cover, id.vars=c("year", "scenario"))
  
  
  # take mean over all scenarios and climreps, here we want the mean cover of the 
  # last 20 years of simulation per scenario for each sub-PFT !!!
  cover <- aggregate(list(cover = cover$value), by = list(PFT = cover$variable, scenario = cover$scenario), FUN = mean)
  
  cover$type <- ifelse(grepl("(meanGCover)", cover$PFT),"Perennial", ifelse(grepl("(meanSCover)", cover$PFT),"Shrub", "Annual"))
  
  # rename PFTs
  cover$PFT<-as.character(cover$PFT) # this is important otherwise it will error
  cover$PFT[cover$PFT=="meanACover0"]<-"Base_A"
  
  cover <- renamePFT_grass(cover)
  cover <- renamePFT_shrub(cover)

  # Cover  in percentage instead of 0-1  
  cover$cover<-cover$cover*100
  
  return(cover)
}

meanCover <- makeMeanCover(PFTcoverall)


# ===================================================
# ------- Plotting cover over time for all scenarios
# ===================================================

plotCoverOverTime<- function(PFTcoverall) { # function to bring data in right format, create line and barplot and combine them in one aggregated plot 
  
  cover<-PFTcoverall[, c("year", "meanGtotalcover", "meanStotalcover", "meanAtotalcover", "scenario")]
  
  cover<-melt(cover, id.vars=c("year", "scenario"))
  
  cover$value <- cover$value*100
  
  cover$type <- ifelse(grepl("(meanGtotalcover)", cover$variable),"Perennial", ifelse(grepl("(meanStotalcover)", cover$variable),"Shrub", "Annual"))
  
  cover <-cover %>% group_by(scenario, year, type) %>% summarise_at(vars(value), funs(mean, sd))
  


  # rename scenarios
  cover$scenario<-as.character(cover$scenario)
  cover$scenario[cover$scenario=="SR50graze"] <- 'Cattle very low'
  cover$scenario[cover$scenario=="SR10graze"] <- 'Cattle very high'
  cover$scenario[cover$scenario=="SR30graze"] <- 'Cattle medium'
  #
  cover$scenario[cover$scenario=="SR10browse"] <- 'Wildlife very high'
  cover$scenario[cover$scenario=="SR30browse"] <- 'Wildlife medium'
  cover$scenario[cover$scenario=="SR50browse"] <- 'Wildlife very low'
  
  # change order of scenarios for plots
  cover$scenario<- factor(cover$scenario, levels=c('Cattle very low', 'Wildlife very low', "Cattle medium", "Wildlife medium",  'Cattle very high', 'Wildlife very high' ))
  

  # set colours for PFTs
  cols<-c( "gold1", "seagreen", "coral")
  
  scenario_list <-unique(cover$scenario)
  
  ### cover over time plot --------------------
  # plot loop, puts every plot in plot_list where it can be accessed later 
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
  

    #### Barplots of last 20 years ------------
    
    cover<-meanCover
    
    cover$PFT<- factor(cover$PFT, levels=c("meanACover0", "meanSCover0", "meanSCover1","meanSCover2", "meanSCover3", "meanSCover4","meanSCover5", "meanSCover6", "meanSCover7", "meanSCover8", "meanSCover9", "meanSCover10", "meanGCover0", "meanGCover1", "meanGCover2", "meanGCover3", "meanGCover4", "meanGCover5", "meanGCover6", "meanGCover7", "meanGCover8"))
    
    
    cover$scenario<-as.character(cover$scenario)
    cover$scenario[cover$scenario=="SR50graze"] <- 'Cattle very low'
    cover$scenario[cover$scenario=="SR10graze"] <- 'Cattle very high'
    cover$scenario[cover$scenario=="SR30graze"] <- 'Cattle medium'
    #
    cover$scenario[cover$scenario=="SR10browse"] <- 'Wildlife very high'
    cover$scenario[cover$scenario=="SR30browse"] <- 'Wildlife medium'
    cover$scenario[cover$scenario=="SR50browse"] <- 'Wildlife very low'
    
    
    cover$scenario<- factor(cover$scenario, levels=c('Cattle very low', 'Wildlife very low', "Cattle medium", "Wildlife medium",  'Cattle very high', 'Wildlife very high' ))
    
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
                axis.title.x=element_blank(),
                legend.direction = "horizontal", legend.position = "none", legend.title = element_blank(), 
                legend.background = element_blank(),
                panel.grid.major = element_line(size = 0.2, linetype = 'solid', colour = "gray"),
                panel.background = element_blank()) 
            
          # change name of plots in barplot_list()
        plotname <- paste0(gsub(" ", "_", scenario_list[i]), "_bar")
      
  
      barplot_list[[plotname]] <- survival20
      }    


  
      # Arrange ggplot2 with adapted height and width for each row and column :
    # plot for legend -------------
      cover$type <- factor(cover$type, levels=c( "Annual", "Perennial", "Shrub"))
      cattle_low_bar<-ggplot(subset(cover, scenario %in% "Cattle very low"),
                             aes(y = cover, x=scenario=="Cattle very low", fill=type)) +
        geom_col(color="whitesmoke", lwd=0.3)+
        ylim(0, 100) +
        ylab(bquote("Mean cover"))+
        scale_fill_manual(values = cols)+
        theme(axis.text.x = element_blank(),
              axis.text.y = element_text(size=12),
              axis.title.y= element_blank(),
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
    
    legend <- get_legend(cattle_low_bar) #extract legend in right order 
    
 
    

  # arrange all line and barplots in one aggregated figure
  coverplots <- plot_grid(plot_list$Cattle_very_low_line,  barplot_list$Cattle_very_low_bar,  
                          plot_list$Wildlife_very_low_line, barplot_list$Wildlife_very_low_bar,  
                          plot_list$Cattle_medium_line,  barplot_list$Cattle_medium_bar,
                          plot_list$Wildlife_medium_line,  barplot_list$Wildlife_medium_bar,
                          plot_list$Cattle_very_high_line, barplot_list$Cattle_very_high_bar, plot_list$Wildlife_very_high_line, barplot_list$Wildlife_very_high_bar,
                          ncol=4,  nrow=3, 
                          rel_widths=c(4, 1.5, 4, 1.5), 
                          align ="h", axis ="bt",
                          labels=c("A", "", "B", "", "C", "", "D", "", "E","", "F", "")
  )
  
  
  cover_legend<- plot_grid(coverplots, legend, nrow=2, rel_heights = c(1, 0.1)) #change nrow back to 2

  
  # ggsave(cover_legend, file="cover_appendix_76.png", width = 32,
  #        height = 20,
  #        units = "cm", dpi=500)
    

  return(cover_legend)
  
  

}

plotCoverOverTime(PFTcoverall)

