# ==============================================
# ------- Model output anlaysis: cover
# ==============================================
# Irob et al., 2021 ---------------------------
# Author of R script: Katja Irob (irob.k@fu-berlin.de)
# ==============================================

rm(list = ls()) # clears working environment

library(tidyverse)
options(dplyr.width = Inf) # enables head() to display all columns
library(grid)
library(gridExtra)
library(cowplot)
library(reshape2)
library(scales)
library(here)

source(here::here("R/Utility.R"))

PFTcoverall <- readfiles(path = "Data/Results")

makeMeanCover <- function(PFTcoverall) {
  cover <- select(PFTcoverall, contains("Cover"), c("year", "scenario", "climrep"))
  cover <- select(cover, starts_with("mean"), c("year", "scenario"))

  cover <- cover[, !names(cover) %in% c("meanGtotalcover", "meanAtotalcover", "meanStotalcover"), drop = F]

  cover <- cover %>% filter(year > 79)

  cover <- melt(cover, id.vars = c("year", "scenario"))

  # take mean over all scenarios and climreps, here we want the mean cover of the last 20 years of simulation per scenario for each sub-PFT

  cover <- aggregate(list(cover = cover$value), by = list(PFT = cover$variable, scenario = cover$scenario), FUN = mean)

  cover$type <- ifelse(grepl("(meanGCover)", cover$PFT), "Perennial", ifelse(grepl("(meanSCover)", cover$PFT), "Shrub", "Annual"))

  # # rename PFTs
  cover$PFT <- as.character(cover$PFT) # this is important otherwise it will error
  cover$PFT[cover$PFT == "meanACover0"] <- "Base_A"

  cover <- renamePFT_grass(cover)
  cover <- renamePFT_shrub(cover)
  
  # Cover  in percentage instead of 0-1
  cover$cover <- cover$cover * 100

  return(cover)
}

meanCover <- makeMeanCover(PFTcoverall)


# ===================================================
# ------- Plotting cover over time for all scenarios
# ===================================================

plotCoverOverTime <- function(PFTcoverall) { # function for generating the combined cover plot

  cover <- PFTcoverall[, c("year", "meanGtotalcover", "meanStotalcover", "meanAtotalcover", "scenario")]

  cover <- melt(cover, id.vars = c("year", "scenario"))

  cover$value <- cover$value * 100 # converting cover to percentage

  cover$type <- ifelse(grepl("(meanGtotalcover)", cover$variable), "Perennial", ifelse(grepl("(meanStotalcover)", cover$variable), "Shrub", "Annual"))

  cover <- cover %>%
    group_by(scenario, year, type) %>%
    summarise_at(vars(value), funs(mean, sd))

  # renaming scenarios

  cover$scenario <- as.character(cover$scenario)
  cover$scenario[cover$scenario == "SR40graze"] <- "Cattle low"
  cover$scenario[cover$scenario == "SR20graze"] <- "Cattle high"
  #
  cover$scenario[cover$scenario == "SR20browse"] <- "Wildlife high"
  cover$scenario[cover$scenario == "SR40browse"] <- "Wildlife low"

  cover$scenario <- factor(cover$scenario, levels = c("Cattle low", "Wildlife low", "Cattle high", "Wildlife high")) # bring scenario in desired order

  # select colours for plotting
  cols <- c("gold1", "seagreen", "coral")

  scenario_list <- unique(cover$scenario)

  plot_list <- list()

  # creating a plot for every scenario and saving it in plot_list()
  for (i in 1:length(scenario_list)) {
    plot <- ggplot(
      subset(cover, scenario == scenario_list[i]),
      aes(x = year, y = mean, colour = type)
    ) +
      geom_ribbon(aes(x = year, ymin = mean - sd, ymax = mean + sd), size = 0.5, fill = "lightgrey", alpha = 0.5) +
      geom_line(size = 1.2) +
      ylim(0, 100) +
      xlab("Years") +
      ylab(bquote("Cover [%]")) +
      scale_colour_manual(values = cols) +
      ggtitle(paste(scenario_list[i])) +
      theme_set(theme_minimal()) +
      theme(
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 16, face = "bold"),
        legend.direction = "horizontal", legend.position = "none", legend.title = element_blank(),
        legend.background = element_blank(),
        panel.grid.major = element_line(size = 0.2, linetype = "solid", colour = "gray"),
        panel.background = element_blank()
      ) +
      guides(col = guide_legend(nrow = 1, byrow = TRUE))

    plotname <- paste0(gsub(" ", "_", scenario_list[i]), "_line") # rename plot according to scenario

    plot_list[[plotname]] <- plot
  }


  #### Barplot of last 20 years ------------

  cover <- meanCover

  # bring sub-PFTs in desired order
  cover$PFT <- factor(cover$PFT, levels = c("meanACover0", "meanSCover0", "meanSCover1", "meanSCover2", "meanSCover3", "meanSCover4", "meanSCover5", "meanSCover6", "meanSCover7", "meanSCover8", "meanSCover9", "meanSCover10", "meanGCover0", "meanGCover1", "meanGCover2", "meanGCover3", "meanGCover4", "meanGCover5", "meanGCover6", "meanGCover7", "meanGCover8"))

  # rename scenarios
  cover$scenario <- as.character(cover$scenario)
  cover$scenario[cover$scenario == "SR40graze"] <- "Cattle_low"
  cover$scenario[cover$scenario == "SR20graze"] <- "Cattle_high"
  #
  cover$scenario[cover$scenario == "SR20browse"] <- "Wildlife_high"
  cover$scenario[cover$scenario == "SR40browse"] <- "Wildlife_low"

  cover$scenario <- factor(cover$scenario, levels = c("Cattle_low", "Wildlife_low", "Cattle_high", "Wildlife_high"))
  scenario_list <- unique(cover$scenario)

  barplot_list <- list()


  # create barplot of last 20 years for every land use scenario and save it in barplot_list()
  for (i in 1:length(scenario_list)) {
    cover$type <- factor(cover$type, levels = c("Shrub", "Perennial", "Annual"))
    cols <- c("coral", "seagreen", "gold1")

    survival20 <- ggplot(
      subset(cover, scenario == scenario_list[i]),
      aes(y = cover, x = scenario, fill = type)
    ) +
      geom_col(color = "whitesmoke", lwd = 0.28) +
      ylim(0, 100) +
      ylab(bquote("Mean cover")) +
      scale_fill_manual(values = cols) +
      theme_set(theme_minimal()) +
      theme(
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.direction = "horizontal", legend.position = "none", legend.title = element_blank(),
        legend.background = element_blank(),
        panel.grid.major = element_line(size = 0.2, linetype = "solid", colour = "gray"),
        panel.background = element_blank()
      )

    barplot_list[[i]] <- survival20
  }

  scen_list <- c("Cattle_low", "Wildlife_low", "Cattle_high", "Wildlife_high")

  names(barplot_list) <- print(paste0(scenario_list, "_bar"))

  barplot_list$Wildlife_high_bar

  # Extract legend from this plot:
  cattle_low_bar <- ggplot(
    subset(cover, scenario %in% "Cattle_low"),
    aes(y = cover, x = scenario == "Cattle_low", fill = type)
  ) +
    geom_col(color = "whitesmoke", lwd = 0.3) +
    ylim(0, 100) +
    ylab(bquote("Mean cover")) +
    scale_fill_manual(values = cols) +
    theme_set(theme_minimal()) +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 12),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      legend.text = element_text(size = 16),
      legend.direction = "horizontal", legend.position = "bottom", legend.title = element_blank(),
      legend.background = element_blank(),
      legend.spacing.x = unit(0.3, "cm"),
      panel.grid.major = element_line(size = 0.2, linetype = "solid", colour = "gray"),
      panel.background = element_blank()
    )
  cattle_low_bar

  get_legend <- function(cattle_low_bar) {
    tmp <- ggplot_gtable(ggplot_build(cattle_low_bar))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
  }

  legend <- get_legend(cattle_low_bar)

  # arrange all plots and legend in one plot ---------------------

  coverplots <- plot_grid(plot_list$Cattle_low_line, barplot_list$Cattle_low_bar, plot_list$Wildlife_low_line, barplot_list$Wildlife_low_bar,
    plot_list$Cattle_high_line, barplot_list$Cattle_high_bar, plot_list$Wildlife_high_line, barplot_list$Wildlife_high_bar,
    ncol = 4, nrow = 2,
    rel_widths = c(4, 1.5, 4, 1.5),
    labels = c("a", "", "b", "", "c", "", "d", ""),
    align = "h", axis = "bt"
  )

  cover_legend <- plot_grid(coverplots, legend, nrow = 2, rel_heights = c(1, 0.1))

  # ggsave(cover_legend, file="cover_combined_all_scenarios.png", width = 32,
  #        height = 20,
  #        units = "cm", dpi=500)

  return(cover_legend)
}

plotCoverOverTime(PFTcoverall)



##################################################
## STATS ------------------
##################################################

# cover -------------------------------

cover <- PFTcoverall[, c("year", "meanGtotalcover", "meanStotalcover", "meanAtotalcover", "scenario")]

cover$TotalCover <- cover$meanGtotalcover + cover$meanStotalcover + cover$meanAtotalcover # calculate total cover

cover <- melt(cover, id.vars = c("year", "scenario", "TotalCover"))

cover$value <- cover$value * 100 # convert to percentage

cover$type <- ifelse(grepl("(meanGtotalcover)", cover$variable), "Perennial", ifelse(grepl("(meanStotalcover)", cover$variable), "Shrub", "Annual")) # rename meta-PFT type
# calculate mean and median cover
meancover <- cover %>%
  group_by(scenario, type) %>%
  summarise_at(vars(value), funs(mean, sd))
mediancover <- cover %>%
  group_by(scenario) %>%
  summarise_at(vars(value), funs(median))

# create extra column for land use intensity
cover$intensity <- ifelse(grepl("(SR20)", cover$scenario), "high", "low")
cover$landuse <- ifelse(grepl("(browse)", cover$scenario), "Wildlife", "Cattle")

cover <- cover %>% filter(year > 79)

if (!require(rcompanion)) {
  install.packages("rcompanion")
}
if (!require(FSA)) {
  install.packages("FSA")
}
library(rcompanion)
library(FSA)

# non-parametric Scheirer-Ray-Hare test----------
scheirerRayHare(TotalCover ~ landuse + intensity, data = cover)
# H = 153.5, p < 0.001

# bring data in right format for post-hoc test and order by descending median
cover$landuse <- factor(cover$landuse, levels = c("Wildlife", "Cattle"))
cover$intensity <- factor(cover$intensity, levels = c("low", "high"))
# order by median from high to low
cover$scenario <- factor(cover$scenario, levels = c("SR40browse", "SR20browse", "SR20graze", "SR40graze"))

# post-hoc Dunn's test to look for differences between land use scenarios --------
DT <- dunnTest(TotalCover ~ scenario, data = cover, method = "bh")
DT # all scenarios differ significantly

# check significant differences
PT <- DT$res
cldList(P.adj ~ Comparison,
  data = PT,
  threshold = 0.05
) # letters indicating significance

## Effect size epsilon^2 ---------

epsilonSquared(x = cover$TotalCover, g = cover$landuse)
# e^2 = 0.48


