# ==============================================
# ------- Model output anlaysis: composition
# ==============================================
# Irob et al., 2021 ---------------------------
# Author of R script: Katja Irob (irob.k@fu-berlin.de)
# ==============================================

rm(list = ls()) # clears working environment


library(tidyverse)
options(dplyr.width = Inf) # enables head() to display all coloums
library(grid)
library(gridExtra)
library(reshape2)
library(scales)
library(gtable)
library(cowplot)
library(data.table)
library(cowplot)

source(here::here("R/Utility.R"))

# Read data and calculate mean cover per scenario and sub-pft for last 20 years

PFTcoverall <- readfiles(path = "Data/Results")

# bringing the df in the right format --------------

cover <- select(PFTcoverall, contains("Cover"), c("year", "scenario", "climrep"))
cover <- select(cover, starts_with("mean"), c("year", "scenario"))

cover <- cover[, !names(cover) %in% c("meanGtotalcover", "meanAtotalcover", "meanStotalcover"), drop = F]

cover <- cover %>% filter(year > 79) # only include last 20 years of simulation

cover <- reshape2::melt(cover, id.vars = c("year", "scenario"))

cover <- cover %>% # rename melted columns
  rename(
    PFT = variable,
    cover = value
  )


cover$type <- ifelse(grepl("(meanGCover)", cover$PFT), "Perennial", ifelse(grepl("(meanSCover)", cover$PFT), "Shrub", "Annual"))

cover <- cover[!cover$type == "Annual", ] # remove annuals

cover$cover <- cover$cover * 100 # percentage

# rename scenarios
cover$scenario <- as.character(cover$scenario)
cover$scenario[cover$scenario == "SR40graze"] <- "Grazing low"
cover$scenario[cover$scenario == "SR20graze"] <- "Grazing high"
#
cover$scenario[cover$scenario == "SR20browse"] <- "Browsing high"
cover$scenario[cover$scenario == "SR40browse"] <- "Browsing low"

cover$scenario <- factor(cover$scenario, levels = c("Grazing low", "Browsing low", "Grazing high", "Browsing high"))


# rename PFTs
cover$PFT <- as.character(cover$PFT) # this is important otherwise it will error

cover <- renamePFT_grass(cover)
cover <- renamePFT_shrub(cover)

namesShrubs <- c("Base", "Base", "Cb", "Cb", "Rd", "Rd", "Rc", "Rc", "Bd", "Bd", "Bc", "Bc", "Bp", "Bp", "Dc", "Dc", "Db", "Db", "Dr", "Dr", "Mb", "Mb")

namesPer <- c("Base", "Base")

## separate by perennials and shrubs -----------

# shrubs ---------------------------------
shrubs <- as.data.table(cover[cover$type == "Shrub", ])

shrubs_agg <- shrubs %>%
  group_by(PFT, scenario, type) %>%
  summarise_at(vars(cover), funs(mean, max, min, sd))


shrubs_agg$treatment <- shrubs_agg$scenario
shrubs_agg$treatment <- as.character(shrubs_agg$treatment)
shrubs_agg$treatment[shrubs_agg$treatment == "Grazing low"] <- "Grazing"
shrubs_agg$treatment[shrubs_agg$treatment == "Grazing high"] <- "Grazing"
#
shrubs_agg$treatment[shrubs_agg$treatment == "Browsing low"] <- "Browsing"
shrubs_agg$treatment[shrubs_agg$treatment == "Browsing high"] <- "Browsing"

shrubs_agg$scenario <- factor(shrubs_agg$scenario, levels = c("Grazing high", "Grazing low", "Browsing high", "Browsing low"))


shrubs_agg$treatment <- as.factor(shrubs_agg$treatment)


shrubs_agg$effect <- shrubs_agg$scenario
shrubs_agg$effect <- as.character(shrubs_agg$effect)
shrubs_agg$effect[shrubs_agg$effect == "Grazing low"] <- "low"
shrubs_agg$effect[shrubs_agg$effect == "Grazing high"] <- "high"
#
shrubs_agg$effect[shrubs_agg$effect == "Browsing high"] <- "low"
shrubs_agg$effect[shrubs_agg$effect == "Browsing low"] <- "high"

shrubs_agg$effect <- as.factor(shrubs_agg$effect)

shrubs_agg$effect <- factor(shrubs_agg$effect, levels = c("low", "high"))


shrubs_agg$PFT2 <- paste(shrubs_agg$PFT, shrubs_agg$effect)

namesShrubs <- c("Base", "Base", "Cb", "Cb", "Rd", "Rd", "Rc", "Rc", "Bd", "Bd", "Bc", "Bc", "Bp", "Bp", "Dc", "Dc", "Db", "Db", "Dr", "Dr", "Mb", "Mb")
namesShrubs <- c("Base", "", "Cb", "", "Rd", "", "Rc", "", "Bd", "", "Bc", "", "Bp", "", "Dc", "", "Db", "", "Dr", "", "Mb", "")

shrubs_agg$treatment <- factor(shrubs_agg$treatment, levels = c("Grazing", "Browsing"))


shrubs_by_strat_bytreatment_swap <- ggplot(shrubs_agg, aes(y = mean, x = PFT, fill = scenario)) +
  geom_bar(stat = "identity", color = "lightgray", width = 0.7, position = position_dodge(width = 0.7)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .2, position = position_dodge(0.8)) +
  ylab(bquote("\nMean cover [%]")) +
  xlab("\nStrategy") +
  ggtitle("Shrubs") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 10), breaks = c(0, 5, 10)) +
  scale_fill_manual(breaks = c("Grazing low", "Grazing high", "Browsing low", "Browsing high"), values = c("coral", "coral4", "cyan", "seagreen")) +
  facet_wrap(. ~ treatment, ncol = 4) +
  theme_set(theme_minimal()) +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 13, face = "bold"),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    strip.text.x = element_text(size = 16),
    strip.text.y = element_text(size = 16),
    legend.text = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold"),
    legend.direction = "horizontal", legend.position = "bottom", legend.title = element_blank(),
    legend.spacing.x = unit(0.3, "cm"),
    legend.background = element_blank(),
    panel.grid.major = element_line(size = 0.2, linetype = "solid", colour = "gray"),
    panel.background = element_blank()
  )

shrubs_by_strat_bytreatment_swap

# perennials -----------------------
perennials <- cover[cover$type == "Perennial", ]

perennials_agg <- perennials %>%
  group_by(PFT, scenario, type) %>%
  summarise_at(vars(cover), funs(mean, max, min, sd))

perennials_agg$scenario <- factor(perennials_agg$scenario, levels = c("Grazing high", "Grazing low", "Browsing high", "Browsing low"))
perennials_agg$treatment <- perennials_agg$scenario
perennials_agg$treatment <- as.character(perennials_agg$treatment)
perennials_agg$treatment[perennials_agg$treatment == "Grazing low"] <- "Grazing"
perennials_agg$treatment[perennials_agg$treatment == "Grazing high"] <- "Grazing"
#
perennials_agg$treatment[perennials_agg$treatment == "Browsing low"] <- "Browsing"
perennials_agg$treatment[perennials_agg$treatment == "Browsing high"] <- "Browsing"

perennials_agg$treatment <- as.factor(perennials_agg$treatment)


perennials_agg$effect <- perennials_agg$scenario
perennials_agg$effect <- as.character(perennials_agg$effect)
perennials_agg$effect[perennials_agg$effect == "Grazing low"] <- "low"
perennials_agg$effect[perennials_agg$effect == "Grazing high"] <- "high"
#
perennials_agg$effect[perennials_agg$effect == "Browsing high"] <- "low"
perennials_agg$effect[perennials_agg$effect == "Browsing low"] <- "high"

perennials_agg$effect <- as.factor(perennials_agg$effect)


perennials_agg$PFT2 <- paste(perennials_agg$PFT, perennials_agg$effect)

namesPer <- c("Base", "", "Cb", "", "Cp", "", "Pr", "", "Pb", "", "Rb", "", "Rp", "", "Bp", "", "Bd", "")

perennials_by_strat_bytreatment_swap <- ggplot(perennials_agg, aes(y = mean, x = PFT, fill = scenario)) +
  geom_bar(stat = "identity", color = "lightgray", width = 0.7, position = position_dodge(width = 0.7)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .2, position = position_dodge(0.8)) +
  ylab(bquote("\nMean cover [%]")) +
  xlab("\nStrategy") +
  ggtitle("Perennials") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 32), breaks = c(0, 5, 10, 15, 20, 25, 30)) +
  scale_fill_manual(breaks = c("Grazing low", "Grazing high", "Browsing low", "Browsing high"), values = c("coral", "coral4", "cyan", "seagreen")) +
  facet_wrap(. ~ treatment, ncol = 4) +
  theme_set(theme_minimal()) +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 13, face = "bold"),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    strip.text.x = element_text(size = 16),
    strip.text.y = element_text(size = 16),
    legend.text = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold"),
    legend.direction = "horizontal", legend.position = "bottom", legend.title = element_blank(),
    legend.spacing.x = unit(0.3, "cm"),
    panel.grid.major = element_line(size = 0.2, linetype = "solid", colour = "gray"),
    panel.background = element_blank()
  )
perennials_by_strat_bytreatment_swap


## make universal legend with scenarios in right order -----

perennials_agg$scenario <- factor(perennials_agg$scenario, levels = c("Grazing low", "Browsing low", "Grazing high", "Browsing high"))

perennials_by_strat_legend <- ggplot(perennials_agg, aes(y = mean, x = PFT, fill = scenario)) +
  geom_bar(stat = "identity", color = "lightgray", width = 0.9, position = position_dodge(width = 0.7)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .2, position = position_dodge(.7)) +
  ylab(bquote("\nMean cover [%]")) +
  xlab("\nStrategy") +
  ggtitle("Perennials") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 30), breaks = c(0, 5, 10)) +
  scale_fill_manual(breaks = c("Grazing low", "Browsing low", "Grazing high", "Browsing high"), values = c("coral", "cyan", "coral4", "seagreen")) +
  facet_wrap(. ~ treatment, ncol = 4) +
  theme_set(theme_minimal()) +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 13, face = "bold"),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    strip.text.x = element_text(size = 16),
    strip.text.y = element_text(size = 16),
    legend.text = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold"),
    legend.direction = "horizontal", legend.position = "bottom", legend.title = element_blank(),
    legend.spacing.x = unit(0.3, "cm"),
    panel.grid.major = element_line(size = 0.2, linetype = "solid", colour = "gray"),
    panel.background = element_blank()
  )
perennials_by_strat_legend

legend <- get_legend(perennials_by_strat_legend)


# combine all plots together -----------------

perennials_by_strat_bytreatment_swap <- perennials_by_strat_bytreatment_swap + theme(legend.position = "none")
shrubs_by_strat_bytreatment_swap <- shrubs_by_strat_bytreatment_swap + theme(legend.position = "none")



compplots <- plot_grid(perennials_by_strat_bytreatment_swap, shrubs_by_strat_bytreatment_swap,
  ncol = 2, nrow = 1,
  rel_widths = c(7, 7, 7, 7),
  align = "h", axis = "bt",
  labels = c("a", "b")
)

comp_legend <- plot_grid(compplots, legend, nrow = 2, rel_heights = c(1, 0.1))
comp_legend


ggsave(comp_legend, file="composition_comb_revised.tiff", width = 32,
       height = 20,
       units = "cm", dpi=600)


##################################################
## Composition stats -----------------------------
##################################################

cover <- select(PFTcoverall, contains("Cover"), c("year", "scenario"))
cover <- select(cover, starts_with("mean"), c("year", "scenario"))

cover <- cover[, !names(cover) %in% c("meanGtotalcover", "meanAtotalcover", "meanStotalcover"), drop = F]

cover <- cover %>% filter(year > 79)

cover <- reshape2::melt(cover, id.vars = c("year", "scenario"))

cover <- cover %>%
  rename(
    PFT = variable,
    cover = value
  )
cover$landuse <- ifelse(grepl("(browse)", cover$scenario), "Wildlife", "Cattle")
cover$type <- ifelse(grepl("(meanGCover)", cover$PFT), "Perennial", ifelse(grepl("(meanSCover)", cover$PFT), "Shrub", "Annual"))

# rename PFTs
cover$PFT <- as.character(cover$PFT) # this is important otherwise it will error

cover$PFT[cover$PFT == "meanGCover7"] <- "Perennial_Bp"
cover$PFT[cover$PFT == "meanGCover8"] <- "Perennial_Bd"
cover$PFT[cover$PFT == "meanGCover1"] <- "Perennial_Cb"
cover$PFT[cover$PFT == "meanGCover2"] <- "Perennial_Cp"
cover$PFT[cover$PFT == "meanGCover3"] <- "Perennial_Pr"
cover$PFT[cover$PFT == "meanGCover4"] <- "Perennial_Pb"
cover$PFT[cover$PFT == "meanGCover5"] <- "Perennial_Rb"
cover$PFT[cover$PFT == "meanGCover6"] <- "Perennial_Rp"
cover$PFT[cover$PFT == "meanGCover0"] <- "Perennial_Base"


cover$PFT[cover$PFT == "meanSCover4"] <- "Shrub_Bd"
cover$PFT[cover$PFT == "meanSCover6"] <- "Shrub_Bp"
cover$PFT[cover$PFT == "meanSCover5"] <- "Shrub_Bc"
cover$PFT[cover$PFT == "meanSCover1"] <- "Shrub_Cb"
cover$PFT[cover$PFT == "meanSCover8"] <- "Shrub_Db"
cover$PFT[cover$PFT == "meanSCover9"] <- "Shrub_Dr"
cover$PFT[cover$PFT == "meanSCover7"] <- "Shrub_Dc"
cover$PFT[cover$PFT == "meanSCover10"] <- "Shrub_Mb"
cover$PFT[cover$PFT == "meanSCover2"] <- "Shrub_Rd"
cover$PFT[cover$PFT == "meanSCover3"] <- "Shrub_Rc"
cover$PFT[cover$PFT == "meanSCover0"] <- "Shrub_Base"

cover$PFT[cover$PFT == "meanACover0"] <- "Annual_Base"

# non-parametric Scheirer-Ray-Hay-Test ---------------------
if (!require(rcompanion)) {
  install.packages("rcompanion")
}
if (!require(FSA)) {
  install.packages("FSA")
}
library(rcompanion)
library(FSA)

# total comp
scheirerRayHare(cover ~ PFT + landuse, data = cover)
# H = 19778.7, p = 0
DT <- dunnTest(cover ~ PFT, data = cover, method = "bh")
DT

PT <- DT$res

cldList(P.adj ~ Comparison,
  data = PT,
  threshold = 0.05
)

# perennials --

scheirerRayHare(cover ~ PFT + scenario, data = perennials)
# 2388.4, p = 0

DT <- dunnTest(cover ~ PFT, data = perennials, method = "bh")

# shrubs --

scheirerRayHare(cover ~ PFT + scenario, data = shrubs)
# 3163.7, p = 0

DT <- dunnTest(cover ~ PFT, data = shrubs, method = "bh")


## Effect size ---------

# order by median from high to low
cover$scenario <- factor(cover$scenario, levels = c("SR40browse", "SR20browse", "SR20graze", "SR40graze"))

epsilonSquared(x = cover$cover, g = cover$scenario)

# perennials ---
epsilonSquared(x = perennials$cover, g = perennials$scenario)

# shrubs ---
epsilonSquared(x = shrubs$cover, g = shrubs$scenario)
