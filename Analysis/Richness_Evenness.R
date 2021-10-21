# ==============================================
# ------- Model output analysis: biodiversity
# ==============================================
# Irob et al., 2021 ---------------------------
# Author of R script: Katja Irob (irob.k@fu-berlin.de)
# ==============================================
## CALC RICHNESS AND EVENNESS MANUALLY ########
## SET THRESHOLD FOR RICHNESS CALCULATION #####
## MAKE PLOTS ###########################

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
library(vegan)
library(cowplot)

source(here::here("R/Utility.R"))

# Read data

PFTcoverall <- readfiles(path = "Data/Results")

# ==============================================
# ------- Calc mean Richness based on cover of last 20 years for all scenarios
# ==============================================

cover <- select(PFTcoverall, contains("Cover"), c("year", "scenario", "climrep"))
cover <- select(cover, starts_with("mean"), c("year", "scenario", "climrep"))

cover <- cover[, !names(cover) %in% c("meanGtotalcover", "meanAtotalcover", "meanStotalcover"), drop = F]

cover <- cover %>% filter(year > 79) # only include cover of last 20 years of simulation

cover <- melt(cover, id.vars = c("year", "scenario", "climrep"))

cover <- cover %>% # rename columns
  rename(
    PFT = variable,
    cover = value
  )

# rename scenarios --
cover$scenario <- as.character(cover$scenario)
cover$scenario[cover$scenario == "SR40graze"] <- "Grazing low"
cover$scenario[cover$scenario == "SR20graze"] <- "Grazing high"
#
cover$scenario[cover$scenario == "SR20browse"] <- "Browsing high"
cover$scenario[cover$scenario == "SR40browse"] <- "Browsing low"

cover$scenario <- factor(cover$scenario, levels = c("Grazing low", "Grazing high", "Browsing low", "Browsing high"))

cover$type <- ifelse(grepl("(meanGCover)", cover$PFT), "Perennial", ifelse(grepl("(meanSCover)", cover$PFT), "Shrub", "Annual")) # create properly named meta PFT column

cover$cover <- cover$cover * 100 # cover in percentage

# CALC RICHNESS --------
cover$Richness <- ifelse(cover$cover > 2.5, 1, 0) # only include cover values above a threshold of 2.5% to count as alive sub-PFT, if cover is above this threshold, put 1 in richness column, otherwise 0

# aggregate data ---
sumperyearrich <- cover %>%
  group_by(scenario, type, year, climrep) %>%
  summarise_at(vars(Richness), funs(sumrich = sum)) # only consider mean of last 20 years of simulation (and all climreps)

sumperclimrep <- sumperyearrich %>%
  group_by(scenario, type, climrep) %>%
  summarise_at(vars(sumrich), funs(speciesRich = mean, sd)) # only consider mean of last 20 years of simulation (and all climreps)

meanrich20total <- sumperyearrich %>%
  group_by(scenario, type) %>%
  summarise_at(vars(sumrich), funs(speciesRich = mean, sd)) # only consider mean of last 20 years of simulation (and all climreps)


## PLOT BIODIV --------------------------------------

richtotal <- meanrich20total %>%
  group_by(scenario) %>%
  summarise_at(vars(speciesRich, sd), funs(sum)) # only consider mean of last 20 years of simulation (a

richtotal$type <- "Total"

richtotal <- richtotal[, c(1, 4, 2, 3)] # bring df in right order


# plot to extract legend from ----------------------
cols <- c("gold1", "seagreen", "coral", "black")


meanrichtotal <- bind_rows(meanrich20total, richtotal)

richplot <- ggplot(meanrichtotal, aes(y = speciesRich, ymin = speciesRich - sd, ymax = speciesRich + sd, x = scenario, color = type)) +
  geom_point(size = 2) +
  ylim(0, 20) +
  ylab(bquote("Richness")) +
  xlab("\nLand use type") +
  scale_color_manual(values = cols) +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.direction = "horizontal", legend.position = "bottom", legend.title = element_blank(),
    legend.background = element_blank(),
    # legend.spacing.x = unit(0.3, 'cm'),
    panel.grid.major = element_line(size = 0.2, linetype = "solid", colour = "gray"),
    panel.background = element_blank()
  )
richplot

get_legend <- function(richplot) {
  tmp <- ggplot_gtable(ggplot_build(richplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(richplot)

# jitter and pointrange --------------------------------

richness <- ggplot(sumperclimrep, aes(x = scenario, y = speciesRich)) +
  geom_jitter(aes(color = type), position = position_jitter(0.2), alpha = 0.4) +
  geom_point(aes(color = type), size = 5, data = meanrich20total) +
  geom_pointrange(aes(ymin = speciesRich - sd, ymax = speciesRich + sd), size = 1, data = richtotal) +
  ylab(bquote("Richness")) +
  xlab("\nLand use type") +
  scale_color_manual(values = c("gold1", "seagreen", "coral")) +
  theme_set(theme_minimal()) +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    legend.text = element_text(size = 16),
    legend.direction = "horizontal", legend.position = "bottom", legend.title = element_blank(),
    legend.background = element_blank(),
    axis.line = element_line(colour = "gray"),
    panel.grid.major = element_line(size = 0.2, linetype = "solid", colour = "gray"),
    panel.background = element_blank()
  )
richness

### CALCULATE EVENNESS ----------------------------------

div <- cover[-c(4:5)] # delete PFT and cover column

# ---- by climrep and type ------------------

sumperyear <- div %>%
  group_by(scenario, type, year, climrep) %>%
  summarise_at(vars(Richness), funs(Richness = sum)) # only consider mean of last 20 years of simulation (and all climreps)

richperclimreptype <- sumperyear %>%
  group_by(scenario, type, climrep) %>%
  summarise_at(vars(Richness), funs(speciesRich = mean)) #

evenspreadtype <- richperclimreptype %>% spread(type, speciesRich, convert = T)

richperclimrepscen <- richperclimreptype %>%
  group_by(scenario, climrep) %>%
  summarise_at(vars(speciesRich), funs(speciesRich = sum)) #

S <- richperclimrepscen$speciesRich # include richness S

evennessdf <- evenspreadtype[1:2]
# calc evenness
evennessdf$Total <- diversity(evenspreadtype[-c(1:2)], index = "simpson") / log(S)


# PLOT EVENNESS ---------------------------------------


eventotal <- evennessdf %>%
  group_by(scenario) %>%
  summarise_at(vars(Total), funs(Total = mean, median)) # only consider mean of last 20 years of simulation (a
evensd <- evennessdf %>%
  group_by(scenario) %>%
  summarise_at(vars(Total), funs(sd = sd))

eventotal$sd <- evensd$sd

Evenness <- ggplot(evennessdf, aes(x = scenario, y = Total)) +
  geom_jitter(position = position_jitter(0.2), alpha = 0.4) +
  geom_pointrange(aes(ymin = Total - sd, ymax = Total + sd), size = 1, data = eventotal) +
  ylab(bquote("Evenness \nPilou's J")) +
  xlab("\nLand use type") +
  theme_set(theme_minimal()) +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    legend.text = element_text(size = 16),
    legend.direction = "horizontal", legend.position = "none", legend.title = element_blank(),
    legend.background = element_blank(),
    axis.line = element_line(colour = "gray"),
    panel.grid.major = element_line(size = 0.2, linetype = "solid", colour = "gray"),
    panel.background = element_blank()
  )
Evenness

## combine plots ------

# remove legends
richness <- richness + theme(legend.position = "none")
Evenness <- Evenness + theme(legend.position = "none")

richevenplots <- plot_grid(richness, Evenness,
  ncol = 2, nrow = 1,
  rel_widths = c(12, 12),
  align = "h", axis = "bt",
  labels = c("a", "b")
)

biodivricheven_legend <- plot_grid(richevenplots, legend, nrow = 2, rel_heights = c(1, 0.1))
biodivricheven_legend


ggsave(biodivricheven_legend, file="richness_evenness_revised.tiff", width = 34,
       height = 16,
       units = "cm", dpi=600)



###################################################
### evaluate Richness statistically ---------------
###################################################


library(nlme)
library(lme4)

# check assumptions -----

biodiv <- cover[-c(4:5)] # delete PFT and cover columns


# for means check meanrich20total and richtotal
richperclimrep <- sumperyear %>%
  group_by(scenario, climrep) %>%
  summarise_at(vars(Richness), funs(speciesRich = mean)) #

shapiro.test(richperclimrep$speciesRich) # p < 0.01


## Richness -----------------------

mod1 <- lmer(Richness ~ scenario + type + (1 | climrep), data = biodiv)
plot(mod1)

richmod <- glmer(Richness ~ scenario + type + (1 | climrep), data = biodiv, family = "poisson") # model with richness, scenario and type and climrep as random factor
richmod2 <- glmer(Richness ~ scenario + (1 | climrep), data = biodiv, family = "poisson") # model without type

anova(richmod2, richmod) # model including type is better

summary(richmod)
drop1(richmod2, test = "Chi")

# grass richness in cattle 2.945, grass richness in wildlife 9.21 -> increase by fac 3.13


## evenness -----------------

library(rcompanion)
library(FSA)

even <- evennessdf

# order by median
even$scenario <- ordered(even$scenario, levels = c("Cattle low", "Wildlife high", "Wildlife low", "Cattle high"))

kruskal.test(Total ~ scenario, data = even)
# Chi^2 = 109.52, df = 3, p < 0.01

DT <- dunnTest(Total ~ scenario, data = even, method = "bh")

PT <- DT$res

cldList(P.adj ~ Comparison,
  data = PT,
  threshold = 0.05
) # all scenarios differ significantly

## Effect size ---------

epsilonSquared(x = even$Total, g = even$scenario)
# e^2 = 0.92
