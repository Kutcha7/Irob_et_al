# ==============================================
# ------- Model output anlaysis: factor analysis and clustering, functional dispersion
# ==============================================
# Irob et al., 2021 ---------------------------
# Author of R script: Katja Irob (irob.k@fu-berlin.de)
# ==============================================

rm(list = ls()) # clears working environment

library(tidyverse)
library(cowplot)
library(reshape2)
library(scales)
library(here)
library(FD)
library(psych)
library(GPArotation)
library(factoextra)
library(cowplot)
library(NbClust)
library(ggrepel)

source(here::here("R/Utility.R"))


# Read data and calculate mean cover per scenario and sub-pft for last 20 years

PFTcoverall <- readfiles(path = "Data/Results")

### bringing the DF in the right format -----

cover <- select(PFTcoverall, contains("Cover"), c("year", "scenario"))
cover <- select(cover, starts_with("mean"), c("year", "scenario"))

cover <- cover[, !names(cover) %in% c("meanGtotalcover", "meanAtotalcover", "meanStotalcover"), drop = F]

cover <- cover %>% filter(year > 79) # only consider last 20 years
coverforCluster <- cover

cover <- melt(cover, id.vars = c("year", "scenario"))

cover <- cover %>%
  rename(
    PFT = variable,
    cover = value
  )

# create extra columns for land use and broad PFT types ---
cover$landuse <- ifelse(grepl("(browse)", cover$scenario), "Wildlife", "Cattle")
cover$type <- ifelse(grepl("(meanGCover)", cover$PFT), "Perennial", ifelse(grepl("(meanSCover)", cover$PFT), "Shrub", "Annual"))

# rename PFTs ---
cover$PFT <- as.character(cover$PFT) # this is important otherwise it will error

# renaming PFTs according to trait matrix ---
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


#### #### #### #### #### #### #### #### #### #### 
#### Functional diversity Matrices --------------
#### #### #### #### #### #### #### #### #### #### 

# create abundance of strategy type by scenario matrix ------

# bring abundance matrix in same order as trait matrix 

cover$PFT <- factor(cover$PFT, levels = c(
  "Perennial_Bp", "Perennial_Bd", "Perennial_Cb", "Perennial_Cp", "Perennial_Pr",
  "Perennial_Pb", "Perennial_Rb", "Perennial_Rp", "Perennial_Base", "Shrub_Bd",
  "Shrub_Bp", "Shrub_Bc", "Shrub_Cb", "Shrub_Db", "Shrub_Dr",
  "Shrub_Dc", "Shrub_Mb", "Shrub_Rd", "Shrub_Rc", "Shrub_Base",
  "Annual_Base"
))

meancover <- cover %>%
  group_by(scenario, landuse, PFT) %>%
  summarise_at(vars(cover), funs(mean)) # only consider mean of last 20 years of simulation (and all climreps)
meancover$cover <- meancover$cover * 100 # cover in percentage

# separate dfs for wildlife and cattle ---
Wildlife <- meancover[meancover$landuse == "Wildlife", ]
Cattle <- meancover[meancover$landuse == "Cattle", ]

Wildlife <- Wildlife[, -2]
Cattle <- Cattle[, -2]

# create abundance matrix for wildlife ---
Wild_mat <- spread(Wildlife, PFT, cover)

Wild_Mat <- data.matrix(Wild_mat, rownames.force = NA)
Wild_Mat <- Wild_Mat[, -1]
rownames(Wild_Mat) <- Wild_mat$scenario # this is important for FD function input

# set threshold to only include species above a cover of 2% in biodiv measures
Wild_Mat <- replace(Wild_Mat, Wild_Mat < 2.0, 0)

Wild_mat2_updated <- Wild_Mat[, -c(10:13, 15, 17, 20)] # delete extinct species

# write.table(Wild_Mat, file="wildlife_matrix.txt", sep="\t", row.names=T, quote=F) # save abundance matrix

# bring in trait matrix

wild_trait <- read.table(here("./Data/Matrices_for_EFA", "trait_matrix_wildlife.txt"), header = T)

wild_trait_updated <- wild_trait[-c(10:13, 15, 17, 20), ] # delete extinct species

# RUN FD FUNCTION ------------------------

wild_FD <- dbFD(wild_trait_updated, Wild_mat2_updated, messages = T)
# FDis SR20browse = 1.82, SR40browse = 1.19

# plot dengrogram of species based on functional traits
wild_clust <- dbFD(wild_trait_updated, Wild_mat2_updated,
  corr = "cailliez",
  calc.FGR = TRUE, clust.type = "ward"
)


# cattle -----------------------------

cattle_mat <- spread(Cattle, PFT, cover)

Cattle_Mat <- data.matrix(cattle_mat, rownames.force = NA)
Cattle_Mat <- Cattle_Mat[, -1]
rownames(Cattle_Mat) <- cattle_mat$scenario

Cattle_Mat <- replace(Cattle_Mat, Cattle_Mat < 2.0, 0)

Cattle_Mat_no_extinct_sp <- Cattle_Mat[, -c(1:3, 6:7, 20)] # delete extinct species

# write.table(Cattle_Mat, file="cattle_matrix.txt", sep="\t", row.names=T, quote=F) # save matrix to local

# bring in the trait matrix
cattle_trait <- read.table(here("./Data/Matrices_for_EFA", "trait_matrix_cattle.txt"), header = T)

cattle_trait <- cattle_trait[-c(1:3, 6:7, 20), ] # delete species in trait matrix

cattle_FD <- dbFD(cattle_trait, Cattle_Mat_no_extinct_sp, messages = T)
# FDis SR20graze = 2.62, SR40graze = 2.48


# testing different clustering type methods to determine the optimal number of clusters --
clust <- dbFD(cattle_trait, Cattle_Mat_no_extinct_sp,
  corr = "cailliez",
  calc.FGR = TRUE, clust.type = "ward"
)

clust2 <- dbFD(cattle_trait, Cattle_Mat_no_extinct_sp,
  corr = "cailliez",
  calc.FGR = TRUE, clust.type = "kmeans", km.sup.gr = 2)


#############################################
##### Factor analysis / Cluster analysis ######
#############################################

df2 <- data.frame(t(cattle_trait[-1]))

# calculating optimal number of factors
coverforCluster <- coverforCluster[1:21]
parallel <- fa.parallel(coverforCluster, fm = "minres", fa = "fa")
# scree plots
# factor analysis for 2 factors
twofactor <- fa(coverforCluster, nfactors = 2, rotate = "oblimin", fm = "minres")

# saving data for later graphs and further analysis
plotting_data <- (twofactor[["loadings"]])
plotting_data <- matrix(plotting_data[1:42], ncol = 2)
plotting_data <- as.data.frame(plotting_data)


## some data preparation for later graphs ---

Wild_Mat_trans <- data.frame(t(Wild_Mat))


Cattle_Mat_trans <- data.frame(t(Cattle_Mat))


plotting_data_clust <- (twofactor[["loadings"]])



#############################
###### cluster analysis #####
#############################

## cluster analysis based on the factor analysis ---
# determining the optimal number of clusters
fviz_nbclust(plotting_data , kmeans, method =  "silhouette")
fviz_nbclust(plotting_data , kmeans, method =  "wss")
fviz_nbclust(plotting_data , kmeans, method =  "gap_stat")
# plots indicate 2-3 clusters

# cluster analysis with  2 and 3 clusters
# calculation of within cluster some of squares by cluster to look at variability within clusters
# 2 clusters
km.res_2clusters <- kmeans(plotting_data, 2, nstart = 25) 
km.res_2clusters
# WCS perennial cluster = 3.27, WCS shrub cluster = 0.33
# 3 clusters
km.res_3clusters <- kmeans(plotting_data, 3, nstart = 25)
km.res_3clusters

#### including PFTs in plotting data
plotting_data$name <- c("Base_P","Cb", "Cp", "Pr", "Pb", "Rb", "Rp", "Bp", "Bd", "Base_S", "Cb", "Rd", "Rc","Bd","Bc","Bp", "Dc","Db", "Dr","Mb" , "Base_A" )

## Exploratory factor analysis ---
# we conducted an exploratory factor analysis (EFA) which is an extraction method that searches for underlying factors which explain the variability within the data - 2 factors where extracted - 
# based on those factors, 2 clusters were identified which generally correspond to shrubs vs perennials)
plotting_data$class <- 1

## calculating centroids ---
# calculating centroids is done the same way functional dispersion is calculated (mean of the 2 dimensional distance (i.e mean position within the 2 dimensional coordinate system) weighted by the abundance of the PFT)
plotting_data$abundance <- Wild_Mat_trans$SR20browse
plotting_data$Type <- rep("Wildlife_high", length(plotting_data$abundance))
plotting_data2 <- plotting_data
plotting_data3 <- plotting_data
plotting_data4 <- plotting_data

plotting_data2$abundance <- Wild_Mat_trans$SR40browse
plotting_data3$abundance <- Cattle_Mat_trans$SR20graze
plotting_data4$abundance <- Cattle_Mat_trans$SR40graze
plotting_data2$Type <- rep("Wildlife_low", length(plotting_data$abundance))
plotting_data3$Type <- rep("Cattle_high", length(plotting_data$abundance))
plotting_data4$Type <- rep("Cattle_low", length(plotting_data$abundance))

plotting_data <- rbind(plotting_data2, plotting_data, plotting_data3, plotting_data4)

## generating the centroids ---
plotting_data$controid_dataV1 <- plotting_data$V1 * plotting_data$abundance
plotting_data$controid_dataV2 <- plotting_data$V2 * plotting_data$abundance


subsets <- split(plotting_data, plotting_data$Type)
## Cattle high
Cattle_high_centroidx <- sum(subsets[["Cattle_high"]]$controid_dataV1) / sum(subsets[["Cattle_high"]]$abundance)
Cattle_high_centroidy <- sum(subsets[["Cattle_high"]]$controid_dataV2) / sum(subsets[["Cattle_high"]]$abundance)

## Cattle low
Cattle_low_centroidx <- sum(subsets[["Cattle_low"]]$controid_dataV1) / sum(subsets[["Cattle_low"]]$abundance)
Cattle_low_centroidy <- sum(subsets[["Cattle_low"]]$controid_dataV2) / sum(subsets[["Cattle_low"]]$abundance)

## Wildlife high
Wildlife_high_centroidx <- sum(subsets[["Wildlife_high"]]$controid_dataV1) / sum(subsets[["Wildlife_high"]]$abundance)
Wildlife_high_centroidy <- sum(subsets[["Wildlife_high"]]$controid_dataV2) / sum(subsets[["Wildlife_high"]]$abundance)

## Wildlife low
Wildlife_low_centroidx <- sum(subsets[["Wildlife_low"]]$controid_dataV1) / sum(subsets[["Wildlife_low"]]$abundance)
Wildlife_low_centroidy <- sum(subsets[["Wildlife_low"]]$controid_dataV2) / sum(subsets[["Wildlife_low"]]$abundance)

scen_list <- c("Cattle low", "Wildlife low", "Cattle high", "Wildlife high")

######
###### create a new variable for PFT (Shrub, Perennial, Annual)

t <- c(rep("Perenial", 9), rep("Shrub", 11), "Annual")
plotting_data$PFT <- c(rep(t, 4))

plotting_data$Type <- as.character(plotting_data$Type)
plotting_data$Type[plotting_data$Type == "Cattle_low"] <- "Cattle low"
plotting_data$Type[plotting_data$Type == "Cattle_high"] <- "Cattle high"
#
plotting_data$Type[plotting_data$Type == "Wildlife_high"] <- "Wildlife high"
plotting_data$Type[plotting_data$Type == "Wildlife_low"] <- "Wildlife low"

plotting_data$Type <- factor(plotting_data$Type, levels = c("Cattle low", "Wildlife low", "Cattle high", "Wildlife high"))

## creating the plot ----------------------------

cols <- c("coral", "seagreen", "gold1")

plot2 <- ggplot(
  data = plotting_data, id.vars = c("V1", "V2", "abundance", "name"),
  aes(x = V1, y = V2)
) +
  geom_point(aes(x = V1, y = V2, size = abundance, col = PFT)) +
  geom_text_repel(max.overlaps = Inf, aes(label = name)) +
  ylab(bquote("Sensitivity to herbivory")) +
  xlab("Grasslike vs. Shrublike") +
  scale_color_manual(values = cols) +
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
  facet_wrap(~Type, ncol = 2) +
  theme(legend.position = "none")

## helper plot to get legend from ---

plot3 <- ggplot(
  data = plotting_data, id.vars = c("V1", "V2", "abundance", "name"),
  aes(x = V1, y = V2, label = name)
) +
  geom_point(aes(x = V1, y = V2, size = abundance, col = PFT)) +
  geom_label_repel(aes(label = name)) +
  ylab(bquote("Sensitivity to herbivory")) +
  xlab("Grasslike vs. Shrublike") +
  scale_color_manual(values = cols) +
  facet_wrap(~Type, ncol = 2)



## getting the legend ---

get_legend <- function(plot3) {
  tmp <- ggplot_gtable(ggplot_build(plot3))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(plot3)

## function to split up faceted graph into multiple graphs ---

splitFacet <- function(x, n = NULL) {
  facet_vars <- names(x$facet$params$facets)
  if (is.null(n)) {
    x$facet <- ggplot2::ggplot()$facet
    datasets <- split(x$data, x$data[facet_vars])
  } else {
    inter0 <- interaction(x$data[facet_vars], drop = TRUE)
    inter <- ceiling(as.numeric(inter0) / n)
    datasets <- split(x$data, inter)
  }
  new_plots <- lapply(datasets, function(new_data) {
    x$data <- new_data
    x
  })
}


## splitting the plot and including the centroids ---

new_plots2 <- splitFacet(plot2,1)
new_plots2[[1]] <- new_plots2[[1]] + geom_segment( aes(x=Cattle_low_centroidx, y=Cattle_low_centroidy, xend=V1, yend=V2),col ="coral", linetype = "dashed")+   geom_text(label = "FDis: 2.48", x = 0.7, y=0.7)+  geom_point( aes(x=Cattle_low_centroidx, y=Cattle_low_centroidy))

new_plots2[[3]] <- new_plots2[[3]] + geom_segment( aes(x=Cattle_high_centroidx, y=Cattle_high_centroidy, xend=V1, yend=V2),col ="coral", linetype = "dashed")+   geom_text(label = "FDis: 2.62", x = 0.7, y=0.7)+  geom_point( aes(x=Cattle_high_centroidx, y=Cattle_high_centroidy))
new_plots2[[2]] <- new_plots2[[2]] + geom_segment( aes(x=Wildlife_low_centroidx, y=Wildlife_low_centroidy, xend=V1, yend=V2),col ="coral", linetype = "dashed")+   geom_text(label = "FDis: 1.19", x = 0.7, y=0.7)+  geom_point( aes(x=Wildlife_low_centroidx, y=Wildlife_low_centroidy))
new_plots2[[4]] <- new_plots2[[4]] + geom_segment( aes(x=Wildlife_high_centroidx, y=Wildlife_high_centroidy, xend=V1, yend=V2),col ="coral", linetype = "dashed")+   geom_text(label = "FDis: 1.82", x = 0.7, y=0.7)+  geom_point( aes(x=Wildlife_high_centroidx, y=Wildlife_high_centroidy))



## reassembling the plot ---
coverplots <- plot_grid(new_plots2[[1]], new_plots2[[2]] + theme(
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank(),
  axis.title.y = element_blank()
),
new_plots2[[3]], new_plots2[[4]] + theme(
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank(),
  axis.title.y = element_blank()
),
ncol = 2, nrow = 2, labels = c("a", "b", "c", "d"),
align = "hv"
)
## adding the legend to the plot ---

CFA_legend <- plot_grid(coverplots, legend, ncol = 2, rel_widths = c(5, 1)) # change nrow back to 2
CFA_legend
# saving the plot
# ggsave(CFA_legend, file="Plot_Cluster_by_cover.tiff", width = 34,
#        height = 16,
#        units = "cm", dpi=600)

