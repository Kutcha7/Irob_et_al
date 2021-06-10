################################################################################
###### Parameterisation of plant strategy types in browsing scenario ##########
################################################################################
# Sensitivity output ----------------
# Irob et al., 2021 ---------------------------
# Author of R script: Katja Irob (irob.k@fu-berlin.de)
# ==============================================


require(dplyr)
require(data.table)
require(ggplot2)
library(plotly)
library(viridis)

rm(list=ls()) # clears working environment 
# read file path
all_paths <-
  list.files(path = "~/Input/Browse/",
             pattern = "*.txt",
             full.names = TRUE)

########
# read file content
all_content <-
  all_paths %>%
  lapply(read.table,
         header = TRUE,
         sep = "\t",
         encoding = "UTF-8")


files <- dir("~/Input/Browse/") 
setwd("~/Split_files_browse/")
parms <- read.table("~/Parms/Sens_output_parameter_browsing_na.txt", header = T, sep="\t")

PFT <-as.list(gsub("_sensoutput.*", "", files))
# make sure split files folders (graze + browse) are empty!!! -----------
setwd("~/Split_files_browse/")


for (i in 1:length(all_content)){
  subsets <- split(all_content[[i]], all_content[[i]][["ParameterName"]])
  N <- gsub (":","", names(subsets))
  for (j in 1:length(subsets)){
    write.table(subsets[j], file = paste0(PFT[i],"_",N[j], ".txt"), sep="\t", row.names = FALSE, quote=FALSE)
}
}


# read file path
all_paths <-
  list.files(path = "~/Split_files_browse",
             pattern = "*.txt",
             full.names = TRUE)


# read file content
  all_content <-
  all_paths %>%
  lapply(read.table,
         header = TRUE,
         sep = "\t",
         encoding = "UTF-8")

# read file name
all_filenames <- all_paths %>%
  basename() %>%
  as.list()

###### now calculate the linear regression and put everything in one dataframe
### create empty matrix
resultsdf <- matrix  (ncol=8, nrow=length(all_content))
for(i in 1:length(all_content)){
  resultsdf[i,] <- runif(2)
  
}

##### turn it into a dataframe
resultsdf <- data.frame(resultsdf)
colnames(resultsdf) <- c("Cover", "Parameter95", "Parameter105", "default", "significance", "PFT", "Parameter", "Rsquared")
resultsdf$default <- parms$default

#### Fill PFTs
files <- dir("~/Split_files_browse")
PFT <-as.list(gsub("_.*", "", files))

resultsdf$PFT <- PFT
#Fill parameter

files <- dir("~/Split_files_browse")
Parameter <- (gsub(".*_", "", files))
Parameter <- (gsub(".txt", "", Parameter))

resultsdf$Parameter <- Parameter

 
##### now fill the Dataframe with the right values 
for(i in 1:length(all_content)){
 names(all_content[[i]]) <- c("ParameterName", "ParameterValue", "Cover")
 helplm <- lm(all_content[[i]][["Cover"]]~all_content[[i]][["ParameterValue"]])
 summary <- summary(helplm)
 resultsdf$Rsquared[i] <- summary[["r.squared"]]
 
 # extracting slope and intersect from helplm summary
 aP1 <- coef(helplm)["(Intercept)"] 
 bP1 <- summary(helplm)$coefficients[2]
 
 
 defcovP1 <- aP1 + bP1*  resultsdf$default[i]# cover in linear regression at default parameter value
 resultsdf$Cover[i] <-  c(defcovP1)
 
 # calculating +/- 5% cover change values 
 defPlusP1 <- defcovP1*1.1 
 defMinP1 <- defcovP1*0.9

resultsdf$Parameter105 [i]<- (defPlusP1-aP1)/bP1 
resultsdf$Parameter95 [i] <- (defMinP1-aP1)/bP1 

# check and print if parameter change was significant in linear regression
anova <- anova(helplm)
if (anova$`Pr(>F)`[1] < 0.05 ){
  resultsdf$significance[i] <- "significant"
}
  else{
    resultsdf$significance[i] <- "not significant"
  }

}

names(resultsdf)[7] <- "ParameterName"

unique(resultsdf$ParameterName) # check if no levels are doubled 


# change perennials grazeprefer to 0 -> model will error if negative input values are given
setwd("~/Sensitivity/") # set  dir to sensitivity

resultsdf$PFT <- vapply(resultsdf$PFT, paste, collapse = ", ", character(1L))

resultsdf_sens <- resultsdf[, c(6, 7, 4, 1, 2, 3, 5, 8)]

write.table(resultsdf_sens, file = "resultsdf_sens_browse.txt", sep="\t", row.names= F, quote=F)


# combine file content list and file name list
all_lists <- mapply(c, all_content, all_filenames, SIMPLIFY = FALSE)


# unlist all lists and change column name
all_result <- rbindlist(all_lists, fill = T)
# change column name
names(all_result)[3] <- "Cover"
names(all_result)[4] <- "PFT"


unique(all_result$ParameterName)


all_result$PFT <- gsub("_.*", "", all_result$PFT)
subsets_PFT <- split(all_result, all_result$PFT)

# plot sensoutput for perennials and shrubs ---------------


for (i in 1:length(subsets_PFT)) {
  
  helpdf <- do.call(rbind.data.frame, subsets_PFT[i])
  subsetsresults <- split(resultsdf, resultsdf$PFT)
  helpdf2 <- do.call(rbind.data.frame, subsetsresults[i])
  names(helpdf2)[7] <- "ParameterName"
  
  helpdf2$Parameter95
  streu_plot<- ggplot(helpdf, aes(x=ParameterValue, y=Cover))+
    geom_smooth(method='lm', formula= y~x)+
    geom_vline(aes(xintercept=default), data=helpdf2, colour="red")+
    geom_vline(aes(xintercept=Parameter95), data=helpdf2, linetype="dashed")+
    geom_vline(aes(xintercept=Parameter105), data=helpdf2, linetype="dashed")+
    facet_grid(~ParameterName, scales = "free_x")+
    theme(text = element_text(size=14),
          strip.text.x = element_text(size = 7.5),
          axis.text.x = element_text(angle=90, hjust=1, size=7)) +
    xlab("Parameter Value")+
    ylab(bquote("Cover")) 
  
  ggsave(streu_plot, file=paste(helpdf$PFT[1], "_browse_lines_10.png", sep=''), dpi=300)

  
}


#### scatterPLOTS ---

setwd("~/Browse/")

path<-("~/Sensitivity/")

readfiles<- function(path= "~/Input/Browse/") {
  files<-list.files(path= path, pattern=".txt")
  
  outputfiles<-lapply(files, function(x) {
    read.table(file=x, header=T, sep="\t")
  })
  
  PFT <-as.list(gsub("_.*", "", files))
  all_out<-Map(cbind, outputfiles, PFT=PFT) # adding extra column with PFT name 
  all_out<-do.call("rbind", all_out) # merging list into df
  
  return(all_out)
}

all_browse <-readfiles()

all_browse$climreps<-rep(1:30)

all_browse <- with(all_browse, all_browse[order(PFT, ParameterName), ]) # puts parameters in alphabetical order

#### PLOT of all output + default & intercept ------- 

PFT_list<-unique(all_browse$PFT)

for (i in 1:length(PFT_list)) {
  
  streu_plot<- ggplot(subset(all_browse, all_browse$PFT==PFT_list[i]), aes(x=ParameterValue, y=Cover, colour=climreps))+
    geom_point()+
    geom_smooth(method='lm', formula= y~x)+
    scale_colour_viridis()+
    facet_grid(~ParameterName, scales = "free_x")+
    theme(text = element_text(size=14),
          strip.text.x = element_text(size = 8, angle=90),
          axis.text.x = element_text(angle=90, hjust=1, size=7)) +
    xlab("Parameter Value")+
    ylab(bquote("Cover")) +
    ggtitle(paste(PFT_list[i], " Browsing", sep=''))
  
  ggsave(streu_plot, file=paste(path, PFT_list[i], "_browse_streu_10.png", sep=''), dpi=300)
  
  
}



