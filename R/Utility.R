#### reading in all outputfiles returned as one dataframe
readfiles <- function(path) {
  files <- list.files(path = here::here(path), pattern = "yearly", full.names = T)


  outputfiles <- lapply(files, function(x) {
    read.table(file = x, header = T, sep = "\t", skipNul = TRUE)
  })

  # extracting scenario and climrep from filename
  scenarios <- as.list(gsub(".*EH_\\s*|_.*", "", files))
  climrep <- as.list(gsub(".*climrep-\\s*|_.*", "", files))

  PFTs <- Map(cbind, outputfiles, scenario = scenarios, climrep = climrep) # adding extra column with scenario name
  PFTs <- do.call("rbind", PFTs) # merging list into df


  PFTs <- select(PFTs, contains("Cover"), c("year", "scenario", "climrep")) # extracting only parameters of interest
  PFTs <- select(PFTs, starts_with("mean"), c("year", "scenario", "climrep"))


  no <- c("meanRCover")
  PFTs <- PFTs[, !names(PFTs) %in% no, drop = F] # drop = F means that it should be a df not a list



  return(PFTs)
}

# Function to rename grass pfts to reflect strategies in a dataframe
renamePFT_grass <- function(df){
  df$PFT[df$PFT=="meanGCover0"]<-"Base"
  df$PFT[df$PFT=="meanGCover1"]<-"Cb"
  df$PFT[df$PFT=="meanGCover2"]<-"Cp"
  df$PFT[df$PFT=="meanGCover3"]<-"Pr"
  df$PFT[df$PFT=="meanGCover4"]<-"Pb"
  df$PFT[df$PFT=="meanGCover5"]<-"Rb"
  df$PFT[df$PFT=="meanGCover6"]<-"Rp"
  df$PFT[df$PFT=="meanGCover7"]<-"Bp"
  df$PFT[df$PFT=="meanGCover8"]<-"Bd"
  return(df)
}

# Function to rename shrub pfts to reflect strategies in a dataframe
renamePFT_shrub <- function(df){
  df$PFT[df$PFT=="meanSCover0"]<-"Base"
  df$PFT[df$PFT=="meanSCover1"]<-"Cb"
  df$PFT[df$PFT=="meanSCover2"]<-"Rd"
  df$PFT[df$PFT=="meanSCover3"]<-"Rc"
  df$PFT[df$PFT=="meanSCover4"]<-"Bd"
  df$PFT[df$PFT=="meanSCover5"]<-"Bc"
  df$PFT[df$PFT=="meanSCover6"]<-"Bp"
  df$PFT[df$PFT=="meanSCover7"]<-"Dc"
  df$PFT[df$PFT=="meanSCover8"]<-"Db"
  df$PFT[df$PFT=="meanSCover9"]<-"Dr"
  df$PFT[df$PFT=="meanSCover10"]<-"Mb"
  return(df)
}

# Calculate mean cover of last 20 years for all sub-pfts per scenario

makeMeanCover <- function(df) {
  cover <- select(df, contains("Cover"), c("year", "scenario", "climrep"))
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
