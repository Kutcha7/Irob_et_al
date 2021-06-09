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
  PFTs <- PFTs[, !names(PFTs) %in% no, drop = F] # drop =F means that it should be a df not a list



  return(PFTs)
}