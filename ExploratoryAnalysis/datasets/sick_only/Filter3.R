## 2) Add a subfolder "raw" in which you store the RData-file that was 
##    produced by "load_csv.R" and which you want to filter.
## 3) Make sure to indicate a valid path to the "functions" containing all necessary auxiliary functions
## 4) recommended usage: Terminal > RScript FilterAndExplore.R path
##    default path is "../../functions"

args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  warning("Path to functions folder was not supplied, using default path.\n")
  args <- "../../functions/"
} 

#load necessary functions
require("MASS")
require("data.table")
require("maps")
require("ggplot2")
file.sources <- list.files(path=args[1],pattern="*.R")
file.sources <- paste0(args[1],file.sources)
sapply(file.sources,source,.GlobalEnv)

#load RData files to process
files_to_process <- list.files(path="raw/","*.RData")

#create output directory if it doesn't already exist
if (!file.exists("processed")){
  dir.create("processed")
}

#create output directory for plots
if (!file.exists("plots")){
  dir.create("plots")
}

#process files
for (i in files_to_process){
  tag <- gsub("(.*)\\.RData","\\1",i)
  name <- load(paste0("raw/",i),verbose=T)
  df <- get(name)
  rows_before <- nrow(df)
  dg <- 0.01
  dist <- 10000
  #remove all tweets outside specific window
  coord_USA <- c(-125,-66,25,50)
  selec_coords <- coord_selection(df,coord_USA)
  df <- selec_coords[[1]]
  rm(selec_coords)
  gc()
  rows_after <- nrow(df)
  rm_raw_window <- rows_before-rows_after
  
  df <- state_lookup(df,target=c("state","."),dg=dg,dist=dist,exact=T)
  save(list=c("rows_after","rm_raw_window","coord_USA","df","dg","dist"),paste0("processed/processed_", tag, ".RData"))
}
