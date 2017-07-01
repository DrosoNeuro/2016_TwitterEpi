## 1) Put this file into the folder you want to have your output stored in
## 2) Add a subfolder "singles" in which you store the single RData-file that was 
##    produced by the "feather_to_matrix function" and which you want to combine and filter.
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

#create output directory if it doesn't already exist
if (!file.exists("processed")){
  dir.create("processed")
}

#create output directory for plots
if (!file.exists("plots")){
  dir.create("plots")
}

#create output directory for combined bigmatrix files
if (!file.exists("raw")){
  dir.create("raw")
}

#start and end date
start <- as.Date("2011-03-05")
end <- as.Date("2015-02-28")

#load RData files to process
files_to_process <- list.files(path="raw/","*.RData")

load(paste0("raw/",files_to_process[1]))
cdc_data_nat_reg <- cdc_data_nat_reg[,c(2,6,13,15,16),with=F]
colnames(cdc_data_nat_reg) <- c("region","rel_sick","sick","total","date")
cdc_data_nat_reg[,healthy:= total-sick]
cdc_data_nat_reg[,rel_healthy:=healthy/total]
cdc_data_nat_reg <- cdc_data_nat_reg[,.(region,date,sick,total,rel_sick,rel_healthy)]
cdc_data_nat_reg <- cdc_data_nat_reg[date>start & date<=end,]

load(paste0("raw/",files_to_process[2]))
cdc_data_state <- cdc_data_state[,c(1,4,5,6),with=F]
colnames(cdc_data_state) <- c("statename","activity_level","activity_level_label","date")
cdc_data_state <- cdc_data_state[date>start & date<=end,]

save(list=c("cdc_data_nat_reg","cdc_data_state","start","end"),file="processed/cdc_data.RData")


