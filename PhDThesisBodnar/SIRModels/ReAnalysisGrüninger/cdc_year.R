source("solver_multi.R")


library("chron")
data = read.csv("data/curves/cdc_long.csv",sep="\t")
data1 <- read.csv("data/curves/prediction_r_results.csv",sep=",")
plot(data1$cdcoffset,ylim=c(0,10),xlim=c(0,150),col="red")
lines(data1$full_base,col="blue")
lines(data1$full_both2,col="blue",lty=5)
lines(data1$predictions_both2,col="black",lty=2)

data2 <- read.csv("data/curves/find_cutoffs_correlations.csv",sep=" ")

datasub = data[50:100,]

datasub$ILI = datasub$ILI/100

data$ILI = data$ILI/100

year1 = data[1:46,]$ILI
year2 = data[47:94,]$ILI
year3 = data[95:152,]$ILI

alldata = list(year1,year2,year3)

print(fitSIR(year1))
print(fitSIR(year2))
print(fitSIR(year3))

print(fitSIR(alldata))
