source("solver_multi.r")


library("chron")
data = read.csv("../data/curves/cdc_long.csv",sep="\t")

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
