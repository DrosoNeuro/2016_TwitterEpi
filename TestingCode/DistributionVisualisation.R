#Data Visualisation: http://flowingdata.com/2012/05/15/how-to-visualize-and-compare-distributions/

library("data.table") #for faster reading/handling of data

# Load crime data
crime <- fread("http://datasets.flowingdata.com/crimeRatesByState-formatted.csv")

# Remove Washington, D.C.
crime.new <- crime[crime$state != "District of Columbia",]

# Remove national averages
crime.new <- crime.new[crime.new$state != "United States",]

# Box plots ------
boxplot(crime.new$robbery, horizontal=TRUE, main="Robbery Rates in US")

# Box plots for all crime rates
boxplot(crime.new[,state:=NULL], horizontal=TRUE, main="Crime Rates in US") #stat:=NULL is to remove state column

#Histograms -----------
# Histogram
hist(crime.new$robbery, breaks=10)

# Multiple histograms
par(mfrow=c(3, 3))
colnames <- dimnames(crime.new)[[2]]
for (i in 2:8) {
  hist(crime[[i]], xlim=c(0, 3500), breaks=seq(0, 3500, 100), main=colnames[(i-1)], probability=TRUE, col="gray", border="white")
}

#Density Plots ----
# Density plot
par(mfrow=c(3, 3))
colnames <- dimnames(crime.new)[[2]]
for (i in 2:8) {
  d <- density(crime[[i]])
  plot(d, type="n", main=colnames[(i-1)])
  polygon(d, col="red", border="gray")
}

# Histograms and density lines
par(mfrow=c(3, 3))
colnames <- dimnames(crime.new)[[2]]
for (i in 2:8) {
  hist(crime[[i]], xlim=c(0, 3500), breaks=seq(0, 3500, 100), main=colnames[i], probability=TRUE, col="gray", border="white")
  d <- density(crime[[i]])
  lines(d, col="red")
}

# Density and rug
par(mfrow=c(1,1))
d <- density(crime$robbery)
plot(d, type="n", main="robbery")
polygon(d, col="lightgray", border="gray")
rug(crime$robbery, col="red")

#violin plots-----
library("vioplot")
vioplot(crime.new$robbery, horizontal=TRUE, col="gray")

#bean plot -----
# Bean plot
library("beanplot")
beanplot(crime.new[,state:=NULL])


