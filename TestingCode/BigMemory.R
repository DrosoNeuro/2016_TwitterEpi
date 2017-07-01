#install.packages("bigmemory")
library("bigmemory")
#install.packages("biganalytics")
library("biganalytics")
#install.packages("bigtabulate")
library("bigtabulate")
#install.packages("bigalgebra")
library("bigalgebra")

setwd("~/Dropbox/UZH_Master/Masterarbeit/TwitterEpi/TestingCode/bigmemory")
x <- read.big.matrix("2008.csv",type="integer",header = TRUE,
                     backingfile="airline.bin",descriptorfile = "airline.desc",
                     extraCols="Age")

y <- rbind(x,x)
summary(x)

birthmonth <- function(y) {
  minYear <- min(y[,'Year'], na.rm=TRUE)
  these <- which(y[,'Year']==minYear)
  minMonth <- min(y[these,'Month'], na.rm=TRUE)
  return(12*minYear + minMonth - 1)
}
planeindices <- bigsplit(x, ccols='TailNum')

planeindices <- split(1:nrow(x), x[,'TailNum'])

planeStart <- sapply(planeindices,
                     function(i) birthmonth(x[i, c('Year','Month'),
                                              drop=FALSE]))

#tutorial
#http://www.chrisbilder.com/compstat/presentations/Xiaojuan/Presentation_bigmemory.pdf
mydata=matrix(c(NA),nrow=10072112,ncol=5)
set.seed(12345)
mydata[,1]=sample(c(1:17770), 10072112, replace = TRUE)
mydata[,2]=sample(c(1:480189), 10072112, replace = TRUE)
mydata[,3]=sample(c(1:5), 10072112, replace = TRUE)
mydata[,4]=sample(c(1999:2005), 10072112, replace = TRUE)
mydata[,5]=sample(c(1:12), 10072112, replace = TRUE)
write.table(mydata, file = "example.txt", sep = " ",row.names
            = F, col.names = F)

gc()
start.time<-proc.time()
x <- read.big.matrix("example.txt", header =F,type = "integer",sep = " ",
                     backingfile ="data.bin", descriptor = "data.desc",
                     col.names = c("movie", "customer","rating","year", "month"), 
                     shared=TRUE)
end.time<-proc.time()
save.time<-end.time-start.time
cat("\n Number of minutes running:",save.time[3]/60, "\n \n")

#use advantage of shared descriptor
start.time<-proc.time()
datadesc<-dget("data.desc")
data<-attach.big.matrix(datadesc)
end.time<-proc.time()
save.time<-end.time-start.time
cat("\n Number of minutes running:", save.time[3]/60, "\n \n")
data
head(data)

is.filebacked(x)


#parallel processing
#install.packages("doParallel")
library("doParallel")
datadesc <- dget("data.desc")
data <- attach.big.matrix(datadesc)
movie_uniq <- data[unique(data[,1]),1]
n <- length(movie_uniq)
movie_av_rate <- big.matrix(n,2,type="double")
movie_av_rate[,1] <- movie_uniq

cl<-makeCluster(spec = 2)
registerDoParallel(cl = cl)
library(foreach)
start.time<-proc.time()
clusterSetRNGStream(cl = cl, iseed = 9182)
res<-foreach(i = 1:n,.combine = rbind) %dopar% {
    require(bigmemory)
    data<-attach.big.matrix(datadesc)
    mean(data[mwhich(data,"movie",movie_uniq[i],"eq"),"rating"])}
stopCluster(cl)
movie_av_rate[,2]<-res
end.time<-proc.time()
save.time<-end.time-start.time


#join two matrices
y <- deepcopy(x)

z <- filebacked.big.matrix(nrow=nrow(y)+nrow(x),ncol=ncol(x),
                           backingfile="joined.bin",
                           descriptor = "joined.desc")

cl<-makeCluster(spec = 2)
registerDoParallel(cl = cl)
library(foreach)
start.time<-proc.time()
clusterSetRNGStream(cl = cl, iseed = 9182)
n <- ncol(z)
mx <- nrow(x)
my <- nrow(y)
mz <- nrow(z)
for (i in 1:n){
  z[1:mx,i] <- x[,i]
  z[(mx+1):mz,i] <- y[,i]
}

nrow(z)
gc(reset=T)


for (j = 1:n){
m <- nrow
foreach(i = 1:n,.combine = rbind) %dopar% {
  data<-attach.big.matrix(datadesc)
  mean(data[mwhich(data,"movie",movie_uniq[i],"eq"),"rating"])}
}
stopCluster(cl)
