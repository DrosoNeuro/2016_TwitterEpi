#Plotting dates using ggplot2
#https://learnr.wordpress.com/2010/02/25/ggplot2-plotting-dates-hours-and-minutes/

library("maptools")

x_seq <- seq(from=as.POSIXct("2010-01-01",tz="GMT"), length.out = 365,by="days")
coord <- matrix(c(-0.13,51.5),nrow=1)

sunrise <- sunriset(coord,x_seq,direction="sunrise",POSIXct.out = TRUE)

head(sunrise,3)

# First the time is converted to a character vector, effectively stripping all the date information. The time is then converted back to POSIXct with today's date - the date is of no interest to us, only the hours-minutes-seconds are.

sunrise$hms <- format(sunrise$time,format="%H:%M:%S")
sunrise$hms <- as.POSIXct(sunrise$hms,format="%H:%M:%S")

head(sunrise,3)

library(ggplot2)
ggplot(sunrise,aes(time,hms)) + geom_line()

#making x-axis clearer
last_plot() + scale_x_datetime("", date_labels = "%b") +   ylab("")


#second try-----
#@http://stackoverflow.com/questions/10770698/understanding-dates-and-plotting-a-histogram-with-ggplot2-in-r

library(scales)
dates <- read.csv("http://pastebin.com/raw.php?i=sDzXKFxJ", sep=",", header=T)

freqs <- aggregate(dates$Date, by=list(dates$Date), FUN=length)
freqs$names <- as.Date(freqs$Group.1, format="%Y-%m-%d")

ggplot(freqs, aes(x=names, y=x)) + geom_bar(stat="identity") +
  scale_x_date(breaks="1 month", labels=date_format("%Y-%b"),
               limits=c(as.Date("2008-04-30"),as.Date("2012-04-01"))) +
  ylab("Frequency") + xlab("Year and Month") +
  theme_bw() 

+ opts(axis.text.x = theme_text(angle=90))

