#http://stackoverflow.com/questions/23414340/convert-to-local-time-zone-using-latitude-and-longitude

lat = 30
long = -2
time1 <- as.POSIXct("2014-02-12 17:00:00", tz = "EST")
# https://developers.google.com/maps/documentation/timezone/
apiurl <- sprintf("https://maps.googleapis.com/maps/api/timezone/%s?location=%s,%s&timestamp=%d&sensor=%s","xml",lat,long,as.numeric(time1),"false")

library(XML)
library(RCurl) #to read https-Adresses; http://stackoverflow.com/questions/28953193/how-to-access-to-https-page-in-r

tz <- xmlParse(getURL(apiurl))[["string(//time_zone_id)"]]

as.POSIXct(format(time1, tz=tz))
# [1] "2014-02-12 22:00:00 CET"

#the approach describe above does not work for big amounts of data > to many calls to the api slow the whole thing down; 
#instead, use: https://github.com/rundel/timezone

#library("devtools") > https://www.rstudio.com/products/rpackages/devtools/
#install_github("rundel/timezone")
library("timezone")

# Los Angeles - 34.0522? N, 118.2428? W
find_tz(-118.2428,34.0522)
find_tz(-118.2428,34.0522, use_google=TRUE)

# Edinburgh - 55.9500? N, 3.2200? W
find_tz(cbind(-3.2200, 55.9500))

# Istanbul - 41.0128? N, 28.9744? E
find_tz(SpatialPoints(cbind(28.9744,41.0128)))
