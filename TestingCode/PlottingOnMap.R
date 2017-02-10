#http://www.milanor.net/blog/maps-in-r-plotting-data-points-on-a-map/
library(rworldmap)
library(rworldxtra)
newmap <- getMap("USA",resolution = "low")
plot(newmap, xlim = xbnds, ylim = ybnds, asp = 1)

#http://svitsrv25.epfl.ch/R-doc/library/maps/html/map.cities.html
map("world", "China")
map.cities(country = "China", capitals = 2)
a<-map("world", "USA",exact="T",xlim=xbnds,ylim=ybnds)

data(us.cities)
map.cities(us.cities,minpop=2e5,col="red")

#see grid_package.R for integration with grid functions

#https://www.r-bloggers.com/plotting-data-over-a-map-with-r/
#https://baseballwithr.wordpress.com/2014/06/06/creating-hexbin-plots/

#https://www.r-bloggers.com/world-map-panel-plots-with-ggplot2-2-0-ggalt/