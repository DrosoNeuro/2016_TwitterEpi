#https://www.r-bloggers.com/how-the-ghana-floods-animation-was-created/
#http://stackoverflow.com/questions/7160565/how-to-create-a-time-scatterplot-with-r
setwd("~/Dropbox/UZH_Master/Masterarbeit/TwitterEpi/TestingCode/animation")
library(ggmap)
library(ggplot2)
library(gganimate)
g <- read.csv("ghanafloods_data.csv")

head(g)
g$date <- as.Date(g$date, format = "%d/%m/%Y")
head(g)

g <- g[order(g$date),]
head(g)

p <- ggmap(get_map("Ghana", zoom = 7))
print(p)
p + geom_point(data = g, aes(x = lon, y = lat))
p + geom_point(data = g, aes(x = lon, y = lat, col = Legend)) +
  scale_color_manual(values ="red1")
p + geom_point(data = g, aes(x = lon, y = lat, col = Legend), size = 7) + scale_color_manual(values ="red1")

p + geom_point(data = g, aes(x = lon, y = lat, col = Legend), size = 7, alpha = 0.3) + scale_color_manual(values ="red1") + 
  labs(title = "Ghana Floods from March, 2016 - July, 2016 \n")


p <- ggmap(get_map("Ghana", zoom = 7))
print(p)
suppressWarnings(p <- p + geom_point(data = g, aes(x = lon, y = lat,frame = date, col = Legend, cumulative = FALSE), 
                    size = 7, alpha = 0.3) + scale_color_manual(values ="red1") + 
  labs(title = "Ghana Floods from March, 2016 - July, 2016 \n"))
#convert = "im convert" if using ImageMagick or convert = "gm convert" if using GraphicMagick
gganimate(p)
#, outfile = "outfile.gif", convert = "im convert", ani.width = 700, title_frame = TRUE)



#http://stackoverflow.com/questions/32442276/combine-multiple-plots-to-a-gif
library(animation)

saveGIF({
  for (i in 1:10) plot(runif(10), ylim = 0:1)
})

saveHTML({
  for (i in 1:10) plot(runif(10), ylim = 0:1)
})
