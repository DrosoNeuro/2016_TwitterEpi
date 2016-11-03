#see http://rstudio-pubs-static.s3.amazonaws.com/5661_7b24039f31ef470889815352c3d9bf4d.html for more info on using hexbin in maps

#see: http://www.thefactmachine.com/binning-spatial-data-with-hexagons/
library("rgeos")
library("ggplot2")
library("maptools") #for readShapePoly
library("broom")
library("raster") #for using crop


root_path <- "C:/Users/DrosoNeuro/Dropbox/UZH_Master/Masterarbeit/Journal/TwitterData/tweets_from_todd"
USA_state <- readShapePoly(paste0(root_path,"/cb_2015_us_state_500k/cb_2015_us_state_500k.shp"))
USA_county <- readShapePoly(paste0(root_path,"/cb_2015_us_county_500k/cb_2015_us_county_500k.shp"))
names(USA)

out <- crop(wrld_simpl,extent(-180,180,0,180))
USA_state <- crop(USA_state,extent(-130,0,25,55))
plot(out)

usa_geom <- tidy(USA_state,region="NAME")

usa_geom <- tidy(USA_state, region = "NAME")  #tidy extracts the coords of each polygon.  Sounds like the same way google maps plots polygons in a mashup.  Holy crap this takes forever!
head(usa_geom)

usa_map_df <- merge(usa_geom, USA_state, by.x = "id", by.y = "NAME")  #Creates the dataframe of polygons.

ggplot(usa_map_df, aes(long, lat, group = group)) + geom_polygon(data = usa_map_df,                                                                 aes(fill = ALAND)) + coord_equal() + scale_fill_gradient(low="yellow",high="red") + geom_path(data = usa_geom, aes(long, lat, group = group),                                                                                                                                                                                                                                                                                                                                                                     lty = 3, lwd = 0.1, color = "white")
# Create a map:
map1 <- ggplot(usa_map_df, aes(long, lat, group = group)) + geom_polygon(data = usa_map_df,                                                                    aes(fill = Bush_pct)) + coord_equal() + scale_fill_gradient(low = "yellow",                                                                                                                                      high = "red") + geom_path(data = usa_geom, aes(long, lat, group = group),                                                                                                                                                                lty = 3, lwd = 0.1, color = "white")
map1