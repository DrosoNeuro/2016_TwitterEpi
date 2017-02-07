library('hexbin')
library("classInt")
library("RCurl")
library("grid")

#http://www.thefactmachine.com/binning-spatial-data-with-hexagons/
x <- getURL("https://raw.githubusercontent.com/thefactmachine/hex-binning-gis-data/master/xyz.csv")
df <- read.csv(text = x)

coord_selec <- c(-125,-66,25,50) #select only tweets from mainland USA
selec <- dataframe[which(dataframe[,"longitude",]>=coord_selec[1] & dataframe[,"longitude"] <= coord_selec[2] & dataframe[,"latitude"] >= coord_selec[3] & dataframe[,"latitude"] <= coord_selec[4]),]

hbin <- hexbin(selec$longitude,selec$latitude,xbins=100,IDs=T)
hbin@count <- log(hbin@count)#log-transforming counts in order to improve readability
#set the colors, number intervals, interval location
cr <- colorRampPalette(c("white","blue"))

gplot.hexbin(hbin,style="colorscale",pen=0,border= 'white', minarea = 0.01, maxarea = 1,colramp=cr,legend=1.5,mincnt=0, xlab="longitude",ylab="latitude",main="Tweets on longitude/latitude")

hvp <- hexViewport(hbin)

# sick_df <- cbind(sick_df,1) #adding ones to 
# colnames(sick_df) <- c('userID','longitude','latitude','time','sick','state',"tweets")
# mtrans <- hexTapply(hbin,sick_df$tweets,sum)

hbin_counts <- log(attr(hbin,"count"))
minT <- min(hbin_counts)
maxT <- max(hbin_counts)
rangeT <- maxT - minT
hbinScale <- (hbin_counts-minT)/rangeT



#ci <- classIntervals(mtransScale, n = 30, style = "quantile")
ci <- classIntervals(hbin_counts, n = 30, style = "fisher")

gplot.hexbin(hbin,style="colorscale",pen=0,border= 'white', minarea = 0.01, maxarea = 1,colramp=cr,legend=1.5,mincnt=0, xlab="longitude",ylab="latitude",main="Tweets on longitude/latitude")


#now we can plot.
pdf(file="hexgrid.pdf")
pushHexport(hvp)
grid.hexagons(hbin,style='colorscale',pen=0,border= 'white',use.count=FALSE,
              minarea = 0.01, maxarea = 1, mincnt = 0, maxcnt = maxT,
              cell.at=hbin_counts,colramp=cr)

grid.hexlegend(legend=3,ysize=3,lcex = 1,mincnt=0,maxcnt=maxT,colramp=cr,colorcut= ci$brks)

popViewport()
dev.off()

gplot.hexbin(hbin)
maxarea = 1, mincnt = 1, maxcnt = maxT,cell.at=mtrans,
#copy device output to pdf
dev.copy2pdf(file="hexgrid.pdf")










library('hexbin')
library("classInt")
library("RCurl")
rm(list=ls())
#=====================================================================
fnZero <- function(fltNumber) {
  #if negative number return '0'
  if (fltNumber < 0) { fltReturn <- 0 }
  else {fltReturn <- fltNumber}
  return(fltReturn)
}
#get the file from github. getURL takes care of R's https issue
x <- getURL("https://raw.githubusercontent.com/thefactmachine/hex-binning-gis-data/master/xyz.csv")
df <- read.csv(text = x)

# clean up negative number, set them to zero.
df$Z <- sapply(df$Z, fnZero)
#clear existing plot
if (names(dev.cur()) == "RStudioGD") { dev.off(dev.list()["RStudioGD"]) }
#create hexgrid
hbin<-hexbin(df$X, df$Y,xbins=40,IDs=TRUE)
#create viewport (i.e plot dimensions, aspect...)
hvp <- hexViewport(hbin)

#default is counts hexbins. This is overidden and means are calculated.
mtrans<-hexTapply(hbin,df$Z,mean,na.rm=TRUE)
mtrans <- hexTapply(hbin,)


#scale means calculated to 0..1
minT <- min(mtrans)
maxT <- max(mtrans)
rangeT <- maxT - minT
mtransScale <- (mtrans - minT) / rangeT

#set the colors, number intervals, interval location
cr <- colorRampPalette(c('#FFFFFF','#0000CC'))
#ci <- classIntervals(mtransScale, n = 30, style = "quantile")
ci <- classIntervals(mtransScale, n = 40, style = "fisher")

#now we can plot.
pushHexport(hvp)
grid.hexagons(hbin,style='colorscale',pen=0,border= 'white',use.count=FALSE,
              minarea = 0.04, maxarea = 1, mincnt = 1, maxcnt = 3000,
              cell.at=mtrans, colramp=cr, colorcut= ci$brks)

popViewport()

#copy device output to pdf
dev.copy2pdf(file="hexgrid.pdf")

## testing special hexbin functions
x <- rnorm(10000)
y <- rnorm(10000)
plot(x,y, pch=".")
hbin <- hexbin(x,y)
str(xys <- hcell2xy(hbin))
points(xys, cex=1.5, col=2) ; title("hcell2xy( hexbin(..) )", col.main=2)

## hcell2xyInt
x<-rnorm(10000)
y<-rnorm(10000)
hbin<-hexbin(x,y,xbins=10)
ijInt<-hcell2xyInt(hbin)

## hexList
xx<-rnorm(100)
yy<-rnorm(100)
x <- runif(100)
y <- runif(100)
xbnds <- c(-3.5,3.5)
ybnds <- c(-3.5,3.5)
fixx <- xbnds
fixy <- ybnds
unzoom <- 1.08

hbin <- hexbin(xx,yy,xbins=5,xbnds=xbnds,ybnds=ybnds)
hbin2 <- hexbin(x,y,xbins=5,xbnds=xbnds,ybnds=ybnds)
bin1 <- list(bin1 = hbin, bin2 = hbin2)
bin1 <- as(bin1, "hexbinList")
bin1@hbins[[1]]@cell
bin1@hbins[[2]]@cell
tmph.xy <- lapply(bin1@hbins, hcell2xy, check.erosion = TRUE)
eroded <- unlist(lapply(bin1@hbins, is, "erodebin"))
shape <- bin1@Shape
xbins <- bin1@Xbins
bin1@hbins[[1]]@cell
bin1@hbins[[2]]@cell

bnds <- hexbin:::make.bnds(bin1@hbins, tmph.xy, xbnds = fixx, ybnds = fixy)

ratiox <- diff(bnds$nxbnds)/diff(bnds$xbnds)
ratioy <- diff(bnds$nybnds)/diff(bnds$ybnds)
ratio <- max(ratioy, ratiox)
nxbnds <- mean(bnds$nxbnds) + c(-1, 1) * (unzoom * ratio * 
                                            diff(bnds$xbnds))/2
nybnds <- mean(bnds$nybnds) + c(-1, 1) * (unzoom * ratio * 
                                            diff(bnds$ybnds))/2
hvp <- hexViewport(bin1@hbins[[1]], xbnds = nxbnds, ybnds = nybnds, 
                   newpage = TRUE)

dx <- (0.5 * diff(bin1@Xbnds))/xbins
dy <- (0.5 * diff(bin1@Ybnds))/(xbins * shape * sqrt(3))
hexC <- hexcoords(dx = dx, dy = dy)

cell.stat <- hexbin:::all.intersect(bin1@hbins)
cell.stat.n <- apply(cell.stat, 1, sum)
i.depth <- max(cell.stat.n)
diff.cols <- vector(mode = "list", length = i.depth)
levcells <- which(cell.stat.n == 1)
whichbin <- apply(cell.stat[levcells, ], 1, which)
nfcol <- length(NULL)
nhb <- bin1@n
nbcol <- nhb - nfcol
