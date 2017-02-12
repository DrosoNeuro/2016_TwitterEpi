library("grid")
library("gridBase")

vp0 <- viewport(x=0.5,y=0.5,width=0.5,height=0.25,angle=45)
pushViewport(vp0)
grid.rect(gp=gpar(lty="dashed"))
vp1 <- viewport(x=0,y=0.5,w=0.5,h=0.5,
                just=c("left","bottom"),
                name="vp1")
vp2 <- viewport(x=0.5,y=0,w=0.5,h=0.5,
                just=c("left","bottom"),
                name="vp2")
pushViewport(vp1)
grid.rect(gp=gpar(col="grey"))
grid.text("some drawign",y=0.8)
upViewport()
pushViewport(vp2)
grid.rect(gp=gpar(col="grey"))
grid.text("someotherText",y=0.2)
upViewport()
downViewport("vp1")
grid.text("MORE text",y=0.2)
popViewport()

grid.rect(gp=gpar(lty="dashed"))
vp <- viewport(width=0.5,height=0.5)
pushViewport(vp)
grid.rect(gp=gpar(col="grey"))
grid.text("quarter of thepage",y=0.85)
pushViewport(vp)
grid.rect()
grid.text("quarter of previous viewport")
popViewport(2)

grid.rect(gp=gpar(col="grey"))
pushViewport(viewport(y=unit(3,"lines"),width=0.9,height=0.8,
                      just="bottom",xscale=c(0,100)))
grid.rect(gp=gpar(col="grey"))
grid.xaxis()
pushViewport(viewport(x=unit(60,"native"),
                      y=unit(0.5,"npc"),
                      width = unit(1,"strwidth","coordinates for everyone"),
                      height=unit(2,"inches")))
grid.rect()
grid.text("coordinates for everyone")
popViewport(2)

pushViewport(viewport(layout=grid.layout(4,5)))
grid.rect(gp=gpar(col="grey"))
grid.segments(c(1:4/5,rep(0,3)),c(rep(0,4),1:3/4),
               c(1:4/5,rep(1,3)),c(rep(1,4),1:3/4),
               gp=gpar(col="grey"))
pushViewport(viewport(layout.pos.col=2:3,layout.pos.row=3))
grid.rect(gp=gpar(lwd=3))
popViewport(2)

#standard plot
grid.rect(gp=gpar(lty="dashed"))
x <- y <- 1:10
pushViewport(plotViewport(c(5.1,4.1,4.1,2.1)))
pushViewport(dataViewport(x,y))
grid.rect()
grid.xaxis()
grid.yaxis()
grid.points(x,y)
grid.text("1:10",x=unit(-3,"lines"),rot=90)
popViewport(2)

#combination with base graphics (using "gridBase")
midpts <- barplot(1:10,axes=F)
axis(2)
axis(1,at=midpts,labels=F)
vps <- baseViewports()
pushViewport(vps$inner,vps$figure,vps$plot)
grid.text(c("one","two","three","four",
            "five","six","seven","eight",
            "nine","ten"),
          x=unit(midpts,"native"),
          y=unit(-1,"lines"),
          just="right",rot=60)
popViewport(3)

#embedding base graphics plots in  grid viewports
data(USArrests)
hc <- hclust(dist(USArrests),"ave")
dend1 <- as.dendrogram(hc)
dend2 <- cut(dend1,h=70)
x <- 1:4
y <- 1:4
height <- factor(round(unlist(lapply(dend2$lower,attr,"height"))))
space <- max(unit(rep(1, 50), "strwidth",
                            as.list(rownames(USArrests))))
dendpanel <- function(x, y, subscripts, ...){
    pushViewport(viewport(y=space, width=0.9,
                            height=unit(0.9, "npc") - space,
                            just="bottom"))
    grid.rect(gp=gpar(col="grey", lwd=5))
    par(plt=gridPLT(), new=TRUE, ps=10)
    plot(dend2$lower[[subscripts]], axes=FALSE)
    popViewport()}

dendpanel(x,y,2)

#plotting a map
minpop <- 5e5
sub_map <-map("world", c("USA","Canada","Mexico"),exact="T",xlim=xbnds,ylim=ybnds,plot=F)
width <- 4.5
height <- 2.7
xrange <- range(xbnds)+ width/2*c(-1,1)
yrange <- range(ybnds) + height/2*c(-1,1)
vp <- viewport(x=0.5,y=0.5,width=0.8,height=0.8,
               xscale=xrange,yscale=yrange)
pushViewport(vp)
grid.xaxis()
grid.yaxis()
grid.rect(gp=gpar(lty="dashed"))
upViewport()
pushViewport(viewport(x=0.5, y=0.5, width=0.8, height=0.8,
                      xscale=xrange, yscale=yrange, clip="on"))

# hvp1 <- hexViewport(my.bins[[1]])
# pushHexport(hvp1)
grid.lines(unit(sub_map$x,"native"),unit(sub_map$y,"native"),
           gp=gpar(col="black"))
grid.hexagons(my.bins[[1]],style="colorscale",colramp=cr)
data(world.cities)
world.cities <- data.table(world.cities)
colnames(world.cities)[4:5] <- c("latitude","longitude")
world.cities <- coord_selection(world.cities,coord)[[1]]
world.cities <- world.cities[pop >5e5,]
lake <- map("lakes",add=TRUE,plot=F,xlim=xbnds,ylim=ybnds,fill=T)
grid.points(unit(world.cities$longitude,"native"),unit(world.cities$latitude,"native"),
      gp=gpar(col="red",lwd=10,lty="solid",fontsize=10),pch="o")
grid.polygon(unit(lake$x,"native"),unit(lake$y,"native"),
           gp=gpar(col="blue",fill="blue",alpha=0.5))
popViewport()


##create barplot with legend
barData <- matrix(sample(1:4, 16, replace = TRUE), ncol = 4)
boxColours <- 1:4
bp <- function(barData) {
    nbars <- dim(barData)[2]
      nmeasures <- dim(barData)[1]
        barTotals <- rbind(rep(0, nbars), apply(barData, 2, cumsum))
          barYscale <- c(0, max(barTotals)*1.05)
            pushViewport(plotViewport(c(5, 4, 4, 1),
                                        yscale = barYscale,
                                        layout = grid.layout(1, nbars)))
            grid.rect()
            grid.yaxis()
            for (i in 1:nbars) {
                pushViewport(viewport(layout.pos.col = i, yscale = barYscale))
                grid.rect(x = rep(0.5, nmeasures),
                            y = unit(barTotals[1:nmeasures, i], "native"),
                            height = unit(diff(barTotals[,i]), "native"),
                      width = 0.8, just = "bottom", gp = gpar(fill = boxColours))
              popViewport()}
              popViewport()
}

legLabels <- c("Group A", "Group B", "Group C", "Something Longer")
boxSize <- unit(0.5, "inches")

leg <- function(legLabels) {
    nlabels <- length(legLabels)
      pushViewport(viewport(layout = grid.layout(nlabels, 1)))
      for (i in 1:nlabels) {
          pushViewport(viewport(layout.pos.row = i))
          grid.rect(width = boxSize, height = boxSize, just = "bottom",
                      gp = gpar(fill = boxColours[i]))
          grid.text(legLabels[i], y = unit(0.5, "npc") - unit(1, "lines"))
          popViewport()
      }
      popViewport()
}

grid.rect(gp = gpar(lty = "dashed"))
legend.width <- max(unit(rep(1, length(legLabels)),
                 "strwidth", as.list(legLabels)) +
                  unit(2, "lines"),
                unit(0.5, "inches") + unit(2, "lines"))
pushViewport(viewport(layout = grid.layout(1, 2,
           widths = unit.c(unit(1,"null"), legend.width))))
grid.rect(gp = gpar(lty = "dashed"))
pushViewport(viewport(layout.pos.col = 1))
bp(barData)
upViewport(2)

pushViewport(viewport(layout.pos.col = 2))
grid.rect(gp = gpar(lty = "dashed",col="red"))
pushViewport(plotViewport(c(5, 0, 4, 0)))
leg(legLabels)
popViewport(3)


grid.show.layout(grid.layout(1, 2, widths = unit.c(unit(1,"null"), legend.width)))
