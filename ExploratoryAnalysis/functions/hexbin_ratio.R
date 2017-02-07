A single object matching ‘hdiffplot’ was found
It was found in the following places
  package:hexbin
  namespace:hexbin
with value

function (bin1, bin2 = NULL, xbnds = NULL, ybnds = NULL, focus = NULL, 
    col.control = list(medhex = "white", med.bord = "black", 
        focus = NULL, focus.border = NULL, back.col = "grey"), 
    arrows = TRUE, size = unit(0.1, "inches"), lwd = 2, eps = 1e-06, 
    unzoom = 1.08, clip = "off", xlab = "", ylab = "", main = deparse(mycall), 
    ...) 
{
    fixx <- xbnds
    fixy <- ybnds
    if (!inherits(bin1, "hexbinList")) {
        if (is.null(bin2) & is.list(bin1)) {
            bin1 <- as(bin1, "hexbinList")
        }
        else if (is.null(bin2) & (!is.list(bin1))) 
            stop(" need at least 2 hex bin objects, or a hexbinList")
        else {
            if (bin1@shape != bin2@shape) 
                stop("bin objects must have same shape parameter")
            if (all(bin1@xbnds == bin2@xbnds) & all(bin1@ybnds == 
                bin2@ybnds)) 
                equal.bounds <- TRUE
            else stop("Bin objects need the same xbnds and ybnd1s")
            if (bin1@xbins != bin2@xbins) 
                stop("Bin objects need the same number of bins")
            nhb <- 2
            bin1 <- list(bin1 = bin1, bin2 = bin2)
            bin1 <- as(bin1, "hexbinList")
        }
    }
    mycall <- sys.call()
    if (length(mycall) >= 4) {
        mycall[4] <- as.call(quote(.....()))
        if (length(mycall) > 4) 
            mycall <- mycall[1:4]
    }
    if (is.null(focus)) 
        focus <- 1:bin1@n
    tmph.xy <- lapply(bin1@hbins, hcell2xy, check.erosion = TRUE)
    eroded <- unlist(lapply(bin1@hbins, is, "erodebin"))
    shape <- bin1@Shape
    xbins <- bin1@Xbins
    bnds <- hexbin:::make.bnds(bin1@hbins, tmph.xy, xbnds = fixx, ybnds = fixy) #make.bnds is not exported, hence the ::: operator is needed
    ratiox <- diff(bnds$nxbnds)/diff(bnds$xbnds)
    ratioy <- diff(bnds$nybnds)/diff(bnds$ybnds)
    ratio <- max(ratioy, ratiox)
    nxbnds <- mean(bnds$nxbnds) + c(-1, 1) * (unzoom * ratio * 
        diff(bnds$xbnds))/2
    nybnds <- mean(bnds$nybnds) + c(-1, 1) * (unzoom * ratio * 
        diff(bnds$ybnds))/2
    hvp <- hexViewport(bin1@hbins[[1]], xbnds = nxbnds, ybnds = nybnds, 
        newpage = TRUE)
    pushHexport(hvp)
    grid.rect()
    grid.xaxis()
    grid.yaxis()
    if (nchar(xlab) > 0) 
        grid.text(xlab, y = unit(-2, "lines"), gp = gpar(fontsize = 16))
    if (nchar(ylab) > 0) 
        grid.text(ylab, x = unit(-2, "lines"), gp = gpar(fontsize = 16), 
            rot = 90)
    if (sum(nchar(main)) > 0) 
        grid.text(main, y = unit(1, "npc") + unit(1.5, "lines"), 
            gp = gpar(fontsize = 18))
    if (clip == "on") {
        popViewport()
        pushHexport(hvp, clip = "on")
    }
    dx <- (0.5 * diff(bin1@Xbnds))/xbins
    dy <- (0.5 * diff(bin1@Ybnds))/(xbins * shape * sqrt(3))
    hexC <- hexcoords(dx = dx, dy = dy)
    if (length(focus) < bin1@n) {
        bin1@hbins <- c(bin1@hbins[focus], bin1@hbins[-focus])
        bin1@Bnames <- c(bin1@Bnames[focus], bin1@Bnames[-focus])
    }
    cell.stat <- all.intersect(bin1@hbins)
    cell.stat.n <- apply(cell.stat, 1, sum)
    i.depth <- max(cell.stat.n)
    diff.cols <- vector(mode = "list", length = i.depth)
    levcells <- which(cell.stat.n == 1)
    whichbin <- apply(cell.stat[levcells, ], 1, which)
    nfcol <- length(focus)
    nhb <- bin1@n
    nbcol <- nhb - nfcol
    fills <- if (is.null(col.control$focus)) {
        if (nbcol > 0) 
            matrix(c(seq(0, 360, length = nfcol + 1)[1:nfcol]/360, 
                rep(0, nbcol), rep(1, nfcol), rep(0, nbcol), 
                rep(1, nfcol), rep(0.9, nbcol)), ncol = 3)
        else matrix(c(seq(0, 360, length = nhb + 1)/360, rep(1, 
            nhb + 1), rep(1, nhb + 1)), ncol = 3)[1:nhb, ]
    }
    else {
        foc.col <- t(rgb2hsv(col2rgb(col.control$focus)))
        if (nbcol > 0) {
            bcol <- matrix(c(rep(0, 2 * nbcol), rep(0.9, nbcol)), 
                ncol = 3)
            rbind(foc.col, bcol)
        }
        else foc.col
    }
    colnames(fills) <- c("h", "s", "v")
    diff.cols[[1]] <- list(fill = fills, border = gray(0.8))
    if (length(levcells) > 0) {
        for (i in unique(whichbin)) {
            pcells <- if (eroded[i]) 
                bin1@hbins[[i]]@cell[bin1@hbins[[i]]@eroded]
            else bin1@hbins[[i]]@cell
            pcells <- which(pcells %in% levcells[whichbin == 
                i])
            pfill <- diff.cols[[1]]$fill[i, ]
            pfill <- hsv(pfill[1], pfill[2], pfill[3])
            hexpolygon(x = tmph.xy[[i]]$x[pcells], y = tmph.xy[[i]]$y[pcells], 
                hexC, border = diff.cols[[1]]$border, fill = pfill)
        }
    }
    if (i.depth > 1) {
        for (dl in 2:(i.depth)) {
            levcells <- which(cell.stat.n == dl)
            if (length(levcells) == 0) 
                next
            whichbin <- apply(cell.stat[levcells, ], 1, function(x) paste(which(x), 
                sep = "", collapse = ":"))
            inter.nm <- unique(whichbin)
            fills <- rep(hsv(1), length(inter.nm))
            i <- 1
            for (bn in inter.nm) {
                who <- as.integer(unlist(strsplit(bn, ":")))
                fills[i] <- mixcolors2(diff.cols[[1]]$fill[who, 
                  ], 1/length(who), where = "LUV")
                i <- i + 1
            }
            diff.cols[[dl]] <- list(fill = fills, border = gray((i.depth - 
                dl)/i.depth))
            i <- 1
            for (ints in inter.nm) {
                bin.i <- as.integer(unlist(strsplit(ints, ":"))[1])
                pcells <- if (eroded[bin.i]) 
                  bin1@hbins[[bin.i]]@cell[bin1@hbins[[bin.i]]@eroded]
                else bin1@hbins[[bin.i]]@cell
                pcells <- which(pcells %in% levcells[whichbin == 
                  ints])
                hexpolygon(x = tmph.xy[[bin.i]]$x[pcells], y = tmph.xy[[bin.i]]$y[pcells], 
                  hexC, border = diff.cols[[dl]]$border, fill = diff.cols[[dl]]$fill[i])
                i <- i + 1
            }
        }
    }
    if (any(eroded)) {
        hmeds <- matrix(unlist(lapply(bin1@hbins[eroded], function(x) unlist(getHMedian(x)))), 
            ncol = 2, byrow = TRUE)
        hexpolygon(x = hmeds[, 1], y = hmeds[, 2], hexC, border = col.control$med.b, 
            fill = col.control$medhex)
        if (arrows) {
            for (i in focus) {
                for (j in focus[focus < i]) {
                  if (abs(hmeds[i, 1] - hmeds[j, 1]) + abs(hmeds[i, 
                    2] - hmeds[j, 2]) > eps) 
                    grid.lines(c(hmeds[i, 1], hmeds[j, 1]), c(hmeds[i, 
                      2], hmeds[j, 2]), default.units = "native", 
                      arrow = arrow(length = size))
                }
            }
        }
    }
    popViewport()
    invisible(hvp)
}
<environment: namespace:hexbin>


> getAnywhere("make.bnds")
A single object matching ‘make.bnds’ was found
It was found in the following places
  namespace:hexbin
with value

function (binlst, xy.lst, xbnds = NULL, ybnds = NULL) 
{
    if (inherits(binlst, "hexbinList")) 
        binlst <- binlst@hbins
    if (is.null(xbnds)) 
        xbnds <- binlst[[1]]@xbnds
    if (is.null(ybnds)) 
        ybnds <- binlst[[1]]@ybnds
    nxbnds <- get.xrange(xy.lst, xbnds)
    nybnds <- get.yrange(xy.lst, ybnds)
    list(xbnds = xbnds, ybnds = ybnds, nxbnds = nxbnds, nybnds = nybnds)
}
<environment: namespace:hexbin>
> 
