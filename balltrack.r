## set working directory
setwd("~/Documents/hawkeye/")


## setup the 3d court function
court3d <- function(courtbg="#588070", linecol="#fffffffc",
                    xlim=c(-15,15), ylim=c(-8,8), zlim=c(0,5),
                    theta=-45, phi=30, plot=TRUE) {
    
    par(mar=c(1,1,1,1), bg=courtbg)
    tm <- persp(matrix(rep(0,4), nrow=2),
                xlim=xlim, ylim=ylim, zlim=zlim,
                col="#00000000", border=NA, theta=theta, phi=phi, xlab="x", scale=FALSE, box=FALSE)
    if (!plot) {
        dev.off()
        return(tm)
    }


    ## define function to draw lines/polygons etc in
    ## 3D space given points in a list/data.frame
    sp3dline <- function(x,pmat,dFUN,...) dFUN(trans3d(pmat=pmat, x=x$x, y=x$y, z=x$z), ...)


    #### draw the court

    ## court lines
    crtlines <- list(
        net = data.frame(x=c(0,0),         y=c(-6.395,6.395),  z=c(0,0)),
        sb1 = data.frame(x=c(-6.4,-6.4),   y=c(-4.115,4.115),  z=c(0,0)),
        sb2 = data.frame(x=c(6.4,6.4),     y=c(-4.115,4.115),  z=c(0,0)),
        cl  = data.frame(x=c(-6.4,6.4),    y=c(0,0),           z=c(0,0)),
        sl1 = data.frame(x=c(-11.9,11.9),  y=c(-4.115,-4.115), z=c(0,0)),
        sl2 = data.frame(x=c(-11.9,11.9),  y=c(4.115,4.115),   z=c(0,0)),
        bl1 = data.frame(x=c(-11.9,-11.9), y=c(-5.485,5.485),  z=c(0,0)),
        bl2 = data.frame(x=c(11.9,11.9),   y=c(-5.485,5.485),  z=c(0,0)),
        tl1 = data.frame(x=c(-11.9,11.9),  y=c(-5.485,-5.485), z=c(0,0)),
        tl2 = data.frame(x=c(-11.9,11.9),  y=c(5.485,5.485),   z=c(0,0))
    )
    invisible(lapply(crtlines, function(x) sp3dline(x, pmat=tm, dFUN=lines, col=linecol)))

    ## net polygon with shading
    netpoly <- data.frame(
        x= c(0,0,0,0,0),
        y= c(-6.395,-6.395,0,6.395,6.395),
        z= c(0,1.07,0.914,1.07,0)
    )
    invisible(Map(sp3dline, list(netpoly), pmat=list(tm), dFUN=list(polygon),
                  density=20, angle=c(45,135), col="#cccccccc", border=NA))

    ## net posts and outline
    netlines <- list(
        netp1 = data.frame(x=c(0,0),   y=c(-6.395,-6.395),  z=c(0,1.07)),
        netp2 = data.frame(x=c(0,0),   y=c(6.395,6.395),    z=c(0,1.07)),
        netc  = data.frame(x=c(0,0,0), y=c(-6.395,0,6.395), z=c(1.07,0.914,1.07))
    )
    invisible(lapply(netlines, function(x) sp3dline(x, pmat=tm, dFUN=lines, col="#000000", lwd=2)))

    ##return the transformation matrix to add any other points
    tm 
}

## save the transformation matrix without keeping an active plot
tm <- court3d(plot=FALSE)


## read input file
dat <- structure(list(point_ID = c("2_13_3_1", "2_13_3_1", "2_13_3_1", 
"2_13_3_1", "2_13_3_1", "2_13_3_1", "2_13_3_1", "2_13_3_1", "2_13_3_1", 
"2_13_3_1", "2_13_3_1", "2_13_3_1", "2_13_3_1", "2_13_3_1", "2_13_3_1", 
"2_13_3_1", "2_13_3_1", "2_13_3_1", "2_13_3_1", "2_13_3_1", "2_13_3_1", 
"2_13_3_1", "2_13_3_1", "2_13_3_1", "2_13_3_1"), set_num = c(2L, 
2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L), game_num = c(13L, 13L, 13L, 
13L, 13L, 13L, 13L, 13L, 13L, 13L, 13L, 13L, 13L, 13L, 13L, 13L, 
13L, 13L, 13L, 13L, 13L, 13L, 13L, 13L, 13L), point_num = c(3L, 
3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 
3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L), serve_num = c(1L, 1L, 1L, 1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
1L, 1L, 1L, 1L, 1L), strike_index = c(1L, 1L, 1L, 1L, 1L, 2L, 
2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 4L, 5L, 5L, 
5L, 5L, 5L), position = c("hit", "peak", "net", "bounce", "peak", 
"hit", "peak", "net", "bounce", "peak", "hit", "peak", "net", 
"bounce", "peak", "hit", "peak", "net", "bounce", "peak", "hit", 
"peak", "net", "bounce", "last"), x = c(11.335, 11.335, 0, -4.58900023, 
-12.4980001, -12.4980001, -2.96000004, 0, 6.89799976, 7.88100004, 
7.9369998, 6.40700006, 0, -7.9369998, -15.1560001, -15.1990004, 
-2.5079999, 0, 4.73899984, 4.53399992, 4.40799999, 4.40799999, 
0, -3.59800005, -12.9379997), y = c(0.941999972, 0.941999972, 
-2.05999994, -3.45700002, -5.88800001, -5.88800001, -3.56999993, 
-2.704, -0.250999987, 0.305999994, 0.340000004, -0.00700000022, 
-1.43799996, -3.14299989, -4.69000006, -4.69899988, -2.04900002, 
-1.43200004, -0.00600000005, 0.368999988, 0.561999977, 0.561999977, 
1.61699998, 2.46600008, 4.72800016), z = c(2.88100004, 2.88100004, 
1.06400001, 0.0359999985, 1.71599996, 1.71599996, 3.50900006, 
3.2579999, 0.0480000004, 1.477, 1.472, 1.48500001, 1.227, 0.0289999992, 
1.17400002, 1.17400002, 12.5050001, 11.2480001, 0.0450000018, 
3.89199996, 2.65100002, 2.65100002, 1.26199996, 0.0309999995, 
3.02800012)), row.names = 4291:4315, class = "data.frame")


#### manipulate the dataset to create some helper variables
#### and clean for plotting

## calculate previous and next position
dat$prev_pos <- c(NA, head(dat$position, -1))
dat$next_pos <- c(tail(dat$position, -1), NA)

## remove duplicate hit/peak or peak/hit stuff
rowdrop <- (
    (dat$position == "peak" & dat$prev_pos == "hit") |
    (dat$position == "hit"  & dat$prev_pos == "peak")
) & c(NA, rowSums(diff(as.matrix(dat[c("x","y","z")]))) < 0.1)
rowdrop[is.na(rowdrop)] <- FALSE
dat <- dat[!rowdrop,]

## re-calculate previous and next position
dat$prev_pos <- c(NA, head(dat$position, -1))
dat$next_pos <- c(tail(dat$position, -1), NA)
rownames(dat) <- NULL

## remove the net point if height is over the net 
dat <- dat[-which(dat$position == "net" & dat$z >= 0.914),]

## adjust the points for smoothness
plot(1)
zsp <- xspline(dat$z, shape=c("hit"=0.5, "peak"=0.5, "net"=0.5, "bounce"=0)[dat$position], draw=FALSE)
dev.off()
zsp <- approx(zsp, xout=seq(1,max(zsp$x),length.out=800))
ysp <- approx(dat$y, xout=zsp$x)$y
xsp <- approx(dat$x, xout=zsp$x)$y

## combine all the points for plotting
tpts <- data.frame(trans3d(pmat=tm, x=xsp, y=ysp, z=zsp$y))

## add a 'shadow' of where the ball is on the ground
## and join it to the original data
shadowpts <- data.frame(trans3d(pmat=tm, x=xsp, y=ysp, z=replace(zsp$y,,0)))
tpts[c("sx","sy")] <- shadowpts


## animate the points
n <- 6
chnk <- rep(0:(nrow(tpts)-(n-1)), each=n)
tptsbig <- tpts[chnk + rep(0:(n-1), nrow(tpts)-(n-2)),]
tptsbig$chnk <- chnk[-1]

png(filename="anim/Rplot%03d.png", width=800,height=500)
colgrad <- sapply(seq(0,1,length.out=n-1), function(x) adjustcolor("orange", alpha.f=x))
shadgrad <- sapply(seq(0,1,length.out=n-1), function(x) adjustcolor("#000000ff", alpha.f=x))

invisible(by(tptsbig, tptsbig$chnk, FUN=function(SD) {
    court3d()
    segments(x0=SD$sx[1:(n-1)], y0=SD$sy[1:(n-1)], x1=SD$sx[2:n], y1=SD$sy[2:n], col=shadgrad, lwd=2)
    segments(x0=SD$x[1:(n-1)], y0=SD$y[1:(n-1)], x1=SD$x[2:n], y1=SD$y[2:n], col=colgrad)
    points(SD$x[n], SD$y[n],col="orange",pch=19,cex=0.8)
}))
dev.off()


#### command line call to ffmpeg all the frames to a video
##ffmpeg -framerate 60 -i Rplot%03d.png -c:v libx264 -r 30 out.mp4







 
