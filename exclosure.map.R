## Adding the exclosure location

gps=read.csv("Exclosure GPS.csv",header=TRUE)


summary(gps$x)
summary(gps$y)

exclosure=subset(gps, type=="Exclosure")
camera=subset(gps, type=="Camera")

str(gps)



### Importing the elevation data

ken.elv=read.table("ken_ele.txt",header=TRUE)

ken.elv$elev=ken.elv$elev+243

## Making the map

tiff("map.tiff", res = 600, width = 11, height =11, units = 'in', bg = "transparent")
par (mfrow=c(1,1), pin=c(7.4, 4.9))


x.range=c(-170,570)
y.range=c(-120,370)



x=seq(0,400, by=10)
y=seq(0,250, by=10)
z=matrix(ken.elv$elev,nrow=41,byrow=TRUE)

#plot(x = 0, y = 0,type = "n", xlim = range(x), 
#ylim = range(y), xlab = "", ylab = "",frame.plot=FALSE,
#axes=FALSE,asp=1)

plot(exclosure$x, exclosure$y, pch=15, xlim =x.range, col="blue",
ylim =y.range, xlab = "", ylab = "",frame.plot=FALSE,
axes=FALSE,asp=1, cex=2)


points(camera$x, camera$y, pch=8, cex=2)

## Creating the grid

grid.yim=-60
inv=120

grid.y=c(grid.yim,grid.yim+inv, grid.yim+2*inv, grid.yim+3*inv, grid.yim+4*inv)

x.range2=c(x.range[1]-100, x.range[2]+100)

lines(x.range2, c(grid.y[1],grid.y[1]), lty=2, lwd=2.5,col="darkseagreen4")
lines(x.range2, c(grid.y[2],grid.y[2]), lty=2, lwd=2.5,col="darkseagreen4")
lines(x.range2, c(grid.y[3],grid.y[3]), lty=2, lwd=2.5,col="darkseagreen4")
lines(x.range2, c(grid.y[4],grid.y[4]), lty=2, lwd=2.5,col="darkseagreen4")
lines(x.range2, c(grid.y[5],grid.y[5]), lty=2, lwd=2.5,col="darkseagreen4")

adjust=-120

grid.x=c(min(x.range)+adjust, min(x.range)+adjust+inv, min(x.range)+adjust+2*inv, min(x.range)+adjust+3*inv,
         min(x.range)+adjust+4*inv,min(x.range)+adjust+5*inv,min(x.range)+adjust+6*inv, min(x.range)+adjust+7*inv)

y.range2=c(y.range[1]-100,y.range[2]+100)

lines(c(grid.x[1],grid.x[1]), y.range2,lty=2, col="darkseagreen4", lwd=2.5)
lines(c(grid.x[2],grid.x[2]), y.range2,lty=2, col="darkseagreen4", lwd=2.5)
lines(c(grid.x[3],grid.x[3]), y.range2,lty=2, col="darkseagreen4", lwd=2.5)
lines(c(grid.x[4],grid.x[4]), y.range2,lty=2, col="darkseagreen4", lwd=2.5)
lines(c(grid.x[5],grid.x[5]), y.range2,lty=2, col="darkseagreen4", lwd=2.5)
lines(c(grid.x[6],grid.x[6]), y.range2,lty=2, col="darkseagreen4", lwd=2.5)
lines(c(grid.x[7],grid.x[7]), y.range2,lty=2, col="darkseagreen4", lwd=2.5)
lines(c(grid.x[8],grid.x[8]), y.range2,lty=2, col="darkseagreen4", lwd=2.5)


rect(min(x),min(y), max(x),max(y), border = "black", lwd=2)
#rect(min(x.range),min(y.range), max(x.range),max(y.range), border = "black", lwd=2)



#contour(x, y, z, lty = "solid", add = TRUE,
#vfont = c("sans serif", "plain"), levels=seq(240,320, by=5),drawlabels = TRUE, 
#col="grey", method ="flattest", lwd=1.5, cex=1.6)

colfun=colorRampPalette(c("darkgreen", "MediumSeaGreen"))
contour(x, y, z, lty = "solid", add = TRUE,
        vfont = c("sans serif", "plain"), levels=seq(240,320, by=5),drawlabels = TRUE, 
        col= colfun(10), method ="flattest", lwd=1.5, cex=1.6)



#axis(1,at=seq(min(x.range), max(x.range), by=100), line=0, cex.axis=1.8,las=1,lwd=2)
#axis(2,at=seq(min(y.range), max(y.range), by=100), line=0, cex.axis=1.8,lwd=2, las=1)

axis(1,at=grid.x, line=0, cex.axis=1.8,las=1,lwd=2)
axis(2,at=grid.y, line=0, cex.axis=1.8,lwd=2, las=1)

mtext(side=1, line =4, cex=2, "East-West Direction (m)")
mtext(side=2, line =4.5, cex=2, "North-South Direction (m)")

#rect(min(x.range),min(y.range), max(x.range),max(y.range), border = "black", lwd=2)

box(lwd=2.5)

### Adding legend
legend(-130,-35, pch=15, col="blue", "Exclosure", lwd=2, cex=2, bty = "n")
legend(-130,-75, pch=8,  "Camera", lwd=2, cex=2, bty = "n")

dev.off()













