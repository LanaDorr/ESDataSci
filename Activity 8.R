install.packages(c("raster"))
library(raster)
library(ggplot2)
library(rgdal)

dirR <- "/Users/lanaraine/Desktop/ES DATA/a08/oneida"

rdatB2 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B02_20m.tif"))
rdatB3 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B03_20m.tif"))
rdatB4 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B04_20m.tif"))
rdatB8 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B08_20m.tif"))

plot(rdatB2/10000)

#stack red green and blue
rgbS <- stack(rdatB4,rdatB3,rdatB2)/10000

#add in contrast stretch
plotRGB(rgbS, stretch="lin")

#get full resolution
plotRGB(rgbS, stretch="lin", maxpixels=rgbS@nrows*rgbS@ncols)
#get total number of cells
1005*2286


#false color map
rgbF <- stack(rdatB3,rdatB2,rdatB4)/10000
plotRGB(rgbF, stretch="lin")

#find NDVI with NIR-red/(NIR + RED)
NDVI <- (rdatB8 - rdatB4) / (rdatB8 + rdatB4)
plot(NDVI)

#read in landcover points
algae <- readOGR(paste0(dirR,"/Oneida/algae.shp"),verbose=FALSE)
agri <- readOGR(paste0(dirR,"/Oneida/agriculture.shp"),verbose=FALSE)
forest <- readOGR(paste0(dirR,"/Oneida/forest.shp"),verbose=FALSE)
water <- readOGR(paste0(dirR,"/Oneida/water.shp"),verbose=FALSE)
wetlands <- readOGR(paste0(dirR,"/Oneida/wetlands.shp"),verbose=FALSE)

#plot points and true color 
plotRGB(rgbS, stretch="lin",maxpixels=2297430)
plot(algae, add=TRUE, col=rgb(0.5,0.5,0.5,0.5), pch=19)
plot(agri, add=TRUE, col=rgb(0.75,0.5,0.5,0.5), pch=19)
plot(forest, add=TRUE, col=rgb(0.75,0.75,0.25,0.5), pch=19)
plot(water, add=TRUE, col=rgb(0.33,0.75,0.75,0.5), pch=19)
plot(wetlands, add=TRUE, col=rgb(0.33,0.33,0.65,0.5), pch=19)
legend("bottomleft", c("algae","agri","forest","water","wetlands"),
       pch = 19, col=c(rgb(0.5,0.5,0.5,0.5),rgb(0.75,0.5,0.5,0.5),
                           rgb(0.75,0.75,0.25,0.5), rgb(0.33,0.75,0.75,0.5),rgb(0.33,0.33,0.65,0.5)),
                       bty="n", cex=0.75)
#set up dat frame with point coordinates
landExtract <- data.frame(landID = as.factor(rep(c("algae","water","agri","forest","wetlands"),each=120)),
                          x=c(algae@coords[,1],water@coords[,1],agri@coords[,1],forest@coords[,1],wetlands@coords[,1]),
                          y=c(algae@coords[,2],water@coords[,2],agri@coords[,2],forest@coords[,2],wetlands@coords[,2]))

allbands <- stack(rdatB2, rdatB3, rdatB4, rdatB8)/1000
extractout <- raster::extract(allbands,landExtract[,2:3])
colnames(extractout) <- c("B02","B03","B04","B08")
rasterEx <- cbind(landExtract,extractout)
head(rasterEx)

ggplot(data = rasterEx, aes(x=B02, y=B08, color=landID)) +
  geom_point(alpha=0.6)+
  theme_classic()
ggplot(data = rasterEx, aes(x=B03, y=B08, color=landID)) +
  geom_point(alpha=0.6)+
  theme_classic()
ggplot(data = rasterEx, aes(x=B04, y=B08, color=landID)) +
  geom_point(alpha=0.6)+
  theme_classic()

exndvi <- raster::extract(NDVI,landExtract[,2:3])
dfndvi <- cbind(landExtract,exndvi)

ggplot(data = dfndvi[dfndvi$landID==c("agri","forest","wetlands"),], aes(x=landID, y=exndvi)) +
  geom_boxplot(width=.1)+
  geom_violin(alpha=.2)+
  theme_classic()


