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








