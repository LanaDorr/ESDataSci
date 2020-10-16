install.packages(c("sp","rgdal","dplyr"))
install.packages(c("rgdal"))
yes
#vector data
library(sp)
#reading in spatial data
library(rgdal)
#data managment package
library(dplyr)
install.packages(c("rgeos","rgdal"))
no

g1966 <- readOGR("/Users/lanaraine/Desktop/ES DATA/a06/GNPglaciers/GNPglaciers_1966.shp")

g2015 <- readOGR("/Users/lanaraine/Desktop/ES DATA/a06/GNPglaciers/GNPglaciers_2015.shp")

str(g2015)

plot(g1966, col = "lightblue2", border = "grey50")

head(g2015@data)

g1966@proj4string

#check glacier names
g1966@data$GLACNAME
g2015@data$GLACNAME

g2015@data$GLACNAME <- ifelse(g2015@data$GLACNAME == 
                                "North Swiftcurrent Glacier", 
                              "N. Swiftcurrent Glacier",
                              ifelse( g2015@data$GLACNAME ==
                                        "Miche Wabun", 
                                      "Miche Wabun Glacier", 
                                      as.character(g2015@data$GLACNAME)))

gdf66 <- data.frame(GLACNAME = g1966@data$GLACNAME,
                    area66 = g1966@data$Area1966)
gdf15 <- data.frame(GLACNAME = g2015@data$GLACNAME,
                    area15 = g2015@data$Area2015)

gAll <- full_join(gdf66, gdf15, by="GLACNAME")

gAll$gdiff <- ((gAll$area66-gAll$area15)/gAll$area66)*100

g1966@data <- left_join(g1966@data, gAll, by = "GLACNAME")

spplot(g1966, "gdiff", main="% change in area", col="transparent")

vulture66 <- g1966[g1966@data$GLACNAME == "Vulture Glacier",]
plot(vulture66, main = "Vulture Glacier in 1966", col="slategray")


plot(gAll$gdiff, gAll$area66,
     pch = 19, 
     col = "royalblue4",
     ylab = "Glacier Area 1966",
     xlab = "Percent Change 1966-2015")

sd(gAll$gdiff)
mean(gAll$gdiff)

min(gAll$gdiff)

boulder66 <- g1966[g1966@data$GLACNAME == "Boulder Glacier",]
boulder15 <- g2015[g2015@data$GLACNAME == "Boulder Glacier",]
plot(boulder66,main = "Boulder Glacier in 1966", col="slategray")
plot(boulder15, main = "Boulder Glacier in 1966", col="slategray")

plot(boulder66, col = "lightsteelblue")
plot(boulder15, col = "royalblue3", add = TRUE)
legend("topright",
       c("Boulder Glacier 1966", "Boulder Glacier 2015"),
       col = c("lightsteelblue", "royalblue3"),
       pch = 15,
       bty = "n")
legend("topleft",
       c("Greatest % Change:
         Boulder Glacier"),
       bty = "n")

pumpelly66 <- g1966[g1966@data$GLACNAME == "Pumpelly Glacier",]
pumpelly15 <- g2015[g2015@data$GLACNAME == "Pumpelly Glacier",]

plot(pumpelly66, col = "lightsteelblue")
plot(pumpelly15, col = "royalblue3", add = TRUE)
legend("bottomright",
       c("Pumpelly Glacier 1966", "Pumpelly Glacier 2015"),
       col = c("lightsteelblue", "royalblue3"),
       pch = 15,
       bty = "n")
legend("topleft",
       c("Least % Change: 
         Pumpelly Glacier"),
       bty = "n")

