library(sp)
library(rgdal)
library(dplyr)

#read in data
si1979 <- readOGR("/Users/lanaraine/Desktop/ES DATA/sea_ice/extent_N_197909_polygon_v3.0")
si1989 <- readOGR("/Users/lanaraine/Desktop/ES DATA/sea_ice/extent_N_198909_polygon_v3.0")
si1999 <- readOGR("/Users/lanaraine/Desktop/ES DATA/sea_ice/extent_N_199909_polygon_v3.0")
si2009 <- readOGR("/Users/lanaraine/Desktop/ES DATA/sea_ice/extent_N_200909_polygon_v3.0")
si2019 <- readOGR("/Users/lanaraine/Desktop/ES DATA/sea_ice/extent_N_200909_polygon_v3.0")
seaiceall <- readOGR("/Users/lanaraine/Desktop/ES DATA/sea_ice_all")

#plot first and last years, 1979 and 2019 
plot(si1979, col = "lightsteelblue", border = "lightsteelblue", 
     main="Change in Arctic Sea Ice Area")
plot(si2019, col =  "royalblue3", border = "royalblue3", add = TRUE)
legend("bottomright",
       c("Area 1979", "Area 2019"),
       col = c("lightsteelblue", "royalblue3"),
       pch = 15,
       bty = "n")

#Find total area every 10 years
asi1979 <- sum(area(si1979))
asi1989 <- sum(area(si1989))
asi1999 <- sum(area(si1999))
asi2009 <- sum(area(si2009))
asi2019 <- sum(area(si2019))

#Make year and area data vectors
vyear <- c(1979,1989,1999,2009,2019)
varea <- c(asi1979,asi1989,asi1999,asi2009,asi2019)

#combone vectors to make year and area data frame
sia10yrs <- data_frame(Year = vyear,
                       Area = varea)

#find mean and standard deviation of areas for every 10 years
mean(varea)
sd(varea)

#find percent difference of area between earliest and latest years
asidiff <- ((asi1979-asi2019)/asi1979)*100



