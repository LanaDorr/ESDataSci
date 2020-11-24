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

#plot 1979, 1999, and 2019 
plot(si1979, col = "dodgerblue", border = "dodgerblue", 
     main="Change in Arctic Sea Ice Area")
plot(si1999, col = "deepskyblue", border = "deepskyblue", add = TRUE)
plot(si2019, col =  "lightblue1", border = "lightblue1", add = TRUE)
legend("bottomright",
       c("Area 1979", "Area 1999", "Area 2019"),
       col = c("dodgerblue", "deepskyblue", "lightblue1"),
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

#combine vectors to make year and area data frame
sia10yrs <- data_frame(Year = vyear,
                       Area = varea)

#find percent difference of area between earliest and latest years
asidiff <- ((asi1979-asi2019)/asi1979)*100

#aggregate sum areas for each year into data frame
siArea <- aggregate(area(seaiceall), by=list(seaiceall$year), FUN="sum", na.rm=TRUE)

#name col in siArea
colnames(siArea) <- c("Year", "TotalArea")

#set up linear model to check assumptions
SIA.mod <- lm(siArea$TotalArea ~ siArea$Year)

#get standardized residuals 
SIA.res <- rstandard(SIA.mod)

#check assumptions
#qq plot and line to check normality of residuals
qqnorm(SIA.res)
qqline(SIA.res)

#check normality again with Shapiro-Wilks
shapiro.test(SIA.res)

#make residual plot to check variance
plot(siArea$Year, SIA.res,
     xlab = "Year",
     ylab = "Standardized Residual")
abline(h=0)

#regression results
summary(SIA.mod)

#make plot of regression
plot(siArea$Year, siArea$TotalArea, 
     pch = 16, 
     col = "lightsteelblue",
     ylab = "Sea Ice Area (m)",
     xlab = "Year")
abline(SIA.mod, lwd=1)


