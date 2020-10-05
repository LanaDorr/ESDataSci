datB <- read.csv("/Users/lanaraine/Desktop/ES DATA/a04/beaver_dam.csv")

head(datB)

plot(datB$dams.n, datB$area.ha, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Surface Water Area (ha)",
     xlab = "Number of Beaver Dams")

#set up linear model so we can check assumptions
dam.mod <- lm(datB$area.ha ~ datB$dams.n)
#get standardized residuals 
dam.res <- rstandard(dam.mod)

#check assumptions before linear regression
#set up qq plot for normality of residuals
#qq plot is better for bigger data than Shapiro
qqnorm(dam.res)
#add qq line in plot
qqline(dam.res)

#check normality again with Shapiro-Wilks
shapiro.test(dam.res)

#make residual plot to check variance
plot(datB$dams.n, dam.res,
     xlab = "Beaver Dams",
     ylab = "Standardized Residual")
#use abline to put a line in plot
#this helps to see variance in data more clearly
#ass horizontal line at zero
abline(h=0)

#see regression results
summary(dam.mod)

#make plot to better see regression
plot(datB$dams.n, datB$area.ha, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Surface Water Area (ha)",
     xlab = "Number of Beaver Dams")
#add regression line and make it thicker with lwd
abline(dam.mod, lwd=2)


pheno <- read.csv("/Users/lanaraine/Desktop/ES DATA/a04/red_maple_pheno.csv")

#set up panel of plots to compare
#give them oen row and two columns
par(mfrow=c(2,4))
plot(pheno$Tmax,pheno$doy,
     pch =19,
     col = "royalblue4",
     ylab = "Day of Leaf Out",
     xlab = "Maximum Temperature (C)")
plot(pheno$Prcp,pheno$doy,
     pch =19,
     col = "royalblue4",
     ylab = "Day of Leaf Out",
     xlab = "Percipitation (mm)")
plot(pheno$Lat,pheno$doy,
     pch =19,
     col = "royalblue4",
     ylab = "Day of Leaf Out",
     xlab = "Latitude")
plot(pheno$Long,pheno$doy,
     pch =19,
     col = "royalblue4",
     ylab = "Day of Leaf Out",
     xlab = "Longitude")
plot(pheno$Tmin,pheno$doy,
     pch =19,
     col = "royalblue4",
     ylab = "Day of Leaf Out",
     xlab = "Minimum Temperature (C)")
plot(pheno$elev,pheno$doy,
     pch =19,
     col = "royalblue4",
     ylab = "Day of Leaf Out",
     xlab = "Elevation (m)")
plot(pheno$siteDesc == "Urban", pheno$doy,
     pch =19,
     col = "royalblue4",
     ylab = "Day of Leaf Out",
     xlab = "Urban Site")
plot(pheno$siteDesc, pheno$doy,
     pch =19,
     col = "royalblue4",
     ylab = "Day of Leaf Out",
     xlab = "Rural Site")

plot(pheno$doy ~ pheno$siteDesc, xlab = "Site Type",
     ylab = "Day of Leaf Out")

plot( ~ pheno$Lat + pheno$Tmax + pheno$Tmin + pheno$Prcp 
    + pheno$elev + pheno$siteDesc)

pheno$urID <- ifelse(pheno$siteDesc == "Urban",1,0)
pheno$urID

mlr <- lm(pheno$doy ~ pheno$Tmax + pheno$Prcp 
          + pheno$elev + pheno$urID)
mlFitted <- fitted(mlr)

qqnorm(mlr.res)
qqline(mlr.res)

plot(mlFitted)
abline(h=100)

summary(mlr)




