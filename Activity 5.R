datW <- read.csv("/Users/lanaraine/Desktop/ES DATA/a02/noaa2011124.csv")

datW$NAME <- as.factor(datW$NAME)
#set up a vector of all names 
nameS <- levels(datW$NAME)
nameS

#make dataframe with prcp, year, site name
#remove NA with na.omit

#first make new dataframe so we can focu on prcp
datP <- na.omit(data.frame(NAME=datW$NAME,
                           year=datW$year,
                           PRCP=datW$PRCP))
#find toatl annual prcp (mm)
pr <- aggregate(datP$PRCP, by=list(datP$NAME,datP$year),
                FUN="sum", na.rm=TRUE)
colnames(pr) <- c("NAME","year","totalP")

pr$ncount <- aggregate(datP$PRCP, by=list(datP$NAME,datP$year),
                       FUN="length")$x

#subset so only sites with enough info in a new dataframe
pr <- pr [pr$ncount >= 364,]

install.packages("ggplot2")
library(ggplot2)

ggplot(data = pr,aes(x=year, y=totalP, color=NAME)) + #data for plot
  geom_point()+ #make points at data point
  geom_path()+ #use lines to connect data points
  labs(x="year", y="Annual Percipitation")+ #make axis labels
  theme_classic() #change plot theme

#look at livermore and morrisville
ca <- pr[pr$NAME == nameS[2], ]
ny <- pr[pr$NAME == nameS[5], ]

#plot ca percip
plot(ca$year, ca$totalP)
plot(ca$year, ca$totalP,
     type = "b",
     pch = 19,
     ylab = "Annual Percipitation (mm))",
     xlab = "Year",
     yaxt = "n",
     ylim =c(0, 1600))
#add y axis
axis(2, seq(200,1600, by=200), las=2 )

#add ny to above plot
points(ny$year, ny$totalP,
       type = "b",
       pch = 19,
       col="tomato3")

#add legend 
legend("topleft",
       c("California", "New York"),
       col = c("black", "tomato3"),
       pch = 19,
       lwd = 1,
       bty = "n")

datTM <- na.omit(data.frame(NAME=datW$NAME,
                           year=datW$year,
                           TMAX=datW$TMAX))
TM <- aggregate(datTM$TMAX, by=list(datTM$NAME,datTM$year),
                FUN="mean", na.rm=TRUE)

colnames(TM) <- c("NAME","year","AVETMAX")

TM$ncount <- aggregate(datTM$TMAX, by=list(datTM$NAME,datTM$year),
                       FUN="length")$x

TM <- TM[TM$ncount >=364, ]


ndTM <- TM[TM$NAME == nameS[3], ]
nyTM <- TM[TM$NAME == nameS[5], ]



plot(ndTM$year, ndTM$AVETMAX,
     type = "b",
     lwd = 2,
     pch = 19,
     col = "blue 4",
     ylab = "Annual Average Max Temperature (C)",
     xlab = "Year",
     yaxt = "n",
     ylim =c(8, 16))
#add y axis
axis(2, seq(8,16, by=2), las=2 )

help("axis")

#add ny to above plot
points(nyTM$year, nyTM$AVETMAX,
       type = "b",
       lwd = 2,
       pch = 19,
       col="tomato3")

#add legend 
legend("topleft",
       c("North Dakota", "New York"),
       col = c("blue 4", "tomato3"),
       pch = 19,
       lwd = 2,
       bty = "n")

help("plot")

library(ggplot2)
install.packages("RColorBrewer")
library(RColorBrewer)

ggplot(data = pr,aes(x=year, y=totalP, color=NAME)) + 
  geom_point()+ 
  geom_path()+ 
  labs(x="year", y="Annual Percipitation")+ 
  theme_classic()


ggplot(data = datW, aes(x=NAME, y=TMIN))+
  geom_violin(fill=rgb(.933,0.953,0.98))+
  geom_boxplot(width=0.2,size=0.25, fill="grey90")+
  theme_classic()

sub <- datW[datW$NAME == nameS[4] & datW$year == 1974,]

sub$DATE <- as.Date(sub$DATE,"%Y-%m-%d")

ggplot(data = sub, aes(x=DATE, y=TMAX))+
  geom_point()+
  geom_path()+
  theme_classic()+
  labs(x="year", y="Maximum Temperature (C)")

ggplot(data = sub, aes(x=DATE, y=PRCP))+
  geom_col(fill="royalblue3")+
  theme_classic()+
  labs(x="year", y="Daily Precipitation (mm)")

CAsub <- datW[datW$NAME == nameS[2] & datW$year == 1974,]

CAsub$DATE <- as.Date(CAsub$DATE,"%Y-%m-%d")

ggplot(data = CAsub, aes(x=DATE, y=TMAX))+
  geom_point()+
  geom_path()+
  theme_classic()+
  labs(x="year", y="Maximum Temperature (C)")

ggplot(data = CAsub, aes(x=DATE, y=PRCP))+
  geom_col(fill="royalblue3")+
  theme_classic()+
  labs(x="year", y="Daily Precipitation (mm)")

NYsub <- datW[datW$NAME == nameS[5] & datW$year >= 2000,]

NYsub$DATE <- as.Date(NYsub$DATE,"%Y-%m-%d")

ggplot(data = NYsub, aes(x=DATE, y=TMIN))+
  geom_point()+
  geom_path()+
  theme_classic()+
  labs(x="year", y="Minimum Temperature (C)")


NDsub <- datW[datW$NAME == nameS[3] & datW$year >= 2000,]

NDsub$DATE <- as.Date(NDsub$DATE,"%Y-%m-%d")

ggplot(data = NDsub, aes(x=DATE, y=TMIN))+
  geom_point()+
  geom_path()+
  theme_classic()+
  labs(x="Year", y="Daily Minimum Temperature (C)")


ggplot(data = pr,aes(x=year, y=totalP, color=NAME)) + 
  geom_point()+ 
  geom_path()+ 
  labs(x="year", y="Annual Percipitation")+ 
  theme_classic()+
  scale_color_brewer(palette = "Set1")
 












