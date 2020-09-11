#activity 2

heights <- c(3,2,3)


datW <- read.csv("/Users/lanaraine/Desktop/ES DATA/a02.csv")

datW <- read.csv("Users/lanaraine/Desktop/ES DATA/a02/noaa2011124.csv")


datW <- read.csv("/Users/lanaraine/Desktop/ES DATA/noaa2011124.csv")


datW$PRCP/10

mean(datW$PRCP,na.rm = TRUE)



datW$PRCP_cm (2)

datW$PRCP[datW$year == 1930]

datW[datW$year <= 1950, ]



hist(datW$TMAX[datW$year == 1930 & datW$NAME == "MORRISVILLE 6 SW, NY US" ])



help(matrix)


#matrix with 2 columns and fill by rows
#first argument is vector of number to fill it
Mat<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=TRUE)
Mat

#matrix that fills by column and first argue is vector to fill it

Mat.bycol<-matrix(c(1,2,3,4,5,6), ncol=2)
Mat.bycol

Mat.bycol<-matrix(c(1,2,3,4,5,6), ncol=2, byrow = FALSE)
Mat.bycol

#subset matrix to see row 1, col 2
Mat.bycol [1,2]
#look at row 1 values
Mat.bycol [1,]
#look at Matrix column 2
Mat.bycol [,2]

#get more info about the dataframe
str(datW)



datW$NAME<- as.factor(datW$NAME)




#find out all unique site names
levels(datW$NAME)

#look at max temp for Aberdeen
mean(datW$TMAX[datW$NAME == "ABERDEEB,WA US"])

#look at max temp for Aberdeen
#set na.rm argument to true in order to ignore NA

mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm=TRUE)

#next find standard deviation 
sd(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm=TRUE)

#calculate average daily temp
#this is half between min and max temp
datW$TAVE <- datW$TMIN + ((datW$TMAX-datW$TMIN)/2)

#get mean across all sites
#the by function is a list of one or more variables to index over
#FUN indicates function we want to use
#to specify any function specific arguments use a comma and add them after the function
#here use na.rm arguments specific to
averageTemp <- aggregate(datW$TAVE, by=list(datW$NAME), FUN="mean",na.rm=TRUE)
averageTemp

#change the auto output of column names to be more meaningful
#note MAAT is common abbrev for Mean Annual Air Temp
colnames(averageTemp) <- c("NAME","MAAT")
averageTemp

#convert level to number for factor data type
#you will have to reference level output or look at the row od data to see the character designation. 
datW$siteN <- as.numeric(datW$NAME)
averageTemp

#make a histogram for the first site in our levels 
#main= is title name argument.
#here you want to paste the actual name of the factor not the numeric index
#since that will be more meaningful
hist(datW$TAVE[datW$siteN == 1],
     freq = FALSE,
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average dailt temperature (degrees C)",
     ylab = "Relative frequency",
     col = "grey75",
     border = "white")

help("hist")

hist(datW$TAVE[datW$siteN == 3],
     freq = FALSE,
     main = paste(levels(datW$NAME)[3]),
     xlab = "Average dailt temperature (degrees C)",
     ylab = "Relative frequency",
     col = "grey75",
     border = "white")


help(dnorm)

#pnorm (volume to evaluate at(will evaluate below too), mean,sd)
pnorm(0, 
      mean(datW$TAVE[datW$siteN == 1],na.rm = TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm = TRUE))

#pnorm with 5 gives all probability (area of the curve) below 5
pnorm(5, 
      mean(datW$TAVE[datW$siteN == 1],na.rm = TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm = TRUE))

#subract pnorm0 by pnorm5 to see range 0-5

pnorm(5, 
      mean(datW$TAVE[datW$siteN == 1],na.rm = TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm = TRUE)) - pnorm(0,
      mean(datW$TAVE[datW$siteN == 1],na.rm = TRUE),                                                     
      sd(datW$TAVE[datW$siteN == 1],na.rm = TRUE))

#pnorm 20 gives probability below 20
#subtracting from one gives area above 20

1 - pnorm(20, 
      mean(datW$TAVE[datW$siteN == 1],na.rm = TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm = TRUE))

#qnorm gives value at which all values and below equal the probability in the argument
#calculating here the value of the 95th quantile or a probability of 0.95
qnorm(0.95, 
      mean(datW$TAVE[datW$siteN == 1],na.rm = TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm = TRUE))

mean(datW$TAVE[datW$siteN == 1],na.rm = TRUE)

1 - pnorm(18.51026, 
      mean(datW$TAVE[datW$siteN == 1]+4,na.rm = TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm = TRUE)) 

1 - pnorm(18.51026, 
          mean(14.43227,na.rm = TRUE),
          sd(datW$TAVE[datW$siteN == 1],na.rm = TRUE)) 

1 - pnorm(18.51026, 
          mean(datW$TAVE[datW$siteN == 1],na.rm = TRUE),
          sd(datW$TAVE[datW$siteN == 1],na.rm = TRUE)) 




#Abberdeen Precipitation Histogram
hist(datW$PRCP[datW$siteN == 1],
     freq = FALSE,
     main = paste(levels(datW$NAME)[1]),
     xlab = "Daily Percipitation",
     ylab = "Relative frequency",
     col = "grey75",
     border = "white")


#use sum and aggregate function to get annual precipitation for each year and site
aggregate(datW$PRCP, by=list(datW$NAME, datW$year), FUN="sum",na.rm=TRUE)
#name annual precipitation data
AnnualPRCP <- aggregate(datW$PRCP, by=list(datW$NAME, datW$year), 
                        FUN="sum",na.rm=TRUE)
#name columns in AnnualPRCP
colnames(AnnualPRCP) <- c("NAME","YEAR","AnnualPRCP")

AnnualPRCP

AnnualPRCP$NAME <- as.numeric(AnnualPRCP$NAME)
levels(datW$NAME)[1]

AnnualPRCP


#hist for annual precipitation in Aberdeen
hist(AnnualPRCP$AnnualPRCP [AnnualPRCP$NAME == 1],
     freq = FALSE,
     main = "ABERDEEN WA, US",
     ylab = "Frequency",
     xlab = "Annual Precipitation",
     col = "grey75",
     border = "white") 

#hist for annual precipitation Mandan
hist(AnnualPRCP$AnnualPRCP [AnnualPRCP$NAME == 3], 
     freq = FALSE,
     main = "MANDAN EXPERIMENT STATION, ND US",
     ylab = "Frequency",
     xlab = "Annual Precipitation",
     col = "grey75",
     border = "white")


pnorm (700, 
      mean(AnnualPRCP$AnnualPRCP[AnnualPRCP$NAME == 3],na.rm = TRUE),
      sd(AnnualPRCP$AnnualPRCP[AnnualPRCP$NAME == 3],na.rm = TRUE))

pnorm (700, 
       mean(AnnualPRCP$AnnualPRCP[AnnualPRCP$NAME == 1],na.rm = TRUE),
       sd(AnnualPRCP$AnnualPRCP[AnnualPRCP$NAME == 1],na.rm = TRUE))

pnorm (700, 
       mean(AnnualPRCP$AnnualPRCP[AnnualPRCP$NAME == 3],na.rm = TRUE),
       sd(AnnualPRCP$AnnualPRCP[AnnualPRCP$NAME == 3],na.rm = TRUE)) - pnorm (700, 
       mean(AnnualPRCP$AnnualPRCP[AnnualPRCP$NAME == 1],na.rm = TRUE),
       sd(AnnualPRCP$AnnualPRCP[AnnualPRCP$NAME == 1],na.rm = TRUE))
     

help("factor")
str(datW)

integer <- c(2.2,2.1,2.3,2.4,2.5)
integer <- c(2,2,2,2,2)
