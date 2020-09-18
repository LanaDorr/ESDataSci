
ch4 <- read.csv("/Users/lanaraine/Desktop/ES DATA/lemming_herbivory.csv")

ch4$herbivory <- as.factor(ch4$herbivory)

#dependent variable ~ independent variable

plot(ch4$CH4_Flux ~ ch4$herbivory)

#shapiro-wilk normality test: good for small data sets
shapiro.test(ch4$CH4_Flux[ch4$herbivory == "Ex"])
shapiro.test(ch4$CH4_Flux[ch4$herbivory == "Ctl"])

#bartlett test
bartlett.test(ch4$CH4_Flux ~ ch4$herbivory)

#welch two sample t test
t.test(ch4$CH4_Flux ~ ch4$herbivory)

help("t.test")

#read in insect data
datI <- read.csv("/Users/lanaraine/Desktop/ES DATA/insect_richness.csv")
datI$urbanName <- as.factor(datI$urbanName)

#shapiro-wilks
shapiro.test(datI$Richness[datI$urbanType == "3"])
shapiro.test(datI$Richness[datI$urbanType == "1"])
shapiro.test(datI$Richness[datI$urbanType == "8"])
shapiro.test(datI$Richness[datI$urbanType == "9"])


#bartlett test
bartlett.test(datI$Richness ~ datI$urbanName)

#specify model for species richness and urban type
in.mod <- lm(datI$Richness ~ datI$urbanName)
#run the ANOVA
in.aov <- aov(in.mod)
#print out ANOVA table
summary(in.aov)

#run Tukey HSD
tukeyT <- TukeyHSD(in.aov)
#view results
tukeyT

#make a plot
#use cex.axis to make axis labels smaller than usual to fit on plot 
plot(tukeyT, cex.axis=0.75)

tapply(datI$Richness, datI$urbanName, "mean")


#set up contigency table
species <- matrix(c(18,8,15,32), ncol = 2, byrow = TRUE)
colnames(species) <- c("Not protected", "Protected")
rownames(species) <- c("Declining", "Stable/Increase")

#make a mosaic plot 
mosaicplot(species, xlab = "population status", ylab = "legal protection", 
           main = "Legal Protection Impacts on Populations" )

mean(species)

#Conduct a chi-squared test
chisq.test(species)



