

# Get the demographic data in
psDemographics = read.csv("~/Documents/capData/PS_Demographics.csv", header=TRUE)
icDemographics = read.csv("~/Documents/capData/IC_Demographics.csv", header=TRUE)

# Delete some of the columns. They don't have the data between the two systems to support analysis
psDemographics$IEP <- NULL
psDemographics$FreeReduced <- NULL

summary(psDemographics)
summary(icDemographics)

# Get the course grade data
psGrades = read.csv("~/Documents/capData/PS_StoredGrades.csv", header=TRUE)
icGrades = read.csv("~/Documents/capData/IC_StoredGrades.csv", header=TRUE)

summary(psGrades)
summary(icGrades)


# Combine the data
demographics <- rbind(icDemographics,psDemographics)
grades <- rbind(icGrades,psGrades)

# Univariate Stats
barplot(table(demographics$graduated),xlab="Graduated",ylab="Frequency")



library(FactoMineR)
library(factoextra)


