

# Get the demographic data in
psDemographics = read.csv("~/Documents/capData/PS_Demographics.csv", header=TRUE)
icDemographics = read.csv("~/Documents/capData/IC_Demographics.csv", header=TRUE)

# Delete some of the columns. They don't have the data between the two systems to support analysis
psDemographics$IEP <- NULL
psDemographics$FREEREDUCED <- NULL

summary(psDemographics)
summary(icDemographics)

# Get the course grade data
psGrades = read.csv("~/Documents/capData/PS_StoredGrades.csv", header=TRUE)
icGrades = read.csv("~/Documents/capData/IC_StoredGrades.csv", header=TRUE)

summary(psGrades)
summary(icGrades)