

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

#transform binary to yes,no
demographics$graduated <- factor(demographics$graduated, levels=c(0,1), labels=c("No", "Yes"))
grades$graduated <- factor(grades$graduated, levels=c(0,1), labels=c("No", "Yes"))

# Remove bad schedGradYear
demographics<-demographics[demographics$schedGradYear != 0, ]

# Drop some columns
grades$percentScore <- NULL


# Univariate Stats
barplot(table(demographics$graduated),xlab="Graduated",ylab="Frequency")



library(FactoMineR)
library(factoextra)

# Find missing values
sapply(grades, function(x) sum(is.na(x)))

#currently won't run because the dataset is simply too large
#res.famd <- FAMD(grades, graph=FALSE)


# Lets get data on people who actually graduated
gradDemo <- demographics[demographics$graduated == 1, ]

# Get set of those who didn't
noGradDemo <- demographics[demographics$graduated != 1, ]
noGradDemo$gradYear <- NULL
noGradDemo$gradSchool <- NULL

noGradGrades <- grades[grades$graduated != 'Yes', ]
noGradGrades <- noGradGrades[complete.cases(noGradGrades), ]

summary(noGradGrades)
res.famd <- FAMD(noGradGrades, graph=FALSE)







# Logistic Regression
# ==============================================

failedGrades <- grades[grades$score %in% c('F','D-','D','D+'), ]
failedGrades$gradYear <- NULL
failedGrades$gradSchool <- NULL
failedGrades$studentNumber <- NULL

summary(failedGrades)
failedGrades <- failedGrades[complete.cases(failedGrades), ]

failedGrades$dropout <- failedGrades$graduated
failedGrades$dropout <- factor(failedGrades$dropout, levels=c(0,1), labels=c(1, 0))
failedGrades$graduated <- NULL


library('caTools')

# Set the seed so we can run multiple times
set.seed(111)

#sample <- sample.split(tele$Churn, SplitRatio=0.70)
train <- failedGrades[1:15000,]
test <- failedGrades[15001:18410,]

model <- glm(dropout ~ ., family = binomial(link="logit"), data = train)
summary(model)


model3 <- glm(dropout ~ gradeLevel+courseName, family = binomial(link = "logit"), data=train)
summary(model3)

predict <- predict(model3, type="response")

# Analyzing results of test
table(train$dropout, predict > 0.5)
library(ROCR)

ROCRpred <- prediction(predict, train$dropout)
ROCRPerf <- performance(ROCRpred, 'tpr', 'fpr')
plot(ROCRPerf, colorize = TRUE, text.adj = c(-0.2,1.7))

auc <- performance(ROCRpred, measure = "auc")
auc <- auc@y.values[[1]]
auc






