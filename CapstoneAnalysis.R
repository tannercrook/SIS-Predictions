

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

# Find missing values
sapply(grades, function(x) sum(is.na(x)))

# Drop some columns
grades$percentScore <- NULL
grades$gradYear <- NULL
grades$gradSchool <- NULL


# Univariate Stats
barplot(table(demographics$graduated),xlab="Graduated",ylab="Frequency")



library(FactoMineR)
library(factoextra)

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
# Get the course grade data
psGrades = read.csv("~/Documents/capData/PS_StoredGrades.csv", header=TRUE)
icGrades = read.csv("~/Documents/capData/IC_StoredGrades.csv", header=TRUE)

grades <- rbind(icGrades,psGrades)


failedGrades <- grades[grades$score %in% c('F','D-'), ]


# Drop some columns
grades$percentScore <- NULL
failedGrades$gradYear <- NULL
failedGrades$gradSchool <- NULL
failedGrades$studentNumber <- NULL
failedGrades$dateStored <- NULL
failedGrades$percentScore <- NULL
failedGrades$schedGradYear<- NULL

failedGrades$score <- as.factor(failedGrades$score)
failedGrades$courseName <- as.factor(failedGrades$courseName)
failedGrades$gradeLevel <- as.factor(failedGrades$gradeLevel)

summary(failedGrades)
failedGrades <- failedGrades[complete.cases(failedGrades), ]

res.famd <- FAMD(failedGrades, graph=TRUE)
fviz_mfa_ind(res.famd, habillage = "graduated" ,addEllipses = TRUE)
fviz_famd_var(res.famd, col.var = "contrib", repel = TRUE,gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

#failedGrades$dropout <- failedGrades$graduated
#failedGrades$dropout <- factor(failedGrades$dropout, levels=c(0,1), labels=c(1, 0))
#failedGrades$graduated <- NULL



library('caTools')

# Set the seed so we can run multiple times
set.seed(111)

#sample <- sample.split(tele$Churn, SplitRatio=0.70)
train <- failedGrades[1:6500,]
test <- failedGrades[6501:8840,]

model <- glm(graduated ~ ., family = binomial(link="logit"), data = train)
summary(model)


model3 <- glm(graduated ~ factor(courseName)+factor(score), family = binomial(link = "logit"), data=train)
summary(model3)

model3 <- glm(graduated ~ factor(gradeLevel)+factor(score), family = binomial(link = "logit"), data=train)
summary(model3)

predict <- predict(model3, type="response")

# Analyzing results of test
table(train$graduated, predict > 0.5)
library(ROCR)

ROCRpred <- prediction(predict, train$graduated)
ROCRPerf <- performance(ROCRpred, 'tpr', 'fpr')
plot(ROCRPerf, colorize = TRUE, text.adj = c(-0.2,1.7))

auc <- performance(ROCRpred, measure = "auc")
auc <- auc@y.values[[1]]
auc

exp(cbind(Odd_Ratio = coef(model3), confint.default(model3)))
odds <- exp(cbind(Odd_Ratio = coef(model3), confint.default(model3)))

write.csv(odds, file = "~/Documents/capData/courseOdds.csv")









# Fail Count Dataset Analysis
# =====================================

failCount = read.csv("~/Documents/capData/failCount.csv", header=TRUE)

# remove technical data
failCount$studentNumber <- NULL
failCount$Total <- NULL

# change NAs to 0s
failCount[is.na(failCount)] <- 0

# Make a function
cat_counts <- function(A){
  if (A == 0) {
    return('0')
  } else if (A >= 1 && A <= 2) {
    return('1-2')
  } else if (A > 2 && A <= 6) {
    return('3-6')
  } else if (A > 6 && A <= 10) {
    return('7-10')
  } else if (A > 10) {
    return('11+')
  }
}

failCount$A_cat <- sapply(failCount$A, cat_counts)
failCount$AMinus_cat <- sapply(failCount$AMinus, cat_counts)
failCount$BPlus_cat <- sapply(failCount$BPlus, cat_counts)
failCount$B_cat <- sapply(failCount$B, cat_counts)
failCount$BMinus_cat <- sapply(failCount$BMinus, cat_counts)
failCount$CPlus_cat <- sapply(failCount$CPlus, cat_counts)
failCount$C_cat <- sapply(failCount$C, cat_counts)
failCount$CMinus_cat <- sapply(failCount$CMinus, cat_counts)
failCount$DPlus_cat <- sapply(failCount$DPlus, cat_counts)
failCount$D_cat <- sapply(failCount$D, cat_counts)
failCount$DMinus_cat <- sapply(failCount$DMinus, cat_counts)
failCount$F_cat <- sapply(failCount$F, cat_counts)

failCount$A_cat <- as.factor(failCount$A_cat)
failCount$AMinus_cat <- as.factor(failCount$AMinus_cat)
failCount$BPlus_cat <- as.factor(failCount$BPlus_cat)
failCount$B_cat <- as.factor(failCount$B_cat)
failCount$BMinus_cat <- as.factor(failCount$BMinus_cat)
failCount$CPlus_cat <- as.factor(failCount$CPlus_cat)
failCount$C_cat <- as.factor(failCount$C_cat)
failCount$CMinus_cat <- as.factor(failCount$CMinus_cat)
failCount$DPlus_cat <- as.factor(failCount$DPlus_cat)
failCount$D_cat <- as.factor(failCount$D_cat)
failCount$DMinus_cat <- as.factor(failCount$DMinus_cat)
failCount$F_cat <- as.factor(failCount$F_cat)


# Fail Count FAMD
# ====================================================

library(FactoMineR)
library(factoextra)

res.famd <- FAMD(failCount, graph=TRUE)

fviz_mfa_ind(res.famd, habillage = "graduated" ,addEllipses = TRUE)

fviz_famd_var(res.famd, col.var = "contrib", repel = TRUE,gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))



# LOGISTICAL REGRESSION
# ======================================================

library('caTools')

#failedGrades$dropout <- failedGrades$graduated
#failCount$graduated <- factor(failCount$graduated, levels=c(0,1), labels=c(1, 0))
#failedGrades$graduated <- NULL

# Set the seed so we can run multiple times
set.seed(111)

#sample <- sample.split(tele$Churn, SplitRatio=0.70)
train <- failCount[1:2000,]
test <- failCount[2001:2594,]


model3 <- glm(graduated ~ ., family = binomial(link = "logit"), data=train)
summary(model3)


model3 <- glm(graduated ~ F_cat+DMinus_cat+D_cat+DPlus_cat+CMinus_cat+C_cat, family = binomial(link = "logit"), data=train)
summary(model3)

predict <- predict(model3, type="response")



# Analyzing results of test
table(train$graduated, predict > 0.5)
library(ROCR)

ROCRpred <- prediction(predict, train$graduated)
ROCRPerf <- performance(ROCRpred, 'tpr', 'fpr')
plot(ROCRPerf, colorize = TRUE, text.adj = c(-0.2,1.7))

auc <- performance(ROCRpred, measure = "auc")
auc <- auc@y.values[[1]]
auc


exp(cbind(Odd_Ratio = coef(model3), confint(model3)))


