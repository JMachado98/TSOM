library("dplyr")

BurgundySipDF <- read.csv("TSOM\\MOD2\\Final Project\\BurgundySip.csv", 
                          na.strings = c('NA', '', ' '), 
                          stringsAsFactors = T
                          )

# SN: Serial number of the sample.
# NAME: Winery name
# WINE: Name of the wine
# YR: Year in which the grapes were harvested – indicator of the AGE of the wine.
# RT: Average rating given to the wine by the test users [from 1-5]
# NUMR: Number of testers that reviewed the wine
# REG: Region of the wine
# PR: Price in euros [€]
# TP: Wine variety
# BD: Body score, defined as the richness and weight of the wine in your mouth [from 1-5]
# ACD: Acidity score, defined as wine's
# "pucker” or tartness; it's what makes a wine
# refreshing and your tongue salivate and want
# another sip [from 1-5]
# RSG: residual sugar level of the wine [from 0 - 16]
# AL: Alcohol percentage of the wine.
# DN: The typical density or specific gravity of
# the wine is generally between 1.080 and 1.090.


str(BurgundySipDF)
summary(BurgundySipDF)

sapply(BurgundySipDF, class)
sum(is.na(BurgundySipDF))
colSums(is.na(BurgundySipDF))

NumericColumns <- c(BurgundySipDF$RT, BurgundySipDF$NUMR, BurgundySipDF$PR, BurgundySipDF$BD, BurgundySipDF$ACD)

CategoricalColumns <- c(BurgundySipDF$SN, BurgundySipDF$NAME, BurgundySipDF$WINE, BurgundySipDF$YR, BurgundySipDF$REG,
                        BurgundySipDF$TP, BurgundySipDF$RSG, BurgundySipDF$AL, BurgundySipDF$DN)

hist(NumericColumns, main="Histogram of NumericColumns", xlab="NumericColumns")

barplot(table(CategoricalColumns), main="Bar Plot of CategoricalColumns", xlab="CategoricalColumns", ylab="Frequency")

boxplot(BurgundySipDF$NUMR ~ BurgundySipDF$NAME, data = BurgundySipDF, main="Boxplot of NUMR by NAME")

cor(BurgundySipDF$NUMR, BurgundySipDF$PR)

t.test(table(BurgundySipDF$NAME, BurgundySipDF$RT))

# One Sample t-test
# 
# data:  table(BurgundySipDF$NAME, BurgundySipDF$RT)
# t = 6.8984, df = 48959, p-value = 5.321e-12
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
# 0.1079370 0.1936152
# sample estimates:
# mean of x 
# 0.1507761 

chisq.test(table(BurgundySipDF$SN, BurgundySipDF$NAME))

# Pearson's Chi-squared test
# 
# data:  table(BurgundySipDF$SN, BurgundySipDF$NAME)
# X-squared = 3531695, df = 1755535, p-value < 2.2e-16

# Null Hypothesis (H₀): No significant difference.
# Alternative Hypothesis (H₁): Significant difference.
# p-value < 0.05 indicates significance.

# p-value is smaller than 0.05 thus it means the Null Hypothesis fail and there's significant difference. 

