library(leaps)
library(MASS)

# Read the data
student <- read.table("Downloads/student-por.csv", sep = ";", header = TRUE)

# Exploratory Data Analysis
boxplot(G3 ~ Walc, data = student, xlab = "Alcohol Consumption (scale of 1 to 5)", ylab = "Final Grades")
boxplot(G3 ~ famrel, data = student, xlab = "Family Relationship (scale of 1 to 5)", ylab = "Final Grades")

# FULL MODEL - ALL COVARIATES MINUS OTHER GRADES
full_model <- lm(G3 ~ . - G1 - G2, data = student)
summary(full_model)

# BEST MODEL - USING BACK & FORWARD SELECTION
best_model <- stepAIC(full_model, direction = "both")
summary(best_model) # with half the number of variables, get same adjR2 as the full model (less params is better!)

# QQPLOT - shows left skew, since theoretical quantiles are much lower than actual quantiles (meaning that data has a longer left tail)
qqnorm(rstandard(best_model)) + abline(a = 0, b = 1) # Violates the assumption of normality

# Since this has a non-normal distribution, this violates our assumption of normality and invalidates the regression analysis.
# For future work, we can do Kruskall Walis test (since it has no distributional assumptions)
# to test for any significant differences between scores of students from different family backgrounds

kruskal.test(G3 ~ famrel, data = student)         # p-val: 0.0144 (significant at 5%)
kruskal.test(G3 ~ Walc, data = student)           # p-val: 6.963e-05 (significant at 5%)
kruskal.test(G3 ~ traveltime, data = student)     # p-val: 0.002043 (significant at 5%)

# etc.
