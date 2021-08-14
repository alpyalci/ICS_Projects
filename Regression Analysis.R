# Libraries
library(dplyr)
library(olsrr)
library(gmodels)
library(xtable)
library(broom)
library(ggplot2)
# Set working path
setwd("~/Desktop/TU Dortmund/semester2/Introduction to Case Studies/Introductory Case Studies project 3/")

ImmoNRW <- read.csv("ImmoDataNRW.csv")
head(ImmoNRW,10)
ImmoNRW<- as.data.frame(ImmoNRW)

# Task 1

# Subtask 1.1.

ImmoDort <- filter(ImmoNRW, regio2=="Dortmund")


colSums(is.na(ImmoDort))
max(colSums(is.na(ImmoDort))) 

ImmoDort_no_park <- ImmoDort %>% select(-noParkSpaces)

ImmoDort_no_NA<- na.omit(ImmoDort_no_park)
colSums(is.na(ImmoDort_no_NA ))


# Subtask 1.2.

ImmoDort_no_NA["sqmPrice"] <- (ImmoDort_no_NA$totalRent/ImmoDort_no_NA$livingSpace)
#ImmoDort_no_NA$sqmPrice <- round(ImmoDort_no_NA$sqmPrice, digit=2)

# Subtask 1.3.


ImmoDort_no_NA$typeOfFlat[ImmoDort_no_NA$typeOfFlat == "loft" | ImmoDort_no_NA$typeOfFlat == "maisonette"| ImmoDort_no_NA$typeOfFlat == "penthouse"| ImmoDort_no_NA$typeOfFlat == "terraced_flat"| ImmoDort_no_NA$typeOfFlat == "other"] <-  "luxurious_artistic_other"
ImmoDort_no_NA$typeOfFlat[ImmoDort_no_NA$typeOfFlat == "ground_floor"|ImmoDort_no_NA$typeOfFlat == "raised_ground_floor"] <- "r_ground_floor"
ImmoDort_no_NA$typeOfFlat[ImmoDort_no_NA$typeOfFlat == "roof_storey"|ImmoDort_no_NA$typeOfFlat == "half_basement"] <- "roof_half_basement"

table(ImmoDort_no_NA$typeOfFlat)


# Task 2

ImmoDort_FULL <- ImmoDort_no_NA %>% select(-ID,-totalRent,-regio2)  

#View(ImmoDort_FULL)
str(ImmoDort_FULL)

#Descriptive Analysis for Numeric Variables

ImmoDort_FULL_Numeric <- ImmoDort_FULL %>% 
  select(yearConstructed, livingSpace, sqmPrice)

xtable(summary(ImmoDort_FULL_Numeric))

#Descriptive Analysis for Categorical Variables
cbind(c("newlyConst", "balcony", "hasKitchen", "lift", "garden"),rbind(table(ImmoDort_FULL$newlyConst)
,table(ImmoDort_FULL$balcony)
,table(ImmoDort_FULL$hasKitchen)
,table(ImmoDort_FULL$lift)
,table(ImmoDort_FULL$garden)
))
table(ImmoDort_FULL$typeOfFlat)
table(ImmoDort_FULL$condition)
table(ImmoDort_FULL$lastRefurbish)
table(ImmoDort_FULL$energyEfficiencyClass)

table(ImmoDort_FULL$floor)



# subtask 2.1.


Linear_Model <- lm(sqmPrice ~ . ,data=ImmoDort_FULL)
#summary(Linear_Model)


Complete_Subset <- ols_step_all_possible(Linear_Model)
str(Complete_Subset)

DF_Complete_Subset <- as.data.frame(Complete_Subset)
DF_Complete_Subset
View(DF_Complete_Subset)


Complete_Subset$predictors[which.min(Complete_Subset$aic)]
Complete_Subset$aic[which.min(Complete_Subset$aic)]


Complete_Subset$predictors[which.min(Complete_Subset$sbc)]
Complete_Subset$sbc[which.min(Complete_Subset$sbc)]

xtable(rbind(c("AIC", "BIC")
,cbind(Complete_Subset$predictors[which.min(Complete_Subset$aic)],Complete_Subset$aic[which.min(Complete_Subset$aic)])
,cbind(Complete_Subset$predictors[which.min(Complete_Subset$sbc)],Complete_Subset$sbc[which.min(Complete_Subset$sbc)])
))



# subtask 2.2.
## Assumptions

Model_AIC <- lm(sqmPrice~newlyConst + yearConstructed+ hasKitchen+livingSpace+lift+typeOfFlat+floor+condition+energyEfficiencyClass, 
                data = ImmoDort_FULL)

# Standardized Residual Plot for Homocedasticity
pdf("resplot.pdf")
plot(Model_AIC$fitted.values, rstandard(Model_AIC), pch = 1,
     main = "Residual Plot", xlab = "Predictions (Eur/m^2)", ylab = "Residuals (Eur/m^2)")
abline(0,0,col = "red")
dev.off()

# Residual Plot for Homocedasticity
residuals <- Model_AIC$fitted.values - ImmoDort_FULL$sqmPrice
plot(Model_AIC$fitted.values, residuals, pch = 1,
     main = "Residual Plot", xlab = "Predictions (Eur/m^2)", ylab = "Residuals (Eur/m^2)")
abline(0,0,col = "red")

# QQ-Plot for Normality with Standardized Residuals
pdf("QQplot.pdf")
qqnorm(rstandard(Model_AIC), xlab = "Theoretical Quantiles", ylab = "Standardized Residuals", main = "QQ-Plot for AIC Residuals")
qqline(rstandard(Model_AIC), col = "red")
dev.off()

# QQ-Plot for Normality with Residuals
qqnorm(residuals, xlab = "Theoretical Quantiles", ylab = "Standardized Residuals", main = "QQ-Plot for AIC Residuals")
qqline(residuals, col = "red")


summary(Model_AIC)

ciAIC <- ci(Model_AIC)
ciAIC

xtable(Model_AIC)

glance <- glance(Model_AIC) %>%
  dplyr::select(adj.r.squared, r.squared, AIC, BIC, p.value)

xtable(glance)


citation(dplyr, olsrr, gmodels, xtable, broom, ggplot2)
