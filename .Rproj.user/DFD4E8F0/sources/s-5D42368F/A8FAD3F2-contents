# Importing package tidyverse to use powerful functions to clean data and create plots.
# Importing package car to use levene's test to check assumptions for ANalysis Of VAriance.
# You need to have both packages installed to run the R-Script.
library(tidyverse)
# Tidyverse import overrides two in built functions filter() and log()
library(car)
# Car import overrides a dplyr function called recode.

# Function that creates a Box-Plot for the Data
createBoxPlot <- function(cannabisData, element){
  ggplot(cannabisData) +
    geom_boxplot(aes(x = Group, y = element, colour = Group))
}

# Function that implements the ANOVA test.
applyAnovaTest <- function(cannabisData, element){
  # Anova Testing
  ANOVA_result <- aov(element ~ Group, data = cannabisData)
  print(summary(ANOVA_result))
  
  # Critical value 0.05 . Checking at type-I-error of 5% . Confidence Interval 95%
  print(qf(p = 0.05, df1 = 3, df2 = ANOVA_result$df.residual, lower.tail = FALSE))
  
  # Return residual values to apply Shapiro Test later for assumption.
  return(ANOVA_result$residuals)
  
}

# Function that checks Constant Spread Assumption [Levene Test]. Variances are equal for ANOVA.
applyLeveneTest <- function(cannabisData, element){
  leveneTest(element ~ Group, data = cannabisData)
}

# Function that checks Normality of Data. [Shapiro-Wilk]. Data comes from Normal Distribution.
applyShapiroTest <- function(residual){
  print(shapiro.test(residual))
}

# Reading the data out of the CSV files.
cannabisData <- read.csv("DATA/potplants_MT5762.csv")
# Selecting only the columns i am interested in.
cannabisData <- cannabisData %>%
  select(Group, Mg, K, Ca, Al, Sc)

cannabisData <- cannabisData %>%
  mutate_if(is.character, as.factor)


# Calling self-declared function that creates BoxPlot for each of the elements.
# Mg
createBoxPlot(cannabisData, cannabisData$Mg)
# K
createBoxPlot(cannabisData, cannabisData$K)
# Ca
createBoxPlot(cannabisData, cannabisData$Ca)
# Al
createBoxPlot(cannabisData, cannabisData$Al)
# Sc
createBoxPlot(cannabisData, cannabisData$Sc)

# Calling Self-declared function to apply ANOVA TEST for each element.
# Mg
MG.residuals <- applyAnovaTest(cannabisData, cannabisData$Mg)
# K
K.residuals <- applyAnovaTest(cannabisData, cannabisData$K)
# Ca
Ca.residuals <- applyAnovaTest(cannabisData, cannabisData$Ca)
# Al
Al.residuals <- applyAnovaTest(cannabisData, cannabisData$Al)
# Sc
Sc.residuals <- applyAnovaTest(cannabisData, cannabisData$Sc)

# Calling Self-declared function to apply Levene Test and test Constant Spread Assumption.
# Mg
applyLeveneTest(cannabisData, cannabisData$Mg)
# K
applyLeveneTest(cannabisData, cannabisData$K)
# Ca
applyLeveneTest(cannabisData, cannabisData$Ca)
# Al
applyLeveneTest(cannabisData, cannabisData$Al)
# Sc
applyLeveneTest(cannabisData, cannabisData$Sc)

# Calling a Self-declared function to apply the Shapiro-Wilk Test, testing Normality.
# Mg
applyShapiroTest(MG.residuals)
# K
applyShapiroTest(K.residuals)
# Ca
applyShapiroTest(Ca.residuals)
# Al
applyShapiroTest(Al.residuals)
# Sc
applyShapiroTest(Sc.residuals)







