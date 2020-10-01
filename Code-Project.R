# Importing package tidyverse to use powerful functions to clean data and create plots.
# Importing package car to use levene's test to check assumptions for ANalysis Of VAriance.
# You need to have both packages (tidyverse, car) installed to run the R-Script.
# Tidyverse import overrides two in built functions filter() and log()
library(tidyverse)
# Car import overrides a dplyr function called recode.
library(car)


# Function that implements the ANOVA test and identifies the different means for H1.
applyAnovaTest <- function(cannabisData, element){
  # Anova Testing
  ANOVA_result <- aov(element ~ Group, data = cannabisData)
  print(summary(ANOVA_result))
  
  # Critical value 0.05 . Checking at type-I-error of 5% . Confidence Interval 95%
  print(qf(p = 0.05, df1 = 3, df2 = ANOVA_result$df.residual, lower.tail = FALSE))

  # Function that identifies the difference of means. Failed ANOVA. Using Tukeys Method
  difference <- TukeyHSD(ANOVA_result)
  plot(difference)
  print(difference)
  
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

# Function that finds the correlation value between 2 continuous variables.
findCorrelation <- function(element1, element2){
  print(cor(element1, element2))
}

# Function that identifies the difference of means. Failed ANOVA. Using Tukeys Method

# Reading the data out of the CSV files.
cannabisData <- read.csv("DATA/potplants_MT5762.csv")
# Selecting only the columns i am interested in.
cannabisData <- cannabisData %>%
  select(Group, Sc, Ti, Se, Sm, Tm)

# Making the groups column a factor type for more control in program.
cannabisData <- cannabisData %>%
  mutate_if(is.character, as.factor)

#### Commands needed to execute Question 1 and 2.
# Calling self-declared function that creates BoxPlot for each of the elements.
# Sc
ggplot(cannabisData) +
  geom_boxplot(aes(x = Group, y = Sc, colour = Group)) +
  xlab("Soil Groups") +
  ylab("Element Sc") +
  ggtitle("Sc Elemental Composition in different soil types")
# Ti
ggplot(cannabisData) +
  geom_boxplot(aes(x = Group, y = Ti, colour = Group)) +
  xlab("Soil Groups") +
  ylab("Element Ti") +
  ggtitle("Ti Elemental Composition in different soil types")
# Se
ggplot(cannabisData) +
  geom_boxplot(aes(x = Group, y = Se, colour = Group)) +
  xlab("Soil Groups") +
  ylab("Element Se") +
  ggtitle("Se Elemental Composition in different soil types")
# Sm
ggplot(cannabisData) +
  geom_boxplot(aes(x = Group, y = Sm, colour = Group)) +
  xlab("Soil Groups") +
  ylab("Element Sm") +
  ggtitle("Sm Elemental Composition in different soil types")
# Tm
ggplot(cannabisData) +
  geom_boxplot(aes(x = Group, y = Tm, colour = Group)) +
  xlab("Soil Groups") +
  ylab("Element Tm") +
  ggtitle("Tm Elemental Composition in different soil types")


#### Commands needed to execute Question 2.
# Calling Self-declared function to apply ANOVA TEST for each element.
# Sc
Sc.residuals <- applyAnovaTest(cannabisData, cannabisData$Sc)
# Ti
Ti.residuals <- applyAnovaTest(cannabisData, cannabisData$Ti)
# Se
Se.residuals <- applyAnovaTest(cannabisData, cannabisData$Se)
# Sm
Sm.residuals <- applyAnovaTest(cannabisData, cannabisData$Sm)
# Tm
Tm.residuals <- applyAnovaTest(cannabisData, cannabisData$Tm)

# Calling Self-declared function to apply Levene Test and test Constant Spread Assumption.
# Mg
applyLeveneTest(cannabisData, cannabisData$Sc)
# K
applyLeveneTest(cannabisData, cannabisData$Ti)
# Ca
applyLeveneTest(cannabisData, cannabisData$Se)
# Al
applyLeveneTest(cannabisData, cannabisData$Sm)
# Sc
applyLeveneTest(cannabisData, cannabisData$Tm)

# Calling a Self-declared function to apply the Shapiro-Wilk Test, testing Normality.
# Mg
applyShapiroTest(Sc.residuals)
# K
applyShapiroTest(Ti.residuals)
# Ca
applyShapiroTest(Se.residuals)
# Al
applyShapiroTest(Sm.residuals)
# Sc
applyShapiroTest(Tm.residuals)




#### Commands needed to execute Question 3.
## calling function to find correlation between 2 elements.
# Sc with Ti 
findCorrelation(cannabisData$Sc, cannabisData$Ti)
# Sm with Tm
findCorrelation(cannabisData$Sm, cannabisData$Tm)
# Sm with Sc
findCorrelation(cannabisData$Sm, cannabisData$Sc)
# Tm with Ti
findCorrelation(cannabisData$Tm, cannabisData$Ti)
# Se with Tm
findCorrelation(cannabisData$Se, cannabisData$Tm)





