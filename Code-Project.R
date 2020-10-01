# Importing package tidyverse to use powerful functions to clean data and create plots.
# Importing package car to use levene's test to check assumptions for ANalysis Of VAriance.
# You need to have all packages (tidyverse, car, pgirmess, onewaytests) installed to run the R-Script.
# Tidyverse import overrides two in built functions filter() and log()
library(tidyverse)
# Car import overrides a dplyr function called recode.
library(car)
# To use alternative Anova tests you need to download these 2 libraries.
library(pgirmess)
library(onewaytests)


# Function that implements the ANOVA test and identifies the different means for H1.
applyAnovaTest <- function(cannabisData, element){
  # Anova Testing
  ANOVA_result <- aov(element ~ Group, data = cannabisData)
  # Outputting the results of the anova tests.
  print(summary(ANOVA_result))

  # Function that identifies the means that are significantly different. Using Tukeys Method
  difference <- TukeyHSD(ANOVA_result)
  # Prefer the table text to see results. Not plotting the data.
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


# Reading the data out of the CSV files.
cannabisData <- read.csv("DATA/potplants_MT5762.csv")
# Selecting only the columns i am interested in.
cannabisData <- cannabisData %>%
  select(Group, Sc, Ti, Se, Sm, Tm)

# Making the groups column a factor type for more control in program.
cannabisData <- cannabisData %>%
  mutate_if(is.character, as.factor)

#### Commands needed to execute Question 1 and 2.
# Using tidyverse function to plot a boxplot for each element and compare 
# the elementl composition in different soil types.
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
# Once Anova Called we test our assumptions using the Anova residuals to make
# sure our results are at the best level they can be.
# applyLeveneTest = Self-declared function to apply Levene Test and test Constant Spread Assumption.
# applyShapiroTest = Self-declared function to apply the Shapiro-Wilk Test, testing Normality.
# Sc
applyLeveneTest(cannabisData, cannabisData$Sc)
Sc.residuals <- applyAnovaTest(cannabisData, cannabisData$Sc)
applyShapiroTest(Sc.residuals)
# Ti
applyLeveneTest(cannabisData, cannabisData$Ti)
Ti.residuals <- applyAnovaTest(cannabisData, cannabisData$Ti)
applyShapiroTest(Ti.residuals)

# Cannot Apply Anova test, because it does not follow normality but has equal 
# standard deviation we can use Kruskal test on Se. Kruskal assumes
# equal standard deviation.
# Se
applyLeveneTest(cannabisData, cannabisData$Se)
Se.reisdual_anova <- applyAnovaTest(cannabisData, cannabisData$Se)
applyShapiroTest(Se.reisdual_anova)
# Fails the shapiro test, indicates does not follow normality. Use Kruskal's
Se.residuals_kruskal <- kruskal.test(Se ~ Group, data=cannabisData)
kruskalmc(Se ~ Group, data=cannabisData)

# Cannot Apply Anova test because data does not have constant spread in there groups of data.
# Resulting we need to use an alternative Anova test that does not assumes equal 
# standard deviations.We will use the Welch's Anova test and we have normality here.
# Sm
applyLeveneTest(cannabisData, cannabisData$Sm)
# Fails constant spread test. So we check for normality
Sm.residuals_anova <- applyAnovaTest(cannabisData, cannabisData$Sm)
applyShapiroTest(Sm.residuals_anova)
# Passes normality. Use welch's Test
# Fails normality assumption and so we use welch's Anova
Sm.residuals_welch <- welch.test(Sm ~ Group, data=cannabisData)

# Tm
applyLeveneTest(cannabisData, cannabisData$Tm)
# Fails constant spread test. So we check for normality
Tm.residuals_anova <- applyAnovaTest(cannabisData, cannabisData$Tm)
applyShapiroTest(Tm.residuals_anova)
# Passes normality. Use welch's Test
# Fails normality assumption and so we use welch's Anova
Tm.residuals_welch <- welch.test(Tm ~ Group, data=cannabisData)


#### Commands needed to execute Question 3.
# calling R function cor(), finds the correlation coefficient ρ which 
# which indicates if two datasets are linear related.
# Sc with Ti 
cor(cannabisData$Sc, cannabisData$Ti)
ggplot(cannabisData,aes(x = Sc, y = Ti, colour = Group)) +
  geom_point() +
  stat_smooth(method = ("lm")) +
  xlab("Element Sc") +
  ylab("Element Ti") + 
  ggtitle("Relationship between elemenet Sc with Ti")
# Sm with Tm
cor(cannabisData$Sm, cannabisData$Tm)
ggplot(cannabisData,aes(x = Sm, y = Tm, colour = Group)) +
  geom_point() +
  stat_smooth(method = ("lm")) +
  xlab("Element Sm") +
  ylab("Element Tm") + 
  ggtitle("Relationship between elemenet Sm with Tm")
# Sm with Sc
cor(cannabisData$Sm, cannabisData$Sc)
ggplot(cannabisData,aes(x = Sm, y = Sc, colour = Group)) +
  geom_point() +
  stat_smooth(method = ("lm")) +
  xlab("Element Sm") +
  ylab("Element Sc") + 
  ggtitle("Relationship between elemenet Sm with Sc")
# Sm with Ti
cor(cannabisData$Sm, cannabisData$Ti)
ggplot(cannabisData,aes(x = Sm, y = Ti, colour = Group)) +
  geom_point() +
  stat_smooth(method = ("lm")) +
  xlab("Element Sm") +
  ylab("Element Ti") + 
  ggtitle("Relationship between elemenet Sm with Ti")
# Sc with Tm
cor(cannabisData$Sc, cannabisData$Tm)
ggplot(cannabisData,aes(x = Sc, y = Tm, colour = Group)) +
  geom_point() +
  stat_smooth(method = ("lm")) +
  xlab("Element Sc") +
  ylab("Element Tm") + 
  ggtitle("Relationship between elemenet Sc with Tm")





