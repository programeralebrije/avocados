# Load Libraries
library(“dplyr”)
library(“rcompanion”)
library(“car”)
# Question Set up 
# Does the average price of avocado diffe between Albany, Houston and Seattle?
#Filter the Data and Remove Missing Values
Cities <- na.omit(avocados %>% filter(region %in% c(“Albany”, “Houston”, “Seattle”)))
# Make Price Numeric
avocados$AveragePrice <- as.numeric(avocados$AveragePrice)
# Test Assumptions
# Normality 
plotNormalHistogram(Cities$AveragePrice)
# The data seems to be normally distributes, therefor it will not need to be transformed and 
# we will proceed to use Bartletts test
# Homogeneity of Variance
bartlett.test(AveragePrice ~ region, data=Cities)
ANOVA <- lm(AveragePrice ~ region, data=Cities)
Anova(ANOVA, Type=“II”, white.adjust=TRUE)
pairwise.t.test(Cities$AveragePrice, Cities$region, p.adjust=“bonferroni”, pool.sd = FALSE)





