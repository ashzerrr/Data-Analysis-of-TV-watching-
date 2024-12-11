library(tidyverse)
library(Hmisc)
library(AICcmodavg)
library(car)
#tv <- read.csv("tv_watching.csv", colClasses = c("factor", "numeric", "numeric", "factor", "factor", "factor"))

# Load data
zodiac <- read.table("zodiac (1).txt", header = TRUE, sep = "\t")

#observed
observed <- zodiac$Births

# Expected
expected<- zodiac$Expected

# Perform Chi-square goodness-of-fit test
 chisq.test(observed, p = expected/sum(expected))

# View the result
print(chisq_test)


# Part 2


#Load data
tv <- read.csv("tv_watching.csv", colClasses = c("factor", "numeric", "numeric", "factor", "factor", "factor"))


#create boxplots
boxplot( Sq.Rt.TV~ Gender:Athlete,
        data = tv,
        main = "TVhours Distribution by Group",
        xlab = "Group",
        ylab = "Tvhours",
        col = "steelblue",
        border = "black",
        las = 2
)

#Use of transformed data

#Run both models

##Additive
Sq.Rt.TV_add <- aov(Sq.Rt.TV ~ Gender + Athlete,data=tv)
summary(Sq.Rt.TV_add)

##With Interaction
Sq.Rt.TV_int <- aov(Sq.Rt.TV ~ Gender * Athlete, data=tv)
summary(Sq.Rt.TV_int)


# Compare model output
model_set <- list(Sq.Rt.TV_add, Sq.Rt.TV_int)
model_names <- c("TV Hours Additive", "TV Hours Interactive")
aictab(model_set, model_names)  # Compare the models using AICc

summary(Sq.Rt.TV_int)

TukeyHSD(Sq.Rt.TV_int)

# assess for assumptions

plot(Sq.Rt.TV_add)
plot(Sq.Rt.TV_int)

# Checking for Normality

# Histogram
hist(tv$Sq.Rt.TV, main = "Histogram of TV Hours", xlab = "TV Hours", col = "steelblue")

#shapiro test
shapiro.test(tv$Sq.Rt.TV)

# Levenes test
leveneTest(Sq.Rt.TV ~ Gender * Athlete, data = tv)

#define model residuals
# Define model residuals
resid_add <- residuals(Sq.Rt.TV_add)
resid_int <- residuals(Sq.Rt.TV_int)

# Create histogram of residuals
hist(resid_add, main = "Histogram of Residuals", xlab = "Residuals", col = "steelblue")
hist(resid_int, main = "Histogram of Residuals", xlab = "Residuals", col = "red")




