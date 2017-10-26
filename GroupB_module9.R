# NR995 Module 9 problem set
# Using GitHub, plots, functions
# Erica Holme, Katie Moran, students
# Last modified: Oct 23 2017

## load mammal sleep dataset through ggplot 2 package
library(ggplot2)
data(msleep)
?msleep
head(msleep)
str(msleep)
summary(msleep)

## determine number of diet types in msleep
unique(msleep$vore)
length(unique(msleep$vore))
# there are 4 diet types (carni, omni, herbi, and insecti) as well as NAs

# visually determine whether sleep totals vary by diet type
msleep2 <- msleep[!is.na(msleep$vore), ] #remove rows with no "vore" data
unique(msleep2$vore) # no longer NAs in vore column
boxplot(msleep$sleep_total ~ msleep$vore, ylab = "Sleep Totals (hours)", 
        xlab = "Diet Type")

# The insect eating mammals appear to sleep more than the other other diet types,
# although there is overlap between the plots and error bars, indicating that the
# results are not significant. Carnivores, herbivores, and omnivores sleep about
# the same amount on average, although there is wide variation between the ranges.
# Omnivores exhibit less variation in total sleep hours between species, but there
# are some observations well above the normal range (4 "outliers" to investigate)

# Create plot of natural log of body weight and sleep cycle
plot(log(msleep$bodywt[msleep$sleep_cycle != "NA"]), 
     msleep$sleep_cycle[msleep$sleep_cycle != "NA"], ylab = "Sleep Cycle (hrs)",
     xlab = "Body Weight (kg)")

