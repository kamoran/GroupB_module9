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
boxplot(msleep2$sleep_total ~ msleep2$vore, ylab = "Sleep Totals (hours)", 
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

## Produce the same with ggplot() with separate panels for conservation status,
## excluding species with incomplete data, including a trend line for each

## Add a natural log column for easier ggplot use
msleep$ln_wt <- log(msleep$bodywt)

head(msleep)
str(msleep)
summary(msleep)

## I found a new way to remove rows with any NAs in them, useful for this question
ms.alldata <- msleep[complete.cases(msleep), ]

ms.alldata
str(ms.alldata)
summary(ms.alldata) 
## checking that complete.cases worked correctly

ggplot(data = ms.alldata, aes(x = ln_wt, y = sleep_cycle, group = conservation)) +
  geom_point() +
  facet_wrap( ~ conservation) +
  stat_smooth(method="lm", se=F)

## The relationship between weight and sleep cycle length is positive in the first
## plot. This trend is the same for species within the domesticated and least concern
## conservation categories, though the domesticated relationship is much steeper
## than in the general plot and the least concern plot. For endangered, vulnerable,
## and near threatened species, there is not enough data to discern a trend. This
## would make sense because we have more abundant lc and domesticated animals to 
## observe, and domesticated animals span in size from small to pretty large, so 
## the trend makes sense. In this data the following are true:

dom <- msleep[msleep$conservation == "domesticated", ]
lc <- msleep[msleep$conservation == "lc", ]
max(dom$bodywt, na.rm = TRUE) #600 kg max domesticated weight
max(lc$bodywt, na.rm = TRUE) #85 kg max lc weight

## which also somewhat verifies the steepness of the trend in comparison. 

## How does the ratio of brain weight to body weight vary by diet type?
msleep3 <- msleep2[complete.cases(msleep2[ ,10:11]), ] ## removes NAs in bodywt and brainwt
summary(msleep3)

se <- function(x){ 
  
  sem <- sd(x)/sqrt(length(x))
  return(sem)
  }

se ## created function for standard error for use in next function

brain_body_ratio <- function(x, y, z){
  brain_body_mean <- as.data.frame(tapply(x/y, z, mean, na.rm = TRUE))
  brain_body_se <- as.data.frame(tapply(x/y, z, se))
  build <- cbind(brain_body_mean, brain_body_se)
  colnames(build) <- c("brain_body_mean", "brain_body_se")
  build$vore <- row.names(build)
  return(build)
  }
