# 0. Data Reading
PWD= read.table("C:/Users/ranam/PWD-our-project-(c)/PWD.RData")
View(PWD)

# 1. Descriptive statistics
# Summarize your data and calculate the following: 
#mean, median, minimum, maximum, first and third quartile (for each variable).
summary(PWD)

#For the categorical variable existing, calculate a frequency table 
# I will calculate the frequency table for the treatment, feeder and sex columns
table(PWD$Treatment)
table(PWD$Feeder)
table(PWD$Sex)

#Calculate the correlation coefficient (ADWG0021 and ADWG2150) 
#and (ADWG0021and ADWG0050)
cor(PWD$ADWG0021,PWD$ADWG2150)
cor(PWD$ADWG0021,PWD$ADWG0050)

#2. Graphics
#Generate a bar chart of a categorical variable for the gender (Sex parameter).
barplot(table(PWD$Sex), xlab="Sex",ylab="Frequency", col=c("yellow","red"))

#Generate a bar chart graph with the mean ADWG0021in  males and females 
barplot(tapply(PWD$ADWG0021,list(sex=PWD$Sex),mean,na.rm=T),
        xlab="Sex",ylab="Mean Average Daily Weight Gain", col=c("blue","pink"))

#Make a histogram of a continuous variable: “ADWG2150” as well as “ADWG0021”.
hist(PWD$ADWG2150,xlab="Average Daily Weight Gain between days 21 & 50",
     main="Distribution of Average Daily Weight Gain between days 21 & 50",
     col="#800020")

# Histogram of “ADWG0021”
hist(PWD$ADWG0021,xlab="Average Daily Weight Gain between days 0 & 21",
     main="Distribution of Average Daily Weight Gain between days 0 & 21",
     col="#D4AF37")

#Make a scatterplot of 2 continuous variables, 
#ADWG0050 and ADWG0021, and add the regression lines for each gender
plot(ADWG0050~ADWG0021, data=PWD) 
abline(lm(PWD$ADWG0050[PWD$Sex==1]~ PWD$ADWG0021[PWD$Sex==1]), col ="black" )
abline(lm(PWD$ADWG0050[PWD$Sex==2]~ PWD$ADWG0021[PWD$Sex==2]),col="#800080")

#Make a boxplot of ADWG0021 and separate boxplots per Treatment (as.factors)
boxplot(PWD$ADWG0021,main="Boxplot of ADWG", col="#008080")
boxplot(ADWG0021~Treatment, data = PWD, main ="ADWG0021 Per Treatment", 
        col=c("#FF8C00", "#FF69B4", "#32CD32","#DC143C","#4169E1"))

#3. Outlier Detection
#Explore the data for any existing outliers, identify them 
#(do NOT remove them if found).

# Select only numeric columns
numeric_cols <- sapply(PWD, is.numeric)

# Apply boxplot outlier detection to each numeric column
outliers_list <- lapply(PWD[, numeric_cols], 
                        function(x) boxplot(x, plot = FALSE)$out)

# View the outliers for each column
outliers_list

#What do you think?
#There are no outliers in the data

outliers0021<-boxplot(PWD$ADWG0021,plot=T)$out
outliers0021
outliers2150<-boxplot(PWD$ADWG2150,plot=T)$out
outliers2150
outliers0050<-boxplot(PWD$ADWG0050,plot=T)$out
outliers0050

#4. Check the normality using two methods
#ADWG0021
qqnorm(PWD$ADWG0021)
qqline(PWD$ADWG0021)
hist(PWD$ADWG0021)
shapiro.test(PWD$ADWG0021)
####### Data is normally distributed

#ADWG2150
qqnorm(PWD$ADWG2150)
qqline(PWD$ADWG2150)
hist(PWD$ADWG2150)
shapiro.test(PWD$ADWG2150)
####### Data is normally distributed

#ADWG0050
qqnorm(PWD$ADWG0050)
qqline(PWD$ADWG0050)
hist(PWD$ADWG0050)
shapiro.test(PWD$ADWG0050)
###### Data is normally distributed

#Check the homoscedasticity using two methods
#I will check the homoscedasticity of the 3 outcomes across the treatment groups
install.packages("car")
library("car")
PWD$Treatment<-factor(PWD$Treatment,labels=c('A','B','C','D','E'))
#ADWG0021
boxplot(ADWG0021~Treatment, data=PWD)
#The null and alternative hypothesis of levene test
#H0= The variences are equall across groups
#HA= At least one group has a different variance
leveneTest(ADWG0021~Treatment, data=PWD)
#Pvalue of 0.8, we fail to reject the null, so the variences are equall

##I will check the homoscedasticity using var.test
#of the 3 outcomes across the sex group
#H0= The variance is equall between the two groups
#HA= The variance is not equal
var.test(ADWG0021~Sex, data=PWD)
#pvalue= 0.4, fail to reject the null, variance is equal
#ADWG2150
boxplot(ADWG2150~Treatment, data=PWD)
leveneTest(ADWG2150~Treatment, data=PWD)
var.test(ADWG2150~Sex, data=PWD)
#Fail to reject the null, equal variance
#ADWG0050
boxplot(ADWG0050~Treatment, data=PWD)
leveneTest(ADWG0050~Treatment, data=PWD)
var.test(ADWG0050~Sex, data=PWD)
#Fail to reject the null, equal variance

#What do you think?
# The PWD data is normally distributed with equal variance

#5. Statistical Inference
#Calculate the 90%, 95%, and 99% confidence intervals 
#for the means of ADWG0021 per each gender.

#First I will calculate the mean of ADWG0021 per gender
#Calculate the mean of Males
mean_ADWG0021_male = mean_ADWG0021_male <- mean(PWD$ADWG0021[PWD$Sex == "1"], na.rm = TRUE)
mean_ADWG0021_male

#Calculate the mean of Females
mean_ADWG0021_female = mean_ADWG0021_male <- mean(PWD$ADWG0021[PWD$Sex == "2"], na.rm = TRUE)
mean_ADWG0021_female
#Then I will calculate the sd
#standard deviation for males
sd_ADWG0021_male <- sd(PWD$ADWG0021[PWD$Sex == "1"], na.rm = TRUE)
sd_ADWG0021_male

#Standard deviation for females
sd_ADWG0021_female <- sd(PWD$ADWG0021[PWD$Sex == "2"], na.rm = TRUE)
sd_ADWG0021_female

#For statistical analysis of the treatment effect
#the pen is the sample size unit
#meaning n = 8 per treatment and total n = 40
n=40
# 90% CI, for males, critical value = 1.64
Left.value_male<-mean_ADWG0021_male - 1.64*sd_ADWG0021_male/sqrt(n)
Right.value_male<-mean_ADWG0021_male + 1.64*sd_ADWG0021_male/sqrt(n)
Left.value_male
Right.value_male
# The 90% CI for males is within this range [137.62 - 146.68]

Left.value_female<-mean_ADWG0021_female - 1.64*sd_ADWG0021_female/sqrt(n)
Right.value_female<-mean_ADWG0021_female + 1.64*sd_ADWG0021_female/sqrt(n)
Left.value_female
Right.value_female
# The 90% CI for females is within this range [136.68 - 147.62]

# 95% males, critical value = 1.96
Left.value_male<-mean_ADWG0021_male - 1.96*sd_ADWG0021_male/sqrt(n)
Right.value_male<-mean_ADWG0021_male + 1.96*sd_ADWG0021_male/sqrt(n)
Left.value_male
Right.value_male
# The 95% CI for males is within this range [136.73 - 147.56]


# 95% females, critical value = 1.96
Left.value_female<-mean_ADWG0021_female - 1.96*sd_ADWG0021_female/sqrt(n)
Right.value_female<-mean_ADWG0021_female + 1.96*sd_ADWG0021_female/sqrt(n)
Left.value_female
Right.value_female
# The 95% CI for females is within this range [135.61 - 148.68]

# 99% males, critical value = 2.57
Left.value_male<-mean_ADWG0021_male - 2.57*sd_ADWG0021_male/sqrt(n)
Right.value_male<-mean_ADWG0021_male + 2.57*sd_ADWG0021_male/sqrt(n)
Left.value_male
Right.value_male
# The 99% CI for males is within this range [135.05 - 149.25]
Left.value_female<-mean_ADWG0021_female - 2.57*sd_ADWG0021_female/sqrt(n)
Right.value_female<-mean_ADWG0021_female + 2.57*sd_ADWG0021_female/sqrt(n)
Left.value_female
Right.value_female
# The 99% CI for females is within this range [133.58 - 150.72]

#How would you describe those inferences, 

# I would say, that I am 95% confident that the true value of the population 
#mean which is the true mean of the ADWG0021 in males lies within this range 
#[136.73 - 147.56]
# and our best estimate of the ADWG0021 in males is 142.15

#and what do you observe in terms of the interval width 
#when requesting higher confidence (i.e., 99% C.I.)?

# As the confidence increases, the precision decreases(the range is wider)
# so at 99% confidence, the range is wider than at 90% confidence



# 6. Hypothesis testing
# A. We hypothesize that ADWG0021is different between male vs female. 
#Assuming normality and homoscedasticity, 
#can you test this hypothesis using the statistical hypothesis framework


# Two groups, normally distributed data of equal variance
# The statistical test of choice is: (Parametric Two sample t-test)

install.packages("dunn.test")
install.packages("report")
install.packages("car")
install.packages("multcomp")
library(dunn.test)
library(report)
library(car)
library(multcomp)

t.test(ADWG0021~Sex, data=PWD, var.equal = TRUE)
# p-value: 0.7 not significant
# Fail to reject the null hypothesis,
# # There is no difference in the mean of ADWG0021 between males and females

# B. Assess whether the previous test assumptions have been met for the test.
#The independent two sample t-test assumptions are
# Normal distribution & equal variance

PWD$Sex<-factor(PWD$Sex,labels=c("male", "female"))
# To check the normality:
# Normality of ADWG0021 in males
qqnorm(PWD[PWD$Sex == "male",]$ADWG0021, main='Males ADWG0021')
qqline(PWD[PWD$Sex == "male",]$ADWG0021)
hist(PWD[PWD$Sex == "male",]$ADWG0021, main='Males ADWG0021')
shapiro.test(PWD[PWD$Sex == "male",]$ADWG0021)
# Shapiro P-value: 0.97 > not significant > fail to reject the null
# The QQplot and the box plot, the data appear to be approximately normally distributed
# The data is normally distributed

# Normality of ADWG0021 in females
qqnorm(PWD[PWD$Sex == "female",]$ADWG0021, main='Females ADWG0021')
qqline(PWD[PWD$Sex == "female",]$ADWG0021)
hist(PWD[PWD$Sex == "female",]$ADWG0021, main='Females ADWG0021')
shapiro.test(PWD[PWD$Sex == "female",]$ADWG0021)
# Shapiro P-value: 0.95 > not significant > fail to reject the null
# The QQplot and the box plot, the data appear to be approximately normally distributed
# The data is normally distributed

# To check Variance
boxplot(ADWG0021~Sex, data=PWD)
leveneTest(ADWG0021~Sex, data=PWD)
# P-value = 0.30 > Not significant > Fail to reject the null
# The two groups are of equal variance
var.test(ADWG0021~Sex, data=PWD)
# P-value = 0.41 > Not significant > Fail to reject the null
# The two groups are of equal variance

# So YES the previous test assumptions have been met

# C. We hypothesize that ADWG0021is “different” in the group receiving 
#Treatment A (normal feed + ZnO)  compared to Treatment B (normal feed + nutraceuticals). 
#Can you test this hypothesis assuming heteroscedasticity?

# Two independent groups
# Unequal variance assuming heteroscedasticity
# The statistical test of Choice is Welch’s t-test
PWD$Treatment<-factor(PWD$Treatment,labels=c("A", "B","C","D","E"))
t.test(ADWG0021 ~ Treatment,data = PWD, subset = Treatment %in% c("A", "B"))
# P-value: 0.02 > Significant > The ADWG0021 is different in groups recieving
# Treatment A compared to Treatment B

# D. Assess the previous test assumption
# I am going to assess the: Normality & Equal Variance
# ADWG0021 in ttt A is not normal in ttt B is not normal
# The two groups are of unequal variance
# So we should have chosen the Mann-Whitney U test (Wilcoxon Sum of Ranks) not the welch's

# Normality of ADWG0021 in Treatment A group
qqnorm(PWD[PWD$Treatment == "A",]$ADWG0021, main='Treatment ((A)) ADWG0021')
qqline(PWD[PWD$Treatment == "A",]$ADWG0021)
hist(PWD[PWD$Treatment == "A",]$ADWG0021, main='Treatment ((A)) ADWG0021')
shapiro.test(PWD[PWD$Treatment == "A",]$ADWG0021)
# Shapiro P-value is significant > Reject the null > Data is not normally distributed
#Checking the QQ plot and the histogram the data also 
# appears to be not normally distributed

# Normality of ADWG0021 in Treatment B group
qqnorm(PWD[PWD$Treatment == "B",]$ADWG0021, main='Treatment ((B)) ADWG0021')
qqline(PWD[PWD$Treatment == "B",]$ADWG0021)
hist(PWD[PWD$Treatment == "B",]$ADWG0021, main='Treatment ((B)) ADWG0021')
shapiro.test(PWD[PWD$Treatment == "B",]$ADWG0021)
# Shapiro p-value is not significant > fail to reject the null > data is normally distributed
# But because of the small sample size, I will not rely oh the Shapiro test,
# The histogram shows a clear Right Skewed data and my objective judgement
# is that the data is not normally distributed


# To check Variance
# Subset the data first
subtreatment <- droplevels(PWD[PWD$Treatment %in% c("A","B"), ])
View(subtreatment)
boxplot(ADWG0021 ~ Treatment, data = subtreatment ,col = c("#E9C46A", "#9B2226"))
leveneTest(ADWG0021~Treatment, data=subtreatment)
# Levene P-value = 0.62 > Not significant > Fail to reject the null
# The two groups are of equal variance
var.test(ADWG0021~Treatment, data=subtreatment)
# var.test P-value = 0.95 > Not significant > Fail to reject the null
# The two groups are of equal variance

# Because of the small sample size, I will not rely on the levene and the F-test p values
# The boxplot shows a clear difference in the variance,
# My objective judgement is that the data is of unequal variance

# E. We hypothesize that ADWG0021is different between the different Treatments. 
#Can you perform a comparison between the different groups, 
#after assessing the assumptions and performing post-hoc testing 
#(assuming normality and homoscedasticity)?

# We have 5 treatment groups
# So after checking the assumptions we will choose between
# ANOVE & Kruskal Wallis

install.packages("dunn.test")
install.packages("report")
install.packages("car")
install.packages("multcomp")
install.packages("ggstatsplot")
library(ggstatsplot)
library(dunn.test)
library(report)
library(car)
library(multcomp)
library(ggplot2)

## Check the equality of variances 
#Can not use var.test because we have more than 2 groups

plot(ADWG0021~Treatment,data=PWD,main="equal variances")

ggplot(PWD) +
  aes(x = Treatment, y = ADWG0021, color = Treatment) +
  geom_boxplot()


leveneTest(ADWG0021~Treatment,data=PWD)
# Levene P-value = 0.89 > Not significant > Fail to reject the null
# given the small sample size in each group (n=8), levene is considered unreliable
# especially that the boxplots shows clear difference in the variance between the groups
# especially treatment A
# My objective judgement is that the 5 groups are of unequal variance

#Check normality within each group
par(mfrow=c(1,1))
hist(PWD[PWD$Treatment=="A",]$ADWG0021,main="normality Treatment #A")
hist(PWD[PWD$Treatment=="B",]$ADWG0021,main="normality Treatment #B")
hist(PWD[PWD$Treatment=="C",]$ADWG0021,main="normality Treatment #C")
hist(PWD[PWD$Treatment=="D",]$ADWG0021,main="normality Treatment #D")
hist(PWD[PWD$Treatment=="E",]$ADWG0021,main="normality Treatment #E")

shapiro.test(PWD[PWD$Treatment == "A",]$ADWG0021)
shapiro.test(PWD[PWD$Treatment == "B",]$ADWG0021)
shapiro.test(PWD[PWD$Treatment == "C",]$ADWG0021)
shapiro.test(PWD[PWD$Treatment == "D",]$ADWG0021)
shapiro.test(PWD[PWD$Treatment == "E",]$ADWG0021)

# According to shapiro test, p-value of Treatment A group is significant
# so the data is not normally distributed
# According to the histograms, all groups appear to be right skewed,


# In case of multiple groups comparison, if one group is not normally distributed
# we chose the non parametric test
# So the statistical test of choice is: Kruskal wallis

kruskal.test(ADWG0021 ~ Treatment, data=PWD)
# P-value is not significant > Fail to reject the null hypothesis >
# there is no difference in the groups means

#Assuming normality and homoscedasticity, the statistical test of choice is:
#One Way Anova

AnovaModel <- aov(ADWG0021 ~ Treatment, data=PWD)
summary(AnovaModel)

##### Anova is not significant 
# and there is no point in doing a post-hoc test

hoc<-TukeyHSD(AnovaModel) 
hoc

# ttt B compared to ttt A is significant by Tukey, but this significance
# is not taken into account because the Anova is not significant.

#### DONE!

