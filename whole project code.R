# 0. Data Reading
PWD= read.table("C:/Users/ranam/stat/PWD-our-project-(c)")
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

View(PWD)
# 7. Linear model
# a. Fit a linear regression to the data and interpret the 
# regression coefficient (for one of the hypotheses mentioned above)

### I will Fit two multiple linear regression model with ADWG0021 as outcome variable.
# and W0 as the regressor. 
simple.regression <- lm(ADWG0021 ~ W0, data=PWD)
summary(simple.regression)

#Model explanation: when W0 is increased by 1 unit, the ADWG0021 is changed
# by 0.64 units. However, the W0 explanation of ADWG0021 is not 
# statistically significant as the p.value is bigger than alpha.
# The Q1 & Q3 of the residuals are not equal so this is an
# indication that the residuals are not normally distributed.
# R^2 of the model is 0.07, indicating that the W0 only explains 7% of 
# the changes in ADWG0021 and it is also not a significant explanation
# because of the high p.value

#b. Calculate and interpret a 95% confidence interval 
#of the regression slope (bonus)

# extract slope and its standard error
beta_W0 <- coef(summary(simple.regression))["W0", "Estimate"]
beta_W0
se_W0   <- coef(summary(simple.regression))["W0", "Std. Error"]
se_W0

# then we apply this formula
# CI= Beta+- 1.96*SE
# 95% confidence interval
Left.value_W0  <- beta_W0 - 1.96 * se_W0
Right.value_W0 <- beta_W0 + 1.96 * se_W0
Left.value_W0
Right.value_W0

# The confidence interval includes zero, which means we are 95% confident
# that the population parameter lies within this range which includes zero
# indicating that it is not significant which is consistant with 
# the model results.

# C. Estimating the average ADWG0021 change with changing the gender from 1 to 2 (bonus).
# Since the sex variable is categorized as numerical in the dataset
# I will fit another linear regression model to include the gender
model2 <- lm(ADWG0021 ~ Sex, data=PWD)
summary(model2)
str(PWD)
#Explanation:
#Even though the residuals seems somewhat normally distributed
# but the change in sex from 1 to 2, ecplains the decrease in ADWG0021 
# by 1.92 units however this explanation is not statistically significant
# because of the very large p.value
# # Th e R^2 is 0.00257, which means that the change in sex 
# from 1 to 2 only captures 0.25 of the variability in the ADWG0021
# and it is not statistically significant p.value larger than alpha