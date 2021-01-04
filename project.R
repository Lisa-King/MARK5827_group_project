#import packages
library(readr)
library(dplyr)
require(dplyr)
library(pastecs)
library(ggplot2)
library(extrafont) 
library(corrplot)
library(caret)
library(naniar)

#font_import()
#loadfonts(device = "win")

#import data
setwd("~/Desktop/MARK5827-Customer Analytics/assign/Materials for Group Research Project.-20201018")

bank_data <- read_csv("bank campaign.csv")

#view data
glimpse(bank_data, width = getOption("width"))
str(bank_data)

#attaching the dataframe to r
attach(bank_data)
#detach(bank_data)

#double to numeric
#bank_data$age = as.numeric(age)

#Getting the names of character variables
factors = bank_data[, sapply(bank_data, class) == 'character'] %>% colnames()
numeric = bank_data[, sapply(bank_data, class) == 'numeric'] %>% colnames()

#converting char to factors
bank_data[,factors] = lapply(bank_data[,factors], as.factor)

#viewing unique values
table(job) %>% as.data.frame()

summary(bank_data)


#Analysing numeric variables
stat.desc(bank_data[, sapply(bank_data, class) == 'numeric'],basic=F)

#plot all numeric variables into one graph;
par(mar=c(1,1,1,1))
boxplot(bank_data[, sapply(bank_data, class) == 'numeric'])

par(mar = c(5, 4, 4, 2))
boxplot(bank_data[,"balance"],
main = "User Balance",
xlab = "Dollars",
ylab = "Balance",
col = "orange",
border = "brown",
horizontal = TRUE,
notch = T,
las=1
)

##Exploratory Data analysis
boxplot(bank_data[,"age"],
main = "Customer age distribution",
xlab = "Age",
col = "orange",
border = "brown",
horizontal = TRUE,
notch = T,
las=1
)

#analysis for age variable, show mean and median value of age;
ggplot(bank_data, aes(x=age)) + 
  geom_histogram(binwidth=3,color="black", fill="white")+
  geom_vline(aes(xintercept=mean(age)),
            color="blue", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=quantile(age,c(.5))),
            color="red", linetype="dashed", size=1)

#show age with marital 
ggplot(bank_data, aes(x=age, color=marital, fill = marital)) +
  geom_histogram(alpha=0.5, position="identity")+
  scale_color_brewer(palette="Dark2")

str(bank_data)
#show age, balance, duration, campaign, pdays, previous and response;
ggplot(bank_data, aes(x=age))+
  geom_histogram(color="black", fill="white")+
  facet_wrap(~response,ncol=3)
ggplot(bank_data, aes(x=balance))+
  geom_histogram(color="black", fill="white")+
  facet_wrap(~response,ncol=3)
ggplot(bank_data, aes(x=duration))+
  geom_histogram(color="black", fill="white")+
  facet_wrap(~response,ncol=10)
ggplot(bank_data, aes(x=campaign))+
  geom_histogram(color="black", fill="white")+
  facet_wrap(~response,ncol=3)
ggplot(bank_data, aes(x=pdays))+
  geom_histogram(color="black", fill="white")+
  facet_wrap(~response,ncol=3)
ggplot(bank_data, aes(x=previous))+
  geom_histogram(color="black", fill="white")+
  facet_wrap(~response,ncol=3)

summary(poutcome)
summary(bank_data)

ggplot(bank_data, aes(x=age))+
  geom_histogram(color="black", fill="white")+
  facet_wrap(~contact,ncol=3)

#Age vs subscription
ggplot(bank_data, aes(x=age))+
  geom_histogram(color="black", fill="white")+
  facet_wrap(~response,ncol=3)

#Subscription based on number of contacts
ggplot(bank_data, aes(x=campaign, color=response, fill=response)) + 
  geom_histogram(alpha=0.9, position="identity", binwidth = 1) +
  theme(legend.title = element_blank())

ggplot(bank_data, aes(x=age, color=response, fill=response)) + 
  geom_histogram(alpha=0.9, position="identity", binwidth = 1) +
  theme(legend.title = element_blank())

ggplot(bank_data, aes(x=duration, color=response, fill=response)) + 
  geom_histogram(alpha=0.9, position="identity", binwidth = 1) +
  theme(legend.title = element_blank())

#converting factorials to numeric and creating a new dataframe
data = bank_data[,numeric]

conv = as.data.frame(data.matrix(bank_data[,factors]))

bank_data %>% group_by(response) %>% summarize(count=n())
conv %>% group_by(response) %>% summarize(count=n())

data = cbind(data,conv)

corrplot(cor(data, method = c("spearman")))

attach(bank_data)

balance[which(balance < 0)] <- 0

bank_data <- bank_data[bank_data$balance > 0, ]
summary(balance)

raw.df <- data.frame(age, job, marital, education,default,balance,housing,loan, contact, month,duration,campaign,pdays,previous, poutcome, response)
#raw.df <- data.frame(marital, education,balance,housing,loan, contact,duration,campaign,pdays,previous, poutcome, response)
raw.df <- raw.df%>%mutate(response = ifelse(response == "no",0,1))
raw.df <- raw.df%>%mutate(default = ifelse(default == "no",0,1))
raw.df <- raw.df%>%mutate(housing = ifelse(housing == "no",0,1))
raw.df <- raw.df%>%mutate(loan = ifelse(loan == "no",0,1))
dmy <- dummyVars(" ~ .", data = raw.df)
trsf <- data.frame(predict(dmy, newdata = raw.df))

#trsf[is.na(trsf)] <- 0

#cols <- c(2:21, 23:39,44:48)
#trsf[,cols] <- data.frame(apply(trsf[cols], 2, as.factor))
trsf <- trsf%>%mutate_if(is.numeric, as.integer)
summary(trsf)
cor(trsf)

detach(bank_data)
attach(trsf)

sum(response)

#install.packages("DescTools")
library(DescTools)
#install.packages("ResourceSelection")
library("ResourceSelection")
#install.packages("car")
library(car)

#using log() function to lower the difference of duration values;
#summary(balance)
#trsf <- trsf[trsf$balance <= 10000,]
#summary(balance)
ln_age <- log(trsf$age)
ln_balance <- log(trsf$balance)
ln_duration <- log(trsf$duration)
ln_campaign <- log(trsf$campaign)

# -----------------------------------------------------------
# Step 1: Define research goal
# -----------------------------------------------------------
# Identify customers who are most likely to respond to the direct mail campaign.

# -----------------------------------------------------------
# Step 2: Specify the model 
# This is task 3
# -----------------------------------------------------------
# The first model: removed age, job, month and unknow varibales, previous, pdays, trsf$maritaldivorce, 
# trsf$educationprimary,+trsf$balance, +trsf$default, +trsf$poutcomefailure +ln_balance +trsf$maritalsingle 
#+trsf$educationsecondary +ln_age
model1<-glm(trsf$response ~ trsf$marital.married
            +trsf$education.tertiary
            +trsf$housing+trsf$loan+trsf$contact.cellular+trsf$contact.telephone
            +trsf$poutcome.success+trsf$poutcome.other
            +ln_duration+ln_campaign,data=trsf, family=binomial, na.action = na.omit)

# Global model evaluation
anova(model1, test="Chisq")
#p_value<0.05, which means it's significant;

h1<-hoslem.test(response, fitted(model1))
h1
# Hosmer Lemeshow test whether the predicted values and the actual values are significantly different.
# Thus, we want the Hosmer Lemeshow Test to be INSIGNIFICANT (>.05)!
# Here it is significant so not ideal.

PseudoR2(model1, which = "Nagelkerke") 
# There is no meaningful R-Square godness of fit measure as it is in linear regression.
# It is always between 0 and 1. The higher the better.
summary(model1)
# We see that many model parameters are not significant
# This is not a problem per se but in our case we may have overspecified the model.
vif(model1)
# some VIFs are also a bit high (>5) 

Conf(model1, cutoff = 7/180)
exp(coef(model1))

#----------------------------------------------------------------------------------
#we remove pdays and previous since they have high correlation with poutcome;
#and we only keep poutcome which idecates outcome of the previous marketing campaign;
#And pdays and previous have high correlation with each other at the same time;

model0<-glm(trsf$response ~ trsf$marital.divorce+trsf$marital.married+trsf$education.primary+trsf$education.secondary
            +trsf$housing+trsf$loan+trsf$contact.cellular+trsf$contact.telephone+ln_duration+trsf$campaign
            +trsf$poutcome.failure+trsf$poutcome.other+trsf$poutcome.success,
            data=trsf, family=binomial, na.action = na.omit)

# The glm() function helps us to specify a generalized linear model.
# GLMs are a class of models that deal with non-normally distributed outcomes, e.g., yes or no.
# Since there are other forms of non normally-distributed outcomes than just binary ones,
# the glm function requires us to specify the family. Here it is binomial (i.e., just two values, response yes or no)
# Model specification is the same as in a standard linear regression.
# Note that we get an error message so we should take this model with caution.

anova(model0, test="Chisq")
#p_value<0.05, which means it's significant;
# Omnibus tests/ANOVA tests whether 
# the variance in a set of data explained by the model is significantly greater than 
# the unexplained variance.
# Thus, we want this test to be significant (p<.05).
# The test proceeds in a step wise manner, adding one of the independent variables
# in each step. We are only interested in the value in the last step.
# Here it is significant so ok.

h0<-hoslem.test(trsf$response, fitted(model0))
h0
#0.4847
# Hosmer Lemeshow test whether the predicted values and the actual values are significantly different.
# "respons" identifies the observed binary, "fitted"model" the predicted.
# The test partitions the data into groups and compares for each one 
# whether there are differences between predictions and observations.
# "g=10" is the default choice for the number of groups the test uses.
# For a good model performance we want them to be NOT different.
# Thus, we want the Hosmer Lemeshow Test to be INSIGNIFICANT (>.05)! 现在值不满足该条件；
# Here it is significant so not ideal.

PseudoR2(model0, which = "Nagelkerke")
#0.3925456
# There is no meaningful R-Square godness of fit measure as it is in linear regression.
# Thus we use Pseudo R-square measures. Here we choose Nagelkerke R-Square).
# It is always between 0 and 1. The higher the better.
# Here it could be better or worse. We could live with that.

# Local model evaluation, i.e., model parameters

summary(model0)
# We see that many model parameters are not significant
# This is not a problem per se but in our case we may have overspecified the model.

vif(model0)
# some VIFs are also a bit high (>5) 

# This model performance is the best one up to now;
#but the PseudoR2 is about 0.3925456, since we remove lots of variables, 
#we may can try to do more data mining work to improve the results

Conf(model0, cutoff = 7/180)
exp(coef(model0))

#----------------------------------------------------------------------------------
#we tried apply ln_campaign <- log(campaign) to model, the PseudoR2 value improve a little bit from 0.3925456 to 0.3931652;
#But the accuracy reduced a little bit from 0.58 to 0.5786; this model is an optional up to now;
model2<-glm(trsf$response ~ trsf$marital.divorced+trsf$marital.married +trsf$education.primary+trsf$education.secondary
            +trsf$housing+trsf$loan+trsf$contact.cellular+
            +trsf$contact.telephone+ln_duration+ln_campaign
#            +trsf$contact.telephone+ln_duration+trsf$campaign
            +trsf$poutcome.failure+trsf$poutcome.other+trsf$poutcome.success,
            data=trsf, family=binomial, na.action = na.omit)

anova(model2, test="Chisq")

h2<-hoslem.test(trsf$response, fitted(model2))
h2

PseudoR2(model2, which = "Nagelkerke")
summary(model2)

vif(model2)

Conf(model2, cutoff = 7/180)
exp(coef(model2))

#----------------------------------------------------------------------------------
#up to now this model has the highest accuracy and PseudoR2 value, but some variables have hight p_values;
#+trsf$monthapr  +trsf$jobblue.collar+trsf$jobadmin.+trsf$jobretired +trsf$jobmanagement +trsf$jobtechnician
#+trsf$monthjun+trsf$monthjul +trsf$poutcomeother +trsf$maritaldivorce +trsf$monthaug +trsf$poutcomefailure 
#+trsf$educationprimary +trsf$monthmay
model3<-glm(trsf$response ~ trsf$marital.divorced+trsf$marital.married
            +trsf$education.secondary++trsf$education.tertiary
            +trsf$housing+trsf$loan+trsf$contact.cellular+trsf$contact.telephone
            +ln_duration+ln_campaign+trsf$poutcome.success+trsf$poutcome.unknown,
            data=trsf, family=binomial, na.action = na.omit)

anova(model3, test="Chisq")

h3<-hoslem.test(trsf$response, fitted(model3))
h3

PseudoR2(model3, which = "Nagelkerke")
#0.4099205
summary(model3)

vif(model3)

Conf(model3, cutoff = 7/180)
#Accuracy : 0.5990, Sensitivity : 0.5527, Specificity : 0.9539
exp(coef(model3))

# -----------------------------------------------------------
#latest version with the best accuracy;
#+trsf$contact.unknown ,+trsf$education.secondary primary(no) +trsf$month.jun +trsf$marital.divorce 
#+ln_campaign +trsf$month.jan 
model10<-glm(trsf$response ~ trsf$poutcome.success+trsf$poutcome.other
            +trsf$contact.cellular+trsf$contact.telephone
            +trsf$marital.married+trsf$education.tertiary
            +trsf$month.aug+trsf$month.nov+trsf$month.jul+trsf$month.may
            +trsf$housing+trsf$loan+ln_duration,data=trsf, family=binomial, na.action = na.omit)

anova(model10, test="Chisq")

h10<-hoslem.test(trsf$response, fitted(model1))
h10

PseudoR2(model10, which = "Nagelkerke")
summary(model10)

vif(model10)

#confusion matrix
Conf(model10, cutoff = 7/180)
exp(coef(model10))


