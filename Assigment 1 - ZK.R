########## Assignment 1 ZK ##############

rm(list=ls(all=TRUE))

library(lsr)
library(psych)
library(dplyr) # for data management
library(gsheet) # to read data from google sheets
library(ggplot2) # for ggplot
library(sciplot)
library(car)
library(lmtest)
library(tidyverse) # for tidy code
library(gridExtra)
library(influence.ME)
library(lm.beta)
library(psych) # for describe	
library(gsheet) # to read data from google sheets	
library(tidyverse) # for tidy code	

setwd("~/Desktop/PSYP13")

data_sample_1 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_1.csv")

## check the dataset ##
View(data_sample_1)
describe(data_sample_1)
summary(data_sample_1)

table(data_sample_1$sex)
table(data_sample_1$age)
table(data_sample_1$pain)
table(data_sample_1$STAI_trait)

# scatterplot
plot(pain ~ age, data = data_sample_1)

#create a new data file
clean_data=data_sample_1 

# fixed the data
clean_data <- data_sample_1 %>% mutate(
household_income = as.numeric(replace(household_income, household_income == "-4562",45620)),
STAI_trait = as.numeric(replace(STAI_trait, STAI_trait == "3.5", 35)))

clean_data %>% summary() #
describe(clean_data)

table(clean_data$household_income)
table(clean_data$STAI_trait)

clean_data %>% 	
  ggplot() +	
  aes(x = pain) +	
  geom_histogram()

clean_data %>% 	
  ggplot() +	
  aes(x = age) +	
  geom_histogram()

clean_data %>% 	
  ggplot() +	
  aes(x = household_income) +	
  geom_histogram()

clean_data %>% 	
  ggplot() +	
  aes(y = pain, x = sex) +	
  geom_point()

#### simple regression model

mod1.1<-lm(pain~age+sex,data=clean_data) 
print(mod1.1)

mod2.2<-lm(pain~age+sex+STAI_trait+pain_cat+mindfulness+cortisol_serum+cortisol_saliva, data = clean_data)
print(mod2.2)

summary(mod1.1) 
summary(mod2.2)

confint(mod1.1)
confint(mod2.2)

# scatterplots 
plot(pain~age+sex, data = clean_data)
plot(pain~age+sex+STAI_trait+pain_cat+mindfulness+cortisol_serum+cortisol_saliva, data = clean_data)

plot1 = clean_data %>% 	
  ggplot() +	
  aes(x = age+sex, y = pain) +	
  geom_point()+	
  geom_smooth(method = "lm")

plot2 = clean_data %>% 	
  ggplot() +	
  aes(x = age+sex+STAI_trait+pain_cat+mindfulness+cortisol_serum+cortisol_saliva, y = pain) +	
  geom_point()+	
  geom_smooth(method = "lm")
print(plot2)

grid.arrange(plot1, plot2, nrow = 1)
###############################################################################


# # Dealing with outliers	
# ## Identifying extreme cases
clean_data %>% 	
  ggplot() +	
  aes(x = age+sex+STAI_trait+pain_cat+mindfulness+cortisol_serum+cortisol_saliva, y = pain) +	
  geom_point()	

# ## Identifying extreme cases with high leverage	
clean_data %>% 	
  ggplot() +	
  aes(x = age+sex+STAI_trait+pain_cat+mindfulness+cortisol_serum+cortisol_saliva y = pain) +	
  geom_point() +	
  geom_smooth(method = "lm")	

mod2.2 %>% 	
  plot(which = 5)	

mod2.2 %>% 	
  plot(which = 4)
  
### cook's difference ###
cooks.distance(model = mod1.1)
cooks.distance(model= mod2.2)

plot(cooks.distance(model = mod1.1))
plot(cooks.distance(model = mod2.2))

plot(x=mod1.1)
plot(x=mod2.2)

hist( x = residuals( mod1.1 ),xlab = "Value of residual", main = "",breaks = 20)
hist( x = residuals( mod2.2 ),xlab = "Value of residual", main = "",breaks = 20)

plot(x = mod1.1, which = 4)
plot(x = mod1.1, which = 5)

plot(x = mod2.2, which = 4)
plot(x = mod2.2, which = 5)


### Assumptions of linear regression ###	

### Normality ###

# QQ plot
mod2.2 %>% 	
  plot(which = 2)

# histogram		
residuals_mod2.2 = enframe(residuals(mod2.2))	
residuals_mod2.2 %>% 	
  ggplot() +	
  aes(x = value) +	
  geom_histogram()	

# skew and kurtosis	
describe(residuals(mod2.2))	


### Linearity ###

mod2.2 %>% 	
  residualPlots()

### Homoscedasticty	###
mod2.2 %>% 	
  plot(which = 3)	

mod2.2 %>% 	
  ncvTest() # NCV test	

mod2.2 %>% 	
  bptest() # Breush-Pagan test	


### collinearity ###

mod2.2 %>% 	
  vif()	

###################################################################################
mod2.2_cortisol = lm(pain ~ cortisol_saliva + cortisol_serum, data = clean_data)	
summary(mod2.2_cortisol)	

mod2.2_cortisol %>% 	
  vif()	

mod2.2_coritsol_inter = lm(pain ~ cortisol_saliva * cortisol_serum, data = clean_data)	

summary(mod2.2_coritsol_inter)	
mod2.2_coritsol_inter %>% 	
  vif()	

##handeling data mullticollinearity##

mod3.3<-lm(pain~cortisol_saliva,cortisol_serum, data= clean_data)

clean_data %>% 	
  select(pain, cortisol_saliva, cortisol_serum) %>% 	
  pairs.panels(col = "red", lm = T)

summary(mod2.2)
summary(mod3.3)
####################################################################################

# removing cortisol_serum to help with the violated collinerity assumotion
mod3<-lm(pain~age+sex+STAI_trait+pain_cat+mindfulness+cortisol_saliva, data = clean_data)
print(mod3)

mod3 %>% 	
  vif()	
###############################################################################################

###### Diagnostic Re-run on model 3 ######################
### Normality ###

# QQ plot
mod3 %>% 	
  plot(which = 2)

# histogram		
residuals_mod3 = enframe(residuals(mod3))	
residuals_mod3 %>% 	
  ggplot() +	
  aes(x = value) +	
  geom_histogram()	

# skew and kurtosis	
describe(residuals(mod3))	


### Linearity ###

mod3 %>% 	
  residualPlots()

### Homoscedasticty	###
mod3 %>% 	
  plot(which = 3)	

mod3 %>% 	
  ncvTest() # NCV test	

mod3 %>% 	
  bptest() # Breush-Pagan test	


### collinearity ###

mod3 %>% 	
  vif()	

##################### Compare the two model with ANOVA ###########################

summary(mod1.1) # vet ej om jag ska ha detta
summary(mod3)

confint(mod1.1)
confint(mod3)

anova(mod1.1,mod3)

summary(mod1.1)
summary(mod3)

AIC(mod1.1)
AIC(mod3)


############################################################################################

# predicting new data #

clean_data2=clean_data # create a new data file
mydata_cleaned2 <- data_sample_1 %>% mutate (sex = as.character(as.numeric(replace(sex, sex == "male", 1))))

newdata_to_predict = as.data.frame(cbind(pain, sex))
predicted_price = predict(mod_house1, newdata = newdata_to_predict)	

###Scatterplots###
plot(pain ~ age+sex+STAI_trait+pain_cat+mindfulness+cortisol_serum+cortisol_saliva, data = clean_data)




######################################################################

## model checking: three kinds of residuals ##
residuals(object = mod1.1)
residuals(object = mod2.2)

rstandard(model = mod1.1)
rstandard(model = mod2.2)

rstudent(model = mod1.1)
rstudent(model=mod2.2)

## Normality of residuals ##
hist( x = residuals( mod1.1 ),xlab = "Value of residual",main = "",breaks = 20)
hist( x = residuals( mod2.2 ),xlab = "Value of residual",main = "",breaks = 20)


## checking the homogenity of variance ##

ncvTest( mod1.1 )
ncvTest( mod2.2 )

coeftest( mod1.1, vcov= hccm )
coeftest( mod2.2, vcov= hccm )

## Checking for collinerity ##

vif( mod = mod1.1 )
vif( mod = mod2.2)

## checking for linearity of the relationship ##
plot(x = mod1.1, which = 1)
plot(x = mod2.2, which = 1)

residualPlots( model = mod1.1 )
residualPlots( model = mod2.2 )

openGraph()
source("GraphPlot.R")
graphics.off()

#########################################################################

clean_data %>% ggplot() + aes(x = age, y = pain) + geom_point() + geom_smooth(method = "lm")
clean_data %>% ggplot() + aes(x= sex, y= pain) + geom_point() + geom_smooth(method="lm")
clean_data %>% ggplot() + aes(x = STAI_trait, y = pain) + geom_point() + geom_smooth(method = "lm")
clean_data %>% ggplot() + aes(x = pain_cat, y = pain) + geom_point() + geom_smooth(method = "lm")
clean_data %>% ggplot() + aes(x = mindfulness, y = pain) + geom_point() + geom_smooth(method = "lm")
clean_data %>% ggplot() + aes(x = cortisol_serum, y = pain) + geom_point() + geom_smooth(method = "lm")
clean_data %>% ggplot() + aes(x = cortisol_saliva, y = pain) + geom_point() + geom_smooth(method = "lm")
  ################# no outliers ################

plot(x = mod2.2, which = 4)
plot(x = mod2.2, which = 5)

###### Remowing the outliers ########

data_nooutliers = clean_data

data_nooutliers = clean_data %>% slice(-c(74,88,123))

mod3_nooutliers<-lm(pain~age+sex+STAI_trait+pain_cat+mindfulness+cortisol_serum+cortisol_saliva, data = data_nooutliers)
print(mod3_nooutliers)

######## re run of the diagnostics with no outliers #########
### Normality ###

# QQ plot
mod3_nooutliers %>% 	
  plot(which = 2)

# histogram		
residuals_mod3_nooutliers = enframe(residuals(mod3_nooutliers))	
residuals_mod3_nooutliers %>% 	
  ggplot() +	
  aes(x = value) +	
  geom_histogram()	

# skew and kurtosis	
describe(residuals(mod3_nooutliers))	


### Linearity ###

mod3_nooutliers %>% 	
  residualPlots()

### Homoscedasticty	###
mod3_nooutliers %>% 	
  plot(which = 3)	

mod3_nooutliers %>% 	
  ncvTest() # NCV test	

mod3_nooutliers %>% 	
  bptest() # Breush-Pagan test	


### collinearity ###

mod3_nooutliers %>% 	
  vif()	


confint(mod1.1)
lm.beta(mod1.1)

######## summary table #########

sm=summary(mod1.1)
sm
sm_table

sm_p_values = as.character(round(sm$coefficients[,4], 3))	
sm_p_values[sm_p_values != "0" & sm_p_values != "1"] = substr(sm_p_values[sm_p_values != "0" & sm_p_values != "1"], 2, nchar(sm_p_values[sm_p_values != "0" & sm_p_values != "1"]))	
sm_p_values[sm_p_values == "0"] = "<.001"	

sm_table = cbind(as.data.frame(round(cbind(coef(mod1.1), confint(mod1.1), c(0, lm.beta(mod1.1)$standardized.coefficients[c(2,3)])), 2)), sm_p_values)	
names(sm_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")	
sm_table["(Intercept)","Std.Beta"] = "0"

sm_table


sm=summary(mod3)
sm
sm_table

sm_p_values = as.character(round(sm$coefficients[,4], 3))	
sm_p_values[sm_p_values != "0" & sm_p_values != "1"] = substr(sm_p_values[sm_p_values != "0" & sm_p_values != "1"], 2, nchar(sm_p_values[sm_p_values != "0" & sm_p_values != "1"]))	
sm_p_values[sm_p_values == "0"] = "<.001"	

sm_table = cbind(as.data.frame(round(cbind(coef(mod3), confint(mod3), c(0, lm.beta(mod3)$standardized.coefficients[c(2,3)])), 2)), sm_p_values)	
names(sm_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")	
sm_table["(Intercept)","Std.Beta"] = "0"

sm_table


### Predictions ###
## Residual sum of square ##
RSS = sum((clean_data$pain - predict(mod3))^2)	
RSS	


mod_mean <- lm(pain ~ 1, data = clean_data)	

## Total sum of square
TSS = sum((clean_data$pain - predict(mod_mean))^2)	
TSS	

## R^2
R2 = 1-(RSS/TSS)	
R2	



