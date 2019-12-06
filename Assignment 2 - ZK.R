########### Assignment 2 - ZK  ################


clean_data=data_sample_1 # create a new data file

# to correct the value as I did in assigmnet 1
clean_data <- data_sample_1 %>% mutate(
  household_income = as.numeric(replace(household_income, household_income == "-4562",45620)),
  STAI_trait = as.numeric(replace(STAI_trait, STAI_trait == "3.5", 35)))

clean_data %>% summary()
describe(clean_data)

#create a new model
mod_new<-lm(pain~age+sex+STAI_trait+pain_cat+mindfulness+cortisol_serum+weight+household_income+IQ, data = clean_data)
print(mod_new)

############### Run basic diagnostics as I did in assignment 1 #####################
# cooks difference#

cooks.distance(model = mod_new)

plot(x = mod_new, which = 4)
plot(x = mod_new, which = 5)

### Normality ###

# QQ plot
mod_new %>% 	
  plot(which = 2)

# histogram		
residuals_mod_new = enframe(residuals(mod_new))	
residuals_mod_new %>% 	
  ggplot() +	
  aes(x = value) +	
  geom_histogram()	

# skew and kurtosis	
describe(residuals(mod_new))	


### Linearity ###

mod_new %>% 	
  residualPlots()


### Homoscedasticty	###
mod_new %>% 	
  plot(which = 3)	

mod_new %>% 	
  ncvTest() # NCV test	

mod_new %>% 	
  bptest() # Breush-Pagan test	


### collinearity ###

mod_new %>% 	
  vif()	


## the linearity was violated ##
fig_1 = clean_data%>% 	
  ggplot() +	
  aes(y = pain, x = pain_cat) +	
  geom_boxplot()+	
  ylim(c(20, 45))

clean_data %>%ggplot() +aes(y = pain, x = pain_cat) +geom_point(size = 3) +geom_line()

anova_model = aov(pain ~ pain_cat, data = clean_data)	
sum_aov = summary(anova_model)	
sum_aov	
#the test is sig

#linear regression model with pain_cat as a predictor of pain	
mod_4 = lm(pain ~ pain_cat, data =  clean_data)	
summary(mod_4)	

# I used a scatterplot to see if i could get an overview
clean_data %>% 	
  ggplot() +	
  aes(y = pain, x = pain_cat) +	
  geom_point()	
 	

### Higher order ranks ###
mod_5 = lm(pain ~ age+sex+STAI_trait+mindfulness+cortisol_serum+weight+household_income+IQ+pain_cat + I(pain_cat^2), data =  clean_data)	
summary(mod_5)	

AIC(mod_4)	
AIC(mod_5)

mod_6= lm(pain ~ age+sex+STAI_trait+mindfulness+cortisol_serum+weight+household_income+IQ+pain_cat + I(pain_cat^2)+ I(pain_cat^3), data =  clean_data)

AIC(mod_5)
AIC(mod_6)

### Re-run of the diagnistic after Higher order rank on mod_5 ###
### Normality ###

# QQ plot
mod_5 %>% 	
  plot(which = 2)

# histogram		
residuals_mod_5 = enframe(residuals(mod_5))	
residuals_mod_5 %>% 	
  ggplot() +	
  aes(x = value) +	
  geom_histogram()	

# skew and kurtosis	
describe(residuals(mod_5))	


### Linearity ###

mod_5 %>% 	
  residualPlots()


### Homoscedasticty	###
mod_5 %>% 	
  plot(which = 3)	

mod_5 %>% 	
  ncvTest() # NCV test	

mod_5 %>% 	
  bptest() # Breush-Pagan test	


### collinearity ###

mod_5 %>% 	
  vif()	

############# removing outliers #############
clean_data_nooutliers = clean_data

clean_data_nooutliers = clean_data %>% slice(-c(55,74,88))

mod3.3.3<-lm(pain~age+sex+STAI_trait+pain_cat+mindfulness+cortisol_serum+weight+household_income+IQ, data = clean_data)
print(mod3.3.3)

############ backward model ################


step(object = mod3, direction = "backward")

#skapar en backward model med de variabler som var kvar i 
new_mod3_backward<-lm( pain ~ age + sex + pain_cat + mindfulness + cortisol_serum + weight, data = clean_data)

######## ANOVA ############
anova(mod3,new_mod3_backward)

AIC(mod3)
AIC(new_mod3_backward)

summary(new_mod3_backward)

### table backward model###
sm=summary(new_mod3_backward)
sm
sm_table

sm_p_values = as.character(round(sm$coefficients[,4], 3))	
sm_p_values[sm_p_values != "0" & sm_p_values != "1"] = substr(sm_p_values[sm_p_values != "0" & sm_p_values != "1"], 2, nchar(sm_p_values[sm_p_values != "0" & sm_p_values != "1"]))	
sm_p_values[sm_p_values == "0"] = "<.001"	

sm_table = cbind(as.data.frame(round(cbind(coef(new_mod3_backward), confint(new_mod3_backward), c(0, lm.beta(new_mod3_backward)$standardized.coefficients[c(2,3)])), 2)), sm_p_values)	
names(sm_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")	
sm_table["(Intercept)","Std.Beta"] = "0"

sm_table


######### NEW DATA ############
data_sample_2 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_2.csv")
view(data_sample_2)

summary(data_sample_2)
describe(data_sample_2)

## predict model using data sample 2 ###
predict(mod3, data_sample_2)

	

