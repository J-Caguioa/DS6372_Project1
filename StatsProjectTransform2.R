#The goal is to predict the rate of heart disease (per 100,000 individuals) 
#across the United States at the county-level from other socioeconomic indicators.

#Combine train values and train labels and remove the indicator column: train dataset
setwd('/Users/spencerfogelman/Desktop/Stats2/Project')
train_values = read.csv('/Users/spencerfogelman/Desktop/Stats2/Project/microsoft-data-science-capstone/Training_values.csv')
train_values = train_values[, -1]
train_labels = read.csv('/Users/spencerfogelman/Desktop/Stats2/Project/microsoft-data-science-capstone/Training_labels.csv')
train_labels= train_labels[, -1]
train = cbind(train_labels, train_values)

#Create train_year A dataset where we only have year A and removed the year column
train_yearA = train[train$yr == 'a',]
train_yearA = train[, -c(34)]

#Clean up the names
names(train_yearA) = sub("area__|econ__|demo__|health__", "", names(train_yearA))
names(train_yearA)

#Check for missing values
dim(train_yearA) #3198 obervations
summary(train_yearA)
num_na = apply(is.na.data.frame(train_yearA), 2, sum)
num_na = as.matrix(num_na)
num_na[order(num_na[,1]), ]
#Exploring Missing
pdf('miceplot.pdf')
library(VIM)
mice_plot <- aggr(train_yearA, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(train_yearA), cex.axis=.2,
                  gap=1, ylab=c("Missing data","Pattern"), cex.numbers=0.4,
                  cex.lab=0.6)
dev.off()
#Drop homicides_per_100k, pct_excessive_drinking: no information on why so many values missing
train_yearA = select(train_yearA, -c('homicides_per_100k', 'pct_excessive_drinking'))
names(train_yearA)
str(train_yearA)
#Imputation with median (will try to impute with more complicated algorithm later)
VarsImput = c('pct_adult_smoking', 'motor_vehicle_crash_deaths_per_100k', 'pop_per_dentist',
              'pop_per_primary_care_physician', 'pct_low_birthweight', 'air_pollution_particulate_matter',
              'pct_physical_inacticity', 'pct_diabetes', 'pct_adult_obesity', 'pct_asian',
              'pct_non_hispanic_african_american', 'pct_non_hispanic_white', 'pct_hispanic',
              'pct_below_18_years_of_age', 'pct_aged_65_years_and_older', 'pct_female', 'pct_uninsured_adults',
              'pct_uninsured_children', 'pct_american_indian_or_alaskan_native')


for (i in VarsImput){
  train_yearA[is.na(train_yearA[, i]), i] = median(train_yearA[, i], na.rm=TRUE)
}
apply(is.na.data.frame(train_yearA), 2, sum)              

##

#recode categorical variables
library(car)
head(train_yearA)
recoded = "
'Metro - Counties in metro areas of fewer than 250,000 population' = 1;
'Metro - Counties in metro areas of 1 million population or more' = 2;
'Nonmetro - Urban population of 2,500 to 19,999, adjacent to a metro area' = 3;
'Nonmetro - Urban population of 2,500 to 19,999, not adjacent to a metro area' = 4;
'Nonmetro - Urban population of 20,000 or more, adjacent to a metro area' = 5;
'Metro - Counties in metro areas of 250,000 to 1 million population' = 6;
'Nonmetro - Completely rural or less than 2,500 urban population, not adjacent to a metro area' = 7;
'Nonmetro - Completely rural or less than 2,500 urban population, adjacent to a metro area' = 8;
'Nonmetro - Urban population of 20,000 or more, not adjacent to a metro area' = 9
"
train_yearA$rucc = car::recode(train_yearA$rucc, recoded)

recoded2 = "
'Small-in a metro area with fewer than 1 million residents' = 1;
'Large-in a metro area with at least 1 million residents or more' = 2;
'Noncore adjacent to a small metro with town of at least 2,500 residents' = 3;
'Noncore not adjacent to a metro/micro area and contains a town of 2,500  or more residents' = 4;
'Micropolitan adjacent to a small metro area' = 5;
'Noncore adjacent to a large metro area' = 6;
'Micropolitan not adjacent to a metro area' = 7;
'Noncore not adjacent to a metro/micro area and does not contain a town of at least 2,500 residents' = 8;
'Noncore adjacent to micro area and does not contain a town of at least 2,500 residents' = 9;
'Micropolitan adjacent to a large metro area' = 10;
'Noncore adjacent to a small metro and does not contain a town of at least 2,500 residents' = 11;
'Noncore adjacent to micro area and contains a town of 2,500-19,999 residents' = 12
"
train_yearA$urban_influence = car::recode(train_yearA$urban_influence, recoded2)

#Create new categorical variable for two way anova called rucc_recoded 
train_yearA$rucc_recoded = NA


train_yearA$rucc_recoded[train_yearA$rucc %in% c(1,2,6)] = 'Metro'
train_yearA$rucc_recoded[!(train_yearA$rucc %in% c(1,2,6))] = 'Nonmetro'

train_yearA[, c('rucc', 'rucc_recoded')]

#Create scatterplots
library(ggplot2)
names(train_yearA)
scatter1 = ggplot(train_yearA, aes(y=train_labels, x=pct_civilian_labor)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE) ##Significant

scatter1_transform = ggplot(train_yearA, aes(y=log(train_labels), x=log(pct_civilian_labor))) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

scatter2 = ggplot(train_yearA, aes(y=train_labels, x=pct_unemployment)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE) ##Significant

scatter2_transform = ggplot(train_yearA, aes(y=log(train_labels), x=log(pct_unemployment))) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE) ##Significant

scatter3 = ggplot(train_yearA, aes(y=train_labels, x=pct_uninsured_adults)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE) ##Significant

scatter3_transform = ggplot(train_yearA, aes(y=log(train_labels), x=log(pct_uninsured_adults))) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

scatter4 = ggplot(train_yearA, aes(y=train_labels, x=pct_uninsured_children)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE) #No relationship

scatter4_transform = ggplot(train_yearA, aes(y=log(train_labels), x=pct_uninsured_children)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE) #No relationship

scatter5 = ggplot(train_yearA, aes(y=train_labels, x=pct_female)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE) #No relationship

scatter5_transform = ggplot(train_yearA, aes(y=log(train_labels), x=pct_female)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE) 

####Age
scatter6 = ggplot(train_yearA, aes(y=train_labels, x=pct_below_18_years_of_age)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE) #Significant

scatter6_transorm = ggplot(train_yearA, aes(y=log(train_labels), x=log(pct_below_18_years_of_age))) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE) #Significant

scatter7 = ggplot(train_yearA, aes(y=train_labels, x=pct_aged_65_years_and_older)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE) #No relationship

scatter7_transform = ggplot(train_yearA, aes(y=log(train_labels), x=pct_aged_65_years_and_older)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE) #No relationship

##Ethnicity
scatter8 = ggplot(train_yearA, aes(y=train_labels, x=pct_hispanic)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE) #No relationship

scatter8_transform = ggplot(train_yearA, aes(y=log(train_labels), x=pct_hispanic)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE) #No relationship

scatter9 = ggplot(train_yearA, aes(y=train_labels, x=pct_non_hispanic_african_american)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE) #Significant

scatter9_transform = ggplot(train_yearA, aes(y=log(train_labels), x=pct_non_hispanic_african_american)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE) #Significant

scatter10 = ggplot(train_yearA, aes(y=train_labels, x=pct_american_indian_or_alaskan_native)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE) #No relationship

scatter10_transform = ggplot(train_yearA, aes(y=log(train_labels), x=pct_american_indian_or_alaskan_native)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE) #No relationship

scatter11 = ggplot(train_yearA, aes(y=train_labels, x=pct_non_hispanic_white)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE) #Maybe significant

scatter11_transform = ggplot(train_yearA, aes(y=log(train_labels), x=pct_non_hispanic_white)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE) #Maybe significant

scatter12 = ggplot(train_yearA, aes(y=train_labels, x=pct_asian)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE) #Not significant

scatter12_transform = ggplot(train_yearA, aes(y=log(train_labels), x=log(pct_asian))) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE) #Not significant

####Education
scatter13 = ggplot(train_yearA, aes(y=train_labels, x=pct_adults_less_than_a_high_school_diploma)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE) #Significant

scatter13_transform = ggplot(train_yearA, aes(y=log(train_labels), x=log(pct_adults_less_than_a_high_school_diploma))) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE) #Siginificant. NEED TO TRANSFORM Y

scatter14 = ggplot(train_yearA, aes(y=train_labels, x=pct_adults_with_high_school_diploma)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

scatter14_transform = ggplot(train_yearA, aes(y=log(train_labels), x=pct_adults_with_high_school_diploma)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE) #Significant

scatter15 = ggplot(train_yearA, aes(y=train_labels, x=pct_adults_with_some_college)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

scatter15_transform = ggplot(train_yearA, aes(y=log(train_labels), x=pct_adults_with_some_college)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

scatter16 = ggplot(train_yearA, aes(y=train_labels, x=pct_adults_bachelors_or_higher)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

scatter16_transform = ggplot(train_yearA, aes(y=log(train_labels), x=log(pct_adults_bachelors_or_higher))) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

scatter16 = ggplot(train_yearA, aes(y=train_labels, x=pct_adults_bachelors_or_higher)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

scatter17 = ggplot(train_yearA, aes(y=train_labels, x=birth_rate_per_1k)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

scatter17_transform = ggplot(train_yearA, aes(y=log(train_labels), x=birth_rate_per_1k)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

scatter18 = ggplot(train_yearA, aes(y=train_labels, x=death_rate_per_1k)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

scatter18_transform = ggplot(train_yearA, aes(y=log(train_labels), x=death_rate_per_1k)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

scatter19 = ggplot(train_yearA, aes(y=train_labels, x=pct_adult_obesity)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

scatter19_transform = ggplot(train_yearA, aes(y=log(train_labels), x=pct_adult_obesity)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE) ##USE LOG

scatter20 = ggplot(train_yearA, aes(y=train_labels, x=pct_diabetes)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

scatter20_transform = ggplot(train_yearA, aes(y=log(train_labels), x=pct_diabetes)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

scatter21 = ggplot(train_yearA, aes(y=train_labels, x=pct_low_birthweight)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

scatter21_transform = ggplot(train_yearA, aes(y=log(train_labels), x=log(pct_low_birthweight))) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

scatter22 = ggplot(train_yearA, aes(y=train_labels, x=pct_physical_inacticity)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

scatter22_transform = ggplot(train_yearA, aes(y=log(train_labels), x=pct_physical_inacticity)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE) ##USE LOG

scatter23 = ggplot(train_yearA, aes(y=train_labels, x=air_pollution_particulate_matter)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

scatter23_transform = ggplot(train_yearA, aes(y=log(train_labels), x=air_pollution_particulate_matter)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

scatter24 = ggplot(train_yearA, aes(y=train_labels, x=motor_vehicle_crash_deaths_per_100k)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

scatter24_transform = ggplot(train_yearA, aes(y=log(train_labels), x=log(motor_vehicle_crash_deaths_per_100k))) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

scatter25 = ggplot(train_yearA, aes(y=train_labels, x=pop_per_dentist)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

scatter25_transform = ggplot(train_yearA, aes(y=log(train_labels), x=log(pop_per_dentist))) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

scatter26 = ggplot(train_yearA, aes(y=train_labels, x=pop_per_primary_care_physician)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

scatter26_transform = ggplot(train_yearA, aes(y=log(train_labels), x=log(pop_per_primary_care_physician))) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

#Combine scatterplots
library(ggpubr)
library(gridExtra)
before_scatter = marrangeGrob(list(scatter1, scatter2, scatter3, scatter4, scatter5,
                          scatter6, scatter7, scatter8, scatter9, scatter10,
                          scatter11, scatter12, scatter12, scatter14, scatter15,
                          scatter16, scatter17, scatter18, scatter19, scatter20,
                          scatter21, scatter22, scatter23, scatter24, scatter25,
                          scatter26), nrow=3, ncol=2)

ggexport(before_scatter, filename = "beforescatter.pdf")

after_scatter = marrangeGrob(list(scatter1_transform, scatter2_transform, scatter3_transform, scatter4_transform, scatter5_transform,
                                  scatter6_transorm, scatter7_transform, scatter8_transform, scatter9_transform, scatter10_transform,
                                  scatter11_transform, scatter12_transform, scatter12_transform, scatter14_transform, scatter15_transform,
                                  scatter16_transform, scatter17_transform, scatter18_transform, scatter19_transform, scatter20_transform,
                                  scatter21_transform, scatter22_transform, scatter23_transform, scatter24_transform, scatter25_transform,
                                  scatter26_transform), nrow=3, ncol=2)

ggexport(after_scatter, filename = "afterscatter.pdf")


#Create transformed dataset:take log transform of labels, 
#pop_per_primary_care_physician,pop_per_dentist, 
#motor_vehicle_crash_deaths_per_100k, pct_low_birthweight, 
#pct_adults_bachelors_or_higher,pct_adults_less_than_a_high_school_diploma,
#pct_asian, pct_below_18_years_of_age, pct_uninsured_adults,pct_unemployment,
#pct_civilian_labor 


TrainATransform = data.frame(train_yearA)
TrainATransform$loglabels = log(train_yearA$train_labels)
TrainATransform$log_pop_per_primary_care_physician = log(train_yearA$pop_per_primary_care_physician)
TrainATransform$log_pop_per_dentist = log(train_yearA$pop_per_dentist)
TrainATransform$log_motor_vehicle_crash_deaths_per_100k = log(train_yearA$motor_vehicle_crash_deaths_per_100k)
TrainATransform$log_pct_low_birthweight = log(train_yearA$pct_low_birthweight)
TrainATransform$log_pct_adults_bachelors_or_higher = log(train_yearA$pct_adults_bachelors_or_higher)
TrainATransform$log_pct_adults_less_than_a_high_school_diploma = log(train_yearA$pct_adults_less_than_a_high_school_diploma)
TrainATransform$log_pct_asian = log(train_yearA$pct_asian + 0.01) #Need to offset because contains 0
TrainATransform$log_pct_below_18_years_of_age = log(train_yearA$pct_below_18_years_of_age)
TrainATransform$log_pct_uninsured_adults = log(train_yearA$pct_uninsured_adults)
TrainATransform$log_pct_unemployment = log(train_yearA$pct_unemployment)
TrainATransform$log_pct_civilian_labor = log(train_yearA$pct_civilian_labor)

#Drop the previous labels
labels_to_drop = names(TrainATransform) %in% c('train_labels', 'pop_per_primary_care_physician', 'pop_per_dentist', 'motor_vehicle_crash_deaths_per_100k',
                                               'pct_low_birthweight', 'pct_adults_bachelors_or_higher', 'pct_adults_less_than_a_high_school_diploma',
                                               'pct_asian', 'pct_below_18_years_of_age', 'pct_uninsured_adults', 'pct_unemployment',
                                               'pct_civilian_labor')

TrainATransform = TrainATransform[, !labels_to_drop]
names(TrainATransform)
summary(TrainATransform)


#Create full model for MLR with no transform
first_model_transform = lm(loglabels ~ ., data=TrainATransform, singular.ok = TRUE)
vif(first_model) #Throws errors

#Check for linearly dependent colunmns and drop
alias(first_model_transform)
to_drop = names(TrainATransform) %in% c('urban_influence', 'rucc_recoded')
TrainATransform = TrainATransform[, !to_drop]

first_model_transform = lm(loglabels ~ ., data=TrainATransform, singular.ok = TRUE)
vif(first_model_transform)[,3]^2 

# Correlation plot on numeric variables. pairwise.complete.obs ignoring any
#pairs with NA.
library(tidyverse)
dfNumeric <- TrainATransform %>% keep(is.numeric)
library(corrplot)
pdf('corrplot.pdf')
corrplot(cor(dfNumeric, use="pairwise.complete.obs"), type='upper', tl.col = "black",
         tl.cex = 0.4, tl.srt = 45, method="color", addCoef.col="black", diag=FALSE,
         number.cex = 0.3, number.digits=2)
dev.off()

#corrplot looks good

#Patterns in residuals? No.
plot(first_model_transform$fitted.values,first_model_transform$residuals,xlab="Fitted Values",ylab="Residuals") 

#Residuals normally distributed? Yes.
qqnorm(first_model_transform$residuals)
qqline(first_model_transform$residuals)

#Selection method to reduce variables
library(leaps)
dim(TrainATransform)
reg.fwd=regsubsets(loglabels~.,data=TrainATransform,method="forward",nvmax=29)

#Look at BIC graph: minimum bic at 18 variables
set.seed(1234)
bics<-summary(reg.fwd)$bic
plot(1:29,bics,type="l",ylab="BIC",xlab="# of predictors")
index<-which(bics==min(bics)) #11 variables
points(index,bics[index],col="red",pch=10)

#50/50 Train and test split
set.seed(1234)
index=sample(1:dim(TrainATransform)[1],1599,replace=F)
train=TrainATransform[index,]
test = TrainATransform[-index,]

#Rerun forward selection
reg.fwd=regsubsets(loglabels~.,data=TrainATransform,method="forward",nvmax=11)

#Really handy predict function
predict.regsubsets =function (object , newdata ,id ,...){
  form=as.formula (object$call [[2]])
  mat=model.matrix(form ,newdata )
  coefi=coef(object ,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

testASE = c()
for (i in 1:11){
  predictions=predict.regsubsets(object=reg.fwd,newdata=test,id=i) 
  testASE[i]=(mean(test$loglabels-predictions))^2
}

#Find the minimum ASE: happens at 6 predictors
plot(1:18,testASE,type="l",xlab="# of predictors",ylab="test vs train ASE")
index=which(testASE==min(testASE))
points(index,testASE[index],col="red",pch=10)
rss=summary(reg.fwd)$rss
lines(1:18,rss/1599,lty=3,col="blue")  #Dividing by 1599 since ASE=RSS/sample size
#does not show up. 

#Run the final forward selection
reg.final=regsubsets(loglabels~.,data=TrainATransform,method="forward",nvmax=6)
coef(reg.fwd,6)

#Fit the final model
final.model<-lm(loglabels~ pct_aged_65_years_and_older+pct_adults_with_high_school_diploma+
                  death_rate_per_1k+pct_diabetes+pct_physical_inacticity+
                  log_pct_adults_less_than_a_high_school_diploma,
                data=TrainATransform)

summary(final.model)

#Look at predictions of final model: looks good
plot(final.model$fitted.values,TrainATransform$loglabels,xlab="Predicted",
     ylab="Heart Disease Death")

#Look at residuals to make sure assumptions are still met for hypothesis testing:
#Looks good
plot(final.model$fitted.values,final.model$residuals,
     xlab="Fitted Values",ylab="Residuals") 
qqnorm(final.model$residuals)
qqline(final.model$residuals)



#Two way ANOVA







