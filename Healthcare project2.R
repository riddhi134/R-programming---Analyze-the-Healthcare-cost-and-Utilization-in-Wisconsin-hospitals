
hope<-read.csv('Project/Projects for Submission/Healthcare/Healthcare/HospitalCosts.csv',header=TRUE)
head(hope)
names(hope)

#1. Record patient statistics:
  #The agency wants to find the age category of people who frequently visit the 
  #hospital and has the maximum expenditure.

  #Age: Age of the patient discharged
  #Totchg: Hospital discharge costs

summary(hope)

#Get number of hospital visits based on age
summary(as.factor(hope$AGE))

#We can see that infants (year is 0-1) have the maximum frequency of visiting hospital
#Plot the graph
hist(hope$AGE,main='Histogram of AGe group and their frequency of visiting',
     xlab='Age group',boarder = 'Black',col = c('light green','dark green',
                                                xlim=c(0,20),ylim=c(0,350)))

#Summarize expenditure based on age group
ExpenseBasedOnAGE<-aggregate(TOTCHG ~ AGE,FUN = sum,data= hope)
ExpenseBasedOnAGE

#Get the maximum expense and its age group
which.max(tapply(ExpenseBasedOnAGE$AGE,ExpenseBasedOnAGE$TOTCHG,FUN = sum))

#plot the graph
barplot(
  tapply(ExpenseBasedOnAGE$TOTCHG, ExpenseBasedOnAGE$AGE, FUN = sum),
  main = 'Barplot of Age wise maximum expense',
  xlab = 'Age group',
  col = c('light blue', 'blue')
)
#Maximum expenditure for 0-1 yr is 678118


#2. Diagnosis-related group that has maximum hospitalization and expenditure
# In order of severity of the diagnosis and treatments and to find out the expensive treatments, 
  #the agency wants to find the diagnosis-related group that has maximum hospitalization and expenditure.
  #Aprdrg: All Patient Refined Diagnosis Related Groups
  #Totchg: Hospital discharge costs

summary(as.factor(hope$APRDRG))

#Get the diagnosis-related group and its hospitalization expenditure
DiagnosisCost = aggregate(TOTCHG ~ APRDRG, FUN = sum, data = hope)

#Get the max diagnostic cost
DiagnosisCost[which.max(DiagnosisCost$TOTCHG),]
#As can be seen here 640 diagnosis related group had a max cost of 437978

#3. Race vs Hospitalization costs
#To make sure that there is no malpractice, the agency needs to analyze 
#if the race of the patient is related to the hospitalization costs.
summary(as.factor(hope$RACE))
  #there is 1 null value which needs to remove
hope=na.omit(hope)
summary(as.factor(hope$RACE))

#As can be seen 484 patients out of 499 fall under group 1, showing that the number
#of observations for 1 category is way higher than others - hence data is skewed.
#This will only affect the results from linear regression or ANOVA analysis

#Linear regression
raceInfluence=lm(TOTCHG ~ RACE,data=hope)
summary(raceInfluence)
  #pValue is 0.69 it is much higher than 0.5
  #We can say that race doesn’t affect the hospitalization costs

#Anaysis using ANOVA
#We can also use anova statistical test for estimating how dependent variable, 
#in this case RACE, affects the independent variable, the hospitalization cost
raceInfluence<-aov(TOTCHG ~ RACE,data=hope)
summary(raceInfluence)
  #The residual variance (deviation from original) (of all other variables) is very high. 
  #This implies that there is very little influence from RACE on hospitalization costs
  #As can be seen, the degree of freedom (Df) for RACE is 1 and that of residuals is 497 observations
  #The F-Value, the test statistic is 0.16 which is much less than 0.5 showing that RACE doesn’t affect teh hospitalization cost.
  #The Pr(>F), the p_value of 0.69 is high confirming that RACE does not affect hospitalization cost.

#4. To properly utilize the costs, the agency has to analyze the severity of the 
#hospital costs by age and gender for the proper allocation of resources.
library(ggplot2)
library(dplyr)

# Calculate total cost by age and gender
cost_summary <- hope %>%
  group_by(AGE, FEMALE) %>%
  summarize(total_cost = sum(TOTCHG), groups = "drop")

#Visualize the severity of hospital costs using a bar plot:
ggplot(cost_summary, aes(x = AGE, y = total_cost, fill = FEMALE)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Hospital Costs by Age and Gender",
       x = "Age Group",
       y = "Total Cost",
       fill = "FEMALE")


gender_distribution <- table(hope$AGE, hope$FEMALE)
gender_distribution

ageGenderInflModel<-lm(formula = TOTCHG ~ AGE + FEMALE, data = HospitalCosts)
summary(ageGenderInflModel)
#Since the pValues of AGE is much lesser than 0.05, the ideal statistical significance level, 
#and it also has three stars (***) next to it, it means AGE has the most statitical significance
#Similarly, gender is also less than 0.05.
#Hence, we can conclude that the model is statistically significant

# 5. Since the length of stay is the crucial factor for inpatients,
#the agency wants to find if the length of stay can be predicted from age, gender, and race.
summary(ageGenderInflModel)
#The p-value is higher than 0.05 for age, gender and race, indicating there is no linear relationship
#between these variables and length of stay.
#Hence, age, gender and race cannot be used to predict the length of stay of inpatients.

#6. Complete analysis
#The agency wants to find the variable that mainly affects hospital costs.
#Significance method - build a model using all independent variables vs dependent variable
hospitalCostModel<-lm(formula = TOTCHG ~ ., data = hope)
summary(hospitalCostModel)
  #As it is apparent from the coefficient values, Age, Length of stay (LOS) and
  #patient refined diagnosis related groups(APRDRG) have three stars (***) next to it.
  #So they are the ones with statistical significance
  #Also, RACE is the least significant. build a model after removing RACE

hcm1<-lm(formula = TOTCHG ~ AGE + FEMALE + LOS + APRDRG, data = hope)
summary(hcm1)

hcm2<-lm(formula = TOTCHG ~ AGE + LOS + APRDRG, data = hope)
summary(hcm2)

hcm3<-lm(formula = TOTCHG ~ AGE + LOS , data = hope)
summary(hcm3)


# Compare the models using ANOVA
anova_result <- anova(hcm1, hcm2, hcm3,hospitalCostModel)

# Print the ANOVA table
print(anova_result)
  #Removing Race and gender doesn’t change the R2 value. It doesn’t impact cost
  #Removing APRDRG in model hcm3 increases the standard error. Hence model hcm2 seems to be better.

#Analysis Conclusion:
#As is evident in the multiple models above, health care costs is dependent on age, 
#length of stay and the diagnosis type.
#Healthcare cost is the most for patients in the 0-1 yrs age group category
#Maximum expenditure for 0-1 yr is 678118
#Length of Stay increases the hospital cost
#All Patient Refined Diagnosis Related Groups also affects healthcare costs
#640 diagnosis related group had a max cost of 437978
#Race or gender doesn’t have that much impact on hospital cost


