###################################################### DATA PREPARATION  ######################################################
# libraries ----
library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(ROCR)
library(corrplot)
library(dplyr)
library(lubridate)


# Loading Files to data frame ----
general_data <- read.csv("general_data.csv", stringsAsFactors=FALSE)
employee_data <- read.csv("employee_survey_data.csv", stringsAsFactors=FALSE)
manager_data <- read.csv("manager_survey_data.csv", stringsAsFactors=FALSE)
in_time <- read.csv("in_time.csv")
out_time <- read.csv("out_time.csv")


# Checking the structure of the dataframes
str(general_data)
str(employee_data)
str(manager_data)
str(in_time)
str(out_time)

# Finding out the length of dataframe
length(unique(employee_data$EmployeeID))
length(unique(general_data$EmployeeID))
length(unique(in_time$X))
length(manager_data$EmployeeID)
length(out_time$X)
#length is 4410 for all data sets 
#This confirms EmployeeID is the key

setdiff(employee_data$EmployeeID,general_data$EmployeeID)
setdiff(employee_data$EmployeeID,in_time$X)
setdiff(employee_data$EmployeeID,manager_data$EmployeeID)
setdiff(employee_data$EmployeeID,out_time$X)
#Identical EmployeeIDs throughout these datasets


#Merging data frames and creating a master data frame
employee <- merge(employee_data,general_data,by = "EmployeeID")
employee <- merge(employee,manager_data,by = "EmployeeID")

names(in_time)[1] = paste("EmployeeID")
names(out_time)[1] = paste("EmployeeID")


#Removing columns having whole columns as NAs
in_time <- in_time[, which(sapply(in_time, function(x) sum(is.na(x))) !=4410)]
out_time <- out_time[, which(sapply(out_time, function(x) sum(is.na(x))) !=4410)]

#USing Lubridate package, subtracting the in_time and out_time to find average working hours of employees
in_time[,-1] <- sapply(in_time[,-1], ymd_hms)
out_time[,-1] <- sapply(out_time[,-1], ymd_hms) 
work_hours <- (out_time[,-1] - in_time[,-1])/3600
emp.hrs <- data.frame(EmployeeID=in_time$EmployeeID, 
                      Avg.hrs = round(rowMeans(work_hours, na.rm = T),2))
employee <- merge(employee,emp.hrs, by = "EmployeeID")


View(employee)  #master file



###################################################### Exploratory Data Analysis ####################################################

str(employee) 
#4410 obs. of 551 variables

#deleting unecessary columns 
#since these have only one level for the entire row we cannot use for any of our analoysis
employee <- employee[ , -which(names(employee) %in% c("StandardHours","Over18","EmployeeCount"))]

#attrition is the dependent variable


#Univariate & Bivariate Analysis 
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")

plot_grid(ggplot(employee,aes(x= EnvironmentSatisfaction, fill = Attrition))+ geom_bar(),
          ggplot(employee,aes(x= JobSatisfaction,fill = Attrition))+geom_bar(),
          ggplot(employee,aes(x= WorkLifeBalance,fill = Attrition))+ geom_bar()
          +bar_theme1,align = "h")

plot_grid(ggplot(employee,aes(x= BusinessTravel,fill = Attrition))+geom_bar(),
          ggplot(employee,aes(x= Department,fill = Attrition))+ geom_bar(),
          ggplot(employee,aes(x = Education,fill = Attrition))+ geom_bar(),
          ggplot(employee,aes(x = EducationField,fill = Attrition))+ geom_bar()
          +bar_theme1,align = "h")

plot_grid(ggplot(employee,aes(x = Gender,fill = Attrition))+ geom_bar(),
          ggplot(employee,aes(x = JobLevel,fill = Attrition))+ geom_bar(),
          ggplot(employee,aes(x = JobRole,fill = Attrition))+ geom_bar(),
          ggplot(employee,aes(x = MaritalStatus,fill = Attrition))+ geom_bar()+bar_theme1,align = "h")

# Histogram and Boxplots for numeric variables
box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

plot_grid(ggplot(employee, aes(x = Age,fill = Attrition))+ geom_histogram(binwidth = 10),
          ggplot(employee, aes(x="",y=Age))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(employee, aes(x = MonthlyIncome,fill = Attrition))+ geom_histogram(binwidth = 10000),
          ggplot(employee, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
#outliers exists

plot_grid(ggplot(employee, aes(x = Age,fill = Attrition))+ geom_histogram(binwidth = 3),
          ggplot(employee, aes(x="",y=Age))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
#Attrition decreases as age increases, no attrition at 57-60 bin

plot_grid(ggplot(employee, aes(x = DistanceFromHome,fill = Attrition))+ geom_histogram(binwidth = 5),
          ggplot(employee, aes(x="",y=DistanceFromHome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)


plot_grid(ggplot(employee, aes(x = NumCompaniesWorked,fill = Attrition))+ geom_histogram(binwidth = 1),
          ggplot(employee, aes(x="",y=NumCompaniesWorked))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)


plot_grid(ggplot(employee, aes(x = TotalWorkingYears,fill = Attrition))+ geom_histogram(binwidth = 10),
          ggplot(employee, aes(x="",y=TotalWorkingYears))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
#there are outliers

#standard hours, Employee count,over18 can be dropped since it has just one level


#checking for multicollinearity using correlation plot

corrplot(cor(employee[,c(2:5,9:10,13,16:27)]), tl.cex = 0.9, mar = c(1,0,0,0))
#Obesrved that multicollinearity exists between few variables.



####################################DATA CLEANING FOR MODEL BUILDING #####################################


# Bringing the variables in the correct format
# converting to factors as required
employee$EmployeeID <- factor(employee$EmployeeID)
employee$Attrition <- factor(employee$Attrition)  #dependent vaariable
employee$EnvironmentSatisfaction <- factor(employee$EnvironmentSatisfaction)
employee$JobSatisfaction <- factor(employee$JobSatisfaction)
employee$WorkLifeBalance <- factor(employee$WorkLifeBalance)
employee$BusinessTravel <- factor(employee$BusinessTravel)
employee$Department <- factor(employee$Department)
employee$Education <- factor(employee$Education)
employee$EducationField <- factor(employee$EducationField)
employee$Gender <- factor(employee$Gender)
employee$JobLevel <- factor(employee$JobLevel)
employee$JobRole <- factor(employee$JobRole)
employee$MaritalStatus <- factor(employee$MaritalStatus)
employee$NumCompaniesWorked <- factor(employee$NumCompaniesWorked)
employee$StockOptionLevel <- factor(employee$StockOptionLevel)
employee$TrainingTimesLastYear <- factor(employee$TrainingTimesLastYear)
employee$JobInvolvement <- factor(employee$JobInvolvement)
employee$PerformanceRating <- factor(employee$PerformanceRating)



# Missing value treatment
sapply(employee, function(x) sum(is.na(x)))
#total of 111 values are missing which we can afford to loose since this is only 2% of the data
employee <- employee[!is.na(employee$EnvironmentSatisfaction),]
employee <- employee[!is.na(employee$JobSatisfaction),]
employee <- employee[!is.na(employee$WorkLifeBalance),]
employee <- employee[!is.na(employee$NumCompaniesWorked),]
employee <- employee[!is.na(employee$TotalWorkingYears),]
#with this all missing values except for intime and out time are removed


#Outlier treatment ----
quantile(employee$Age,seq(0,1,0.01)) #no treatment required

quantile(employee$MonthlyIncome,seq(0,1,0.01))
employee$MonthlyIncome[which(employee$MonthlyIncome >178560)]<- 178560 
# giving a decent number for outliers at 95th percentile

quantile(employee$PercentSalaryHike,seq(0,1,0.01))  #no treatment required

quantile(employee$TotalWorkingYears,seq(0,1,0.01))
employee$TotalWorkingYears[which(employee$TotalWorkingYears >32)]<- 32
# giving a decent number for outliers at 98th percentile

quantile(employee$YearsAtCompany,seq(0,1,0.01))
employee$YearsAtCompany[which(employee$YearsAtCompany >22)]<- 22

quantile(employee$YearsSinceLastPromotion,seq(0,1,0.01))
employee$YearsSinceLastPromotion[which(employee$YearsSinceLastPromotion >9)]<- 9

quantile(employee$YearsWithCurrManager,seq(0,1,0.01))
employee$YearsWithCurrManager[which(employee$YearsWithCurrManager >10)]<- 10

quantile(employee$DistanceFromHome,seq(0,1,0.01))
quantile(employee$Avg.hrs,seq(0,1,0.01))
#no outlier treatment required


# Feature standardisation
#Scaling continous data
employee$Age <- scale(employee$Age)
employee$MonthlyIncome <- scale(employee$MonthlyIncome)
employee$PercentSalaryHike <- scale(employee$PercentSalaryHike)
employee$TotalWorkingYears <- scale(employee$TotalWorkingYears)
employee$YearsAtCompany <- scale(employee$YearsAtCompany)
employee$YearsSinceLastPromotion <- scale(employee$YearsSinceLastPromotion)
employee$YearsWithCurrManager <- scale(employee$YearsWithCurrManager)
employee$DistanceFromHome <- scale(employee$DistanceFromHome)
employee$Avg.hrs <- scale(employee$Avg.hrs)

# converting target variable employee$attrition from No/Yes character to factorwith levels 0/1
employee$Attrition <- ifelse(employee$Attrition=="Yes",1,0)

# Checking attrition rate of employees
attrition <- sum(employee$Attrition)/nrow(employee)
attrition # 16.16% attrition rate.


#DUMMY VARAIBLE CREATION 
#long to wide data conversion ----
dummy <- model.matrix(~JobSatisfaction,data = employee)
dummy <- dummy[,-1]
employee <- cbind(employee,dummy)

dummy <- model.matrix(~EnvironmentSatisfaction,data = employee)
dummy <- dummy[,-1]
employee <- cbind(employee,dummy)

dummy <- model.matrix(~WorkLifeBalance,data = employee)
dummy <- dummy[,-1]
employee <- cbind(employee,dummy)

dummy <- model.matrix(~BusinessTravel,data = employee)
dummy <- dummy[,-1]
employee <- cbind(employee,dummy)

dummy <- model.matrix(~Department,data = employee)
dummy <- dummy[,-1]
employee <- cbind(employee,dummy)

dummy <- model.matrix(~Education,data = employee)
dummy <- dummy[,-1]
employee <- cbind(employee,dummy)

dummy <- model.matrix(~EducationField,data = employee)
dummy <- dummy[,-1]
employee <- cbind(employee,dummy)

levels(employee$Gender)<-c(1,0)

dummy <- model.matrix(~JobLevel,data = employee)
dummy <- dummy[,-1]
employee <- cbind(employee,dummy)

dummy <- model.matrix(~JobRole,data = employee)
dummy <- dummy[,-1]
employee <- cbind(employee,dummy)

dummy <- model.matrix(~MaritalStatus,data = employee)
dummy <- dummy[,-1]
employee <- cbind(employee,dummy)

dummy <- model.matrix(~StockOptionLevel,data = employee)
dummy <- dummy[,-1]
employee <- cbind(employee,dummy)


dummy <- model.matrix(~PerformanceRating,data = employee)
dummy <- dummy[,-1]
employee <- cbind(employee,dummy)



dummy <- model.matrix(~NumCompaniesWorked,data = employee)
dummy <- dummy[,-1]
employee <- cbind(employee,dummy)

dummy <- model.matrix(~TrainingTimesLastYear,data = employee)
dummy <- dummy[,-1]
employee <- cbind(employee,dummy)

dummy <- model.matrix(~JobInvolvement,data = employee)
dummy <- dummy[,-1]
employee <- cbind(employee,dummy)

employee <- employee[ , -which(names(employee) %in% c("Education","EducationField","JobLevel",
                                                      "BusinessTravel","JobSatisfaction","EnvironmentSatisfaction","WorkLifeBalance","Department",
                                                      "NumCompaniesWorked","JobInvolvement","TrainingTimesLastYear","JobRole","MaritalStatus","StockOptionLevel","PerformanceRating"))]

#removing EmployeeID since its not required for analysis
employee <- employee[,-1]


# splitting the data between train and test ----
set.seed(100)

indices = sample.split(employee$Attrition, SplitRatio = 0.7)

train = employee[indices,]

test = employee[!(indices),]



###################################################### MODEL BUILDING  ######################################################
# Logistic Regression: ----

#Initial model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) 


# Stepwise selection
model_2<- stepAIC(model_1, direction="both")
summary(model_2)
vif(model_2)
#removing YearsAtCompany having vif 4.736473 and p value 0.097764


model_3 <- glm(formula = Attrition ~ Age + Gender + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Avg.hrs + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 + 
                 EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                 WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
                 BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                 `DepartmentResearch & Development` + DepartmentSales + Education5 + 
                 JobLevel2 + JobLevel5 + `JobRoleHuman Resources` + JobRoleManager + 
                 `JobRoleManufacturing Director` + `JobRoleResearch Director` + 
                 `JobRoleSales Executive` + MaritalStatusMarried + MaritalStatusSingle + 
                 StockOptionLevel1 + NumCompaniesWorked1 + NumCompaniesWorked4 + 
                 NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + 
                 NumCompaniesWorked8 + NumCompaniesWorked9 + TrainingTimesLastYear1 + 
                 TrainingTimesLastYear4 + TrainingTimesLastYear5 + TrainingTimesLastYear6 + 
                 JobInvolvement3, family = "binomial", data = train)

summary(model_3)
vif(model_3)
#BusinessTravelTravel_Rarely is removed since it has VIF 3.54 and p value is significantly high at 0.008829


model_4 <- glm(formula = Attrition ~ Age + Gender + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Avg.hrs + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 + 
                 EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                 WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
                 BusinessTravelTravel_Frequently +  
                 `DepartmentResearch & Development` + DepartmentSales + Education5 + 
                 JobLevel2 + JobLevel5 + `JobRoleHuman Resources` + JobRoleManager + 
                 `JobRoleManufacturing Director` + `JobRoleResearch Director` + 
                 `JobRoleSales Executive` + MaritalStatusMarried + MaritalStatusSingle + 
                 StockOptionLevel1 + NumCompaniesWorked1 + NumCompaniesWorked4 + 
                 NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + 
                 NumCompaniesWorked8 + NumCompaniesWorked9 + TrainingTimesLastYear1 + 
                 TrainingTimesLastYear4 + TrainingTimesLastYear5 + TrainingTimesLastYear6 + 
                 JobInvolvement3, family = "binomial", data = train)
summary(model_4)
vif(model_4)
# After checking the VIF values, we find the p-values to be significant and hence start checking for insignificant p-values.
#removing Gender because of high Pvalue


model_5 <- glm(formula = Attrition ~ Age + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Avg.hrs + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 + 
                 EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                 WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
                 BusinessTravelTravel_Frequently +  
                 `DepartmentResearch & Development` + DepartmentSales + Education5 + 
                 JobLevel2 + JobLevel5 + `JobRoleHuman Resources` + JobRoleManager + 
                 `JobRoleManufacturing Director` + `JobRoleResearch Director` + 
                 `JobRoleSales Executive` + MaritalStatusMarried + MaritalStatusSingle + 
                 StockOptionLevel1 + NumCompaniesWorked1 + NumCompaniesWorked4 + 
                 NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + 
                 NumCompaniesWorked8 + NumCompaniesWorked9 + TrainingTimesLastYear1 + 
                 TrainingTimesLastYear4 + TrainingTimesLastYear5 + TrainingTimesLastYear6 + 
                 JobInvolvement3, family = "binomial", data = train)
summary(model_5)
#removing MaritalStatusMarried because of high p value 


model_6 <- glm(formula = Attrition ~ Age + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Avg.hrs + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 + 
                 EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                 WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
                 BusinessTravelTravel_Frequently +  
                 `DepartmentResearch & Development` + DepartmentSales + Education5 + 
                 JobLevel2 + JobLevel5 + `JobRoleHuman Resources` + JobRoleManager + 
                 `JobRoleManufacturing Director` + `JobRoleResearch Director` + 
                 `JobRoleSales Executive`  + MaritalStatusSingle + 
                 StockOptionLevel1 + NumCompaniesWorked1 + NumCompaniesWorked4 + 
                 NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + 
                 NumCompaniesWorked8 + NumCompaniesWorked9 + TrainingTimesLastYear1 + 
                 TrainingTimesLastYear4 + TrainingTimesLastYear5 + TrainingTimesLastYear6 + 
                 JobInvolvement3, family = "binomial", data = train)
summary(model_6)
#removing NumCompaniesWorked4 because of high p value 


model_7 <- glm(formula = Attrition ~ Age + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Avg.hrs + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 + 
                 EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                 WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
                 BusinessTravelTravel_Frequently +  
                 `DepartmentResearch & Development` + DepartmentSales + Education5 + 
                 JobLevel2 + JobLevel5 + `JobRoleHuman Resources` + JobRoleManager + 
                 `JobRoleManufacturing Director` + `JobRoleResearch Director` + 
                 `JobRoleSales Executive`  + MaritalStatusSingle + 
                 StockOptionLevel1 + NumCompaniesWorked1 + 
                 NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + 
                 NumCompaniesWorked8 + NumCompaniesWorked9 + TrainingTimesLastYear1 + 
                 TrainingTimesLastYear4 + TrainingTimesLastYear5 + TrainingTimesLastYear6 + 
                 JobInvolvement3, family = "binomial", data = train)
summary(model_7)
#removing Education5 because of high p Value


model_8 <- glm(formula = Attrition ~ Age + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Avg.hrs + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 + 
                 EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                 WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
                 BusinessTravelTravel_Frequently +  
                 `DepartmentResearch & Development` + DepartmentSales +  
                 JobLevel2 + JobLevel5 + `JobRoleHuman Resources` + JobRoleManager + 
                 `JobRoleManufacturing Director` + `JobRoleResearch Director` + 
                 `JobRoleSales Executive`  + MaritalStatusSingle + 
                 StockOptionLevel1 + NumCompaniesWorked1 + 
                 NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + 
                 NumCompaniesWorked8 + NumCompaniesWorked9 + TrainingTimesLastYear1 + 
                 TrainingTimesLastYear4 + TrainingTimesLastYear5 + TrainingTimesLastYear6 + 
                 JobInvolvement3, family = "binomial", data = train)
summary(model_8)
#removing NumCompaniesWorked8 because of high p value


model_9 <- glm(formula = Attrition ~ Age + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Avg.hrs + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 + 
                 EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                 WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
                 BusinessTravelTravel_Frequently +  
                 `DepartmentResearch & Development` + DepartmentSales +  
                 JobLevel2 + JobLevel5 + `JobRoleHuman Resources` + JobRoleManager + 
                 `JobRoleManufacturing Director` + `JobRoleResearch Director` + 
                 `JobRoleSales Executive`  + MaritalStatusSingle + 
                 StockOptionLevel1 + NumCompaniesWorked1 + 
                 NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + 
                  NumCompaniesWorked9 + TrainingTimesLastYear1 + 
                 TrainingTimesLastYear4 + TrainingTimesLastYear5 + TrainingTimesLastYear6 + 
                 JobInvolvement3, family = "binomial", data = train)
summary(model_9)
#removing StockOptionLevel1 


model_10 <- glm(formula = Attrition ~ Age + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Avg.hrs + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 + 
                 EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                 WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
                 BusinessTravelTravel_Frequently +  
                 `DepartmentResearch & Development` + DepartmentSales +  
                 JobLevel2 + JobLevel5 + `JobRoleHuman Resources` + JobRoleManager + 
                 `JobRoleManufacturing Director` + `JobRoleResearch Director` + 
                 `JobRoleSales Executive`  + MaritalStatusSingle + 
                  NumCompaniesWorked1 + 
                 NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + 
                 NumCompaniesWorked9 + TrainingTimesLastYear1 + 
                 TrainingTimesLastYear4 + TrainingTimesLastYear5 + TrainingTimesLastYear6 + 
                 JobInvolvement3, family = "binomial", data = train)
summary(model_10)
#removing `JobRoleSales Executive`  


model_11 <- glm(formula = Attrition ~ Age + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Avg.hrs + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 + 
                  EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
                  BusinessTravelTravel_Frequently +  
                  `DepartmentResearch & Development` + DepartmentSales +  
                  JobLevel2 + JobLevel5 + `JobRoleHuman Resources` + JobRoleManager + 
                  `JobRoleManufacturing Director` + `JobRoleResearch Director` + 
                   MaritalStatusSingle + 
                  NumCompaniesWorked1 + 
                  NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + 
                  NumCompaniesWorked9 + TrainingTimesLastYear1 + 
                  TrainingTimesLastYear4 + TrainingTimesLastYear5 + TrainingTimesLastYear6 + 
                  JobInvolvement3, family = "binomial", data = train)
summary(model_11)
#removing JobLevel 2


model_12 <- glm(formula = Attrition ~ Age + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Avg.hrs + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 + 
                  EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
                  BusinessTravelTravel_Frequently +  
                  `DepartmentResearch & Development` + DepartmentSales +  
                   JobLevel5 + `JobRoleHuman Resources` + JobRoleManager + 
                  `JobRoleManufacturing Director` + `JobRoleResearch Director` + 
                  MaritalStatusSingle + 
                  NumCompaniesWorked1 + 
                  NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + 
                  NumCompaniesWorked9 + TrainingTimesLastYear1 + 
                  TrainingTimesLastYear4 + TrainingTimesLastYear5 + TrainingTimesLastYear6 + 
                  JobInvolvement3, family = "binomial", data = train)
summary(model_12)
#removing TrainingTimesLastYear1 


model_13 <- glm(formula = Attrition ~ Age + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Avg.hrs + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 + 
                  EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
                  BusinessTravelTravel_Frequently +  
                  `DepartmentResearch & Development` + DepartmentSales +  
                  JobLevel5 + `JobRoleHuman Resources` + JobRoleManager + 
                  `JobRoleManufacturing Director` + `JobRoleResearch Director` + 
                  MaritalStatusSingle + 
                  NumCompaniesWorked1 + 
                  NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + 
                  NumCompaniesWorked9 +  
                  TrainingTimesLastYear4 + TrainingTimesLastYear5 + TrainingTimesLastYear6 + 
                  JobInvolvement3, family = "binomial", data = train)
summary(model_13)
#removing TrainingTimesLastYear4 


model_14 <- glm(formula = Attrition ~ Age + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Avg.hrs + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 + 
                  EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
                  BusinessTravelTravel_Frequently +  
                  `DepartmentResearch & Development` + DepartmentSales +  
                  JobLevel5 + `JobRoleHuman Resources` + JobRoleManager + 
                  `JobRoleManufacturing Director` + `JobRoleResearch Director` + 
                  MaritalStatusSingle + 
                  NumCompaniesWorked1 + 
                  NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + 
                  NumCompaniesWorked9 +  
                   TrainingTimesLastYear5 + TrainingTimesLastYear6 + 
                  JobInvolvement3, family = "binomial", data = train)
summary(model_14)
#removing  `JobRoleHuman Resources` 


model_15 <- glm(formula = Attrition ~ Age + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Avg.hrs + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 + 
                  EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
                  BusinessTravelTravel_Frequently +  
                  `DepartmentResearch & Development` + DepartmentSales +  
                  JobLevel5 + JobRoleManager + 
                  `JobRoleManufacturing Director` + `JobRoleResearch Director` + 
                  MaritalStatusSingle + 
                  NumCompaniesWorked1 + 
                  NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + 
                  NumCompaniesWorked9 +  
                  TrainingTimesLastYear5 + TrainingTimesLastYear6 + 
                  JobInvolvement3, family = "binomial", data = train)
summary(model_15)
#removing  NumCompaniesWorked6 


model_16 <- glm(formula = Attrition ~ Age + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Avg.hrs + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 + 
                  EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
                  BusinessTravelTravel_Frequently +  
                  `DepartmentResearch & Development` + DepartmentSales +  
                  JobLevel5 + JobRoleManager + 
                  `JobRoleManufacturing Director` + `JobRoleResearch Director` + 
                  MaritalStatusSingle + 
                  NumCompaniesWorked1 + 
                  NumCompaniesWorked5 + NumCompaniesWorked7 + 
                  NumCompaniesWorked9 +  
                  TrainingTimesLastYear5 + TrainingTimesLastYear6 + 
                  JobInvolvement3, family = "binomial", data = train)
summary(model_16)
#removing JobInvolvement3


model_17 <- glm(formula = Attrition ~ Age + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Avg.hrs + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 + 
                  EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
                  BusinessTravelTravel_Frequently + `DepartmentResearch & Development` + DepartmentSales +  
                  JobLevel5 + JobRoleManager + `JobRoleManufacturing Director` + `JobRoleResearch Director` + 
                  MaritalStatusSingle + NumCompaniesWorked1 + NumCompaniesWorked5 + NumCompaniesWorked7 + 
                  NumCompaniesWorked9 + TrainingTimesLastYear5 + TrainingTimesLastYear6  
                  , family = "binomial", data = train)
summary(model_17)
#removing  NumCompaniesWorked1


model_18 <- glm(formula = Attrition ~ Age + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Avg.hrs + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 + 
                  EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
                  BusinessTravelTravel_Frequently + `DepartmentResearch & Development` + DepartmentSales +  
                  JobLevel5 + JobRoleManager + `JobRoleManufacturing Director` + `JobRoleResearch Director` + 
                  MaritalStatusSingle + NumCompaniesWorked5 + NumCompaniesWorked7 + 
                  NumCompaniesWorked9 + TrainingTimesLastYear5 + TrainingTimesLastYear6  
                , family = "binomial", data = train)
summary(model_18)
# removing NumCompaniesWorked9 


model_19 <- glm(formula = Attrition ~ Age + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Avg.hrs + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 + 
                  EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
                  BusinessTravelTravel_Frequently + `DepartmentResearch & Development` + DepartmentSales +  
                  JobLevel5 + JobRoleManager + `JobRoleManufacturing Director` + `JobRoleResearch Director` + 
                  MaritalStatusSingle + NumCompaniesWorked5 + NumCompaniesWorked7 + 
                   TrainingTimesLastYear5 + TrainingTimesLastYear6  
                , family = "binomial", data = train)
summary(model_19)
#removing JobRoleManager


model_20 <- glm(formula = Attrition ~ Age + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Avg.hrs + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 + 
                  EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
                  BusinessTravelTravel_Frequently + `DepartmentResearch & Development` + DepartmentSales +  
                  JobLevel5 + `JobRoleManufacturing Director` + `JobRoleResearch Director` + 
                  MaritalStatusSingle + NumCompaniesWorked5 + NumCompaniesWorked7 + 
                  TrainingTimesLastYear5 + TrainingTimesLastYear6  
                , family = "binomial", data = train)
summary(model_20)
#removing `JobRoleResearch Director`


model_21 <- glm(formula = Attrition ~ Age + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Avg.hrs + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 + 
                  EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
                  BusinessTravelTravel_Frequently + `DepartmentResearch & Development` + DepartmentSales +  
                  JobLevel5 + `JobRoleManufacturing Director` + 
                  MaritalStatusSingle + NumCompaniesWorked5 + NumCompaniesWorked7 + 
                  TrainingTimesLastYear5 + TrainingTimesLastYear6  
                , family = "binomial", data = train)
summary(model_21)
#removing JobLevel5


model_22 <- glm(formula = Attrition ~ Age + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Avg.hrs + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 + 
                  EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
                  BusinessTravelTravel_Frequently + `DepartmentResearch & Development` + DepartmentSales +  
                   `JobRoleManufacturing Director` + 
                  MaritalStatusSingle + NumCompaniesWorked5 + NumCompaniesWorked7 + 
                  TrainingTimesLastYear5 + TrainingTimesLastYear6  
                , family = "binomial", data = train)
summary(model_22)
#removing TrainingTimesLastYear5


model_23 <- glm(formula = Attrition ~ Age + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Avg.hrs + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 + 
                  EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
                  BusinessTravelTravel_Frequently + `DepartmentResearch & Development` + DepartmentSales +  
                  `JobRoleManufacturing Director` + 
                  MaritalStatusSingle + NumCompaniesWorked5 + NumCompaniesWorked7 + 
                   TrainingTimesLastYear6  
                , family = "binomial", data = train)
summary(model_23)
#removing TrainingTimesLastYear6  


model_24 <- glm(formula = Attrition ~ Age + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Avg.hrs + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 + 
                  EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
                  BusinessTravelTravel_Frequently + `DepartmentResearch & Development` + DepartmentSales +  
                  `JobRoleManufacturing Director` + 
                  MaritalStatusSingle + NumCompaniesWorked5 + NumCompaniesWorked7, family = "binomial", data = train)
summary(model_24)
#removing JobSatisfaction2


model_25 <- glm(formula = Attrition ~ Age + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Avg.hrs + JobSatisfaction3 + JobSatisfaction4 + 
                  EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
                  BusinessTravelTravel_Frequently + `DepartmentResearch & Development` + DepartmentSales +  
                  `JobRoleManufacturing Director` + 
                  MaritalStatusSingle + NumCompaniesWorked5 + NumCompaniesWorked7, family = "binomial", data = train)
summary(model_25)
#removing JobSatisfaction3


model_26 <- glm(formula = Attrition ~ Age + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Avg.hrs + JobSatisfaction4 + 
                  EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
                  BusinessTravelTravel_Frequently + `DepartmentResearch & Development` + DepartmentSales +  
                  `JobRoleManufacturing Director` + 
                  MaritalStatusSingle + NumCompaniesWorked5 + NumCompaniesWorked7, family = "binomial", data = train)
summary(model_26)
#removing Age


model_27 <- glm(formula = Attrition ~ TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Avg.hrs + JobSatisfaction4 + 
                  EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
                  BusinessTravelTravel_Frequently + `DepartmentResearch & Development` + DepartmentSales +  
                  `JobRoleManufacturing Director` + 
                  MaritalStatusSingle + NumCompaniesWorked5 + NumCompaniesWorked7, family = "binomial", data = train)
summary(model_27)
#removing WorkLifeBalance4


model_28 <- glm(formula = Attrition ~ TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Avg.hrs + JobSatisfaction4 + 
                  EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  WorkLifeBalance2 + WorkLifeBalance3 + 
                  BusinessTravelTravel_Frequently + `DepartmentResearch & Development` + DepartmentSales +  
                  `JobRoleManufacturing Director` + 
                  MaritalStatusSingle + NumCompaniesWorked5 + NumCompaniesWorked7, family = "binomial", data = train)
summary(model_28)
#removing WorkLifeBalance2


model_29 <- glm(formula = Attrition ~ TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Avg.hrs + JobSatisfaction4 + 
                  EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                   WorkLifeBalance3 + 
                  BusinessTravelTravel_Frequently + `DepartmentResearch & Development` + DepartmentSales +  
                  `JobRoleManufacturing Director` + 
                  MaritalStatusSingle + NumCompaniesWorked5 + NumCompaniesWorked7, family = "binomial", data = train)
summary(model_29)
#removing WorkLifeBalance3


model_30 <- glm(formula = Attrition ~ TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Avg.hrs + JobSatisfaction4 + 
                  EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  BusinessTravelTravel_Frequently + `DepartmentResearch & Development` + DepartmentSales +  
                  `JobRoleManufacturing Director` + 
                  MaritalStatusSingle + NumCompaniesWorked5 + NumCompaniesWorked7, family = "binomial", data = train)
summary(model_30)



# With 16 significant variables in the model, we have the final model in place.

final_model<- model_30




###############################################MODEL EVALUATION########################

#Prediction of the test data
test_pred = predict(final_model, type = "response", 
                    newdata = test[,-1])

summary(test_pred)
test$prob <- test_pred


test_pred_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))


confusion_matrix <- table(test_actual_attrition,test_pred_attrition)
accuracy <- (confusion_matrix[1,1]+confusion_matrix[2,2])/sum(confusion_matrix)
#accuracy is 0.8581395
sensitivity <- (confusion_matrix[2,2])/(confusion_matrix[2,2]+confusion_matrix[2,1])
#sensitivity is 0.2200957
specificity <- confusion_matrix[1,1]/(confusion_matrix[1,1]+confusion_matrix[1,2])
#specificity is 0.9814986




# Finding out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}



# Creating cutoff values

# Summary of test probability

summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))



cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]
cutoff
#### The optimal probability cutoff is 0.1775758




test_cutoff_attrition <- factor(ifelse(test_pred >=0.1775758, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc
#Accuracy 
#0.7085271 

sens
#Sensitivity 
#0.7033493 

spec
#Specificity 
#0.7095282 



############################ KS -statistic - Test Data ######################

test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)



#on testing  data
pred_object_test<- prediction(test_cutoff_attrition, test_actual_attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)
# 0.4128775 i.e 41.3%
# A	high	KS	statistic	means	model	has	all	attrition	at	the	top,	it	has	has	all	non-attrition	at	the	bottom.
# For	a	good	model,	KS	statistic	would	be	more	than	40%	and	would	lie	in	the	top	few	deciles	(1st	to	4th).	


########################### Lift & Gain Chart #################################


# plotting the lift chart


lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

attrition_decile = lift(test_actual_attrition, test_pred, groups = 10)
attrition_decile



# Plotting the Gain and Lift

plot_grid(ggplot(attrition_decile, aes(x = bucket, y=Gain))+ geom_line(color='blue') +geom_point()+ scale_x_continuous(breaks = seq(1,10,1)) + xlab("Decile") + ylab("Gain%") + ggtitle("Gain Chart"),
          ggplot(attrition_decile, aes(x=bucket,y=Cumlift))+ geom_line(color='blue')+geom_point() +scale_x_continuous(breaks = seq(1,10,1)) + xlab("Decile") + ylab("Lift") + ggtitle("Lift Chart"), 
          align = "v",ncol = 1)



