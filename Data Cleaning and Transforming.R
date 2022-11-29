install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot")
install.packages("xlsx")
library(tidyverse)
library(dplyr)
library(ggplot2)
library(xlsx)


#import data set
data <- read_csv("C:/Users/19028/Documents/Datascience/Health Analytics/CSCI225_Project/Stroke Dataset.csv")

str(data)
summary(data)
head(data)
tail(data)

data %>% count(data$gender)
data %>% count(data$hypertension)
data %>% count(data$heart_disease)
data %>% count(data$ever_married)
data %>% count(data$work_type)
data %>% count(data$Residence_type)
data %>% count(data$smoking_status)
data %>% count(data$stroke)

#checking for empty and na cells
filter(data, gender == "" | age== "" | hypertension== "" | heart_disease== "" | ever_married== "" | work_type== "" |Residence_type == "" | avg_glucose_level== "" |bmi == "" | smoking_status== "" | stroke== "" | is.na(data$gender) == "TRUE" | is.na(data$age) == "TRUE" | 
         is.na(data$hypertension) == "TRUE" | is.na(data$heart_disease) == "TRUE" | 
         is.na(data$ever_married) == "TRUE" | is.na(data$work_type) == "TRUE" | 
         is.na(data$Residence_type ) == "TRUE" | is.na(data$avg_glucose_level) == "TRUE" | 
         is.na(data$bmi ) == "TRUE" | is.na(data$smoking_status) == "TRUE" | 
         is.na(data$stroke) == "TRUE" )

#Changing "N/A" and "Unknown" to NA object
data$bmi[data$bmi == "N/A"] <- NA

data$smoking_status[data$smoking_status == "Unknown"] <- NA

data$work_type[data$work_type == "children"] <- NA

#changing bmi column from character to numerical
data$bmi<- as.double(data$bmi)

#calculating mean of bmi grouped by gender
the_mean <- data  %>% select(bmi,gender)%>% filter(!is.na(bmi)) %>% group_by(gender) %>% summarise(mean(bmi))

#replace N/A with bmi by average of gender
data$bmi[data$gender == "Female" & is.na(data$bmi)] <- the_mean[1,2]
data$bmi[data$gender == "Male" & is.na(data$bmi)] <- the_mean[2,2]

#changing bmi column to numerical
data$bmi<- as.double(data$bmi)

#replace the unknown values of smoking_status with the most frequent category ‘never smoked’
data <- data %>% mutate(smoking_status = replace(smoking_status, is.na(smoking_status), "never smoked"))

#creating new column for Age Category of each observation -> age<=16 = children, age>16 = adult,  age>=60 = senior

data$Age_Category <- with(data, ifelse(age >= 60, 'Senior',
                                       ifelse(age > 16, 'Adult', 'Child')))


#creating new column for BMI_Category
#Underweight	< 18.5	    ----  0
#Normal Weight	18.5 - 24.9		    ----  1
#Overweight	25.0 - 29.9		    ----  2
#Obese class I	30.0 - 34.9		    ----  3
#Obese class II	35.0 - 39.9		    ----  4
#Obese class III	>= 40.0		    ----  5

data$BMI_Category <- with(data, ifelse(bmi >= 40, 5,
                                       ifelse(bmi > 35.0, 4,
                                              ifelse(bmi > 30.0, 3, 
                                                     ifelse(bmi > 25.0, 2, 
                                                            ifelse(bmi > 18.5, 1, 0))))))

data %>% count(data$BMI_Category)

#creating new column for Glucose_Category
#Diabetes	126 mg/dL or above  --  2
#Prediabetes	100 – 125 mg/dL	  --  1
#Normal	99 mg/dL or below	  --  0

data$Glucose_Category <- with(data, ifelse(avg_glucose_level >= 126.0, 2, 
                                           ifelse(avg_glucose_level >= 100, 1, 0)))

summary(data)
sapply(data, class)

#Dealing with Outliers of bmi
data <- subset(data, !(bmi < 12 & Age_Category != "Child"))

data <- subset(data, !(bmi > 50))

# Removing the other data in gender
data <- subset(data, (gender != "Other"))

# Drop ID column.
data= subset(data, select = -c(id))
summary(data)
sapply(data, class)

# gender: Male for 0, Female for 1
data$gender[data$gender == "Male"] <- 0
data$gender[data$gender == "Female"] <- 1
data$gender <- as.double(data$gender)

# ever_married: No for 0, Yes for 1
data$ever_married <- as.character(data$ever_married)
data$ever_married[data$ever_married == "Yes"] <- 1
data$ever_married[data$ever_married == "No"] <- 0
data$ever_married <- as.double(data$ever_married)

#work_type govtjob - 4, never worked - 3, private - 2, self employed - 1, na - 0
data$work_type[data$work_type == "Govt_job"] <- 4
data$work_type[data$work_type == "Never_worked"] <- 3
data$work_type[data$work_type == "Private"] <- 2
data$work_type[data$work_type == "Self-employed"] <- 1
data <- data %>% mutate(work_type = replace(work_type, is.na(work_type), 0))
data$work_type<- as.double(data$work_type)

# Residence_type: Urban for 0, Rural for 1
data$Residence_type<- as.character(data$Residence_type)
data$Residence_type[data$Residence_type == "Urban"] <- 0
data$Residence_type[data$Residence_type == "Rural"] <- 1
data$Residence_type<- as.double(data$Residence_type)

#smoking status never smoked  - 0, formerly smoked - 1, smokes - 2
data$smoking_status[data$smoking_status == "formerly smoked"] <- 1
data$smoking_status[data$smoking_status == "never smoked"] <- 0
data$smoking_status[data$smoking_status == "smokes"] <- 2
data$smoking_status<- as.double(data$smoking_status)

# Age_category Senior - 2, Adult - 1, Child - 0
data$Age_Category[data$Age_Category == "Senior"] <- 2
data$Age_Category[data$Age_Category == "Adult"] <- 1
data$Age_Category[data$Age_Category == "Child"] <- 0
data$Age_Category<- as.double(data$Age_Category)

# Checking for null values
colSums(is.na(data))

sapply(data, class)

stroke <- data