#load library
library(tidyverse)
library(ggplot2)
library(ggcorrplot)
library(dplyr)
library(corrplot)
library(Amelia)
library(cluster)


# ===== Read data =====
setwd('D:\\')
data <- read.csv(file ='WA_Fn-UseC_-Marketing-Customer-Value-Analysis.csv')
head(data)

summary(data)

# ===== Boxplot to check for outliers =====
par(mfrow=c(2,3)) # divide graph area in 3 columns
boxplot(data$Customer_Lifetime_Value, main="Customer Lifetime Value",
        sub=paste("Outlier rows: ", boxplot.stats(data$Customer_Lifetime_Value)$out)) # box plot for "Customer Lifetime Value"
boxplot(data$Income, main="Income",
        sub=paste("Outlier rows: ", boxplot.stats(data$Income)$out)) # box plot for "Income"
boxplot(data$Monthly_Premium_Auto, main="Monthly Premium Auto",
        sub=paste("Outlier rows: ", boxplot.stats(data$Monthly_Premium_Auto)$out)) # box plot for "Monthly Premium Auto"
boxplot(data$Months_Since_Last_Claim, main="Months_Since_Last_Claim",
        sub=paste("Outlier rows: ", boxplot.stats(data$Months_Since_Last_Claim)$out)) # box plot for "Months_Since_Last_Claim"
boxplot(data$Months_Since_Last_Claim, main="Months_Since_Policy_Inception",
        sub=paste("Outlier rows: ", boxplot.stats(data$Months_Since_Policy_Inception)$out)) # box plot for "Months_Since_Policy_Inception"
boxplot(data$Months_Since_Last_Claim, main="Total_Claim_Amount",
        sub=paste("Outlier rows: ", boxplot.stats(data$Total_Claim_Amount)$out)) # box plot for "Total_Claim_Amount"


# ===== REMOVE OUTLIERS Customer_Lifetime_Value =====
# Filter data with data$Customer_Lifetime_Value <= 52000
data <- data[data$Customer_Lifetime_Value <= 52000,]
head(data[order(-data$Customer_Lifetime_Value),], 10)

# box plot for "Customer Lifetime Value"
par(mfrow=c(1,1))
boxplot(data$Customer_Lifetime_Value, main="Customer Lifetime Value",
        sub=paste("Outlier rows: ", boxplot.stats(data$Customer_Lifetime_Value)$out)) # box plot for "Customer Lifetime Value"
par(mar = c(3, 3, 2, 2))


# ===== Identify character features =====
character_cols <- sapply(data, is.character)
head(data[, character_cols])

# ===== Check unique value =====

# Print the unique values State
print(unique(data$State))

# Print the unique values Response
print(unique(data$Response))

# Print the unique values Coverage
print(unique(data$Coverage))

# Print the unique values Education
print(unique(data$Education))

# Print the unique values EmploymentStatus
print(unique(data$EmploymentStatus))

# Print the unique values Gender
print(unique(data$Gender))

# Print the unique values Location_Code
print(unique(data$Location_Code))

# Print the unique values Marital_Status
print(unique(data$Marital_Status))

# Print the unique values Policy_Type
print(unique(data$Policy_Type))

# Print the unique values Policy
print(unique(data$Policy))

# Print the unique values Renew_Offer_Type
print(unique(data$Renew_Offer_Type))

# Print the unique values Sales_Channel
print(unique(data$Sales_Channel))

# Print the unique values Vehicle_Class
print(unique(data$Vehicle_Class))

# Print the unique values Vehicle_Size
print(unique(data$Vehicle_Size))

# ===== CONVERT CHARACTER TO NUMERIC =====

# Response: No- 0, Yes- 1
data$Response <- ifelse(data$Response == "No", 0, 1)

# Convert character vector to numeric
# State
data$State <- as.numeric(factor(data$State))

# EmploymentStatus
data$EmploymentStatus <- as.numeric(factor(data$EmploymentStatus))

# Gender
data$Gender <- as.numeric(factor(data$Gender))

# Marital_Status
data$Marital_Status <- as.numeric(factor(data$Marital_Status))

# Sales_Channel
data$Sales_Channel <- as.numeric(factor(data$Sales_Channel))

# Vehicle_Class
data$Vehicle_Class <- as.numeric(factor(data$Vehicle_Class))

# Convert character vector to numeric add label
# Coverage ("Premium","Extended","Basic")
data$Coverage <- as.numeric(factor(data$Coverage, levels = c("Premium","Extended","Basic")))

# Education ("Doctor","Master","Bachelor","College","High School or Below")
data$Education <- as.numeric(factor(data$Education, levels = c("Doctor","Master","Bachelor","College","High School or Below")))

# Location_Code ("Urban", "Suburban", "Rural")
data$Location_Code <- as.numeric(factor(data$Location_Code, levels = c("Urban", "Suburban", "Rural")))

# Policy_Type ("Special Auto", "Corporate Auto", "Personal Auto")
data$Policy_Type <- as.numeric(factor(data$Policy_Type, levels = c("Special Auto", "Corporate Auto", "Personal Auto")))

# Policy ("Special L1", "Special L2", "Special L3", "Corporate L1", "Corporate L2", "Corporate L3", "Personal L1", "Personal L2", "Personal L3")
data$Policy <- as.numeric(factor(data$Policy, levels = c("Special L1", "Special L2", "Special L3", "Corporate L1", "Corporate L2", "Corporate L3", "Personal L1", "Personal L2", "Personal L3")))

# Renew_Offer_Type ("Offer1", "Offer2", "Offer3", "Offer4")
data$Renew_Offer_Type <- as.numeric(factor(data$Renew_Offer_Type, levels = c("Offer1", "Offer2", "Offer3", "Offer4")))

# Vehicle_Size ("Small", "Medsize", "Large")
data$Vehicle_Size <- as.numeric(factor(data$Vehicle_Size, levels = c("Small", "Medsize", "Large")))

summary(data)

# ===== SCALE DATA =====

columns_to_scale <- c(
  "Customer_Lifetime_Value", "Income", "Monthly_Premium_Auto",
  "Months_Since_Last_Claim", "Months_Since_Policy_Inception",
  "Number_of_Open_Complaints", "Number_of_Policies", "Total_Claim_Amount"
)

scale_data <- scale(data[columns_to_scale])
head(scale_data)

data <- data[, !(names(data) %in% columns_to_scale)]

head(data)

# ===== COMBINE DATA & SCALE DATA =====
df <- cbind(data, scale_data)
head(df)
dim(df)

summary(df)


####
# ===== Identify numeric features =====
numeric_cols <- sapply(df, is.numeric)
df <- df[, numeric_cols]
head(df)
dim(df)

# ===== check missing value =====
missmap(df, main="Missing values")


# ===== Save file =====
# write.csv(df, file = "data_processed.csv", row.names = FALSE)


# check missing value
missmap(df, main="Missing values")
