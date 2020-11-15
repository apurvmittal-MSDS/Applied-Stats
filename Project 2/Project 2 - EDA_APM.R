# Install Libraries
library(caret)
library(groupdata2)

# Read the Data
bank_Data <- read.csv2("/Users/apurv/Documents/SMU/6372 - Applied Statistics/R-Code/Applied-Stats/Project 2/bank-additional-full.csv", header = TRUE, sep = ";")

# Convert response variable to a factor
bank_Data$y<-as.factor(bank_Data$y)

# Check summary of original data
summary(bank_Data)

# Several "Unknown" or missing data. Assigning all unknown as NA 
bank_Data[bank_Data=="unknown"] <- NA
sum(is.na(bank_Data))

# Removing all the NAs
bank.nona = na.omit(bank_Data)
sum(is.na(bank.nona))

# Since the data is imbalance and the number of "Yes" for the response variable are very small compared to "No". Downsampling the data to make it balanced.
set.seed(12345)
bank.ds = downSample(bank.nona, y=bank.nona$y)
summary(bank.ds)


# Write the downsampled data to a .csv for reference

write.csv(bank.ds,file="/Users/apurv/Documents/SMU/6372 - Applied Statistics/R-Code/Applied-Stats/Project 2/downsampled.csv")

#########____________________________##############
#########___________EDA______________##############
#########____________________________##############



