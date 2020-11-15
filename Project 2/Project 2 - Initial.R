library(caret)
library(groupdata2)
bank_Data <- read.csv2("/Users/apurv/Documents/SMU/6372 - Applied Statistics/R-Code/Applied-Stats/Project 2/bank-additional-full.csv", header = TRUE, sep = ";")


bank_Data$y<-as.factor(bank_Data$y)

summary(bank_Data)
set.seed(123)
z<-downsample(bank_Data, cat_col = "y")

summary(z)

write.csv(z,file="/Users/apurv/Documents/SMU/6372 - Applied Statistics/R-Code/Applied-Stats/Project 2/downsampled.csv")
View(z)


#um(is.na(bank_additional))
#sum(is.na(bank_additional))

df[df == 0] <- NA
bank_additional[bank_additional=="unknown"] <- NA

#####
#####
bank_additional = read.csv(file.choose(), sep=";")
bank_additional
sum(is.na(bank_additional))
bank_additional[bank_additional=="unknown"] <- NA
sum(is.na(bank_additional))
bank_additional
nrows(bank_additional)
nrow(bank_additional)
bankaddi.nona = na.omit(bank_additional)
sum(is.na(bankaddi.nona))
nrow(bankaddi.nona)
bankaddi.nona$y = as.factor(bankaddi.nona$y)
summary(bankaddi.nona)
summary(bank_additional)
bankadditional_ds = downSample(bankaddi.nona, y=bankaddi.nona$y)
summary(bankadditional_ds)
set.seed(12345)
bankadditional_ds = downSample(bankaddi.nona, y=bankaddi.nona$y)
summary(bankadditional_ds)

#### Eric
df = bank.additional.full
df[c("y")]<- lapply(df[c("y")], factor)

library(caret)
library(plyr)
library(dplyr)

set.seed(12345)
for (val in 1:length(df)) {
  df_clean = df %>% filter(!grepl('unknown',df[,val]))
}
summary(df_clean$y)

df_downsample <- downSample(df_clean, df$y)
summary(df_downsample$Class)
summary(df_downsample)