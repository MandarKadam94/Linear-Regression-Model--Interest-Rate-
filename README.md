# Linear-Regression-Model--Interest-Rate-
We are creating Linear Regression Model for Loan Interest Rate.

## Linear Regression  Model ##


# Data Preperation #


# We are taking real life example for these purpose, I have used CSV file for these purpose.

setwd('C:/Users/mandar kadam/Desktop/R data scientist/Excel Data')

getwd()

loan=read.csv("loans data.csv",stringsAsFactors = FALSE)

library(dplyr)

getwd()

loan=read.csv("loans data.csv",stringsAsFactors = FALSE)

glimpse(loan)

# Data Cleaning and Manipulation #


loan=loan%>%
mutate(Interest.Rate=as.numeric(gsub("%","",Interest.Rate)),
         Debt.To.Income.Ratio=as.numeric(gsub("%","",Debt.To.Income.Ratio)),
         Open.CREDIT.Lines=as.numeric(Open.CREDIT.Lines),
         Amount.Requested=as.numeric(Amount.Requested),
         Amount.Funded.By.Investors=as.numeric(Amount.Funded.By.Investors),
         Revolving.CREDIT.Balance=as.numeric(Revolving.CREDIT.Balance) #---- we have to convert data to numeric if it is Character.

glimpse(loan)
table(loan$Employment.Length)
loan=loan %>%
mutate(el=ifelse(substr(Employment.Length,1,2)=="10",10,Employment.Length),
         el= ifelse(substr(Employment.Length,1,1)=="<",0,el),
         el=gsub("years","",el),
         el=gsub("year","",el),
         el=as.numeric(el)
  )%>%
  select(-Employment.Length)%>%
  na.omit()           #---- we have to convert data to numeric if it is Character and delete the old column as we have created dummy columns for same and we are omitting Na if they are present.


table(loan$el)
glimpse(loan)
table(loan$Home.Ownership)
loan=loan%>%
  mutate(Hw_Rent=as.numeric(Home.Ownership=="RENT"),
         Hw_Mort=as.numeric(Home.Ownership=="MORTGAGE")) %>%
  select(-Home.Ownership)
glimpse(loan)   #---- we have to convert data to numeric if it is Character and delete the old column as we have created dummy columns for same.

loan=loan%>%
  mutate(f1=as.numeric(substr(FICO.Range,1,3)),
         f2=as.numeric(substr(FICO.Range,5,7)),
         fico=0.5*(f1+f2))%>%
  select(-FICO.Range,-f1,-f2) #---- we have to convert data to numeric if it is Character and delete the old column as we have created dummy columns for same.


table(loan$Loan.Purpose)
sort(round(tapply(loan$Interest.Rate,loan$Loan.Purpose,mean),0))
loan=loan %>% 
  mutate(lp_14=as.numeric(Loan.Purpose %in% c("debt_consolidation","moving")),
         lp_13=as.numeric(Loan.Purpose %in% c("credit_card","other","small_business",'house')),
         lp_12=as.numeric(Loan.Purpose %in% c("home_improvement","vacation","wedding","medical")))
loan=loan %>%select(-Loan.Purpose)  #---- we have to convert data to numeric if it is Character and delete the old column as we have created dummy columns for same.


View(loan)
table(loan$State)
length(unique(loan$State))
table(loan$Loan.Length)
loan=loan%>%mutate(LL_36=as.numeric(Loan.Length=="36 months"))%>%
select(-Loan.Length)  #---- we have to convert data to numeric if it is Character and delete the old column as we have created dummy columns for same.


glimpse(loan) 
loan=loan%>%
select(-State) #---delete the old column because we dont need these column as they are not useful for future.
ncol(loan)
select(loan)

#---------------------------------------Data Modelling----------------------------------- 


#------ Create Test and Train Data -----#

set.seed(2)
s=sample(1:nrow(loan),0.7*nrow(loan)) # we have 70 % of data in Train and Test we have 30%.
loan_train=loan[s,]
loan_test=loan[-s,]
str(loan_test)


library(car)
fit=lm(Interest.Rate~. -ID,data=loan_train) # we have created fit model and applied linear model on it.
fit
library(car)
vif(fit)
sort(vif(fit),decreasing = T)
summary(fit)


fit=lm(Interest.Rate~. -ID -Amount.Requested -Amount.Funded.By.Investors,data=loan_train) # Removing data which does not effect too much on interest rate.
sort(vif(fit),decreasing = T)
summary(fit)

fit=lm(Interest.Rate~. -ID -Amount.Requested -Amount.Funded.By.Investors
       -Debt.To.Income.Ratio-Revolving.CREDIT.Balance-Open.CREDIT.Lines-el
       -lp_12-Hw_Rent-Hw_Mort,data=loan_train)  # Removing data which does not effect too much on interest rate.
summary(fit)


##------------------------------------------Data Prediction-----------------------------------------------##

# Training

loan_train$predict=predict(fit,newdata = loan_train,type = "response")
View(loan_train)

rmse_train=sqrt(mean((loan_train$Interest.Rate-loan_train$predict)^2))
rmse_train

# Testing

loan_test$predict=predict(fit,newdata = loan_test,type = "response")
View(loan_test)
head(loan_test,5)

rmse_test=sqrt(mean((loan_test$Interest.Rate-loan_test$predict)^2))
rmse_test


# Asssumptions

plot(fit,which=1) #independent
plot(fit,which=2) #normal distibution
plot(fit,which=3) #constant variance
plot(fit,which=4) #cooks distance

