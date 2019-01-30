rm(list = ls())

myPackages = c("lubridate",
               "readr",
               "tidyr",
               "questionr",
               "stringr",
               "xgboost")

missingPackages = setdiff(myPackages, rownames(installed.packages())) 
if (length(missingPackages) > 0) { 
  install.packages(missingPackages)
}

library(readr)
library(lubridate)
library(tidyr)
library(questionr)
library(stringr)
library(xgboost)

###########load data
data_all<-read.csv('loan_stat542.csv')
test_id<-read.csv('Project3_test_id.csv')

set.seed(8139)
########### Data preprocessing

#summary(as.factor(data$loan_status))#check content & whether there are missing value about loan status

Preprocessing<-function(data){
  
  if ("loan_status" %in% colnames(data)) {
data$loan_status<-ifelse(data$loan_status=='Fully Paid',0,1)#Binary Response;Response = 0, if 'Fully Paid'
}
#emp_length
#replace the NA's with mideian
no.na_emp_length<-na.rm(data$emp_length)
#length(no.na_emp_length)/2
#summary(no.na_emp_length)
#we can know the median is 6 years

data$emp_length[is.na(data$emp_length)]<-"6 years"
summary(data$emp_length)
data$emp_length<-as.character(data$emp_length)
#Convert emp_length to integers:
data$emp_length[data$emp_length =="< 1 year"]<-'0'
data$emp_length[data$emp_length =="10+ years"]<-'10'

numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 
data$emp_length<-as.integer(numextract(data$emp_length))

#home_ownership
#Replace the values ANY and NONE with OTHER:
data$home_ownership[data$home_ownership == 'ANY']<-'OTHER'
data$home_ownership[data$home_ownership == 'NONE']<-'OTHER'
data$home_ownership<-droplevels(data$home_ownership)#drop levels with no obs

#"fico_range_low" "fico_range_high"  
#We only need to keep one of the FICO scores. We'll take the average of the two and call it fico_score
data$fico_score<-(data$fico_range_high+data$fico_range_low)/2

#earliest_cr_line
cr_line<-as.character(data$earliest_cr_line)
date<-as.Date(paste("01-",cr_line, sep=""), format="%d-%b-%Y")
month<-as.numeric(format(date,"%m"))
year<-as.numeric(format(date,"%Y"))
#min(year), the earlist year is 1944
data$earliest_cr_line<-(year-min(year))*12+month

data$dti[is.na(data$dti)] = 0

data$mort_acc[is.na(data$mort_acc)] = 0

data$pub_rec_bankruptcies[is.na(data$pub_rec_bankruptcies)] = 0

data$revol_util[is.na(data$revol_util)] = 0

rownames(data) = data$id
#drop variable
#The grade is implied by the subgrade, so let's drop the grade column.
#There are too many different job titles for this feature to be useful, so we drop it.
#There are too many different titles,and the purpose variable contains similar info, So we drop it .
#Zip and state contain similar info, but there are too many different zip, so we just keep state
data<-data[,!names(data)%in%c('X','id','grade','emp_title','title',"zip_code","fico_range_high","fico_range_low")]
return(data)
}
########
train<-read.csv("train.csv")
train_clean<-Preprocessing(train)
train_X<-model.matrix(~ ., subset(train_clean, select = -c(loan_status)))
train_Y<-train_clean$loan_status

#rm(data.train.cleaned)
test<-read.csv('test.csv')
test_clean<-Preprocessing(test)
test_X<-model.matrix(~ ., test_clean)


#rm(data.test.cleaned)
library(xgboost)
print("XGBoost model")
start.time = Sys.time()
mod = xgboost(data = train_X,
              label = train_Y,
              nrounds = 25,
              objective = "binary:logistic",
              eval_metric = "logloss",
              verbose = T)
xgboost_predictions = predict(mod, test_X)

write.csv(data.frame(id = rownames(test_X),
                     prob = xgboost_predictions),
          "mysubmission1.txt",
          row.names = FALSE,
          quote = FALSE)
print(paste0("Run time: ", as.numeric(difftime(Sys.time(), start.time, units = 'min'))))

