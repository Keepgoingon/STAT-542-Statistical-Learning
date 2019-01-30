myPackages = c("glmnet", "psych","xgboost","randomForest","gbm")
missingPackages = setdiff(myPackages, rownames(installed.packages())) 
if (length(missingPackages) > 0) { 
  install.packages(missingPackages)
}
library(psych) 
library(glmnet) 
library(xgboost)
library(randomForest)
library(gbm)
#load data
FILENM_TRAIN = 'train.csv'
FILENM_TEST = 'test.csv'
train_data = read.csv(FILENM_TRAIN)
test_data = read.csv(FILENM_TEST)

# PART 1 
#Pre-processing
 #1.Remove missing value
pre_missing=function(train_data,test_data){
  train_data$Garage_Yr_Blt[is.na(train_data$Garage_Yr_Blt)] =
    train_data$Year_Built[is.na(train_data$Garage_Yr_Blt)]
  test_data$Garage_Yr_Blt[is.na(test_data$Garage_Yr_Blt)] =
    test_data$Year_Built[is.na(test_data$Garage_Yr_Blt)]
  list(train_data = train_data, test_data = test_data)
}

#2.Combined highly correlated data
#Add new variable-total bath using Full_Bath,Half_Bath,Bsmt_Full_Bath and Bsmt_Half_Bath; 
#Later, remove Full_Bath,Half_Bath,Bsmt_Full_Bath and Bsmt_Half_Bath
Pre_add_var=function(train_data,test_data){
  train_data$Total_Bath = train_data$Full_Bath+(train_data$Half_Bath*0.5)+
    train_data$Bsmt_Full_Bath+(train_data$Bsmt_Half_Bath * 0.5)
  test_data$Total_Bath = test_data$Full_Bath + (test_data$Half_Bath*0.5) +
    test_data$Bsmt_Full_Bath + (test_data$Bsmt_Half_Bath*0.5)
  
#Add new variable-Total Square Feet,drop Gr_Liv_Area,Total_Bsmt_SF,1stFlrSF,2ndFlrSF later
  train_data$Total_SqFeet = train_data$Gr_Liv_Area + train_data$Total_Bsmt_SF
  test_data$Total_SqFeet = test_data$Gr_Liv_Area + test_data$Total_Bsmt_SF
  
#Add new variable-Total Porch in square feet,drop Open_Porch_SF, Enclosed_Porch later
  train_data$Total_PorchSqFeet =  train_data$Open_Porch_SF + train_data$Enclosed_Porch + 
    train_data$Three_season_porch + train_data$Screen_Porch
  test_data$Total_PorchSqFeet =  test_data$Open_Porch_SF + test_data$Enclosed_Porch + 
    test_data$Three_season_porch + test_data$Screen_Porch
  
#Add new variable Year; Drop Year_Remod_Add later
  train_data$Year = train_data$Year_Sold - train_data$Year_Remod_Add
  test_data$Year = test_data$Year_Sold - test_data$Year_Remod_Add
#Add new variable BsmtFinTotal; Drop BsmtFin_SF_1, BsmtFinSF2 later
  train_data$BsmtFinTotal = train_data$BsmtFin_SF_1 + train_data$BsmtFin_SF_2
  test_data$BsmtFinTotal = test_data$BsmtFin_SF_1 + test_data$BsmtFin_SF_2
  
  list(train_data = train_data, test_data = test_data)
}

#3.Remove categorical variables that contain one "dominating" category; remove unnecessary variable  
pre_rm_var=function(train_data,test_data){
  Dropvar = c('Longitude', 'Latitude',
              'Full_Bath', 'Half_Bath', 'Bsmt_Full_Bath', 'Bsmt_Half_Bath',
              'Gr_Liv_Area','Total_Bsmt_SF','First_Flr_SF', 'Second_Flr_SF',
              'Open_Porch_SF', 'Enclosed_Porch', 'Three_season_porch', 'Screen_Porch', 'Bedroom_AbvGr',
              'Year_Built','Year_Remod_Add',
              'BsmtFin_SF_1','BsmtFin_SF_2','BsmtFin_SF_1', 'BsmtFinSF2',
              'Street','Low_Qual_Fin_SF','Condition_2','Pool_QC','Heating','Roof_Matl',
              'Utilities','Land_Slope','Misc_Feature','Garage_Cond','Garage_Yr_Blt',
              'Land_Contour','Alley','Central_Air','Misc_Val')
train_data = train_data[, !colnames(train_data) %in% Dropvar]
test_data = test_data[, !colnames(test_data) %in% Dropvar] 
 list(train_data = train_data, test_data = test_data)
}           
#4.Winsorization
Winsorization = function(train_data){
  Non_numer_Var=c("PID", "Overall_Qual","Overall_Cond", "Garage_Cars","Wood_Deck_SF",
                  "Mas_Vnr_Area","BsmtFin_SF_2", "Second_Flr_SF","Bedroom_AbvGr","Kitchen_AbvGr",'Sale_Price')
   for (i in 1:dim(train_data)[2]){
    if(!is.numeric(train_data[,i]) || colnames(train_data)[i] %in% Non_numer_Var) next;
    med = median(train_data[,i])
    #cat("median is",med,"\n" )
    y = train_data[,i] - med
cutoff = mad(y, center=0)*3
    y[ y > cutoff ] = cutoff
    y[y < -cutoff ] = -cutoff
   train_data[,i] = y + med
  }
   train_data
}
#5.Processing categorical data
Cat_Levels= list(
  MS_SubClass = c("One_Story_1946_and_Newer_All_Styles","Two_Story_1946_and_Newer",
                  "One_and_Half_Story_Finished_All_Ages", "One_Story_PUD_1946_and_Newer",
                  "One_Story_1945_and_Older", "Two_Story_PUD_1946_and_Newer",
                  "Two_Story_1945_and_Older","Split_or_Multilevel",
                  "Duplex_All_Styles_and_Ages","Two_Family_conversion_All_Styles_and_Ages", 
                  "Split_Foyer", "Two_and_Half_Story_All_Ages", "One_and_Half_Story_Unfinished_All_Ages",
                  "PUD_Multilevel_Split_Level_Foyer", "Others"),
  Neighborhood = c("North_Ames","College_Creek","Old_Town","Edwards","Somerset",
                   "Northridge_Heights","Gilbert","Sawyer","Northwest_Ames",
                   "Sawyer_West","Mitchell","Brookside","Crawford",
                   "Iowa_DOT_and_Rail_Road", "Timberland", "Northridge","Stone_Brook",
                   "South_and_West_of_Iowa_State_University", "Clear_Creek", "Meadow_Village",
                   "Briardale", "Bloomington_Heights", "Veenker", "Northpark_Villa", "Blueste",
                   "Others"),
  MS_Zoning = c("Residential_Low_Density", "Residential_Medium_Density", 
                "Floating_Village_Residential","Residential_High_Density",
                "C_all", "Others"),
  Lot_Shape = c("Regular", "Slightly_Irregular", "Others"),
  Lot_Config = c("Inside","Corner","CulDSac", "Others"),
  Condition_1 = c("Norm","Feedr", "Others"),
  Bldg_Type = c("OneFam","TwnhsE","Duplex","Twnhs", "Others"),
  House_Style = c("One_Story","Two_Story","One_and_Half_Fin", 
                  "SLvl","Others"),
  Overall_Qual = c("Very_Poor", "Poor","Fair","Below_Average",
                   "Average","Above_Average","Good", "Very_Good", 
                   "Excellent", "Very_Excellent"),
  Overall_Cond = c("Very_Poor", "Poor","Fair","Below_Average",
                   "Average","Above_Average","Good", "Very_Good", 
                   "Excellent", "Very_Excellent"),
  Roof_Style = c("Gable","Hip", "Others"),
  Exterior_1st = c("VinylSd","MetalSd","HdBoard","Wd Sdng","Plywood","CemntBd", "Others"),
  Exterior_2nd = c("VinylSd","MetalSd","HdBoard","Wd Sdng","Plywood","CmentBd", "Others"),
  Mas_Vnr_Type = c("None","BrkFace","Stone", "Others"),
  Exter_Qual = c("Poor", "Fair","Typical","Good" ,"Excellent"),
  Exter_Cond = c("Poor", "Fair","Typical","Good" ,"Excellent"),
  Foundation = c("PConc","CBlock", "BrkTil", "Others"),
  Bsmt_Qual = c("No_Basement","Poor","Fair","Typical","Good","Excellent"),
  Bsmt_Cond = c("No_Basement","Poor","Fair","Typical","Good","Excellent"),
  Bsmt_Exposure = c("No_Basement", "No", "Mn","Av", "Gd"),
  BsmtFin_Type_1 = c("No_Basement","Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"),
  BsmtFin_Type_2 = c("No_Basement","Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"),
  Heating_QC = c("Poor", "Fair","Typical","Good" ,"Excellent"),
  Electrical = c("SBrkr","FuseA", "Others"),
  Kitchen_Qual = c("Poor", "Fair","Typical","Good" ,"Excellent"),
  Functional = c("Sal", "Sev", "Maj2", "Maj1", "Mod", "Min2", "Min1", "Typ"),
  Fireplace_Qu = c("No_Fireplace", "Poor", "Fair","Typical","Good","Excellent"),
  Garage_Type = c("Attchd","Detchd", "BuiltIn", "No_Garage", "Others"),
  Garage_Finish = c("No_Garage", "Unf", "RFn", "Fin"),
  Garage_Qual = c("No_Garage", "Poor", "Fair","Typical","Good","Excellent"),
  Paved_Drive = c("Dirt_Gravel","Partial_Pavement", "Paved"),
  Fence = c("No_Fence", "Minimum_Wood_Wire", "Good_Wood", "Minimum_Privacy", "Good_Privacy"),
  Others_Feature = c("None", "Others"),
  Sale_Type = c("WD ", "New", "Others"),
  Sale_Condition = c("Normal", "Partial", "Abnorml", "Others")
)
pre_categorical = function(train_data, test_data){
  for (name in names(Cat_Levels)) {
    if(!name %in% colnames(train_data)) next;
    n = length(Cat_Levels[[name]])
    if(Cat_Levels[[name]][n] != "Others") {
      train_levels = Cat_Levels[[name]][Cat_Levels[[name]] %in% unique(train_data[,name])]
      test_levels = Cat_Levels[[name]][Cat_Levels[[name]] %in% unique(test_data[,name])]
      train_data[,name] = ordered(train_data[,name], levels = train_levels)
if(sum(!test_levels %in% train_levels) > 0) {
  test_data[,name] = ordered(test_data[,name], levels = Cat_Levels[[name]])
  train_index = which(Cat_Levels[[name]] %in% train_levels)
  for(le_index in 1:length(test_levels)){
  level = test_levels[le_index]
  if(level %in% train_levels) next;
  index = which(Cat_Levels[[name]] == level)
  index = train_index[which.min(abs(train_index - index))]
  test_data[,name][test_data[,name] == test_levels[le_index]] = Cat_Levels[[name]][index]
   }
  }
  test_data[,name] = ordered(test_data[,name], levels = train_levels)
    }else {
      train_data[,name] = factor(train_data[,name])
      test_data[,name] = factor(test_data[,name], levels=levels(train_data[,name]))
      if(sum(is.na(test_data[,name])) > 0) {
        max_level = names(sort(table(train_data[,name]), decreasing = TRUE))[1]
        test_data[,name][is.na(test_data[,name])] = max_level
      }
    }
  }
list(train_data = train_data, test_data = test_data)
}

# 6. Processing numeric data
#Changing Year_Sold into Mo_Sold to factor
Pre_numeric = function(train_data, test_data) {
  train_data$Year_Sold = factor(train_data$Year_Sold)
  train_data$Mo_Sold = factor(train_data$Mo_Sold)
  test_data$Year_Sold = factor(test_data$Year_Sold, levels=levels(train_data$Year_Sold))
  test_data$Mo_Sold = factor(test_data$Mo_Sold, levels=levels(train_data$Mo_Sold))
  return(list(train_data = train_data, test_data = test_data))
}

#7 Log_Sale_Price
Log_Sale_Price = function (train_data, test_data) {
  train_data$Sale_Price = log(train_data$Sale_Price)
  if(hasName(test_data, "Sale_Price")) {
    test_data$Sale_Price = log(test_data$Sale_Price)
  }
list(train_data = train_data, test_data = test_data)
}
#Data_preprocessing
Data_preprocessing = function(train_data, test_data){
  r = pre_missing(train_data, test_data)
  r = Pre_add_var(r$train_data, r$test_data)
  r = pre_rm_var(r$train_data, r$test_data)
  r = pre_categorical(r$train_data, r$test_data)
  r = Pre_numeric(r$train_data, r$test_data)
  r = Log_Sale_Price(r$train_data, r$test_data)
  r$train_data = Winsorization(r$train_data)
#Remove PID in training set, separate "possible" y in test data
  r$train_data = r$train_data[, !colnames(r$train_data)== 'PID']
  if(hasName(r$test_data, "Sale_Price")){
    true_test_value= r$test_data$Sale_Price
    r$test_data = r$test_data[, !colnames(r$test_data) =='Sale_Price']  
  } else {
    true_test_y = NULL
  }
  list(train_data = r$train_data, test_data = r$test_data, true_test_value = true_test_value)
}
#model 1
rF_pred = function(train_data, test_data){
  rF_train = train_data[, !colnames(train_data) == "Sale_Price"]
  rFModel = randomForest(rF_train, train_data$Sale_Price, ntree=500);
  predict(rFModel, test_data)
}

#model 2
Xgboost_pred = function(train_data, test_data) {
  Xgboost_train = train_data[, colnames(train_data) != 'Sale_Price']
  Xgboost_train = model.matrix(~., Xgboost_train)[,-1]
  Xgboost_model = xgboost(Xgboost_train,
                          label=train_data$Sale_Price,
                          eta = 0.1,
                          max_depth = 10,
                          nrounds = 1000,
                          verbose = F)
  Xgboost_test = test_data[, colnames(test_data) != "PID"]
  Xgboost_test = model.matrix(~., Xgboost_test)[,-1]
  predictions=predict(Xgboost_model, Xgboost_test)
}

RMSE = function (pred_value,T_value) {
  sqrt(mean((pred_value - T_value)^2))
}
log_RMSE = function (T_value, pred_value) {
  sqrt(mean((log(pred_value) - log(T_value))^2))
}

train_predict = function(train_data, test_data, reg_functions, output_filename, true_test_y = NULL){
  r = Data_preprocessing(train_data, test_data)
  yhat_test = exp(reg_functions(r$train_data, r$test_data))
  if (!is.null(true_test_y)){
   cat("RMSE is:", log_RMSE(yhat_test, true_test_y), "\n")
  }
  output = cbind(test_data$PID, yhat_test)
  colnames(output) = c("PID", "Sale_Price")
  write.csv(output, output_filename, row.names = FALSE)
}
####spilt data
#all_data = read.csv("Ames_data.csv")
#Project1_test_id =  read.table("Project1_test_id.txt", quote="\"", comment.char="")
#text_index = 5
#cat("Test ID index:", text_index, "\n")
#test_PID = Project1_test_id[,text_index]
#train_data = all_data[!all_data$PID %in% test_PID,]
#test_data = all_data[all_data$PID %in% test_PID,!colnames(all_data) %in% c("Sale_Price")]
#true_test_value = all_data[all_data$PID %in% test_PID,]$Sale_Price
#write.csv(train_data, "train.csv", row.names = FALSE)
#write.csv(test_data, "test.csv", row.names = FALSE)
####test

start_time = proc.time()[3]
train_predict(train_data, test_data, rF_pred, "mysubmission1.txt", true_test_value)
Running_time1 = proc.time()[3]-start_time
cat("Running_time of Random Forset is ",Running_time1,"s")
start_time = proc.time()[3]
train_predict(train_data, test_data, Xgboost_pred, "mysubmission2.txt", true_test_value)
Running_time2 = proc.time()[3]-start_time
cat("Running_time of Boosting is ",Running_time2,"s")

