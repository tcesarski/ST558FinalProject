library(plumber)
library(readr)

#Read in the data.
api_diabetes_data <- read_csv("diabetes_binary_health_indicators_BRFSS2015.csv")

#Convert all relevant predictors to factors with relevant level names.
api_diabetes_data$Diabetes_binary <- factor(api_diabetes_data$Diabetes_binary, levels = c(0, 1), labels = c("no_diabetes", "diabetes"))

api_diabetes_data$HighBP <- factor(api_diabetes_data$HighBP, levels = c(0, 1), labels = c("no_hbp", "hbp"))

api_diabetes_data$HighChol <- factor(api_diabetes_data$HighChol, levels = c(0, 1), labels = c("no_high_chol", "high_chol"))

api_diabetes_data$HeartDiseaseorAttack <- factor(api_diabetes_data$HeartDiseaseorAttack, levels = c(0, 1), labels = c("no_hd_or_attack", "hd_or_attack"))

api_diabetes_data$GenHlth <- factor(api_diabetes_data$GenHlth, levels = c(1, 2, 3, 4, 5), labels = c("excellent", "very_good", "good", "fair", "poor"))

api_diabetes_data$DiffWalk <- factor(api_diabetes_data$DiffWalk, levels = c(0, 1), labels = c("no_difficulty", "difficulty"))

#Use the glm function to indicated generalized linear model.
#Using logistic model #3 with interactions between HighChol & HDorAttack and interaction between GenHlth & DiffWalk.
#Use the data set read in above.
#Family = binomial indicates a logistic model. 
final_model_diabetes <- glm(Diabetes_binary ~ HighBP + HighChol*HeartDiseaseorAttack + GenHlth*DiffWalk,
                            data = api_diabetes_data,
                            family = binomial)

#Print out information about the final model.
final_model_diabetes

#Determine most prevalent class of each categorical variable to use as defaults.

#No high blood pressure is more prevalent than high blood pressure.
table(api_diabetes_data$HighBP)

#No high cholesterol is more prevalent than high cholesterol.
table(api_diabetes_data$HighChol)

#No heart disease or attack is more prevalent than heart disease or attack.
table(api_diabetes_data$HeartDiseaseorAttack)

#Very good is the most prevalent class of general health rating.
table(api_diabetes_data$GenHlth)

#No difficulty walking is more prevalent than difficulty walking.
table(api_diabetes_data$DiffWalk)


#Info endpoint
#This endpoint doesn't take any parameters.
#It outputs my name and my github pages site.
#* @get /info
function(){
  info <- paste("Taylor Cesarski", 
                "https://tcesarski.github.io/ST558FinalProject/",
                sep = " ")
  return(info)
}

#Model
#The model takes in 5 parameters (predictors used in model)
#The endpoint is called pred.
#* @param HighBP High Blood Pressure
#* @param HighChol High Cholesterol
#* @param HeartDiseaseorAttack Heart Disease or Attack
#* @param GenHlth General Health Rating
#* @param DiffWalk Difficulty Walking
#* @get /pred
#The default values are the most prevalent class of each.
function(HighBP = "no_hbp", 
         HighChol = "no_high_chol", 
         HeartDiseaseorAttack = "no_hd_or_attack",
         GenHlth = "very_good", 
         DiffWalk = "no_difficulty"){
  
  
  #Create a data frame to pass to the predict function. 
  predictors_data <- data.frame(HighBP = HighBP,
                                HighChol = HighChol,
                                HeartDiseaseorAttack = HeartDiseaseorAttack,
                                GenHlth = GenHlth,
                                DiffWalk = DiffWalk)
  
  #Predict the values using response (so you get a probability) based on the model above.
  diabetes_predict <- predict(final_model_diabetes, newdata = predictors_data, type = "response")
  
  #Return a sentence of the probability of having diabetes based on inputs.
  return(paste0("The probability of diabetes is ", diabetes_predict))
  
}

#Example calls to API: 

#No HBP, no High Cholesterol, no heart disease, poor general health, and no difficulty walking.
#http://127.0.0.1:8000/pred?HighBP=no_hbp&HighChol=no_high_chol&HeartDiseaseorAttack=no_hd_or_attack&GenHlth=poor&DiffWalk=no_difficulty

#HBP, High Cholesterol, no heart disease or attack, fair general health, and difficulty walking.
#http://127.0.0.1:8000/pred?HighBP=hbp&HighChol=high_chol&HeartDiseaseorAttack=no_hd_or_attack&GenHlth=fair&DiffWalk=difficulty

#HBP, no high cholesterol, no heart disease or attack, excellent general health, and no difficulty walking.
#http://127.0.0.1:8000/pred?HighBP=hbp&HighChol=no_high_chol&HeartDiseaseorAttack=no_hd_or_attack&GenHlth=excellent&DiffWalk=no_difficulty
