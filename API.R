library(plumber)
library(tidyverse)
library(caret)
api_diabetes_data <- read_csv("diabetes_binary_health_indicators_BRFSS2015.csv")
api_diabetes_data$Diabetes_binary <- as.factor(api_diabetes_data$Diabetes_binary)
api_diabetes_data$HighBP <- as.factor(api_diabetes_data$HighBP)
api_diabetes_data$HighChol <- as.factor(api_diabetes_data$HighChol)
api_diabetes_data$HeartDiseaseorAttack <- as.factor(api_diabetes_data$HeartDiseaseorAttack)
api_diabetes_data$GenHlth <- as.factor(api_diabetes_data$GenHlth)
api_diabetes_data$DiffWalk <- as.factor(api_diabetes_data$DiffWalk)


final_model_diabetes <- glm(Diabetes_binary ~ HighBP + HighChol*HeartDiseaseorAttack + GenHlth*DiffWalk,
                            data = api_diabetes_data,
                            family = binomial)

meanHBP <- mean(as.numeric(api_diabetes_data$HighBP) - 1)
meanChol <- mean(as.numeric(api_diabetes_data$HighChol) - 1)
meanHD <- mean(as.numeric(api_diabetes_data$HeartDiseaseorAttack) - 1)
meanGen <- mean(as.numeric(api_diabetes_data$GenHlth) - 1)
meanWalk <- mean(as.numeric(api_diabetes_data$DiffWalk) - 1)

#Info endpoint
#* @get /info
function(){
  info <- paste("Taylor Cesarski", 
                        "https://tcesarski.github.io/ST558FinalProject/",
                        sep = " ")
  return(info)
}

#http://localhost:PORT/info

#Model
#* @param HighBP High Blood Pressure
#* @param HighChol High Cholesterol
#* @param HeartDiseaseorAttack Heart Disease or Attack
#* @param GenHlth General Health Rating
#* @param DiffWalk Difficulty Walking
#* @get /pred
function(HighBP = meanHBP, 
         HighChol = meanChol, 
         HeartDiseaseorAttack = meanHD,
         GenHlth = meanGen, 
         DiffWalk = meanWalk){
  
  HighBP <- as.factor(as.integer(HighBP))
  HighChol <- as.factor(as.integer(HighChol))
  HeartDiseaseorAttack <- as.factor(as.integer(HeartDiseaseorAttack))
  GenHlth <- as.factor(as.integer(GenHlth))
  DiffWalk <- as.factor(as.integer(DiffWalk))
  
  predictors_data <- data.frame(HighBP = HighBP,
                                HighChol = HighChol,
                                HeartDiseaseorAttack = HeartDiseaseorAttack,
                                GenHlth = GenHlth,
                                DiffWalk = DiffWalk)
  
  diabetes_predict <- predict(final_model_diabetes, newdata = predictors_data, type = "response")
  
  return(paste0("The probability of diabetes is ", diabetes_predict))
  
}
