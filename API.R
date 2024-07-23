library(plumber)
library(readr)

#Read in the data.
api_diabetes_data <- read_csv("diabetes_binary_health_indicators_BRFSS2015.csv")

#Convert all relevant predictors to factors.
api_diabetes_data$Diabetes_binary <- as.factor(api_diabetes_data$Diabetes_binary)
api_diabetes_data$HighBP <- as.factor(api_diabetes_data$HighBP)
api_diabetes_data$HighChol <- as.factor(api_diabetes_data$HighChol)
api_diabetes_data$HeartDiseaseorAttack <- as.factor(api_diabetes_data$HeartDiseaseorAttack)
api_diabetes_data$GenHlth <- as.factor(api_diabetes_data$GenHlth)
api_diabetes_data$DiffWalk <- as.factor(api_diabetes_data$DiffWalk)

#Use the glm function to indicated generalized linear model.
#Using logistic model #3 with interactions between HighChol & HDorAttack and interaction between GenHlth & DiffWalk.
#Use the data set read in above.
#Family = binomial indicates a logistic model. 
final_model_diabetes <- glm(Diabetes_binary ~ HighBP + HighChol*HeartDiseaseorAttack + GenHlth*DiffWalk,
                            data = api_diabetes_data,
                            family = binomial)

#When converting to as.numeric it converts to 1 and 2 so subtract 1. 
#The means will be used to pass as the default values.
meanHBP <- mean(as.numeric(api_diabetes_data$HighBP) - 1)
meanChol <- mean(as.numeric(api_diabetes_data$HighChol) - 1)
meanHD <- mean(as.numeric(api_diabetes_data$HeartDiseaseorAttack) - 1)
meanGen <- mean(as.numeric(api_diabetes_data$GenHlth) - 1)
meanWalk <- mean(as.numeric(api_diabetes_data$DiffWalk) - 1)

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
#The default values are the means.
function(HighBP = meanHBP, 
         HighChol = meanChol, 
         HeartDiseaseorAttack = meanHD,
         GenHlth = meanGen, 
         DiffWalk = meanWalk){
  #Since the means are naturally not a level of the factor, first convert to an integer then convert to a factor so that it can be passed in the model.
  HighBP <- as.factor(as.integer(HighBP))
  HighChol <- as.factor(as.integer(HighChol))
  HeartDiseaseorAttack <- as.factor(as.integer(HeartDiseaseorAttack))
  GenHlth <- as.factor(as.integer(GenHlth))
  DiffWalk <- as.factor(as.integer(DiffWalk))
 
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

#No HBP, no High Cholesterol, no difficulty walking, no heart disease, and poor health rating.
#http://127.0.0.1:8000/pred?HighBP=0&HighChol=0&HeartDiseaseorAttack=0&GenHlth=5&DiffWalk=0

#HBP, High Cholesterol, no heart disease or attack, very good general health, and no difficulty walking.
#http://127.0.0.1:8000/pred?HighBP=1&HighChol=1&HeartDiseaseorAttack=0&GenHlth=2&DiffWalk=0

#HBP, no high cholesterol, no heart disease or attack, fair general health, and no difficulty walking.
#http://127.0.0.1:8000/pred?HighBP=1&HighChol=0&HeartDiseaseorAttack=0&GenHlth=4&DiffWalk=0
