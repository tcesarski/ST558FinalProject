library(plumber)

api_diabetes_data <- read_csv("diabetes_binary_health_indicators_BRFSS2015.csv")
trControl <- trainControl (method = "cv", number =5)


#Set the seed for reproducibility.
set.seed(50)
#Use 5 fold cross validation. To use logLoss as a metric later, set classProbs = TRUE and summaryFunction = mnLogLoss.
trctrl <- trainControl(method = "cv", 
                       number = 5, 
                       classProbs = TRUE, 
                       summaryFunction = mnLogLoss)

#Train the first logistic model using all 5 main effect terms and interaction terms between High Cholesterol and Heart Disease/Attack and an interaction between General Health and Difficulty Walking.
#Use the training data set.
#The method is glm (generalized linear model) with family as binomial to fit logistic.
#Preprocess the data by centering and scaling.
#Use the train control above to tell the model how to train.
log_fit_3 <- train(Diabetes_binary ~ HighBP + HighChol*HeartDiseaseorAttack + GenHlth*DiffWalk,
                   data = diabetes_train,
                   method = "glm",
                   family = "binomial",
                   metric = "logLoss",
                   preProcess = c("center", "scale"),
                   trControl = trctrl)
#Print out information about the model. The logLoss was 0.3327.
log_fit_3


best_diabetes_model <- function(api_diabetes_data) {
  predictions <- predict(model, newdata = input_data)
  return(predictions)
}

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
function(HighBP, HighChol, HeartDiseaseorAttack, GenHlth, DiffWalk){
  
  HighBP <- factor(HighBP, levels = c(0, 1), labels = c("no_hbp", "hbp"))
  
  HighChol <- factor(HighChol, levels = c(0, 1), labels = c("no_high_chol", "high_chol"))
  
  HeartDiseaseorAttack <- factor(HeartDiseaseorAttack, levels = c(0, 1), labels = c("no_hd_or_attack", "hd_or_attack"))
  
  GenHlth <- factor(GenHlth, levels = c(1, 2, 3, 4, 5), labels = c("excellent", "very_good", "good", "fair", "poor"))
  
  DiffWalk <- factor(DiffWalk, levels = c(0, 1), labels = c("no_difficulty", "difficulty"))
  
  predictors_data <- data.frame(HighBP = HighBP,
                                HighChol = HighChol,
                                HeartDiseaseorAttack = HeartDiseaseorAttack,
                                GenHlth = GenHlth,
                                DiffWalk = DiffWalk)
  
  diabetes_predict <- predict(log_fit_3, newdata = predictors_data, type = "response")
}
