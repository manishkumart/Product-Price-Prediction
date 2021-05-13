#' This script consists of the models like Decision tree, Random forest, SVM, Gradient boosting, Neural networks.
#' It gives the comparision between count vectorizer and TF-IDF
#'
#' @return
#' @export
#'
#' @examples
#' cv_df = featured engineered count vectorized data frame
#' cv_df = featured engineered TF-IDF data frame
#' Model_Building(cv_df, tf_df)
#' ...............................................................................................................
#' Function returns a list which is as follows
#'
#' list("DecisionTree_cv"=cv_dt,"DecisionTree_tf"= tf_dt,
#'      "RandomForest_cv"=cv_rf, "RandomForest_tf"=tf_rf,
#'      "SVM_cv" = cv_svm, "SVM_tf" = tf_svm,
#'      "GradientBoosting_cv"=cv_gbm,"GradientBoosting_tf"=tf_gbm,
#'      "NeuralNetworks_cv" = cv_nn,"NeuralNetworks_tf" = tf_nn,
#'      "tf_model" = tf_metrics,"cv_model"=cv_metrics)
#' ...............................................................................................................
#' To access each object in the list, save the return type in a object like below
#'
#' model_result = Model_Building(cv_df, tf_df)
#' ...............................................................................................................
#'
#' model$DecisionTree_cv -- Decision tree model with count vectorizer feature
#' model$DecisionTree_tf -- Decision tree model with TF-IDF feature
#' ...............................................................................................................
#'
#' model$RandomForest_cv -- Random forest model with count vectorizer feature
#' model$RandomForest_tf -- Random forest model with TF-IDF feature
#'
#' ...............................................................................................................
#'
#' model$SVM_cv -- SVM model with count vectorizer feature
#' model$SVM_tf -- SVM model with TF-IDF feature
#'
#' ...............................................................................................................
#'
#' model$GradientBoosting_cv -- Gradient boosting with count vectorizer feature
#' model$GradientBoosting_tf -- Gradient boosting with TF-IDF feature
#'
#' ...............................................................................................................
#'
#' model$NeuralNetworks_cv -- Neural networks with count vectorizer feature
#' model$NeuralNetworks_tf -- Neural networks with TF-IDF feature
#'
#' ...............................................................................................................
#'
#' model$tf_model -- Dataframe with metrics MAE, MAPE, RMSE, MSE of the above models with TF-IDF feature
#' model$cv_model -- Dataframe with metrics MAE, MAPE, RMSE, MSE of the above models with count vectorizer feature
#'
#'................................................................................................................
#'
Model_Building <- function(cv_df, tf_df){

  # Decision Tree
  cv_dt <- rpart(n_price ~ ., data = cv_df) # Countvectorizer
  tf_dt <- rpart(n_price ~ ., data = tf_df) # TF-IDF

  # Random Forest
  cv_rf <- randomForest(n_price ~ ., data = cv_df, mtry = 100, importance = TRUE) # Countvectorizer
  tf_rf <- randomForest(n_price ~ ., data = tf_df, mtry = 100, importance = TRUE) # TF-IDF

  # SVM
  cv_svm = svm(formula = n_price ~ .,data = cv_df, kernel = 'linear') # Countvectorizer
  tf_svm = svm(formula = n_price ~ .,data = tf_df,kernel = 'linear') # TF-IDF

  # Gradient Boosting
  cv_gbm = gbm(formula = n_price ~ .,data = cv_df,distribution = "gaussian",n.trees = 50) # Countvectorizer
  tf_gbm = gbm(formula = n_price ~ .,data = tf_df,distribution = "gaussian",n.trees = 50) # TF-IDF

  # Neural Networks
  cv_nn = neuralnet(n_price ~ ., data = cv_df, hidden = 3 , linear.output = T, stepmax=1e+06 ) # Countvectorizer
  tf_nn = neuralnet(n_price ~ ., data = tf_df, hidden = 3 , linear.output = T, stepmax=1e+06 ) # TF-IDF

  tf_metrics <- data.frame(Test = c("MAE","MAPE","RMSE","MSE"),
             DecisionTree = c(mae(tf_df$n_price, predict(tf_dt,tf_df)),mape(tf_df$n_price, predict(tf_dt,tf_df)),rmse(tf_df$n_price, predict(tf_dt,tf_df)),mse(tf_df$n_price, predict(tf_dt,tf_df))),
             RandomForest = c(mae(tf_df$n_price, predict(tf_rf,tf_df)),mape(tf_df$n_price, predict(tf_rf,tf_df)),rmse(tf_df$n_price, predict(tf_rf,tf_df)),mse(tf_df$n_price, predict(tf_rf,tf_df))),
             SVM = c(mae(tf_df$n_price, predict(tf_svm,tf_df)),mape(tf_df$n_price, predict(tf_svm,tf_df)),rmse(tf_df$n_price, predict(tf_svm,tf_df)),mse(tf_df$n_price, predict(tf_svm,tf_df))),
             nn = c(mae(tf_df$n_price, compute(tf_nn,head(tf_df,1))$net.result),mape(tf_df$n_price, compute(tf_nn,head(tf_df,1))$net.result),rmse(tf_df$n_price, compute(tf_nn,head(tf_df,1))$net.result),mse(tf_df$n_price, compute(tf_nn,head(tf_df,1))$net.result))
  )


  cv_metrics <- data.frame(Test = c("MAE","MAPE","RMSE","MSE"),
             DecisionTree = c(mae(cv_df$n_price, predict(cv_dt,cv_df)),mape(cv_df$n_price, predict(cv_dt,cv_df)),rmse(cv_df$n_price, predict(cv_dt,cv_df)),mse(cv_df$n_price, predict(cv_dt,cv_df))),
             RandomForest = c(mae(cv_df$n_price, predict(cv_rf,cv_df)),mape(cv_df$n_price, predict(cv_rf,cv_df)),rmse(cv_df$n_price, predict(cv_rf,cv_df)),mse(cv_df$n_price, predict(cv_rf,cv_df))),
             SVM = c(mae(cv_df$n_price, predict(cv_svm,cv_df)),mape(cv_df$n_price, predict(cv_svm,cv_df)),rmse(cv_df$n_price, predict(cv_svm,cv_df)),mse(cv_df$n_price, predict(cv_svm,cv_df))),
             nn = c(mae(cv_df$n_price, compute(cv_nn,head(cv_df,1))$net.result),mape(cv_df$n_price, compute(cv_nn,head(cv_df,1))$net.result),rmse(cv_df$n_price, compute(cv_nn,head(cv_df,1))$net.result),mse(cv_df$n_price, compute(cv_nn,head(cv_df,1))$net.result))
  )
  model_info <- list("DecisionTree_cv"=cv_dt,"DecisionTree_tf"= tf_dt,
                     "RandomForest_cv"=cv_rf, "RandomForest_tf"=tf_rf,
                     "SVM_cv" = cv_svm, "SVM_tf" = tf_svm,
                     "GradientBoosting_cv"=cv_gbm,"GradientBoosting_tf"=tf_gbm,
                     "NeuralNetworks_cv" = cv_nn,"NeuralNetworks_tf" = tf_nn,
                     "tf_model" = tf_metrics,"cv_model"=cv_metrics)

  return(model_info)
}



