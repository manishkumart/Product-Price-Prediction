

#' Data manipulation, text cleaning and feature engineering for the training data
#' Sequence followed is:
#' 1. Data manipulation
#' 2. Text cleaning
#' 3. Feature engineering
#' @param train_df
#'
#' @return
#' @export
#'
#' @examples
#' Data_Manipulation(Training dataframe)
#' Text_Cleaning(Training dataframe)
#' Feature_Engineering(Training dataframe)
#'
Data_Manipulation <- function(train_df){

  # Replace brand_name column null with "unknown brand"
  train_df['brand_name'][train_df['brand_name'] == ""] <- "Unknown brand"
  train_df['brand_name'] = na.fill(train_df['brand_name'], "Unknown brand")

  # Replace category_name null with "Other"

  train_df['category_name'][train_df['category_name'] == ""] <- "Other/Other/Other"
  train_df['category_name'] = na.fill(train_df['category_name'], "Other/Other/Other")

  # Replace item_description null with "No description"

  train_df['item_description'][train_df['item_description'] == ""] <- "No description"
  train_df['item_description'] = na.fill(train_df['item_description'], "No description")

  # Seperating category variable into 3 categories


  category_name = train_df$category_name
  train_df = train_df  %>% separate(category_name, c("main_category", "subcat_1", "subcat_2"), sep="/")
  train_df$category_name = category_name

  #Label enncoding category,brand_name,subcat_1,subcat_2
  lbl_main_category <- LabelEncoder$new()
  lbl_main_category$fit(train_df$main_category)

  lbl_brand_name <- LabelEncoder$new()
  lbl_brand_name$fit(train_df$brand_name)

  lbl_subcat_1 <- LabelEncoder$new()
  lbl_subcat_1$fit(train_df$subcat_1)

  lbl_subcat_2 <- LabelEncoder$new()
  lbl_subcat_2$fit(train_df$subcat_2)

  train_df$n_main_category <- lbl_main_category$transform(train_df$main_category)
  train_df$n_brand_name = lbl_brand_name$transform(train_df$brand_name)
  train_df$n_subcat_1 = lbl_subcat_1$transform(train_df$subcat_1)
  train_df$n_subcat_2 = lbl_subcat_2$transform(train_df$subcat_2)

  print("Data successfully manipulated")
  return(train_df)
}
Text_Cleaning <- function(train_df){
  # Text cleaning - Stemming and Lemmatization for item description
  # Removing the rows which is not having proper description
  train_df <- subset(train_df, item_description != "No description yet")

  #replace unwanted symbols
  train_df$description = str_replace_all(train_df$item_description, "[[:punct:]]", " ")

  #Lemmatization
  train_df$clean_description = lemmatize_strings(train_df$description)


  # Making all the text lower case
  train_df$lower_clean_description = tolower(train_df$clean_description)


  print("Text successfully manipulated")
  return(train_df)

}

Feature_Engineering <- function(train_df){

  # Countvectorizer

  cv <- CountVectorizer$new(min_df = 0.7, max_features = 5000, remove_stopwords = TRUE)
  print("CountVectorizer",Sys.time())
  cv$fit(train_df$lower_clean_description)
  print("CoutVectorizer",Sys.time())
  cv_features = cv$transform(train_df$lower_clean_description)
  cv_df = train_df[ , c('item_condition_id', 'n_main_category', 'n_subcat_1', 'n_subcat_2', 'n_brand_name', 'price', 'shipping')]
  colnames(cv_df) <- c('item_condition_id', 'n_main_category', 'n_subcat_1', 'n_subcat_2', 'n_brand_name', 'n_price', 'n_shipping')

  cv_df = cbind(cv_df,data.frame(cv_features))
  cv_df = data.frame(lapply(cv_df, as.integer))

  # TF-IDF

  tf <- TfIdfVectorizer$new(min_df = 0.7, max_features = 5000, remove_stopwords = TRUE)
  print("TF-IDF",Sys.time())
  tf$fit(train_df$lower_clean_description)
  print("TF-IDF",Sys.time())
  tf_features = tf$transform(train_df$lower_clean_description)
  tf_df = train_df[ , c('item_condition_id', 'n_main_category', 'n_subcat_1', 'n_subcat_2', 'n_brand_name', 'price', 'shipping')]
  colnames(tf_df) <- c('item_condition_id', 'n_main_category', 'n_subcat_1', 'n_subcat_2', 'n_brand_name', 'n_price', 'n_shipping')

  tf_df = cbind(tf_df,data.frame(tf_features))
  tf_df = data.frame(lapply(tf_df, as.integer))

  new_list <- list("countvectorizer"= cv_df, "tfidf"= tf_df)
  return(new_list)

}
