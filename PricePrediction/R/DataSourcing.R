#Reading the data
#' Function that Reads data from the source
#'
#' @return
#' @export
#'
#' @examples
#' read_data()
#' It returns a list object
#' data = read_data()
#' data$train_data -- Train data
#' data$test_data -- Test data
#'
read_data <-function(){
  train_df = read.csv(file = 'train.tsv', sep = '\t', header = TRUE)
  test_df = read.csv(file = 'test.tsv', sep = '\t', header = TRUE)
  data_list <-list("train_data"=train_df, "test_data"=test_df)
  return(data_list)
}


