
#' R Shiny Dashboard
# Build the UI using fluidPage function

ui <- fluidPage(

  titlePanel("Product Price Prediction"),

  selectInput("item_condition_id", "Item Condition ID", c(train_df$item_condition_id)),

  selectInput("shipping", "Shipping", c(train_df$shipping)),

  selectInput("main_category", "Main Category", c(train_df$main_category)),
  selectInput("subcat_1", "Subcategory 1", c(train_df$subcat_1)),

  selectInput("subcat_2", "Subcategory 2", c(train_df$subcat_2)),

  selectInput("brand_name", "Brand Name", c(train_df$brand_name)),

  textInput("item_description", "Item Description", ""),

  actionButton("submit", "Submit"),

  verbatimTextOutput("value")

)

server <- function(input, output, session) {

  data <- reactive({
    data.frame(item_condition_id = req(input$item_condition_id), n_shipping = req(input$shipping),
               n_main_category = lbl_main_category$transform(req(input$main_category)),
               n_brand_name = lbl_brand_name$transform(req(input$brand_name)),
               n_subcat_1 = lbl_subcat_1$transform(req(input$subcat_1)),
               n_subcat_2 = lbl_subcat_2$transform(req(input$subcat_2)),
               clean_description = gsub('[[:digit:]]+', '', lemmatize_strings(str_replace_all(req(input$item_description), "[[:punct:]]", " ")))
    )

  })

  tf_features <- reactive({
    manupulated_data = data()
    manupulated_data$clean_description = as.character(manupulated_data$clean_description)
    zz = tryCatch(tf$transform(manupulated_data$clean_description), error=function(e) 0)
    df = cbind(manupulated_data,data.frame(zz))
    df[rownames(rf$importance)[!(rownames(rf$importance) %in% colnames(df))]] = 0
    data.frame(lapply(df[rownames(rf$importance)], as.integer))
  })


  output$value <- renderText({
    df = tf_features()
    predict(rf,df[rownames(rf$importance)])

  })


  session$onSessionEnded(function() {
    stopApp()
  })

}

shinyApp(ui, server)


