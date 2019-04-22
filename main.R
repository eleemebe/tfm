library(caret)

base_directory <- "~/Documents/TFM"
datasets_dir <- "DATASETS"
setwd(paste0(base_directory, "/", datasets_dir))

# Function to find all the files that contains the data and the information about the data to use.
# @return: dataframe containing the dataset identifier, the path of its metadata file and the path to the dataset.
#          The returned paths are the relative paths to the working directory path, that should be set previously.
locate_input_files <- function(){
  files <- list.files(path = ".", recursive = TRUE)
  
  df_files <- data.frame(matrix(ncol=3, nrow = 0))
  colnames(df_files) <- c("dataset_id", "dataset_metadata", "dataset_data")
  
  
  for(file in files){
    print(file)
    
    if(grepl("METADATA", file)){
      
      dataset_id <- strsplit(file, "/")[[1]][1]
      df_files[nrow(df_files)+1, ] <- list(dataset_id = dataset_id, dataset_metadata = "", dataset_data= "")
      
      df_files[df_files[,'dataset_id']==dataset_id,'dataset_metadata'] <- file
      
      metadata_df <- parse_metadata(file)
      dataset_data <- metadata_df[which(metadata_df$key=='data'),'value']
      df_files[df_files[,'dataset_id']==dataset_id,'dataset_data'] <- paste0(dataset_id, "/", dataset_data)
      # lines <- readLines(file, warn=FALSE)
      # for(line in lines){
      #   if(grepl("data:", line)){
      #     dataset_data <- strsplit(line, " ")[[1]][2]
      #     df_files[df_files[,'dataset_id']==dataset_id,'dataset_data'] <- paste0(dataset_id, "/", dataset_data)
      #   }
      # }
      
      
    }
  }
  return(df_files)
}
  
input_file_df <- locate_input_files()

#
#
#
#

parse_metadata <- function(metadata_file_path) {
  metadata_df <- data.frame(matrix(ncol=2, nrow = 0))
  colnames(metadata_df) <- c("key", "value")
  lines <- readLines(metadata_file_path, warn=FALSE)
  for(line in lines){
    print(line)
    field_key <- strsplit(line, ":")[[1]][1]
    field_value <- trimws(strsplit(line, ":")[[1]][2])
    print(field_value)
    metadata_df[nrow(metadata_df)+1, ] <- list(key = field_key, value = field_value)
  } 
  return(metadata_df)
}
metadata_df <- parse_metadata(input_file_df[1,2])




for (i in 1:length(df_files[,'dataset_data'])) {
#for (i in 1:1) {
  print(paste0("Reading file: ", files[i]))
  dataset_name <- strsplit(files[i], ".csv")
  
  data <- read.csv2(paste0("datasets/",files[i]), header = F, sep = ",")
  target_index <- dim(data)[2]
  
  # Convert target to factor
  data[,target_index] <- factor(data[,target_index])
  
  # Divide data in train and test sets
  train_percentage <- 0.75
  train_size <- floor(train_percentage*nrow(data))
  set.seed(111)
  sample <- sample.int(n=nrow(data), size= train_size, replace = F)
  train_data <- data[sample,]
  test_data <- data[-sample,]
  
  target <- data[,target_index]
  test_target <- target[-sample]
  
  models <- list("adaboost")#"glmboost",
  formula <- as.formula(paste(colnames(train_data)[target_index], ' ~ .' ))
  for(model in models){
    print(paste0("   Training model: ", model))
    #fit <- train(formula, data=train_data, method = model)
    fitControl <- trainControl(method = "cv", number = 2, allowParallel = T)
    fit <- train(formula, data=train_data, method = model, trControl = fitControl)
    
    predictions <- predict.train(fit, newdata=test_data, type = "prob") #type="prob" para la ROC
    output_filename <- write.csv2(predictions, file=paste0("output/",dataset_name,"_",model ,".csv"))
    
    #conf <- confusionMatrix(data=predictions, reference = test_target)
    #print(paste0("      F1 score: ", conf$byClass[7]))
  }
  
}