library(readxl)
library(caret)
library(PRROC)
library(ROCR)
library(stringr)

base_directory <- "~/Documents/TFM"
datasets_dir <- "DATASETS"
setwd(paste0(base_directory, "/", datasets_dir))

#
#
#
#

parse_metadata <- function(metadata_file_path) {
  metadata_df <- data.frame(matrix(ncol=2, nrow = 0))
  colnames(metadata_df) <- c("key", "value")
  lines <- readLines(metadata_file_path, warn=FALSE)
  for(line in lines){
    field_key <- strsplit(line, ":")[[1]][1]
    field_value <- trimws(strsplit(line, ":")[[1]][2])
    metadata_df[nrow(metadata_df)+1, ] <- list(key = field_key, value = field_value)
  } 
  return(metadata_df)
}

# Function to find all the files that contains the data and the information about the data to use.
# @return: dataframe containing the dataset identifier, the path of its metadata file and the path to the dataset.
#          The returned paths are the relative paths to the working directory path, that should be set previously.
locate_input_files <- function(){
  files <- list.files(path = ".", recursive = TRUE)
  
  df_files <- data.frame(matrix(ncol=4, nrow = 0))
  colnames(df_files) <- c("dataset_id", "dataset_metadata", "dataset_data", "external_label_data")
  
  
  for(file in files){
    if(grepl("METADATA", file)){
      dataset_id <- strsplit(file, "/")[[1]][1]
      df_files[nrow(df_files)+1, ] <- list(dataset_id = dataset_id, dataset_metadata = "", dataset_data= "")
      
      df_files[df_files[,'dataset_id']==dataset_id,'dataset_metadata'] <- file
      
      metadata_df <- parse_metadata(file)
      dataset_data <- metadata_df[which(metadata_df$key=='data'),'value']
      df_files[df_files[,'dataset_id']==dataset_id,'dataset_data'] <- paste0(dataset_id, "/", dataset_data)
      
      external_label_data <- metadata_df[which(metadata_df$key=='labels'),'value']
      print(external_label_data)
      if(length(external_label_data)>0){
        df_files[df_files[,'dataset_id']==dataset_id,'external_label_data'] <- paste0(dataset_id, "/", external_label_data)
      }
      else{
        df_files[df_files[,'dataset_id']==dataset_id,'external_label_data'] <- ""
      }
    }
  }
  return(df_files)
}

load_data <- function(metadata_df, data_file) {
  format <- metadata_df[which(metadata_df$key=='format'),'value']
  sep=""
  if (format=="xlsx") {
    raw_data <- as.data.frame(read_excel(data_file))
  }else if (format=='comma'){
    sep=","
    raw_data <- read.csv2(data_file, header = F, sep = sep)
  }else if (format=='tab'){
    sep="\t"
    raw_data <- read.csv2(data_file, header = F, sep = sep)
  }else if (format=="semicolon"){
    sep=";"
    raw_data <- read.csv2(data_file, header = F, sep = sep)
  }else if (format=="space") {
    sep=""
    raw_data <- read.csv2(data_file, header = F, sep = sep)
  }else{
    print("Format not captured")
    print(format)
  }
  return(raw_data)
}

convert_data <- function(metadata_df, loaded_data, loaded_external_label_data = data.frame()) {
  columns_df <- metadata_df[which(grepl("column", metadata_df$key)),]
  preprocessed_df = loaded_data
  if(sum(columns_df['key'] == 'column_all')==1){
    column_type <- metadata_df[which(metadata_df['key'] == 'column_all'),'value']
    if(column_type == "numerical"){
      preprocessed_df <- data.frame(lapply(loaded_data, function(x) as.numeric(x)))
    } else {
      preprocessed_df <- data.frame(lapply(loaded_data, function(x) as.factor(x)))
    }
    preprocessed_df$target <- factor(loaded_external_label_data[,])
  } else {
    for (i in 1:nrow(columns_df)) {
      column_index <- i
      column_type <- metadata_df[which(metadata_df['key'] == paste0('column_',column_index)),'value']
      if(column_type == "numerical"){
        preprocessed_df[,column_index] <- as.numeric(loaded_data[,column_index])
      } else if(column_type == "categorical"){
        preprocessed_df[,column_index] <- factor(loaded_data[,column_index])
      } else if(column_type == "target"){
        preprocessed_df[,column_index] <- factor(loaded_data[,column_index])
        colnames(preprocessed_df)[column_index] <- "target"
      }
    }
  }
  
  for (column in names(preprocessed_df)){
    if(is.factor(preprocessed_df[[column]])){
      if(length(levels(preprocessed_df[[column]]))==1){
        preprocessed_df <- preprocessed_df[,!(names(preprocessed_df) %in% column)]
      }else {
        # Para hacer que los nombres del target coincidan con los ya entrenados
        if(all(make.names(levels(preprocessed_df[[column]])) == levels(preprocessed_df[[column]]))){
          levels <- levels(preprocessed_df[[column]])
        }else{
          levels <- unique(c(preprocessed_df[[column]]))
        }
        preprocessed_df[[column]] <- factor(preprocessed_df[[column]], labels=make.names(levels))
      }
    }
  }
  
  return(preprocessed_df)
}


for (column in names(train_data)){
  if(is.factor(train_data[[column]])){
    if(length(levels(train_data[[column]]))==1){
      train_data <- train_data[,!(names(train_data) %in% column)]
    }
  }
}


remove_NAs <- function(df){
  df <- df[complete.cases(df), ]
  return(df)
}

extract_train_test_datasets <- function(df) {
  train_percentage <- 0.75
  train_size <- floor(train_percentage*nrow(df))
  set.seed(111)
  sample <- sample.int(n=nrow(df), size= train_size, replace = F)
  train_data <- df[sample,]
  test_data <- df[-sample,]
  return(list(train_data, test_data))
}

load_model_properties <- function(train_data){
  model_properties <- data.frame(matrix(ncol=2, nrow = 0))
  colnames(model_properties) <- c('model','tgrid')
  
  tgrid_rf <- expand.grid(mtry = 2:(ncol(train_data)-1)) 
  model_properties[nrow(model_properties)+1, ] <- list(model = "rf", tgrid = list(tgrid_rf))
  
  tgrid_svmLinear <- expand.grid(C = c(10^-5, 10^-3, 10^-1, 10^1, 10^3, 10^5)) 
  model_properties[nrow(model_properties)+1, ] <- list(model = "svmLinear", tgrid = list(tgrid_svmLinear))
  
  tgrid_svmPoly <- expand.grid(degree = 0:6, scale = c(10^-15, 10^-13, 10^-11, 10^-9,10^-7,10^-5,10^-3,10^-1,10, 10^3), C = c(10^-5, 10^-3, 10^-1, 10^1, 10^3, 10^5)) 
  model_properties[nrow(model_properties)+1, ] <- list(model = "svmPoly", tgrid = list(tgrid_svmPoly))
  
  tgrid_glm <- NULL
  model_properties[nrow(model_properties)+1, ] <- list(model = "glm", tgrid = list(tgrid_glm))
  
  tgrid_rpart <- expand.grid(cp = c(0,0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1)) 
  model_properties[nrow(model_properties)+1, ] <- list(model = "rpart", tgrid = list(tgrid_rpart))
  
  tgrid_xgbLinear  <- expand.grid(nrounds=2, lambda= c(0, 0.25, 0.5, 0.75, 1), alpha=c(0, 0.25, 0.5, 0.75, 1), eta = c(0.01, 0.001, 0.0001)) 
  model_properties[nrow(model_properties)+1, ] <- list(model = "xgbLinear", tgrid = list(tgrid_xgbLinear))
  
  tgrid_ada  <- expand.grid(iter= c(20, 50, 100, 500), maxdepth=1:5, nu=c(0, 0.25, 0.5, 0.75, 1)) 
  model_properties[nrow(model_properties)+1, ] <- list(model = "ada", tgrid = list(tgrid_ada))
  
  tgrid_nb  <- expand.grid(fL = 0:5, usekernel = c(TRUE, FALSE), adjust = 0:5) 
  model_properties[nrow(model_properties)+1, ] <- list(model = "nb", tgrid = list(tgrid_nb))
  
  tgrid_knn  <- expand.grid(k = 1:20) 
  model_properties[nrow(model_properties)+1, ] <- list(model = "knn", tgrid = list(tgrid_knn))
  
  return(model_properties)
}



#models <- list('rf', 'svmLinear', 'svmPoly', 'glm', 'rpart', 'ada','xgbLinear',  'nb', 'knn') #glm, rpart  
models <- list()

input_file_df <- locate_input_files()

for (dataset_index in 1:dim(input_file_df)[1]){
  dataset_id <- input_file_df[dataset_index,1]
  print(dataset_id)
  metadata_path <- input_file_df[dataset_index,2]
  data_path <- input_file_df[dataset_index,3]
  external_label_data_path <- input_file_df[dataset_index,4]
  metadata_df <- parse_metadata(metadata_path)
  loaded_data <- load_data(metadata_df, data_path)
  loaded_data_without_NAs <- remove_NAs(loaded_data)
  loaded_external_label_data_without_NAs <- NULL
  if(input_file_df[dataset_index,4] != ""){
    loaded_external_label_data <- load_data(metadata_df, external_label_data_path)
    loaded_external_label_data_without_NAs <- remove_NAs(loaded_external_label_data)
  }
  converted_df <- convert_data(metadata_df, loaded_data_without_NAs, loaded_external_label_data_without_NAs)
  train_test <- extract_train_test_datasets(converted_df)
  train_data <- train_test[1][[1]]
  test_data <- train_test[2][[1]]
  actuals_filename <- write.csv(test_data$target, file=paste0("../actuals/",dataset_id,"_actuals.csv"))
  target_index <- match('target', colnames(train_data))
  formula <- as.formula(paste(colnames(train_data)[target_index], ' ~ .' ))
  model_properties <- load_model_properties(train_data)
  
  for(model in models){
     try({
        print(model)
        fitControl <- trainControl(method = "cv", number = 2, allowParallel = T, classProbs = TRUE)
        tgrid = model_properties[which(model_properties$model==model), 'tgrid'][[1]]
        fit <- train(formula, data=train_data, method = model, trControl = fitControl, tuneGrid = tgrid)
        predictions <- predict.train(fit, newdata=test_data, type = "prob") #type="prob" para la ROC
        output_filename <- write.csv(predictions, file=paste0("../output/",dataset_id,"_",model ,".csv"))
       })
  }
}


actuals_dir <- "actuals"
output_dir <- "output"

setwd(paste0(base_directory, "/", output_dir))

df_results <- data.frame(matrix(ncol=3, nrow = 0))
colnames(df_results) <- c("dataset_model", "model", "f1")
files <- list.files(path = ".", recursive = TRUE)

for(file in files){
  print(file)
  dataset_id <- substr(file,1,str_locate(file, "[^_]+$")[1]-2)
  dataset_model <- sub(".csv","",file)
  model <- substr(dataset_model, str_locate(dataset_model, "[^_]+$")[1],nchar(dataset_model))
  
  setwd(paste0(base_directory, "/", actuals_dir))
  actuals <- read.csv2(paste0(dataset_id,"_actuals.csv"), header = T, sep = ",")$x
  
  setwd(paste0(base_directory, "/", output_dir))
  
  # Choose less frequent class as prediction target (positive)
  tt = table(actuals)
  positive_class = names(tt[which.min(tt)])
  negative_class = levels(actuals)[levels(actuals) != positive_class]
  
  
  probs <- as.numeric(read.csv2(file, header = T, sep = ",", stringsAsFactors = F)[,positive_class])
  
  
  #probs <- as.numeric(read.csv2(file, header = T, sep = ",", stringsAsFactors = F)[,2])
  #probs_2 <- as.numeric(read.csv2(file, header = T, sep = ",", stringsAsFactors = F)[,3])
  ##results <- cbind(actuals,probs)
  #first_level = levels(actuals$x)[1]
  #second_level = levels(actuals$x)[2]
  #fg <- probs[actuals$x == first_level]
  #bg <- probs[actuals$x == second_level]
  #pr_1 <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
  #print(pr_1$auc.integral)
  #pr_2 <- pr.curve(scores.class0 = bg, scores.class1 = fg, curve = T)
  #print(pr_2$auc.integral)
  #pr_1 <- pr.curve(probs,probs_2)
  #print(pr_1$auc.integral)
  #pr_2 <- pr.curve(probs_2,probs)
  #print(pr_2$auc.integral)
  #if(pr_1$auc.integral > pr_2$auc.integral){
  #  prroc <- pr_1
  #}else{
  #  prroc <- pr_2
  #}
  
  pred <- prediction(probs,actuals, label.ordering = c(negative_class,positive_class))
  perf <- performance(pred,"f")
  bestF1ScoreIndex <- which.max(perf@"y.values"[[1]])
  f1 <- perf@"y.values"[[1]][bestF1ScoreIndex]
  # f1_1 <- perf@"y.values"[[1]][bestF1ScoreIndex]
  # 
  # pred_2 <- prediction(probs,actuals, label.ordering = c(levels(actuals[1])[2],levels(actuals[1])[1]))
  # perf_2 <- performance(pred_2,"f")
  # bestF1ScoreIndex_2 <- which.max(perf_2@"y.values"[[1]])
  # f1_2 <- perf_2@"y.values"[[1]][bestF1ScoreIndex_2]
  # 
  # if(f1_1 > f1_2){
  #   f1 <- f1_1
  # }else{
  #   f1 <- f1_2
  # }
  # 
  #prec = performance(pred,"prec")@"y.values"[[1]]
  #rec = performance(pred,"rec")@"y.values"[[1]]
  #prauc <- integrate(approxfun(data.frame(rec,prec)),0,1)$value
  
  
  #df_results[nrow(df_results)+1, ] <- list(dataset_model = dataset_model, prroc = list(prroc))
  #df_results[nrow(df_results)+1, ] <- list(dataset_model = dataset_model, prroc = prroc$auc.integral, f1 = f1)
  df_results[nrow(df_results)+1, ] <- list(dataset_model = dataset_model, model = model, f1 = f1)
}




# Agrupamos datasets
base_directory <- "~/Documents/TFM"
datasets_dir <- "DATASETS"
setwd(paste0(base_directory, "/", datasets_dir))
df_summary <- data.frame(matrix(ncol=5, nrow = 0))
colnames(df_summary) <- c("dataset_id", "num_examples", "num_variables", "perc_categorical_variables", "perc_numerical_variables")
for (dataset_index in 1:dim(input_file_df)[1]){
  dataset_id <- input_file_df[dataset_index,1]
  print(dataset_id)
  metadata_path <- input_file_df[dataset_index,2]
  data_path <- input_file_df[dataset_index,3]
  external_label_data_path <- input_file_df[dataset_index,4]
  metadata_df <- parse_metadata(metadata_path)
  loaded_data <- load_data(metadata_df, data_path)
  loaded_data_without_NAs <- remove_NAs(loaded_data)
  loaded_external_label_data_without_NAs <- NULL
  if(input_file_df[dataset_index,4] != ""){
    loaded_external_label_data <- load_data(metadata_df, external_label_data_path)
    loaded_external_label_data_without_NAs <- remove_NAs(loaded_external_label_data)
  }
  converted_df <- convert_data(metadata_df, loaded_data_without_NAs, loaded_external_label_data_without_NAs)
  examples_df = subset(converted_df,select=-c(target))
  num_examples = nrow(examples_df)
  num_variables = ncol(examples_df)
  perc_numerical_variables = sum(unlist(lapply(examples_df, is.numeric)))/num_variables
  perc_categorical_variables = sum(unlist(lapply(examples_df, is.factor)))/num_variables
  df_summary[nrow(df_summary)+1, ] <- list(dataset_id = dataset_id, num_examples = num_examples, num_variables = num_variables, perc_numerical_variables = perc_numerical_variables, perc_categorical_variables = perc_categorical_variables)
}

## Todo variables categoricas
has_all_categorical_variables = df_summary[df_summary[,"perc_categorical_variables"]==1,1]
pattern = paste(has_all_categorical_variables, collapse="|")
plot_data <- df_results[grepl(pattern,df_results[,"dataset_model"]),][,c("model","f1")]
boxplot(f1~model, data=plot_data)

## Todo variables numericas
has_all_numerical_variables = df_summary[df_summary[,"perc_numerical_variables"]==1,1]
pattern = paste(has_all_numerical_variables, collapse="|")
plot_data <- df_results[grepl(pattern,df_results[,"dataset_model"]),][,c("model","f1")]
boxplot(f1~model, data=plot_data)

## Mix de variables categoricas y numericas
has_some_numerical_variables = df_summary[df_summary[,"perc_numerical_variables"]>0 && df_summary["perc_numerical_variables"]<1,1]
pattern = paste(has_some_numerical_variables, collapse="|")
plot_data <- df_results[grepl(pattern,df_results[,"dataset_model"]),][,c("model","f1")]
boxplot(f1~model, data=plot_data)

## Por numero variables: menos de 10 columnas
has_less_than_ten_variables = df_summary[df_summary[,"num_variables"]<10,1]
pattern = paste(has_less_than_ten_variables, collapse="|")
plot_data <- df_results[grepl(pattern,df_results[,"dataset_model"]),][,c("model","f1")]
boxplot(f1~model, data=plot_data)

## Por numero variables: igual o mas de 10 columnas
has_more_than_ten_variables = df_summary[df_summary[,"num_variables"]>=10,1]
pattern = paste(has_more_than_ten_variables, collapse="|")
plot_data <- df_results[grepl(pattern,df_results[,"dataset_model"]),][,c("model","f1")]
boxplot(f1~model, data=plot_data)

## Por numero de ejemplos: menos de 500
has_less_than_500_examples = df_summary[df_summary[,"num_examples"]<500,1]
pattern = paste(has_less_than_500_examples, collapse="|")
plot_data <- df_results[grepl(pattern,df_results[,"dataset_model"]),][,c("model","f1")]
boxplot(f1~model, data=plot_data)

## Por numero de ejemplos: mas de 500
has_more_than_500_examples = df_summary[df_summary[,"num_examples"]>=500,1]
pattern = paste(has_more_than_500_examples, collapse="|")
plot_data <- df_results[grepl(pattern,df_results[,"dataset_model"]),][,c("model","f1")]
boxplot(f1~model, data=plot_data)



# NEXT STEPS: AGRUPAR DATASETS POR TIPOS PARA AVERIGURAR QUE MODELO FUNCIONA MEJOR CON QUE TIPO DD DATASETS. 



#fit <- train(formula, data=train_data, method = "rf", tuneGrid = tgrid, trControl = fitControl, ntree=10, metric="Kappa", allowParallel=F)


# tgrid_mxnet  <- expand.grid(layer1=c(64, 128, 256),  layer2=c(16, 32, 64), layer3=c(2,4,8,16),learning.rate=c(10^-5, 5*10^-5, 10^-4, 10^-3,10^-2, 1, 10),momentum=0.9, dropout=c(0, 0.1, 0.2, 0.3, 0.4, 0.5), activation='relu') 
# model_properties[nrow(model_properties)+1, ] <- list(model = "mxnet", tgrid = list(tgrid_mxnet))
# library(mxnet)

# How to install mxnet
# its necessary a version r higher than 3.4.+
# > cran <- getOption('repos')
# > cran['dmlc'] <- 'https://s3-us-west-2.amazonaws.com/apache-mxnet/R/CRAN/'
# > options(repos = cran)
# > install.packages('mxnet', dependencies = T)



















" Falta:
1. bajar hasta 20 datasets y elegir 10 modelos 

datasets: googledrive folder tfm
modelos: rf/ranger, svmLinear, svmPoly, glm, rpart, xgbDART/xgbLinear, ada, nb, knn, mxnet

-> DONE

2. crear archivo metadata para cada dataset para categorizar variables como categoricas/numericas
añadir un archivo METADATA a cada uno de los datasets para indicar el tipo de atributo que es: numerical, categorical o target.

-> DONE


3. ajustar el trainControl para los modelos que elijamos
rf/ranger -> rf con mtry 2:length(dataset_columns)
svmLinear -> C (10^-5, 10^-3, 10^-1, 10^1, 10^3 o 10^5)
svmPoly -> degree: (0 a 6), scale: (10^-15, 10^-13, 10^-11, 10^-9, ..., 10^3), C: (10^-5, 10^-3, 10^-1, 10^1, 10^3 o 10^5)
glm -> none
rpart -> cp = 0 a 1 exponencialmente valores mas cercanos a cero
xgbDART/xgbLinear -> xgbLinear - nrounds = (2), lambda=(0 a 1), alpha=(0 a 1), eta=(0.01, 0.001, 0.0001), 
ada -> iter=(20, 50 100, 500), maxdepth =(1 a 5), nu=(0 a 1)
nb -> fL = (0 a 1,2,3,4,5), usekernel= TRUE or False, adjust=(0 a 5)
knn -> k =(1 a 20)
mxnet -> layer1 =(64, 128, 256), layer2=(16, 32, 64), layer3=(2,4,8,16), learning.rate=(10^-5 a 10 exponencialmente), momentum=(0.9), dropout=(0 a 0,5), activation=('relu')

4. Codificarlo en R:
  4.1: prepareara los data files
  4.2: codificiar el entrenamineto de los modelos 

5. Presentar resultados en funcion de: 
5.1 Paralelizable o no
5.2 Datasets con mayoria de variables categoricas
5.3 Datasets con mayoria de variables numericas
5.4 por tamaño de dataset
5.5 Por numero de variables 

6. Escribir memo
"

# install.packages("drat", repos="https://cran.rstudio.com")
# drat:::addRepo("dmlc")
# install.packages("mxnet")
# 
# 
# install.packages('devtools') #assuming it is not already installed
# 
# library(devtools)
# 
# install_github('andreacirilloac/updateR')
# 
# library(updateR)
# 
# updateR(admin_password = 'Admin user password')
# 
# 
# cran <- getOption("repos")
# cran["dmlc"] <- "https://s3-us-west-2.amazonaws.com/apache-mxnet/R/CRAN/"
# options(repos = cran)
# install.packages("mxnet",dependencies = T)
# library(mxnet)
# cran <- getOption("repos")
# cran["dmlc"] <- "https://s3.amazonaws.com/mxnet-r/"
# options(repos = cran)
# install.packages("mxnet")
# 
# cran <- getOption("repos")
# cran["dmlc"] <- "https://apache-mxnet.s3-accelerate.dualstack.amazonaws.com/R/CRAN/"
# options(repos = cran)
# install.packages("mxnet")