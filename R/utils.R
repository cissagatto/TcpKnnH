###############################################################################
# TEST PARTITIONS FROM HIERARCHICAL COMMUNITIES DETECTION METHODS WITH K-NN   #
# Copyright (C) 2025                                                          #
#                                                                             #
# This code is free software: you can redistribute it and/or modify it under  #
# the terms of the GNU General Public License as published by the Free        #
# Software Foundation, either version 3 of the License, or (at your option)   #
# any later version. This code is distributed in the hope that it will be     #
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty of      #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General    #
# Public License for more details.                                            #
#                                                                             #
# Prof. PhD Elaine Cecilia Gatto                                              #
# Federal University of Lavras (UFLA) Campus Lavras - Minas Gerais            #
# Applied Computer Department (DAC)                                           #
#                                                                             #
# Prof. PhD Ricardo Cerri                                                     #
# State University of São Paulo Campus São Carlos                             #
#                                                                             #
# Prof. PhD Mauri Ferrandin                                                   #
# Federal University of Santa Catarina Campus Blumenau                        #
#                                                                             #
# Prof. PhD Alan Demetrius                                                    #
# Federal University of Sao Carlos (UFSCar) Campus Sao Carlos - São Paulo     #
# Computer Department (DC)                                                    #
#                                                                             #
###############################################################################


FolderRoot = "~/TcpKnnH"
FolderScripts = "~/TcpKnnH/R"

#' Extract Label Space from Cross-Validation Folds
#'
#' This function extracts the label space from multiple folds of a dataset
#' during cross-validation. It loads the label data for each fold and returns
#' the label names and corresponding values.
#'
#' @param parameters A list containing necessary parameters, including:
#' \item{Folders$folderCVTR}{Path to the training fold files}
#' \item{dataset_name}{Base name of the dataset files}
#' \item{ds$LabelStart}{Starting column index of labels}
#' \item{ds$LabelEnd}{Ending column index of labels}
#' \item{number_folds}{Total number of cross-validation folds}
#'
#' @return A list with:
#' \item{NamesLabels}{A vector of label names}
#' \item{Classes}{A list where each element corresponds to label values for a fold}
#'
#' @export
#'
#' @examples
#' parameters <- list(
#'   Folders = list(folderCVTR = "path/to/folds"),
#'   dataset_name = "my_dataset",
#'   ds = list(LabelStart = 5, LabelEnd = 10),
#'   number_folds = 10
#' )
#' result <- labelSpace(parameters)
#' print(result$NamesLabels)  # Output: Label names
labelSpace <- function(parameters){

  retorno = list()

  # return all fold label space
  classes = list()

  # from the first FOLD to the last
  k = 1
  while(k<=number_folds){

    # cat("\n\tFold: ", k)

    # enter folder train
    setwd(parameters$Folders$folderCVTR)

    # get the correct fold cross-validation
    nome_arquivo = paste(dataset_name, "-Split-Tr-", k, ".csv", sep="")

    # open the file
    arquivo = data.frame(read.csv(nome_arquivo))

    # split label space from input space
    classes[[k]] = arquivo[,ds$LabelStart:ds$LabelEnd]

    # get the names labels
    namesLabels = c(colnames(classes[[k]]))

    # increment FOLD
    k = k + 1

    # garbage collection
    gc()

  } # End While of the 10-folds

  # return results
  retorno$NamesLabels = namesLabels
  retorno$Classes = classes
  return(retorno)

  gc()
  cat("\n################################################################")
  cat("\n# FUNCTION LABEL SPACE: END                                    #")
  cat("\n################################################################")
  cat("\n\n\n\n")
}



#' Retrieve Dataset Information
#'
#' This function extracts information from a dataset object, typically from a file
#' like "datasets-hpmlk.csv". It returns various attributes related to the dataset.
#'
#' @param dataset A dataframe or list containing dataset attributes.
#'
#' @return A list with the extracted dataset attributes, including:
#' \item{id}{Dataset ID}
#' \item{name}{Dataset name}
#' \item{instances}{Number of instances}
#' \item{inputs}{Number of input attributes}
#' \item{labels}{Number of labels}
#' \item{LabelsSets}{Label sets information}
#' \item{single}{Single-label classification indicator}
#' \item{maxfreq}{Maximum frequency of labels}
#' \item{card}{Cardinality of the dataset}
#' \item{dens}{Density of the dataset}
#' \item{mean}{Mean value of dataset attributes}
#' \item{scumble}{Scumble measure}
#' \item{tcs}{TCS value (Label co-occurrence complexity)}
#' \item{attStart}{Starting index of attributes}
#' \item{attEnd}{Ending index of attributes}
#' \item{labStart}{Starting index of labels}
#' \item{labEnd}{Ending index of labels}
#' \item{distinct}{Number of distinct values}
#' \item{xn, yn, gridn, xt, yt, gridt}{Grid-related attributes}
#'
#' @export
#'
#' @examples
#' dataset <- data.frame(ID = 1, Name = "SampleData", Instances = 100, Inputs = 10, Labels = 5)
#' info <- infoDataSet(dataset)
#' print(info$name)  # Output: "SampleData"
infoDataSet <- function(dataset){

  retorno = list()

  retorno$id = dataset$ID
  retorno$name = dataset$Name
  retorno$instances = dataset$Instances
  retorno$inputs = dataset$Inputs
  retorno$labels = dataset$Labels
  retorno$LabelsSets = dataset$LabelsSets
  retorno$single = dataset$Single
  retorno$maxfreq = dataset$MaxFreq
  retorno$card = dataset$Card
  retorno$dens = dataset$Dens
  retorno$mean = dataset$Mean
  retorno$scumble = dataset$Scumble
  retorno$tcs = dataset$TCS
  retorno$attStart = dataset$AttStart
  retorno$attEnd = dataset$AttEnd
  retorno$labStart = dataset$LabelStart
  retorno$labEnd = dataset$LabelEnd
  retorno$distinct = dataset$Distinct
  retorno$xn = dataset$xn
  retorno$yn = dataset$yn
  retorno$gridn = dataset$gridn
  retorno$xt = dataset$xt
  retorno$yt = dataset$yt
  retorno$gridt = dataset$gridt

  return(retorno)

  gc()
}


#' Create and Organize Directory Structure
#'
#' This function generates and organizes the directory structure needed for storing
#' results, datasets, partitions, reports, and other relevant files in a machine learning workflow.
#'
#' @param parameters A list containing configuration parameters, including:
#' \itemize{
#'   \item{\code{Folder.Results}}{The root directory for storing results.}
#'   \item{\code{FolderRoot}}{The main root directory.}
#'   \item{\code{Validation}}{The validation method used (e.g., "Silhouette", "Macro-F1", "Micro-F1", "Hamming-Loss").}
#'   \item{\code{Classifier}}{The name of the classifier being used.}
#'   \item{\code{Similarity}}{The similarity measure used in evaluation.}
#' }
#'
#' @return A list containing paths to the created directories:
#' \item{folderResults}{Main results directory.}
#' \item{folderUtils}{Directory for utility files.}
#' \item{folderDatasets}{Directory for dataset storage.}
#' \item{folderLabelSpace}{Subdirectory for label space data.}
#' \item{folderNamesLabels}{Subdirectory for label names.}
#' \item{folderCV}{Cross-validation directory.}
#' \item{folderCVTR}{Training data directory within cross-validation.}
#' \item{folderCVTS}{Test data directory within cross-validation.}
#' \item{folderCVVL}{Validation data directory within cross-validation.}
#' \item{folderPartitions}{Directory for data partitions.}
#' \item{folderCommunities}{Directory for community detection results.}
#' \item{Python}{Directory for Python scripts.}
#' \item{folderValidation}{Directory for validation results.}
#' \item{folderTest}{Directory for test results.}
#' \item{folderRootReports}{Root directory for reports.}
#' \item{folderReports}{Directory for specific reports.}
#' \item{folderRepSim}{Directory for similarity-based reports.}
#' \item{folderRepClassifier}{Directory for classifier-specific reports.}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' params <- list(Folder.Results = "results", FolderRoot = "root",
#'                Validation = "Silhouette", Classifier = "SVM", Similarity = "Cosine")
#' dirs <- directories(params)
#' print(dirs$folderResults)
#' }
directories <- function(parameters) {
  retorno <- list()
  folderResults <- parameters$Folder.Results
  folderRoot <- parameters$FolderRoot
  retorno$folderResults <- folderResults

  create_and_set_dir <- function(path) {
    if (!dir.exists(path)) dir.create(path, recursive = TRUE)
    return(path)
  }

  retorno$folderResults <- create_and_set_dir(folderResults)
  retorno$folderUtils <- create_and_set_dir(paste(FolderRoot, "/utils", sep=""))
  retorno$folderDatasets <- create_and_set_dir(paste(folderResults, "/datasets", sep=""))
  retorno$folderLabelSpace <- create_and_set_dir(paste(retorno$folderDatasets, "/LabelSpace", sep=""))
  retorno$folderNamesLabels <- create_and_set_dir(paste(retorno$folderDatasets, "/NamesLabels", sep=""))
  retorno$folderCV <- create_and_set_dir(paste(retorno$folderDatasets, "/CrossValidation", sep=""))
  retorno$folderCVTR <- create_and_set_dir(paste(retorno$folderCV, "/Tr", sep=""))
  retorno$folderCVTS <- create_and_set_dir(paste(retorno$folderCV, "/Ts", sep=""))
  retorno$folderCVVL <- create_and_set_dir(paste(retorno$folderCV, "/Vl", sep=""))
  retorno$folderPartitions <- create_and_set_dir(paste(folderResults, "/Partitions", sep=""))
  retorno$folderCommunities <- create_and_set_dir(paste(folderResults, "/Communities", sep=""))
  retorno$Python <- create_and_set_dir(paste(FolderRoot, "/Python", sep=""))

  validation_name <- switch(parameters$Validation,
                            "Silhouette" = "Silho",
                            "Macro-F1" = "MaF1",
                            "Micro-F1" = "MiF1",
                            "Hamming-Loss" = "HL"  # Mantendo "HL" para Hamming-Loss
  )

  class_name <- parameters$Classifier

  retorno$folderValidation <- create_and_set_dir(paste(folderResults, "/Val-", validation_name, "-", class_name, sep=""))
  retorno$folderTest <- create_and_set_dir(paste(folderResults, "/Test-", validation_name, "-", class_name, sep=""))

  retorno$folderRootReports <- create_and_set_dir(paste(FolderRoot, "/Reports", sep=""))
  retorno$folderReports <- create_and_set_dir(paste(folderResults, "/Reports", sep=""))
  # retorno$folderRepSim <- create_and_set_dir(paste(retorno$folderRootReports, "/", parameters$Similarity, sep=""))
  retorno$folderRepClassifier <- create_and_set_dir(paste(retorno$folderRepSim, "/", class_name, sep=""))


  return(retorno)
}



#' Compute and Plot ROC Curve for Multi-Label Classification
#'
#' This function calculates the ROC (Receiver Operating Characteristic) curve,
#' plots it, and computes the AUC (Area Under the Curve) for multi-label classification.
#'
#' @param f An integer indicating the fold number (useful for cross-validation).
#' @param y_pred A vector or dataframe containing the predicted probabilities.
#' @param test A dataframe containing the true labels (ground truth).
#' @param Folder A string specifying the directory where the ROC plot and metrics should be saved.
#' @param nome A string used to name the saved files.
#'
#' @return A list containing:
#' \item{roc_auc}{A dataframe with the ROC AUC value.}
#' \item{roc_auc_macro}{A dataframe with the macro-averaged ROC AUC value.}
#' \item{roc_auc_micro}{A dataframe with the micro-averaged ROC AUC value.}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- roc.curva(1, predicted_probs, test_labels, "results", "model_roc")
#' }
roc.curva <- function(f, y_pred, test, Folder, nome){

  retorno <- list()

  #####################################################################
  y_pred= sapply(y_pred, function(x) as.numeric(as.character(x)))
  res = mldr_evaluate(test, y_pred)

  ###############################################################
  # PLOTANDO ROC CURVE
   name = paste(Folder, "/", nome, "-roc.pdf", sep="")
   pdf(name, width = 10, height = 8)
   print(plot(res$roc, print.thres = 'best', print.auc=TRUE,
              print.thres.cex=0.7, grid = TRUE, identity=TRUE,
              axes = TRUE, legacy.axes = TRUE,
              identity.col = "#a91e0e", col = "#1161d5",
              main = paste("fold ", f, " ", nome, sep="")))
   dev.off()
   cat("\n")

  ###############################################################
  aucRoc = data.frame(as.numeric(res$roc$auc))
  names(aucRoc) = "roc-auc"
  # write.csv(aucRoc,
  #           paste(Folder, "/", nome, "-roc-auc.csv", sep=""),
  #           row.names = FALSE)
  retorno$roc_auc = aucRoc

  aucMacro = data.frame(as.numeric(res$macro_auc))
  names(aucMacro) = "roc-auc-macro"
  # write.csv(aucMacro,
  #           paste(Folder, "/", nome, "-roc-auc-macro.csv", sep=""),
  #           row.names = FALSE)
  retorno$roc_auc_macro = aucMacro

  aucMicro = data.frame(as.numeric(as.numeric(res$micro_auc)))
  names(aucMicro) = "roc-auc-micro"
  # write.csv(aucMicro ,
  #           paste(Folder, "/", nome, "-roc-auc-micro.csv", sep=""),
  #           row.names = FALSE)
  retorno$roc_auc_micro = aucMicro

  # ###############################################################
  # # SALVANDO AS INFORMAÇÕES DO ROC SEPARADAMENTE
  # name = paste(Folder, "/", nome, "-roc-1.txt", sep="")
  # output.file <- file(name, "wb")
  #
  # write(" ", file = output.file, append = TRUE)
  # write("percent: ", file = output.file, append = TRUE)
  # write(res$roc$percent, file = output.file, append = TRUE)
  #
  # write(" ", file = output.file, append = TRUE)
  # write("sensitivities: ", file = output.file, append = TRUE)
  # write(res$roc$sensitivities, file = output.file, append = TRUE)
  #
  # write(" ", file = output.file, append = TRUE)
  # write("specificities: ", file = output.file, append = TRUE)
  # write(res$roc$specificities, file = output.file, append = TRUE)
  #
  # write(" ", file = output.file, append = TRUE)
  # write("thresholds: ", file = output.file, append = TRUE)
  # write(res$roc$thresholds, file = output.file, append = TRUE)
  #
  # write(" ", file = output.file, append = TRUE)
  # write("direction: ", file = output.file, append = TRUE)
  # write(res$roc$direction, file = output.file, append = TRUE)
  #
  # write(" ", file = output.file, append = TRUE)
  # write("cases: ", file = output.file, append = TRUE)
  # write(res$roc$cases, file = output.file, append = TRUE)
  #
  # write(" ", file = output.file, append = TRUE)
  # write("controls: ", file = output.file, append = TRUE)
  # write(res$roc$controls, file = output.file, append = TRUE)
  #
  # write(" ", file = output.file, append = TRUE)
  # write("auc: ", file = output.file, append = TRUE)
  # write(res$roc$auc, file = output.file, append = TRUE)
  #
  # write(" ", file = output.file, append = TRUE)
  # write("original predictor: ", file = output.file, append = TRUE)
  # write(res$roc$original.predictor, file = output.file, append = TRUE)
  #
  # write(" ", file = output.file, append = TRUE)
  # write("original response: ", file = output.file, append = TRUE)
  # write(res$roc$original.response, file = output.file, append = TRUE)
  #
  # write(" ", file = output.file, append = TRUE)
  # write("predictor: ", file = output.file, append = TRUE)
  # write(res$roc$predictor, file = output.file, append = TRUE)
  #
  # write(" ", file = output.file, append = TRUE)
  # write("response: ", file = output.file, append = TRUE)
  # write(res$roc$response, file = output.file, append = TRUE)
  #
  # write(" ", file = output.file, append = TRUE)
  # write("levels: ", file = output.file, append = TRUE)
  # write(res$roc$levels, file = output.file, append = TRUE)
  #
  # close(output.file)
  #
  # ###############################################################
  # # SALVANDO AS OUTRAS INFORMAÇÕES
  # name = paste(Folder, "/", nome, "-roc-2.txt", sep="")
  # sink(name, type = "output")
  # print(res$roc)
  # cat("\n\n")
  # str(res)
  # sink()

  return(retorno)
}



#' Compute Confusion Matrix for Multi-Label Classification
#'
#' This function calculates the confusion matrix (True Positives, False Positives,
#' False Negatives, and True Negatives) for a multi-label classification problem.
#'
#' @param true A dataframe containing the true labels (ground truth).
#' @param pred A dataframe containing the predicted labels.
#' @param type A string indicating the type of evaluation (e.g., "test" or "validation").
#' @param salva A string specifying the directory where the confusion matrix should be saved.
#' @param nomes.rotulos A vector of strings containing label names for better interpretability.
#'
#' @return The function does not return a value but saves various confusion matrix metrics as CSV files.
#' @export
#'
#' @examples
#' \dontrun{
#' matrix.confusao(true_labels, predicted_labels, "test", "results", c("label1", "label2"))
#' }
matrix.confusao <- function(true, pred, type, salva, nomes.rotulos){

  bipartition = data.frame(true, pred)

  num.instancias = nrow(bipartition)
  num.rotulos = ncol(true) # número de rótulos do conjunto

  num.positive.instances = apply(bipartition, 2, sum) # número de instâncias positivas
  num.negative.instances = num.instancias - num.positive.instances   # número de instâncias negativas  # salvando

  res = rbind(num.positive.instances, num.negative.instances)
  #name = paste(salva, "/", type, "-ins-pn.csv", sep="")
  #write.csv(res, name, , row.names = FALSE)

  true_1 = data.frame(ifelse(true==1,1,0)) # calcular rótulo verdadeiro igual a 1
  total_true_1 = apply(true_1, 2, sum)

  true_0 = data.frame(ifelse(true==0,1,0)) # calcular rótulo verdadeiro igual a 0
  total_true_0 = apply(true_0, 2, sum)

  pred_1 = data.frame(ifelse(pred==1,1,0)) # calcular rótulo predito igual a 1
  total_pred_1 = apply(pred_1, 2, sum)

  pred_0 = data.frame(ifelse(pred==0,1,0)) # calcular rótulo verdadeiro igual a 0
  total_pred_0 = apply(pred_0, 2, sum)

  matriz_totais = cbind(total_true_0, total_true_1, total_pred_0, total_pred_1)
  row.names(matriz_totais) = nomes.rotulos
  #name = paste(salva, "/", type, "-trues-preds.csv", sep="")
  #write.csv(matriz_totais, name, row.names = FALSE)

  # Verdadeiro Positivo: O modelo previu 1 e a resposta correta é 1
  TPi  = data.frame(ifelse((true_1 & true_1),1,0))
  tpi = paste(nomes.rotulos, "-TP", sep="")
  names(TPi) = tpi

  # Verdadeiro Negativo: O modelo previu 0 e a resposta correta é 0
  TNi  = data.frame(ifelse((true_0 & pred_0),1,0))
  tni = paste(nomes.rotulos, "-TN", sep="")
  names(TNi) = tni

  # Falso Positivo: O modelo previu 1 e a resposta correta é 0
  FPi  = data.frame(ifelse((true_0 & pred_1),1,0))
  fpi = paste(nomes.rotulos, "-FP", sep="")
  names(FPi) = fpi

  # Falso Negativo: O modelo previu 0 e a resposta correta é 1
  FNi  = data.frame(ifelse((true_1 & pred_0),1,0))
  fni = paste(nomes.rotulos, "-FN", sep="")
  names(FNi) = fni

  fpnt = data.frame(TPi, FPi, FNi, TNi)
  name = paste(salva, "/", type, "-tfpn.csv", sep="")
  #write.csv(fpnt, name, row.names = FALSE)

  # total de verdadeiros positivos
  TPl = apply(TPi, 2, sum)
  tpl = paste(nomes.rotulos, "-TP", sep="")
  names(TPl) = tpl

  # total de verdadeiros negativos
  TNl = apply(TNi, 2, sum)
  tnl = paste(nomes.rotulos, "-TN", sep="")
  names(TNl) = tnl

  # total de falsos negativos
  FNl = apply(FNi, 2, sum)
  fnl = paste(nomes.rotulos, "-FN", sep="")
  names(FNl) = fnl

  # total de falsos positivos
  FPl = apply(FPi, 2, sum)
  fpl = paste(nomes.rotulos, "-FP", sep="")
  names(FPl) = fpl

  matriz_confusao_por_rotulos = data.frame(TPl, FPl, FNl, TNl)
  colnames(matriz_confusao_por_rotulos) = c("TP","FP", "FN", "TN")
  row.names(matriz_confusao_por_rotulos) = nomes.rotulos
  name = paste(salva, "/", type, "-matrix-confusion.csv", sep="")
  write.csv(matriz_confusao_por_rotulos, name, row.names = FALSE)
}


#' Evaluate Multi-Label Classification Performance
#'
#' This function evaluates a multi-label classification model using a confusion matrix,
#' calculates evaluation metrics, and saves the results to CSV and text files.
#'
#' @param f Integer. The fold number of the cross-validation.
#' @param y_true Data frame. The true labels of the dataset.
#' @param y_pred Data frame. The predicted labels of the dataset.
#' @param salva Character. The directory where the results will be saved.
#' @param nome Character. The base name for the output files.
#'
#' @return None. The function saves evaluation results as files.
#'
#' @details
#' This function computes the multi-label confusion matrix and derived evaluation metrics.
#' It outputs two CSV files:
#' \itemize{
#'   \item A file containing evaluated metrics.
#'   \item A file containing confusion matrix statistics (absolute values and percentages).
#' }
#'
#' Additionally, it saves a text file with the confusion matrix.
#'
#' @examples
#' \dontrun{
#'   f <- 1
#'   y_true <- data.frame(label1 = c(1,0,1), label2 = c(0,1,1))
#'   y_pred <- data.frame(label1 = c(1,1,0), label2 = c(0,1,1))
#'   salva <- "results"
#'   nome <- "model"
#'   avaliacao(f, y_true, y_pred, salva, nome)
#' }
#'
#' @export
avaliacao <- function(f, y_true, y_pred, salva, nome){

  salva.0 = paste(salva, "/", nome, "-conf-mat.txt", sep="")
  sink(file=salva.0, type="output")
  confmat = multilabel_confusion_matrix(y_true, y_pred)
  print(confmat)
  sink()

  resConfMat = multilabel_evaluate(confmat)
  resConfMat = data.frame(resConfMat)
  names(resConfMat) = paste("Fold-", f, sep="")
  salva.1 = paste(salva, "/", nome, "-evaluated.csv", sep="")
  write.csv(resConfMat, salva.1)

  conf.mat = data.frame(confmat$TPl, confmat$FPl,
                        confmat$FNl, confmat$TNl)
  names(conf.mat) = c("TP", "FP", "FN", "TN")

  conf.mat.perc = data.frame(conf.mat/nrow(y_true$dataset))
  names(conf.mat.perc) = c("TP.perc", "FP.perc", "FN.perc", "TN.perc")

  wrong = conf.mat$FP + conf.mat$FN
  wrong.perc = wrong/nrow(y_true$dataset)

  correct = conf.mat$TP + conf.mat$TN
  correct.perc = correct/nrow(y_true$dataset)

  conf.mat.2 = data.frame(conf.mat, conf.mat.perc, wrong, correct,
                          wrong.perc, correct.perc)
  salva.2 = paste(salva, "/", nome, "-utiml.csv", sep="")
  write.csv(conf.mat.2, salva.2)
}


#' Split DataFrame by KNN Models
#'
#' This function splits a dataframe into a list of dataframes,
#' where each element corresponds to a specific KNN model.
#'
#' @param df A dataframe containing KNN-related data, including a "knn" column.
#' @param nome_base A base name (string) for identification purposes (not used in function logic).
#'
#' @return A named list of dataframes, where each element contains data for a specific KNN model.
#'         The "knn" column is removed from the resulting dataframes.
#' @export
#'
#' @examples
#' \dontrun{
#' split_knn_results <- splitKnn(df, "my_dataset")
#' }
# splitKnn <- function(df, nome_base) {
#   retorno <- list()
#   knn_values <- unique(df$knn)
#   for (knn_val in knn_values) {
#     df_knn <- subset(df, knn == knn_val)
#     df_knn <- df_knn[, !(names(df_knn) %in% "knn")]
#     nome_variavel <- paste0("knn_", knn_val)
#     retorno[[nome_variavel]] <- df_knn
#   }
#
#   return(retorno)
# }


#' Split Dataframe by KNN Values
#'
#' This function splits a dataframe into multiple subsets based on unique KNN values.
#' Each subset contains data corresponding to a specific KNN value, excluding the original "knn" column.
#'
#' @param df A dataframe containing a column named "knn" that indicates different KNN values.
#' @param nome_base A string representing the base name for naming the output lists (not used inside the function).
#'
#' @return A list where each element corresponds to a dataframe filtered by a unique KNN value.
#'         The names of the list elements follow the format "knn_X", where X is the KNN value.
#'
#' @export
#'
#' @examples
#' df <- data.frame(knn = c(1, 1, 2, 2), value = c(10, 20, 30, 40))
#' result <- splitKnn(df, "example")
#' print(names(result))  # Output: "knn_1", "knn_2"
#' print(result$knn_1)   # Output: Data for knn = 1
splitKnn <- function(df, nome_base) {
  retorno <- list()
  knn_values <- unique(df$knn)

  for (knn_val in knn_values) {
    df_knn <- subset(df, knn == knn_val)
    df_knn <- df_knn[, !(names(df_knn) %in% "knn")]
    nome_variavel <- paste0("knn_", knn_val)
    retorno[[nome_variavel]] <- df_knn
  }

  return(retorno)
}


#' Split AUPRC DataFrame by KNN Models
#'
#' This function splits an AUPRC dataframe into a list of dataframes,
#' where each element corresponds to a specific KNN model and AUPRC type (Micro/Macro).
#'
#' @param df A dataframe containing AUPRC metrics with columns: "fold", "Knn", "Micro.AUPRC", and "Macro.AUPRC".
#' @param nome_base A base name (string) for identification purposes (not used in function logic).
#'
#' @return A named list of dataframes, where each element contains AUPRC data for a specific KNN model and type (Micro/Macro).
#' @export
#'
#' @examples
#' \dontrun{
#' split_auprc_results <- splitAuprc(df, "my_dataset")
#' }
splitAuprc <- function(df, nome_base) {
  retorno <- list()
  knn_values <- unique(df$Knn)
  for (knn_val in knn_values) {
    df_knn <- subset(df, Knn == knn_val)
    df_micro <- df_knn[, c("fold", "Micro.AUPRC")]
    df_macro <- df_knn[, c("fold", "Macro.AUPRC")]
    nome_micro <- paste0("knn_", knn_val, "_micro")
    nome_macro <- paste0("knn_", knn_val, "_macro")
    retorno[[nome_micro]] <- df_micro
    retorno[[nome_macro]] <- df_macro
  }
  return(retorno)
}


#' Split Performance DataFrame by KNN Models
#'
#' This function splits a performance dataframe into a list of dataframes,
#' where each element corresponds to a specific KNN model.
#'
#' @param df A dataframe containing performance metrics with columns: "fold", "measures", and KNN model names.
#'
#' @return A named list of dataframes, where each element contains the reshaped performance data for a specific KNN model.
#' @export
#'
#' @examples
#' \dontrun{
#' split_results <- splitPerformance(df)
#' }
splitPerformance <- function(df) {
  retorno <- list()
  knn_cols <- names(df)[3:ncol(df)]
  for (knn_col in knn_cols) {
    df_knn <- df[, c("fold", "measures", knn_col)]
    df_wide <- reshape(df_knn,
                       idvar = "fold",
                       timevar = "measures",
                       direction = "wide")
    colnames(df_wide) <- sub(paste0("^", knn_col, "\\."), "", colnames(df_wide))
    knn_nome_corrigido <- gsub("knn([0-9]+)", "knn_\\1", knn_col)
    retorno[[knn_nome_corrigido]] <- df_wide
  }
  return(retorno)
}


#' Merge and Save Performance Results
#'
#' This function merges multiple performance metrics (ROC AUC, AUPRC, etc.) into a single dataframe
#' and saves it as a CSV file.
#'
#' @param roc_auc_1 A list containing ROC AUC values for different KNN models.
#' @param roc_auc_macro_1 A list containing macro ROC AUC values for different KNN models.
#' @param roc_auc_micro_1 A list containing micro ROC AUC values for different KNN models.
#' @param auprc_proba_1 A list containing AUPRC values (both micro and macro) for different KNN models.
#' @param resultados_proba_1 A list containing probability-based results for different KNN models.
#'
#' @return This function does not return a value; it writes merged results to CSV files.
#' @export
#'
#' @examples
#' \dontrun{
#' mergeAndSaveResults(roc_auc_1, roc_auc_macro_1, roc_auc_micro_1, auprc_proba_1, resultados_proba_1)
#' }
mergeAndSaveResults <- function(roc_auc_1,
                                roc_auc_macro_1,
                                roc_auc_micro_1,
                                auprc_proba_1,
                                resultados_proba_1) {

  # Obter todos os KNNs presentes em `resultados_proba_1`
  knn_nomes <- names(resultados_proba_1)

  for (knn in knn_nomes) {
    # Construir o nome correto para acessar as listas
    roc_auc <- roc_auc_1[[knn]]["roc.auc"]
    roc_auc_macro <- roc_auc_macro_1[[knn]]["roc.auc.macro"]
    roc_auc_micro <- roc_auc_micro_1[[knn]]["roc.auc.micro"]

    # Buscar os valores de AUPRC e renomear as colunas
    auprc_micro <- auprc_proba_1[[paste0(knn, "_micro")]]["Micro.AUPRC"]
    auprc_macro <- auprc_proba_1[[paste0(knn, "_macro")]]["Macro.AUPRC"]

    colnames(auprc_micro) <- "auprc.micro"
    colnames(auprc_macro) <- "auprc.macro"

    # Criar o dataframe final concatenando as colunas corretamente
    df_final <- cbind(resultados_proba_1[[knn]],
                      roc_auc,
                      roc_auc_macro,
                      roc_auc_micro,
                      auprc_micro,
                      auprc_macro)

    # Criar o nome do arquivo
    file_name <- paste0(parameters$Folders$folderTest,
                        "/Performance-", toupper(knn), ".csv")

    # Salvar como CSV
    write.csv(df_final, file = file_name, row.names = FALSE)

    cat("Arquivo salvo:", file_name, "\n")
  }
}



###############################################################################
# CONTACT INFORMATION                                                          #
# In case of errors, contact: elainececiliagatto@gmail.com                     #
# Thank you for your collaboration!                                            #
###############################################################################
