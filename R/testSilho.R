###############################################################################
# TEST COMMUNITIES PARTITIONS
# Copyright (C) 2022
#
# This code is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free
# Software Foundation, either version 3 of the License, or (at your option)
# any later version. This code is distributed in the hope that it will be
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
# Public License for more details.
#
# Elaine Cecilia Gatto | Prof. Dr. Ricardo Cerri | Prof. Dr. Mauri Ferrandin
# Federal University of Sao Carlos (UFSCar: https://www2.ufscar.br/) Campus
# Sao Carlos Computer Department (DC: https://site.dc.ufscar.br/)
# Program of Post Graduation in Computer Science
# (PPG-CC: http://ppgcc.dc.ufscar.br/)
# Bioinformatics and Machine Learning Group
# (BIOMAL: http://www.biomal.ufscar.br/)
#
###############################################################################


###############################################################################
# SET WORKSAPCE                                                               #
###############################################################################
FolderRoot = "~/TcpKnnH"
FolderScripts = paste(FolderRoot, "/R", sep="")

#' Build and Test Hybrid Partitions with Silhouette
#'
#' This function constructs and evaluates hybrid partitions using silhouette-based methods.
#' It processes multiple folds in parallel, converts datasets between CSV and ARFF formats,
#' and executes classification tasks using the CLUS framework.
#'
#' @param parameters A list containing:
#'   - `Number.Folds` (integer): Number of validation folds.
#'   - `Folders$folderPartitions` (string): Path to partitions folder.
#'   - `Folders$folderCommunities` (string): Path to communities folder.
#'   - `Folders$folderTestSilho` (string): Path to silhouette test folder.
#'   - `Folders$folderCVTR` (string): Path to cross-validation train dataset.
#'   - `Folders$folderCVVL` (string): Path to cross-validation validation dataset.
#'   - `Folders$folderCVTS` (string): Path to cross-validation test dataset.
#'   - `Folders$folderUtils` (string): Path to utility scripts and tools.
#'   - `Dataset.Name` (string): Name of the dataset.
#'   - `Dataset.Info$AttStart` (integer): Start index of attributes in the dataset.
#'   - `Dataset.Info$AttEnd` (integer): End index of attributes in the dataset.
#'   - `Dataset.Info$LabelStart` (integer): Start index of label columns.
#'   - `Choosed$all.methods.choosed` (data.frame): Selected methods for processing.
#'   - `Choosed$all.partitions.choosed` (data.frame): Chosen partitions for each fold and method.
#'   - `best.silhouette$all.silhouette` (data.frame): Best silhouette partitions.
#'
#' @return None. The function generates output files and directories but does not return a value.
#'
#' @details
#' The function performs the following:
#' - Processes folds in parallel using `%dopar%`.
#' - Reads train, validation, and test datasets for each fold.
#' - Identifies the best partitioning strategy using silhouette metrics.
#' - Converts CSV files to ARFF format for CLUS processing.
#' - Executes CLUS classification tasks and extracts predictions.
#' - Saves processed datasets, predictions, and cleans up intermediate files.
#'
#' @examples
#' \dontrun{
#' parameters <- list(
#'   Number.Folds = 5,
#'   Folders = list(
#'     folderPartitions = "/path/to/partitions",
#'     folderCommunities = "/path/to/communities",
#'     folderTestSilho = "/path/to/test/silho",
#'     folderCVTR = "/path/to/cv/train",
#'     folderCVVL = "/path/to/cv/validation",
#'     folderCVTS = "/path/to/cv/test",
#'     folderUtils = "/path/to/utils"
#'   ),
#'   Dataset.Name = "my_dataset",
#'   Dataset.Info = list(
#'     AttStart = 1, AttEnd = 20, LabelStart = 21
#'   ),
#'   Choosed = list(
#'     all.methods.choosed = data.frame(),
#'     all.partitions.choosed = data.frame()
#'   ),
#'   best.silhouette = list(
#'     all.silhouette = data.frame()
#'   )
#' )
#' silho.build.test(parameters)
#' }
#'
#' @import dplyr
#' @import foreach
#' @importFrom foreign read.arff
#' @export
silho.build.test <- function(parameters){

  f = 1
  buildParalel <- foreach(f = 1:parameters$Number.Folds) %dopar%{
  #while(f<=parameters$Number.Folds){

    cat("\n#=========================================================")
    cat("\n# Fold: ", f)
    cat("\n#=========================================================")

    ########################################################################
    FolderRoot = "~/TcpKnnH"
    FolderScripts = paste(FolderRoot, "/R", sep="")

    setwd(FolderScripts)
    source("utils.R")
    source("libraries.R")


    #########################################################################
    converteArff <- function(arg1, arg2, arg3){
      str = paste("java -jar ", parameters$FoldersfolderUtils,
                  "/R_csv_2_arff.jar ", arg1, " ", arg2, " ", arg3, sep="")
      print(system(str))
      cat("\n")
    }

    ##########################################################################
    constroiParticoes <- function(TotalParticoes){

      data <- data.frame(matrix(NA,    # Create empty data frame
                                nrow = TotalParticoes,
                                ncol = 2))

      names(data) = c("numberPartition", "numberGroup")

      i = 1
      a = 1
      while(i<=nrow(data)){
        data[i,1] = a + 1
        data[i,2] = a + 1
        i = i + 1
        a = a + 1
        gc()
      }

      return(data)

    }


    ###########################################################################
    FolderPSplit = paste(parameters$Folders$folderPartitions,
                         "/Split-", f, sep="")
    print(FolderPSplit)

    FolderSplitComm = paste(parameters$Folders$folderCommunities,
                            "/Split-", f, sep="")
    print(FolderSplitComm)

    FolderTSplit = paste(parameters$Folders$folderTestSilho,
                         "/Split-", f, sep="")
    if(dir.exists(FolderTSplit)==FALSE){dir.create(FolderTSplit)}
    print(FolderTSplit)

    ########################################################################
    #cat("\nOpen Train file ", f, "\n")
    setwd(parameters$Folders$folderCVTR)
    nome_arq_tr = paste(parameters$Dataset.Name, "-Split-Tr-", f,
                        ".csv", sep="")
    arquivo_tr = data.frame(read.csv(nome_arq_tr))

    #####################################################################
    #cat("\nOpen Validation file ", f, "\n")
    setwd(parameters$Folders$folderCVVL)
    nome_arq_vl = paste(parameters$Dataset.Name, "-Split-Vl-", f,
                        ".csv", sep="")
    arquivo_vl = data.frame(read.csv(nome_arq_vl))

    ########################################################################
    #cat("\nOpen Test file ", f, "\n")
    setwd(parameters$Folders$folderCVTS)
    nome_arq_ts = paste(parameters$Dataset.Name, "-Split-Ts-", f,
                        ".csv", sep="")
    cat("\n\t\t", nome_arq_ts)
    arquivo_ts = data.frame(read.csv(nome_arq_ts))

    # juntando treino com validação
    arquivo_tr2 = rbind(arquivo_tr, arquivo_vl)

    #######################################################3
    all = data.frame(parameters$Choosed$all.methods.choosed)
    all.2 = filter(all, all$split == f)
    total = nrow(all.2)

    u = 1
    while(u<=total){

      cat("\n#=========================================================")
      cat("\n# Knn = ", u)
      cat("\n#=========================================================")

      FolderPartKnn = paste(FolderPSplit, "/knn-", u, sep="")
      print(FolderPartKnn)

      FolderPartComm = paste(FolderSplitComm, "/knn-", u, sep="")
      print(FolderPartComm)

      FolderTKnn = paste(FolderTSplit, "/Knn-", u, sep="")
      if(dir.create(FolderTKnn)==FALSE){dir.create(FolderTKnn)}
      print(FolderTKnn)

      all = data.frame(parameters$Choosed$all.methods.choosed)
      all.2 = data.frame(filter(all, all$split == f))

      nome = paste("knn-", u, sep="")
      all.3 = data.frame(filter(all.2, all.2$sparsification == nome))

      partitions = data.frame(parameters$Choosed$all.partitions.choosed)
      partitions = data.frame(filter(partitions, fold == f))
      partitions = data.frame(filter(partitions, knn == u))

      best = data.frame(parameters$best.silhouette$all.silhouette)
      best = data.frame(filter(best, best$fold==f))
      best = data.frame(filter(best, best$knn==u))

      num.part = best$part
      num.groups = best$part

      partitions = data.frame(filter(partitions, partitions$method == all.3$method))
      partitions = partitions[,c(-1,-2,-3)]
      labels = partitions$labels
      groups = partitions[,num.part]
      particao = data.frame(labels, groups)

      g = 1
      while(g<=num.groups){

        cat("\n#=========================================================")
        cat("\n# Group = ", g)
        cat("\n#=========================================================")

        ###############################################################
        FolderTestGroup = paste(FolderTKnn, "/Group-", g, sep="")
        if(dir.exists(FolderTestGroup)== FALSE){dir.create(FolderTestGroup) }


        ###############################################################
        #cat("\nSpecific Group: ", g, "\n")
        grupoEspecifico = filter(particao, groups == g)


        ################################################################
        cat("\nTRAIN: Mount Group ", g, "\n")
        atributos_tr = arquivo_tr2[parameters$Dataset.Info$AttStart:parameters$Dataset.Info$AttEnd]
        n_a = ncol(atributos_tr)
        classes_tr = select(arquivo_tr2, grupoEspecifico$labels)
        n_c = ncol(classes_tr)
        grupo_tr = cbind(atributos_tr, classes_tr)
        fim_tr = ncol(grupo_tr)


        #####################################################################
        #cat("\n\tTRAIN: Save Group", g, "\n")
        setwd(FolderTestGroup)
        nome_tr = paste(parameters$Dataset.Name, "-split-tr-", f, "-group-",
                        g, ".csv", sep="")
        write.csv(grupo_tr, nome_tr, row.names = FALSE)


        #####################################################################
        #cat("\n\tINICIO FIM TARGETS: ", g, "\n")
        inicio = parameters$Dataset.Info$LabelStart
        fim = fim_tr
        ifr = data.frame(inicio, fim)
        write.csv(ifr, "inicioFimRotulos.csv", row.names = FALSE)


        ####################################################################
        #cat("\n\tTRAIN: Convert Train CSV to ARFF ", g , "\n")
        nome_arquivo_2 = paste(parameters$Dataset.Name, "-split-tr-", f,
                               "-group-", g, ".arff", sep="")
        arg1Tr = nome_tr
        arg2Tr = nome_arquivo_2
        arg3Tr = paste(inicio, "-", fim, sep="")
        str = paste("java -jar ", parameters$Folders$folderUtils,
                    "/R_csv_2_arff.jar ", arg1Tr, " ", arg2Tr, " ",
                    arg3Tr, sep="")
        print(system(str))


        ##################################################################
        #cat("\n\tTRAIN: Verify and correct {0} and {1} ", g , "\n")
        arquivo = paste(FolderTestGroup, "/", arg2Tr, sep="")
        str0 = paste("sed -i 's/{0}/{0,1}/g;s/{1}/{0,1}/g' ", arquivo, sep="")
        print(system(str0))


        ######################################################################
        #cat("\n\tTEST: Mount Group: ", g, "\n")
        atributos_ts = arquivo_ts[parameters$Dataset.Info$AttStart:parameters$Dataset.Info$AttEnd]
        classes_ts = select(arquivo_ts, grupoEspecifico$labels)
        grupo_ts = cbind(atributos_ts, classes_ts)
        fim_ts = ncol(grupo_ts)


        ######################################################################
        #cat("\n\tTEST: Save Group ", g, "\n")
        setwd(FolderTestGroup)
        nome_ts = paste(parameters$Dataset.Name, "-split-ts-", f, "-group-",
                        g, ".csv", sep="")
        write.csv(grupo_ts, nome_ts, row.names = FALSE)


        ####################################################################
        #cat("\n\tTEST: Convert CSV to ARFF ", g , "\n")
        nome_arquivo_3 = paste(parameters$Dataset.Name, "-split-ts-", f,
                               "-group-", g , ".arff", sep="")
        arg1Ts = nome_ts
        arg2Ts = nome_arquivo_3
        arg3Ts = paste(inicio, "-", fim, sep="")
        str = paste("java -jar ", parameters$Folders$folderUtils,
                    "/R_csv_2_arff.jar ", arg1Ts, " ", arg2Ts, " ",
                    arg3Ts, sep="")
        system(str)


        #####################################################################
        #cat("\n\tTEST: Verify and correct {0} and {1} ", g , "\n")
        arquivo = paste(FolderTestGroup, "/", arg2Ts, sep="")
        str0 = paste("sed -i 's/{0}/{0,1}/g;s/{1}/{0,1}/g' ", arquivo, sep="")
        cat("\n")
        print(system(str0))
        cat("\n")


        #####################################################################
        #cat("\nCreating .s file for clus")
        if(inicio == fim){

          nome_config = paste(parameters$Dataset.Name, "-split-", f, "-group-",
                              g, ".s", sep="")
          sink(nome_config, type = "output")

          cat("[General]")
          cat("\nCompatibility = MLJ08")

          cat("\n\n[Data]")
          cat(paste("\nFile = ", nome_arquivo_2, sep=""))
          cat(paste("\nTestSet = ", nome_arquivo_3, sep=""))

          cat("\n\n[Attributes]")
          cat("\nReduceMemoryNominalAttrs = yes")

          cat("\n\n[Attributes]")
          cat(paste("\nTarget = ", fim, sep=""))
          cat("\nWeights = 1")

          cat("\n")
          cat("\n[Tree]")
          cat("\nHeuristic = VarianceReduction")
          cat("\nFTest = [0.001,0.005,0.01,0.05,0.1,0.125]")

          cat("\n\n[Model]")
          cat("\nMinimalWeight = 5.0")

          cat("\n\n[Output]")
          cat("\nWritePredictions = {Test}")
          cat("\n")
          sink()

          ###################################################################
          cat("\nExecute CLUS: ", g , "\n")
          nome_config2 = paste(FolderTestGroup, "/", nome_config, sep="")
          str = paste("java -jar ", parameters$Folders$folderUtils,
                      "/Clus.jar ", nome_config2, sep="")
          print(system(str))
          cat("\n")

        } else {

          nome_config = paste(parameters$Dataset.Name, "-split-", f,
                              "-group-", g, ".s", sep="")
          sink(nome_config, type = "output")

          cat("[General]")
          cat("\nCompatibility = MLJ08")

          cat("\n\n[Data]")
          cat(paste("\nFile = ", nome_arquivo_2, sep=""))
          cat(paste("\nTestSet = ", nome_arquivo_3, sep=""))

          cat("\n\n[Attributes]")
          cat("\nReduceMemoryNominalAttrs = yes")

          cat("\n\n[Attributes]")
          cat(paste("\nTarget = ", inicio, "-", fim, sep=""))
          cat("\nWeights = 1")

          cat("\n")
          cat("\n[Tree]")
          cat("\nHeuristic = VarianceReduction")
          cat("\nFTest = [0.001,0.005,0.01,0.05,0.1,0.125]")

          cat("\n\n[Model]")
          cat("\nMinimalWeight = 5.0")

          cat("\n\n[Output]")
          cat("\nWritePredictions = {Test}")
          cat("\n")
          sink()

          cat("\nExecute CLUS: ", g , "\n")
          nome_config2 = paste(FolderTestGroup, "/", nome_config, sep="")
          str = paste("java -jar ", parameters$Folders$folderUtils,
                      "/Clus.jar ", nome_config2, sep="")
          print(system(str))
          cat("\n")

        }

        ##################################################################
        #cat("\n\nOpen predictions")
        nomeDoArquivo = paste(FolderTestGroup, "/", parameters$Dataset.Name,
                              "-split-", f,"-group-", g,
                              ".test.pred.arff", sep="")
        predicoes = data.frame(foreign::read.arff(nomeDoArquivo))


        #####################################################################
        #cat("\nS\nPLIT PREDICTIS")
        if(inicio == fim){
          #cat("\n\nOnly one label in this group")

          ###################################################################
          #cat("\n\nSave Y_true")
          setwd(FolderTestGroup)
          classes = data.frame(predicoes[,1])
          names(classes) = colnames(predicoes)[1]
          write.csv(classes, "y_true.csv", row.names = FALSE)

          #################################################################
          #cat("\n\nSave Y_true")
          rot = paste("Pruned.p.", colnames(predicoes)[1], sep="")
          pred = data.frame(predicoes[,rot])
          names(pred) = colnames(predicoes)[1]
          setwd(FolderTestGroup)
          write.csv(pred, "y_predict.csv", row.names = FALSE)

          ####################################################################
          rotulos = c(colnames(classes))
          n_r = length(rotulos)
          gc()

        } else {

          ##############################################################
          #cat("\n\nMore than one label in this group")
          comeco = 1+(fim - inicio)


          ####################################################################
          cat("\n\nSave Y_true")
          classes = data.frame(predicoes[,1:comeco])
          setwd(FolderTestGroup)
          write.csv(classes, "y_true.csv", row.names = FALSE)


          ##################################################################
          cat("\n\nSave Y_true")
          rotulos = c(colnames(classes))
          n_r = length(rotulos)
          nomeColuna = c()
          t = 1
          while(t <= n_r){
            nomeColuna[t] = paste("Pruned.p.", rotulos[t], sep="")
            t = t + 1
            gc()
          }
          pred = data.frame(predicoes[nomeColuna])
          names(pred) = rotulos
          setwd(FolderTestGroup)
          write.csv(pred, "y_predict.csv", row.names = FALSE)
          gc()
        } # FIM DO ELSE

        # deleting files
        um = paste(parameters$Dataset.Name, "-split-", f, "-group-", g, ".model", sep="")
        dois = paste(parameters$Dataset.Name, "-split-", f, "-group-", g, ".s", sep="")
        tres = paste(parameters$Dataset.Name, "-split-tr-", f, "-group-", g, ".arff", sep="")
        quatro = paste(parameters$Dataset.Name, "-split-ts-", f, "-group-", g, ".arff", sep="")
        cinco = paste(parameters$Dataset.Name, "-split-tr-", f, "-group-", g, ".csv", sep="")
        seis = paste(parameters$Dataset.Name, "-split-ts-", f, "-group-", g, ".csv", sep="")
        sete = paste(parameters$Dataset.Name, "-split-", f, "-group-", g, ".out", sep="")
        oito = paste("Variance_RHE_1.csv")

        setwd(FolderTestGroup)
        unlink(um, recursive = TRUE)
        unlink(dois, recursive = TRUE)
        unlink(tres, recursive = TRUE)
        unlink(quatro, recursive = TRUE)
        unlink(cinco, recursive = TRUE)
        unlink(seis, recursive = TRUE)
        unlink(sete, recursive = TRUE)
        unlink(oito, recursive = TRUE)

        g = g + 1
        gc()
      } # fim do grupo

      u = u + 1
      gc()
    } # end KNN

    #f = f + 1
    gc()
  } # fim do for each

  gc()
  cat("\n###########################################################")
  cat("\n# TEST SILHO: Build and Test Hybrid Partitions End        #")
  cat("\n###########################################################")
  cat("\n\n\n\n")
}


#' Gather Predictions from KNN Models
#'
#' This function gathers predictions from multiple KNN models across different folds.
#' It reads prediction files, aggregates results, and removes temporary files.
#'
#' @param parameters A list containing the necessary parameters:
#'   - `Number.Folds`: (integer) Number of folds for cross-validation.
#'   - `Folders`: (list) Paths to different directories:
#'       - `folderPartitions`: Path to partition data.
#'       - `folderTestSilho`: Path to test data.
#'       - `folderCommunities`: Path to community data.
#'   - `Choosed`: (list) Selected methods and partitions:
#'       - `all.methods.choosed`: Data frame with selected methods.
#'       - `all.partitions.choosed`: Data frame with selected partitions.
#'   - `best.silhouette`: (list) Best silhouette results:
#'       - `all.silhouette`: Data frame containing the best silhouette values.
#'
#' @return No return value; results are saved as CSV files in the respective directories.
#' @details
#' The function iterates over the specified number of folds and KNN models.
#' For each model, it reads true labels (`y_true.csv`) and predicted labels (`y_predict.csv`),
#' aggregates the results, and writes them back to a consolidated CSV file.
#' Temporary files are removed to free up space.
#'
#' @examples
#' \dontrun{
#' parameters <- list(
#'   Number.Folds = 5,
#'   Folders = list(
#'     folderPartitions = "/path/to/partitions",
#'     folderTestSilho = "/path/to/test_silho",
#'     folderCommunities = "/path/to/communities"
#'   ),
#'   Choosed = list(
#'     all.methods.choosed = data.frame(split = c(1, 2, 3), sparsification = c("knn-1", "knn-2", "knn-3")),
#'     all.partitions.choosed = data.frame(fold = c(1, 2, 3), knn = c(1, 2, 3))
#'   ),
#'   best.silhouette = list(
#'     all.silhouette = data.frame(fold = c(1, 2, 3), knn = c(1, 2, 3), part = c(5, 6, 7))
#'   )
#' )
#' silho.gather.predicts(parameters)
#' }
#'
#' @export
silho.gather.predicts <- function(parameters){

  f = 1
  gatherR <- foreach(f = 1:parameters$Number.Folds) %dopar%{
    #while(f<=parameters$Number.Folds){

    cat("\n#========================================================#")
    cat("\n# Fold: ", f, "                                          #")
    cat("\n#========================================================#")

    #########################################################################
    FolderRoot = "~/TcpKnnH"
    FolderScripts = paste(FolderRoot, "/R", sep="")

    setwd(FolderScripts)
    source("utils.R")

    setwd(FolderScripts)
    source("libraries.R")

    ##########################################################################
    constroiParticoes <- function(TotalParticoes){

      data <- data.frame(matrix(NA,    # Create empty data frame
                                nrow = TotalParticoes,
                                ncol = 2))

      names(data) = c("numberPartition", "numberGroup")

      i = 1
      a = 1
      while(i<=nrow(data)){
        data[i,1] = a + 1
        data[i,2] = a + 1
        i = i + 1
        a = a + 1
        gc()
      }

      return(data)

    }

    ###################################################################

    # "/dev/shm/j-GpositiveGO/Partitions/Split-1"
    FolderPSplit = paste(parameters$Folders$folderPartitions,
                         "/Split-", f, sep="")

    # "/dev/shm/j-GpositiveGO/Test-Silho/Split-1"
    FolderTSplit = paste(parameters$Folders$folderTestSilho,
                         "/Split-", f, sep="")

    # "/dev/shm/j-GpositiveGO/Communities/Split-1"
    FolderCommSplit = paste(parameters$Folders$folderCommunities,
                            "/Split-", f, sep="")

    #######################################################3
    all = data.frame(parameters$Choosed$all.methods.choosed)
    all.2 = filter(all, all$split == f)
    total = nrow(all.2)

    # KNN
    k = 1
    while(k<=total){

      cat("\n#=========================================================")
      cat("\n# Knn = ", k)
      cat("\n#=========================================================")

      FolderComm = paste(FolderCommSplit, "/knn-", k, sep="")
      FolderPartKnn = paste(FolderPSplit, "/knn-", k, sep="")
      FolderTKnn = paste(FolderTSplit, "/Knn-", k, sep="")

      all = data.frame(parameters$Choosed$all.methods.choosed)
      all.2 = data.frame(filter(all, all$split == f))

      nome = paste("knn-", k, sep="")
      all.3 = data.frame(filter(all.2, all.2$sparsification == nome))

      partitions = data.frame(parameters$Choosed$all.partitions.choosed)
      partitions = data.frame(filter(partitions, fold == f))
      partitions = data.frame(filter(partitions, knn == k))

      best = data.frame(parameters$best.silhouette$all.silhouette)
      best = data.frame(filter(best, best$fold==f))
      best = data.frame(filter(best, best$knn==k))

      num.part = best$part
      num.groups = best$part


      ################################################################
      apagar = c(0)
      y_true = data.frame(apagar)
      y_pred = data.frame(apagar)

      # GROUP
      g = 1
      while(g<=num.groups){

        cat("\n#=========================================================")
        cat("\n# Group = ", g)
        cat("\n#=========================================================")

        FolderTestGroup = paste(FolderTKnn, "/Group-", g, sep="")

        #cat("\n\nGather y_true ", g)
        setwd(FolderTestGroup)
        #setwd(FolderTG)
        y_true_gr = data.frame(read.csv("y_true.csv"))
        y_true = cbind(y_true, y_true_gr)

        setwd(FolderTestGroup)
        #setwd(FolderTG)
        #cat("\n\nGather y_predict ", g)
        y_pred_gr = data.frame(read.csv("y_predict.csv"))
        y_pred = cbind(y_pred, y_pred_gr)

        #cat("\n\nDeleting files")
        unlink("y_true.csv", recursive = TRUE)
        unlink("y_predict.csv", recursive = TRUE)
        unlink("inicioFimRotulos.csv", recursive = TRUE)

        g = g + 1
        gc()
      } # FIM DO GRUPO

      #cat("\n\nSave files ", g, "\n")
      setwd(FolderTKnn)
      y_pred = y_pred[,-1]
      y_true = y_true[,-1]
      write.csv(y_pred, "y_predict.csv", row.names = FALSE)
      write.csv(y_true, "y_true.csv", row.names = FALSE)

      k = k + 1
      gc()
    } # FIM DO KNN

    #f = f + 1
    gc()
  } # fim do foreach

  gc()
  cat("\n###############################################################")
  cat("\n# Gather Predicts: END                                        #")
  cat("\n###############################################################")
  cat("\n\n\n\n")

} # fim da função


#' Evaluate Silhouette Test
#'
#' This function evaluates the silhouette test by iterating over a given number of folds.
#' It processes multiple directories, loads necessary scripts, and computes evaluation metrics.
#'
#' @param parameters A list containing various settings and paths required for the evaluation, including:
#'   - `Number.Folds`: The number of folds for evaluation.
#'   - `Folders$folderPartitions`: Path to partition folders.
#'   - `Folders$folderTestSilho`: Path to test silhouette folders.
#'   - `Folders$folderCommunities`: Path to community folders.
#'   - `Choosed$all.methods.choosed`: Data frame with chosen methods.
#'   - `Choosed$all.partitions.choosed`: Data frame with chosen partitions.
#'   - `best.silhouette$all.silhouette`: Data frame with silhouette scores.
#'
#' @return None. The function writes evaluation results to files.
#' @export
silho.evaluate.test <- function(parameters){

  f = 1
  avalParal <- foreach(f = 1:parameters$Number.Folds) %dopar%{
  #while(f<=parameters$Number.Folds){

    cat("\n#=========================================================")
    cat("\n# Fold: ", f)
    cat("\n#=========================================================")

    folders = list()

    ##############################################################
    FolderRoot = "~/TcpKnnH"
    FolderScripts = paste(FolderRoot, "/R/", sep="")

    setwd(FolderScripts)
    source("utils.R")

    setwd(FolderScripts)
    source("libraries.R")

    #################################################################
    # "/dev/shm/j-GpositiveGO/Partitions/Split-1"
    FolderPSplit = paste(parameters$Folders$folderPartitions,
                         "/Split-", f, sep="")

    # "/dev/shm/j-GpositiveGO/Test-Silho/Split-1"
    FolderTSplit = paste(parameters$Folders$folderTestSilho,
                         "/Split-", f, sep="")

    # "/dev/shm/j-GpositiveGO/Communities/Split-1"
    FolderCommSplit = paste(parameters$Folders$folderCommunities,
                            "/Split-", f, sep="")

    #######################################################3
    all = data.frame(parameters$Choosed$all.methods.choosed)
    all.2 = filter(all, all$split == f)
    total = nrow(all.2)

    # KNN
    k = 1
    while(k<=total){

      cat("\n#=========================================================")
      cat("\n# Knn = ", k)
      cat("\n#=========================================================")

      FolderPartKnn = paste(FolderPSplit, "/knn-", k, sep="")
      FolderTKnn = paste(FolderTSplit, "/Knn-", k, sep="")
      FolderComm = paste(FolderCommSplit, "/knn-", k, sep="")

      all = data.frame(parameters$Choosed$all.methods.choosed)
      all.2 = data.frame(filter(all, all$split == f))

      nome = paste("knn-", k, sep="")
      all.3 = data.frame(filter(all.2, all.2$sparsification == nome))

      partitions = data.frame(parameters$Choosed$all.partitions.choosed)
      partitions = data.frame(filter(partitions, fold == f))
      partitions = data.frame(filter(partitions, knn == k))

      best = data.frame(parameters$best.silhouette$all.silhouette)
      best = data.frame(filter(best, best$fold==f))
      best = data.frame(filter(best, best$knn==k))

      #cat("\nData frame")
      apagar = c(0)
      confMatPartitions = data.frame(apagar)
      partitions = c()

      #cat("\nGet the true and predict lables")
      setwd(FolderTKnn)
      #setwd(FolderKnn)
      y_true = data.frame(read.csv("y_true.csv"))
      y_pred = data.frame(read.csv("y_predict.csv"))

      #cat("\nCompute measures multilabel")
      y_true2 = data.frame(sapply(y_true, function(x) as.numeric(as.character(x))))
      y_true3 = mldr_from_dataframe(y_true2 , labelIndices = seq(1,ncol(y_true2 )), name = "y_true2")
      y_pred2 = sapply(y_pred, function(x) as.numeric(as.character(x)))

      #cat("\nSave Confusion Matrix")
      setwd(FolderTKnn)
      #setwd(FolderKnn)
      salva3 = paste("Conf-Mat-Fold-", f, "-Knn-", k, ".txt", sep="")
      sink(file=salva3, type="output")
      confmat = multilabel_confusion_matrix(y_true3, y_pred2)
      print(confmat)
      sink()

      #cat("\nCreating a data frame")
      confMatPart = multilabel_evaluate(confmat)
      confMatPart = data.frame(confMatPart)
      names(confMatPart) = paste("Fold-", f, "-Knn-", k, sep="")
      namae = paste("Split-", f, "-knn-", k,"-Evaluated.csv", sep="")
      setwd(FolderTKnn)
      write.csv(confMatPart, namae)

      #cat("\nDelete files")
      setwd(FolderTKnn)
      unlink("y_true.csv", recursive = TRUE)
      unlink("y_predict.csv", recursive = TRUE)

      k = k + 1
      gc()
    } # FIM DO KNN

    #f = f + 1
    gc()
  } # fim do for each

  gc()
  cat("\n############################################################")
  cat("\n# TEST SILHO: Evaluation Folds END                         #")
  cat("\n############################################################")
  cat("\n\n\n\n")
}


#' Build an Empty Data Frame for Silhouette Evaluation
#'
#' This function creates an empty data frame with predefined evaluation measures.
#'
#' @return A data frame with 22 rows and 11 columns, where the first column is filled with measure names.
#' @export
silho.build.data.frame <- function(){

  data <- data.frame(matrix(NA,    # Create empty data frame
                            nrow = 22,
                            ncol = 11))

  measures = c("accuracy", "average-precision", "clp", "coverage", "F1",
               "hamming-loss", "macro-AUC", "macro-F1", "macro-precision",
               "macro-recall", "margin-loss", "micro-AUC", "micro-F1",
               "micro-precision", "micro-recall", "mlp", "one-error",
               "precision", "ranking-loss", "recall", "subset-accuracy", "wlp")

  data$X1 = measures

  return(data)

}


#' Gather Evaluated Silhouette Test Results
#'
#' This function collects and organizes evaluation results from multiple folds and KNN runs.
#'
#' @param parameters A list containing necessary settings and paths, including:
#'   - `Number.Folds`: The number of folds to process.
#'   - `Folders$folderPartitions`: Path to partition folders.
#'   - `Folders$folderTestSilho`: Path to test silhouette folders.
#'   - `Choosed$all.methods.choosed`: Data frame of selected methods.
#'
#' @return None. The function writes aggregated evaluation results to CSV files.
#' @export
silho.gather.evaluated <- function(parameters){

  # vector with names
  measures = c("accuracy","average-precision","clp","coverage","F1",
               "hamming-loss","macro-AUC", "macro-F1","macro-precision",
               "macro-recall","margin-loss","micro-AUC","micro-F1",
               "micro-precision","micro-recall","mlp","one-error",
               "precision","ranking-loss", "recall","subset-accuracy","wlp")

  # from fold = 1 to number_folders
  f = 1
  while(f<=parameters$Number.Folds){

    # data frame
    apagar = c(0)
    avaliadoFinal = data.frame(apagar, measures)
    avaliadoKnn = data.frame(apagar, measures)
    folds = c(0)
    threshold = c(0)
    nomesThreshold = c(0)
    nomesFolds = c(0)

    ######################################################################

    # "/dev/shm/j-GpositiveGO/Partitions/Split-1"
    FolderPSplit = paste(parameters$Folders$folderPartitions,
                         "/Split-", f, sep="")

    # "/dev/shm/j-GpositiveGO/Test-Silho/Split-1"
    FolderTSplit = paste(parameters$Folders$folderTestSilho,
                         "/Split-", f, sep="")

    #######################################################3
    all = data.frame(parameters$Choosed$all.methods.choosed)
    all.2 = filter(all, all$split == f)
    total = nrow(all.2)

    # KNN
    k = 1
    while(k<=total){

      cat("\n#==========================")
      cat("\n# Fold \t", f)
      cat("\n# Knn \t", k)
      cat("\n#==========================")

      # "/dev/shm/j-GpositiveGO/Partitions/Split-1/knn-1"
      FolderPartKnn = paste(FolderPSplit, "/knn-", k, sep="")


      # "/dev/shm/j-GpositiveGO/Test-Silho/Split-1/Knn-1"
      FolderTKnn = paste(FolderTSplit, "/Knn-", k, sep="")

      ######################################################################
      setwd(FolderTKnn)
      str = paste("Split-", f, "-knn-", k, "-Evaluated.csv", sep="")
      avaliado = data.frame(read.csv(str))
      names(avaliado)[1] = "medidas"
      avaliadoKnn = cbind(avaliadoKnn, avaliado[,2])
      nomesThreshold[k] = paste("Fold-", f, "-Knn-", k, sep="")
      names(avaliadoKnn)[k+2] = nomesThreshold[k]
      unlink(str, recursive = TRUE)

      k = k + 1
      gc()
    } # FIM DO KNN

    avaliadoKnn = avaliadoKnn[,-1]
    setwd(FolderTSplit)
    write.csv(avaliadoKnn, paste("Evaluated-Fold-", f, ".csv", sep=""),
              row.names = FALSE)

    f = f + 1
    gc()

  } # end folds


  gc()
  cat("\n###############################################################")
  cat("\n# TEST SILHO: Gather Evaluations End                          #")
  cat("\n###############################################################")
  cat("\n\n\n\n")
}

#' @title Organize Evaluation Results for Silhouette Test
#' @description This function processes and organizes evaluation results from multiple folds of a silhouette-based clustering test.
#'
#' @param parameters A list containing necessary parameters, including folder paths and the number of folds.
#'
#' @details
#' - Reads evaluation results from each fold.
#' - Constructs a dataframe for each fold and stores evaluation metrics.
#' - Computes summary statistics (mean, median, standard deviation) across all folds.
#' - Saves the processed evaluation results in CSV format.
#'
#' @return Saves organized evaluation data into CSV files, categorized by evaluation metrics across multiple folds.
#'
silho.organize.evaluation <- function(parameters){

  dfs = list()
  dfs2 = list()

  x = 1
  while(x<=parameters$Number.Folds){
    dfs[[x]] = silho.build.data.frame()
    x = x + 1
    gc()
  }

  cat("\n#=======================================")
  # from fold = 1 to number_folders
  f = 1
  while(f<=parameters$Number.Folds){

    cat("\n# Fold: ", f)

    ################################################################

    FolderPartSplit = paste(parameters$Folders$folderPartitions,
                            "/Split-", f, sep="")

    setwd(FolderPartSplit)
    knn_H = data.frame(read.csv(paste("fold-", f,
                                      "-knn-h-choosed.csv", sep="")))
    total_knn_H = nrow(knn_H)

    FolderTSplit = paste(parameters$Folders$folderTestSilho,
                         "/Split-", f, sep="")

    #########################################################
    #setwd(FolderTempKnn)
    setwd(FolderTSplit)
    str = paste("Evaluated-Fold-", f, ".csv", sep="")
    dfs2[[f]] = data.frame(read.csv(str))

    unlink(str, recursive = TRUE)

    f = f + 1
    gc()

  } # end folds
  cat("\n#========================================")


  numCol = ncol(dfs2[[1]])-1

  # vector with names
  measures = c("accuracy", "average-precision", "clp", "coverage",
               "F1", "hamming-loss", "macro-AUC", "macro-F1",
               "macro-precision", "macro-recall", "margin-loss",
               "micro-AUC", "micro-F1", "micro-precision", "micro-recall",
               "mlp", "one-error", "precision", "ranking-loss",
               "recall", "subset-accuracy", "wlp")
  apagar = c(0)
  nomesKnn = c()
  nomes = c()

  k = 1
  while(k<=numCol){
    cat("\n\nK: ", k)

    resultado = data.frame(measures, apagar)
    nomesFolds = c()
    nomeKnn1 = paste("Evaluated-10Folds-Knn-", k, ".csv", sep="")
    nomeKnn2 = paste("Mean-10Folds-Knn-", k, ".csv", sep="")
    nomeKnn3 = paste("Median-10Folds-Knn-", k, ".csv", sep="")
    nomeKnn4 = paste("SD-10Folds-Knn-", k, ".csv", sep="")

    cat("\n#========================================")
    f = 1
    while(f<=parameters$Number.Folds){

      cat("\n# Fold: ", f)

      # pegando apenas o fold especifico
      res = data.frame(dfs2[[f]])
      nomesColunas = colnames(res)

      # pegando a partir da segunda coluna
      a = k + 1
      res = res[,a]

      resultado = cbind(resultado, res)
      b = ncol(resultado)
      names(resultado)[b] = nomesColunas[a]

      nomes[f] = paste("Fold-",f,"-Knn-", k, sep="")

      f = f + 1
      gc()
    } # fim do fold
    cat("\n#========================================")


    resultado = data.frame(resultado[,-2])
    setwd(parameters$Folders$folderTestSilho)
    write.csv(resultado, nomeKnn1, row.names = FALSE)

    # calculando a média dos 10 folds para cada medida
    media = data.frame(apply(resultado[,-1], 1, mean))
    media = cbind(measures, media)
    names(media) = c("Measures", "Mean10Folds")
    write.csv(media, nomeKnn2, row.names = FALSE)

    mediana = data.frame(apply(resultado[,-1], 1, median))
    mediana = cbind(measures, mediana)
    names(mediana) = c("Measures", "Median10Folds")
    write.csv(mediana, nomeKnn3, row.names = FALSE)

    dp = data.frame(apply(resultado[,-1], 1, sd))
    dp = cbind(measures, dp)
    names(dp) = c("Measures", "SD10Folds")
    write.csv(dp, nomeKnn4, row.names = FALSE)

    k = k + 1
    gc()
  } # fim do k

  gc()
  cat("\n################################################################")
  cat("\n# TEST SILHO: Organize End                                     #")
  cat("\n################################################################")
  cat("\n\n\n\n")
}



#' Run the Silhouette Test Workflow
#'
#' This function executes the full Silhouette test workflow, including building, testing, evaluating, and organizing partitions.
#'
#' @param parameters A list containing necessary settings and paths, including:
#'   - `Folders$folderTestSilho`: Path to store test silhouette results.
#'   - `Dataset.Name`: Name of the dataset being processed.
#'
#' @return None. The function writes runtime statistics to a CSV file.
#' @export
testSilhouette <- function(parameters){


  cat("\n\n#############################################################")
    cat("\n# RUN TEST SILHO: build and test partitions                 #")
    cat("\n#############################################################\n\n")
  timeBuild = system.time(resBT <- silho.build.test(parameters))


  cat("\n\n############################################################")
    cat("\n# RUN TEST SILHO: Matrix Confusion                         #")
    cat("\n############################################################\n\n")
  timeSplit = system.time(resGather <- silho.gather.predicts(parameters))


  cat("\n\n#############################################################")
    cat("\n# RUN TEST SILHO: Evaluation Fold                           #")
    cat("\n#############################################################\n\n")
  timeAvalia = system.time(resEval <- silho.evaluate.test(parameters))


  cat("\n\n#############################################################")
    cat("\n# RUN TEST SILHO: Gather Evaluation                         #")
    cat("\n#############################################################\n\n")
  timeGather = system.time(resGE <- silho.gather.evaluated(parameters))


  cat("\n\n###########################################################")
    cat("\n# RUN TEST SILHO: Organize Evaluation                     #")
    cat("\n###########################################################\n\n")
  timeOrg = system.time(resOA <- silho.organize.evaluation(parameters))


  cat("\n\n############################################################")
    cat("\n# RUN TEST SILHO: Save Runtime                             #")
    cat("\n############################################################\n\n")
  Runtime = rbind(timeBuild,
                  timeSplit,
                  timeAvalia,
                  timeGather,
                  timeOrg)
  setwd(parameters$Folders$folderTestSilho)
  write.csv(Runtime, paste(parameters$Dataset.Name,
                           "-test-silho-runtime-1.csv", sep=""),
            row.names = FALSE)
}


###############################################################################
# CONTACT INFORMATION                                                          #
# In case of errors, contact: elainececiliagatto@gmail.com                     #
# Thank you for your collaboration!                                            #
###############################################################################
