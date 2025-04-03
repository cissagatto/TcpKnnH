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

cat("\n\n##############################################################")
cat("\n# TcpKnnH SET WORK SPACE                                  #")
cat("\n##############################################################\n\n")

FolderRoot = "~/TcpKnnH"
FolderScripts = "~/TcpKnnH/R"


##################################################################################################
# Runs for all datasets listed in the "datasets.csv" file                                        #
# n_dataset: number of the dataset in the "datasets.csv"                                         #
# number_cores: number of cores to paralell                                                      #
# number_folds: number of folds for cross validation                                             #
# delete: if you want, or not, to delete all folders and files generated                         #
##################################################################################################
execute <- function(parameters){

  FolderRoot = "~/TcpKnnH"
  FolderScripts = "~/TcpKnnH/R"

  if(parameters$Number.Cores  == 0){
    cat("\n\n##################################################################################################")
      cat("\n# Zero is a disallowed value for number_cores. Please choose a value greater than or equal to 1. #")
      cat("\n##################################################################################################\n\n")
  } else {
    cl <- parallel::makeCluster(parameters$Number.Cores)
    doParallel::registerDoParallel(cl)
    print(cl)

    if(parameters$Number.Cores==1){
      cat("\n\n###########################################################")
        cat("\n# RUN: Running Sequentially!                              #")
        cat("\n###########################################################\n\n")
    } else {
      cat("\n\n######################################################################")
        cat("\n# RUN: Running in parallel with ", parameters$Number.Cores, " cores! #")
        cat("\n######################################################################\n\n")
    }
  }

  retorno = list()

  setwd(FolderScripts)
  source("libraries.R")

  setwd(FolderScripts)
  source("utils.R")

  setwd(FolderScripts)
  source("run-rf.R")

  setwd(FolderScripts)
  source("test-asoc.R")

  cat("\n\n################################################################")
    cat("\n# RUN: Get the label space                                     #")
    cat("\n################################################################\n\n")
  timeLabelSpace = system.time(resLS <- labelSpace(parameters))
  parameters$LabelSpace = resLS

  cat("\n\n#######################################################")
  cat("\n# RUN RF: Get labels                                  #")
  cat("\n#######################################################\n\n")
  arquivo = paste(parameters$Folders$folderNamesLabels, "/" ,
                  dataset_name,"-NamesLabels.csv",sep = "")
  namesLabels = data.frame(read.csv(arquivo))
  colnames(namesLabels) = c("id", "labels")
  namesLabels = c(namesLabels$labels)
  parameters$Config$NamesLabels = namesLabels


  if(parameters$Validation=="Silhouette"){

    cat("\n\n################################################################")
    cat("\n# RUN: SILHOUETTE                                    #")
    cat("\n################################################################\n\n")

    # setwd(FolderScripts)
    # source("validateSilho.R")
    # source("testSilho.R")
    #
    # cat("\n\n################################################################")
    # cat("\n# RUN: choosed                                     #")
    # cat("\n################################################################\n\n")
    # timeChoosed = system.time(resChoosed <- silho.choosed(parameters))
    # parameters$Choosed = resChoosed
    #
    #
    # cat("\n\n#############################################################")
    #   cat("\n# VALIDATION WITH SILHOUETTE                                #")
    #   cat("\n#############################################################\n\n")
    # timeVal = system.time(resTHP <- silhouete(parameters))
    #
    #
    # cat("\n\n#############################################################")
    # cat("\n# BEST SILHOUETTE                                             #")
    # cat("\n#############################################################\n\n")
    # timeBest = system.time(resBest <- silho.best.partitions(parameters))
    # parameters$best.silhouette = resBest


    # cat("\n\n#######################################################")
    # cat("\n# COPY VALIDATION TO GOOGLE DRIVE                       #")
    # cat("\n#########################################################\n\n")
    # origem1 = parameters$Folders$folderValSilho
    # destino1 = paste("nuvem:Clus/Communities/Test/",
    #                  similarity, "/Silhouette/", dataset_name,
    #                  "/Knn-H/Validation", sep="")
    # comando1 = paste("rclone copy ", origem1, " ",
    #                  destino1, sep="")
    # cat("\n\n\n", comando1, "\n\n\n")
    # a = print(system(comando1))
    # a = as.numeric(a)
    # if(a != 0){
    #   stop("Erro RCLONE")
    #   quit("yes")
    # }
    # cat("\n\n")

    # str <- paste(parameters$Folders$folderRepSim, "/", parameters$Dataset.Name, sep="")
    # if (!dir.exists(str)) dir.create(str, recursive = TRUE)
    #
    # # Compactar a pasta de validação
    # tar_file_validation <- paste(parameters$Dataset.Name, "-validation.tar.gz", sep="")
    # str2 <- paste("tar -czf ", tar_file_validation, " -C ", parameters$Folders$folderValidation, " .", sep="")
    # print(system(str2))
    #
    # # Mover o arquivo compactado para o destino
    # str3 <- paste("mv ", tar_file_validation, " ", str, sep="")
    # print(system(str3))
    #
    # # Compactar a pasta de relatórios
    # tar_file_reports <- paste(parameters$Dataset.Name, "-validation.tar.gz", sep="")
    # str4 <- paste("tar -czf ", tar_file_reports, " -C ", parameters$Folders$folderReports, " .", sep="")
    # print(system(str4))
    #
    # # Mover o arquivo compactado para o destino
    # str5 <- paste("mv ", tar_file_reports, " ", str, sep="")
    # print(system(str5))
#
#
#     cat("\n\n############################################################")
#     cat("\n# DELETING VALIDATION DIRECTORY                            #")
#     cat("\n############################################################\n\n")
#     system(paste("rm -r ", parameters$Folders$folderValidation, sep=""))

    if(parameters$Classifier=="Clus"){

      cat("\n\n############################################################")
      cat("  \n# TEST CLUS SIHOUETTE                                      #")
      cat("  \n############################################################\n\n")

    } else {

      cat("\n\n##########################################################")
      cat("  \n# TEST RANDOM FORESTS SIHOUETTE                          #")
      cat("  \n##########################################################\n\n")
      timeTRFS = system.time(resTRFS <- execute.run.rf(parameters))
      result_set <- t(data.matrix(timeTRFS))
      setwd(parameters$Folders$folderTest)
      write.csv(result_set, "runtime-run.csv", row.names = FALSE)


    }


  } else if (parameters$Validation=="Macro-F1"){

    cat("\n\n#####################################################")
    cat("  \n# RUN: MACRO-F1                                     #")
    cat("  \n#####################################################\n\n")


    cat("\n\n################################################################")
    cat("\n# RUN: CHOOSED                                     #")
    cat("\n################################################################\n\n")
    timeChoosed = system.time(resChoosed <- maf1.choosed(parameters))
    parameters$Choosed = resChoosed


    cat("\n\n#############################################################")
      cat("\n# VALIDATION WITH CLUS MACRO-F1                             #")
      cat("\n#############################################################\n\n")
    timeVal = system.time(resTHP <- maf1.validate(parameters))


    cat("\n\n#############################################################")
      cat("\n# BEST PARTITIONS MACRO-F1                                  #")
      cat("\n#############################################################\n\n")
    parameters$Best = 8
    timeBest = system.time(resTHP <- maf1.best.partitions(parameters))


    # cat("\n\n#############################################################")
    #   cat("\n# RUN COPY VALIDATION TO GOOGLE DRIVE                       #")
    #   cat("\n#############################################################\n\n")
    # origem1 = parameters$Folders$folderValMaF1
    # destino1 = paste("nuvem:Clus/Communities/Test/",
    #                  similarity, "/Macro-F1/", dataset_name,
    #                  "/Knn-H/Validation", sep="")
    # comando1 = paste("rclone copy ", origem1, " ",
    #                  destino1, sep="")
    # cat("\n\n\n", comando1, "\n\n\n")
    # a = print(system(comando1))
    # a = as.numeric(a)
    # if(a != 0){
    #   stop("Erro RCLONE")
    #   quit("yes")
    # }
    # cat("\n\n")

    str = paste(parameters$Folders$folderRepMaF1, "/",
                parameters$Dataset.Name, sep="")
    if(dir.exists(str)==FALSE){dir.create(str)}

    str2 = paste("cp -r ", parameters$Folders$folderValMaF1,
                 " ", str, sep="")
    print(system(str2))

    str2 = paste("cp -r ", parameters$Folders$folderReports ,
                 "/* ", str , sep="")
    print(system(str2))


    cat("\n\n#############################################################")
    cat("\n# DELETING VALIDATION DIR                                     #")
    cat("\n#############################################################\n\n")
    system(paste("rm -r ", parameters$Folders$folderValMaF1, sep=""))


    cat("\n\n#############################################################")
    cat("\n# CHOOSED                                     #")
    cat("\n#############################################################\n\n")
    timeChoosed = system.time(resChoosed <- maf1.choosed(parameters))
    parameters$choosed = resChoosed


    if(parameters$Classifier=="Clus"){
      cat("\n\n#############################################################")
      cat("\n# TEST WITH CLUS MACRO-F1                                   #")
      cat("\n#############################################################\n\n")
      timeTest = system.time(resTHP <- test.MacroF1(parameters))

    } else {


    }

  } else if (parameters$Validation=="Micro-F1"){

    cat("\n\n#####################################################")
    cat("  \n# RUN: MICRO-F1                                    #")
    cat("  \n#####################################################\n\n")

  } else {

    cat("\n\n#####################################################")
    cat("  \n# RUN: HAMMING LOSS                                 #")
    cat("  \n#####################################################\n\n")

  }

  cat("\n\n#############################################################")
  cat("\n# RUN: Stop Parallel                                          #")
  cat("\n###############################################################\n\n")
  on.exit(stopCluster(cl))

  cat("\n\n#############################################################")
  cat("\n# RUN: END                                                    #")
  cat("\n###############################################################\n\n")

  gc()

}

###############################################################################
# CONTACT INFORMATION                                                          #
# In case of errors, contact: elainececiliagatto@gmail.com                     #
# Thank you for your collaboration!                                            #
###############################################################################
