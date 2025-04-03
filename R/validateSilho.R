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

#' Perform Silhouette Analysis for kNN-Based Multilabel Partitioning
#'
#' This function computes the silhouette scores for different kNN-based partitions
#' to evaluate clustering quality in a multilabel setting.
#'
#' @param parameters A list containing the following elements:
#'   - `Number.Folds` (integer): Number of folds for cross-validation.
#'   - `Folders$folderValidation` (string): Path to validation folder.
#'   - `Folders$folderCommunities` (string): Path to communities folder.
#'   - `Folders$folderPartitions` (string): Path to partitions folder.
#'   - `LabelSpace$Classes` (list): List of label space classes.
#'   - `Dataset.Info$Labels` (integer): Number of labels in the dataset.
#'
#' @return No explicit return value. The function generates CSV reports containing:
#'   - Silhouette scores for each partition.
#'   - Best partition per fold based on silhouette values.
#'
#' @details
#' The function follows these steps:
#' - Iterates through cross-validation folds.
#' - Reads dataset label information and partitions.
#' - Iterates through different kNN configurations, extracting partition data.
#' - Computes silhouette scores:
#'   - If a partition has only one group, assigns NA.
#'   - If multiple groups exist, computes silhouette values and saves plots.
#' - Stores results for each partition and selects the best one.
#' - Saves CSV reports for analysis.
#'
#' @examples
#' \dontrun{
#' parameters <- list(
#'   Number.Folds = 5,
#'   Folders = list(
#'     folderValidation = "/path/to/validation",
#'     folderCommunities = "/path/to/communities",
#'     folderPartitions = "/path/to/partitions"
#'   ),
#'   LabelSpace = list(Classes = list(c("A", "B", "C"))),
#'   Dataset.Info = list(Labels = 3)
#' )
#' silhouete(parameters)
#' }
#'
#' @import cluster ggplot2 dplyr
#' @export
silhouete <- function(parameters){

  f = 1
  silhoueteParalel <- foreach(f = 1:parameters$Number.Folds) %dopar% {
  #while(f<=number_folds){

    FolderRoot = "~/TcpKnnH"
    FolderScripts = "~/TcpKnnH/R"

    setwd(FolderScripts)
    source("libraries.R")
    source("utils.R")

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

    fold = c(0)
    knn = c(0)
    part = c(0)
    maximo = c(0)
    minimo = c(0)
    mediana = c(0)
    media = c(0)
    primeiroQuadrante = c(0)
    terceiroQuadrante = c(0)
    valueSilhouete = c(0)
    bestPartition = data.frame(fold, knn, part, maximo, minimo,
                               mediana, media, primeiroQuadrante,
                               terceiroQuadrante, valueSilhouete)

    ########################################################################
    #  "/dev/shm/j-GpositiveGO/Validation/Split-1"
    FolderSplitVal = paste(parameters$Folders$folderValidation,
                           "/Split-", f, sep="")
    if(dir.exists(FolderSplitVal)==FALSE){dir.create(FolderSplitVal)}

    # "/dev/shm/j-GpositiveGO/Communities/Split-1"
    FolderSplitComm = paste(parameters$Folders$folderCommunities,
                            "/Split-", f, sep="")

    # "/dev/shm/j-GpositiveGO/Partitions/Split-1"
    FolderPSplit = paste(parameters$Folders$folderPartitions,
                         "/Split-",f, sep="")

    ########################################################################
    # get the space label
    espacoDeRotulos = data.frame(parameters$LabelSpace$Classes[f])
    espacoDeRotulos2 = data.frame(t(espacoDeRotulos))
    labels = rownames(espacoDeRotulos2)
    espacoDeRotulos2 = cbind(labels, espacoDeRotulos2)
    espacoDeRotulos2 = data.frame(espacoDeRotulos2[order(espacoDeRotulos2$labels,
                                                         decreasing = FALSE),])

    ########################################################################
    #cat("\n\nGrupos por particão")
    knn_H = data.frame(read.csv(paste( FolderPSplit,
                                      "/fold-", f,
                                      "-knn-h-choosed.csv", sep="")))
    total_knn_H = nrow(knn_H)

    at = data.frame(fold = f, sparc = total_knn_H)

    write.csv(at, paste(parameters$Folders$folderValidation,
                               "/fold-", f, "-sparcification.csv",
                        sep=""),
              row.names = FALSE)

    # do primeiro knn ao ultimo
    k = 1
    while(k<=total_knn_H){

      cat("\n#=========================================================")
      cat("\n# Knn = ", k)
      cat("\n#=========================================================")

      # "/dev/shm/j-GpositiveGO/Partitions/Split-1/knn-1"
      FolderPartKnn = paste(FolderPSplit, "/knn-", k, sep="")

      # "/dev/shm/j-GpositiveGO/Communities/Split-1/knn-1"
      FolderPartComm = paste(FolderSplitComm, "/knn-", k, sep="")

      # "/dev/shm/j-GpositiveGO/Partitions/Split-1"
      FolderPSplit = paste(parameters$Folders$folderPartitions, "/Split-",f, sep="")

      # "/dev/shm/j-GpositiveGO/Validation/Split-1/knn-1"
      FolderKnnVal = paste(FolderSplitVal, "/knn-", k, sep="")
      if(dir.exists(FolderKnnVal)==FALSE){dir.create(FolderKnnVal)}

      knn_H = data.frame(read.csv(paste(FolderPSplit,
                                        "/fold-", f,
                                        "-knn-h-choosed.csv", sep="")))
      total_knn_H = nrow(knn_H)

      knn_H = knn_H[k,]

      particoes = data.frame(read.csv(paste(FolderPartComm,
                                            "/knn-", k,
                                            "-", knn_H$method,
                                            "-partitions-hierarchical.csv",
                                            sep="")))
      TotalParticoes = ncol(particoes)-1
      numPart = (parameters$Dataset.Info$Labels-1)

      ########################################################################
      fold = c(0)
      part = c(0)
      knn = c (0)
      maximo = c(0)
      minimo = c(0)
      mediana = c(0)
      media = c(0)
      primeiroQuadrante = c(0)
      terceiroQuadrante = c(0)
      valueSilhouete = c(0)
      Silhouete = data.frame(fold, part, knn, maximo, minimo, mediana, media,
                             primeiroQuadrante, terceiroQuadrante,
                             valueSilhouete)

      p = 2
      while(p<=numPart){

        cat("\n#=========================================================")
        cat("\n# Partition = ", p)
        cat("\n#=========================================================")

        # "/dev/shm/j-GpositiveGO/Validation/Split-1/knn-1/Partition-2"
        FolderPartVal = paste(FolderKnnVal, "/Partition-", p, sep="")
        if(dir.exists(FolderPartVal)==FALSE){dir.create(FolderPartVal)}

        ########################################################################
        cat("\n get the number of groups for this partition")
        particao = particoes[,c(1,p)]
        names(particao) = c("labels", "groups")

        res = constroiParticoes(TotalParticoes)
        res = filter(res, numberPartition == p)
        numGroups = as.numeric(res$numberGroup)

        #######################################################################
        if(numGroups==1){
          cat("\nOnly one group of labels (global partition)")
          fold = f
          part = p
          knn = k
          maximo = NA
          minimo = NA
          mediana = NA
          media = NA
          primeiroQuadrante = NA
          terceiroQuadrante = NA
          valueSilhouete = NA
          Silhouete = rbind(Silhouete, data.frame(fold, part, knn, maximo,
                                                  minimo, mediana, media,
                                                  primeiroQuadrante,
                                                  terceiroQuadrante,
                                                  valueSilhouete))
          write.csv(Silhouete[-1,], paste(FolderKnnVal, "/fold-", f, "-knn-",
                                          k, "-silho.csv", sep=""),
                    row.names = FALSE)

        } else {
          #cat("\ntwo or more labels in the group")
          groups_label_space = cbind(particao, espacoDeRotulos2)
          groups_label_space = groups_label_space[,c(-1,-3)]
          a = dist(groups_label_space)
          b = as.dist(a)
          sil = silhouette(groups_label_space[,1], b)
          sil = sortSilhouette(sil)
          write.csv(sil, paste(FolderPartVal, "/silho-fold", f, "-knn-", k,
                               "-part-", p, ".csv", sep=""),
                    row.names = FALSE)

          if(all(is.na(sil))==TRUE){
            #cat("\nOne label per group (local partition)\n")
            fold = f
            part = p
            knn = k
            maximo = NA
            minimo = NA
            mediana = NA
            media = NA
            primeiroQuadrante = NA
            terceiroQuadrante = NA
            valueSilhouete = NA
            Silhouete = rbind(Silhouete, data.frame(fold, part, knn, maximo,
                                                    minimo, mediana, media,
                                                    primeiroQuadrante,
                                                    terceiroQuadrante,
                                                    valueSilhouete))
            write.csv(Silhouete[-1,], paste(FolderKnnVal, "/fold-", f,
                                            "-knn-", k,
                                            "-silho.csv", sep=""),
                      row.names = FALSE)

          } else {
            #cat("\nMore than one label per group\n")
            pdf(paste(FolderPartVal, "/silho-fold-", f, "-knn-", k,
                      "-part-", p, ".pdf", sep=""), width = 10, height = 8)
            print(plot(sil))
            dev.off()
            cat("\n")

            pdf(paste(FolderPartVal, "/fviz-silh-fold-", f, "-knn-", k,
                      "-part-", p, ".pdf", sep=""), width = 10, height = 8)
            print(fviz_silhouette(sil))
            dev.off()
            cat("\n")

            # Summary of silhouette analysis
            si.sum = summary(sil)
            res.si.sum = unlist(si.sum)

            fold = f
            part = p
            knn = k
            maximo = res.si.sum$si.summary.Max.
            minimo = res.si.sum$si.summary.Min.
            mediana = res.si.sum$si.summary.Median
            media = res.si.sum$si.summary.Mean
            primeiroQuadrante = res.si.sum$`si.summary.1st Qu.`
            terceiroQuadrante = res.si.sum$`si.summary.3rd Qu.`
            valueSilhouete = res.si.sum$avg.width
            Silhouete = rbind(Silhouete, data.frame(fold, part, knn, maximo,
                                                    minimo, mediana, media,
                                                    primeiroQuadrante,
                                                    terceiroQuadrante,
                                                    valueSilhouete))
            write.csv(Silhouete[-1,], paste(FolderKnnVal, "/fold-", f,
                                            "-knn-", k, "-silho.csv", sep=""),
                      row.names = FALSE)
          } # fim do if

        } # fim do if

        p = p + 1
        gc()
      } # fim da partição

      Silhouete = Silhouete[-1,]
      indice = as.numeric(which.max(Silhouete$valueSilhouete))
      silhouete2 = Silhouete[indice,]
      bestPartition = rbind(bestPartition, silhouete2)

      k = k + 1
      gc()

    } # fim do knn

    write.csv(bestPartition[-1,], paste(FolderSplitVal, "/fold-", f,
                                        "-best-silho.csv", sep=""),
              row.names = FALSE)

    #f = f + 1
    gc()

  } # fim do fold

  gc()
  cat("\n#################################################################")
  cat("\n# END COMPUTE SILHOUETE                                         #")
  cat("\n#################################################################")
  cat("\n\n\n\n")
}


#' Process and Save kNN-Based Partitioning Methods
#'
#' This function processes multiple folds of kNN-based partitions, extracts
#' partitioning results for different methods (eb, fg, wt), and saves them
#' into CSV files for further analysis.
#'
#' @param parameters A list containing the following elements:
#'   - `Dataset.Info$Labels` (integer): Total number of labels in the dataset.
#'   - `Folders$folderReports` (string): Path to the reports directory.
#'   - `Folders$folderPartitions` (string): Path to the partitions directory.
#'   - `Number.Folds` (integer): Number of cross-validation folds.
#'
#' @return A list containing:
#'   - `all.partitions.choosed`: Data frame with all chosen partitions.
#'   - `all.methods.choosed`: Data frame with all selected kNN methods.
#'   - `all.eb.partitions`: Data frame with "eb" partitioning method results.
#'   - `all.fg.partitions`: Data frame with "fg" partitioning method results.
#'   - `all.wt.partitions`: Data frame with "wt" partitioning method results.
#'   - `sparcification`: Data frame summarizing the number of methods per fold.
#'   - `all.partitions`: Data frame of all possible partitions and groups.
#'   - `all.hybrid.partitions`: Data frame of hybrid partitions.
#'
#' @details
#' The function performs the following operations:
#' - Generates a list of partitions and groups.
#' - Iterates over each fold to read kNN partitioning results.
#' - Reads different partitioning method outputs (`eb`, `fg`, `wt`).
#' - Stores and merges results into consolidated data frames.
#' - Saves outputs as CSV files in the reports directory.
#'
#' @examples
#' \dontrun{
#' parameters <- list(
#'   Dataset.Info = list(Labels = 10),
#'   Folders = list(
#'     folderReports = "/path/to/reports",
#'     folderPartitions = "/path/to/partitions"
#'   ),
#'   Number.Folds = 5
#' )
#' results <- silho.choosed(parameters)
#' }
#'
#' @import dplyr
#' @export
silho.choosed <- function(parameters){

  retorno = list()

  # generating partitions and groups
  partitions = seq(1,parameters$Dataset.Info$Labels,by=1)
  groups = seq(1,parameters$Dataset.Info$Labels,by=1)
  all.partitions = data.frame(partitions, groups)

  partitions = seq(2,(parameters$Dataset.Info$Labels-1),by=1)
  groups = seq(2,(parameters$Dataset.Info$Labels-1),by=1)
  all.hybrid.partitions = data.frame(partitions, groups)

  # saving information
  setwd(parameters$Folders$folderReports)
  write.csv(all.partitions, "all-partitions.csv", row.names = FALSE)

  # data frames
  todos.knn.h = data.frame()
  todos.eb = data.frame()
  todos.fg = data.frame()
  todos.wt = data.frame()
  todos = data.frame()

  # vectors
  total = c(0)
  nomes = c()

  # for all folds
  f = 1
  while(f<=number_folds){
    cat("\n#=======================================================#")
    cat("\n# FOLD ", f, "                                          #")
    cat("\n#=======================================================#\n")

    # folder name
    #  "/dev/shm/j-GpositiveGO/Partitions/Split-1"
    FolderSplit = paste(parameters$Folders$folderPartitions,
                        "/Split-", f,
                        sep="")

    # file name
    # "/dev/shm/j-GpositiveGO/Partitions/Split-1/fold-1-knn-h-choosed.csv"
    knn.h = paste(FolderSplit, "/fold-", f, "-knn-h-choosed.csv", sep="")

    # open file with all methods chosen for FOLD=1
    a.knn.h = data.frame(read.csv(knn.h))

    # how many methods there are?
    total[f] = nrow(a.knn.h)

    # what is it folds?
    nomes[f] = paste("fold-",f,sep="")

    # save the data frame with that information
    todos.knn.h = rbind(todos.knn.h, a.knn.h)

    # through all knn sparcification
    k = 1
    while(k<=nrow(a.knn.h)){
      cat("\n\tKNN ", k)

      # "/dev/shm/j-GpositiveGO/Partitions/Split-1/knn-1"
      FolderKnn = paste(FolderSplit, "/knn-", k, sep="")

      # file names
      eb = paste(FolderKnn, "/knn-", k, "-eb-partitions-hierarchical.csv",
                 sep="")

      fg = paste(FolderKnn, "/knn-", k, "-fg-partitions-hierarchical.csv",
                 sep="")

      wt = paste(FolderKnn, "/knn-", k, "-wt-partitions-hierarchical.csv",
                 sep="")

      # open files
      a.eb = data.frame(read.csv(eb))
      a.fg = data.frame(read.csv(fg))
      a.wt = data.frame(read.csv(wt))

      # gather info with the specific fold and knn
      d.eb = data.frame(fold = f, knn = k, a.eb)
      d.fg = data.frame(fold = f, knn = k, a.fg)
      d.wt = data.frame(fold = f, knn = k, a.wt)

      todos.eb = rbind(todos.eb, d.eb)
      todos.fg = rbind(todos.fg, d.fg)
      todos.wt = rbind(todos.wt, d.wt)

      m.eb = data.frame(fold = f, knn = k, method = "eb", a.eb)
      m.fg = data.frame(fold = f, knn = k, method = "fg", a.fg)
      m.wt = data.frame(fold = f, knn = k, method = "wt", a.wt)

      res = rbind(m.eb, m.fg, m.wt)
      todos = rbind(todos, res)

      k = k + 1
      gc()
    } # fim do knn

    f = f + 1
    gc()
  } # fim do fold

  setwd(parameters$Folders$folderReports)

  # saving sparcification
  sparcification = data.frame(nomes, total)
  write.csv(sparcification, "sparcification.csv", row.names = FALSE)

  # saving all information
  write.csv(todos, "all-partitions.choosed.csv", row.names = FALSE)
  write.csv(todos.knn.h, "all-knn-h-choosed.csv", row.names = FALSE)
  write.csv(todos.eb, "all-eb-partitions.csv", row.names = FALSE)
  write.csv(todos.fg, "all-fg-partitions.csv", row.names = FALSE)
  write.csv(todos.wt, "all-wt-partitions.csv", row.names = FALSE)

  # return
  retorno$all.partitions.choosed = todos
  retorno$all.methods.choosed = todos.knn.h
  retorno$all.eb.partitions = todos.eb
  retorno$all.fg.partitions = todos.fg
  retorno$all.wt.partitions = todos.wt
  retorno$sparcification = sparcification
  retorno$all.partitions = all.partitions
  retorno$all.hybrid.partitions = all.hybrid.partitions
  return(retorno)

  cat("\n\n##########################################################")
  cat("\n# FINISH CHOOSED                                           #")
  cat("\n############################################################\n\n")

}


#' Extract and Save Best Silhouette Partitions
#'
#' This function processes validation folds, extracts the best silhouette partitions,
#' and computes the frequency of partitions per kNN value.
#'
#' @param parameters A list containing:
#'   - `Number.Folds` (integer): Number of validation folds.
#'   - `Folders$folderValidation` (string): Path to the validation folder.
#'   - `Folders$folderReports` (string): Path to the reports directory.
#'
#' @return A list containing:
#'   - `all.silhouette`: Data frame with best silhouette partitions.
#'   - `sparcification`: Data frame summarizing sparcification values.
#'
#' @details
#' The function performs the following:
#' - Iterates over validation folds to read silhouette results.
#' - Reads and merges sparcification data.
#' - Computes the frequency of partitions per kNN value.
#' - Saves processed data into CSV files.
#'
#' @examples
#' \dontrun{
#' parameters <- list(
#'   Number.Folds = 5,
#'   Folders = list(
#'     folderValidation = "/path/to/validation",
#'     folderReports = "/path/to/reports"
#'   )
#' )
#' results <- silho.best.partitions(parameters)
#' }
#'
#' @import dplyr
#' @export
silho.best.partitions <- function(parameters){

  retorno = list()

  all.silho = data.frame()
  sparcification = data.frame()

  cat("\n#=========================================================")
  f = 1
  while(f<=parameters$Number.Folds){

    cat("\n# FOLD = \t", f)

    FolderSplit = paste(parameters$Folders$folderValidation,
                        "/Split-", f, sep="")

    # fold-1-best-silho.csv
    nome = paste(FolderSplit, "/fold-", f, "-best-silho.csv", sep="")

    resultado = data.frame(read.csv(nome))
    all.silho = rbind(all.silho, resultado)

    # fold-1-sparcification.csv
    nome2 = paste(parameters$Folders$folderValidation
                  ,"/fold-", f, "-sparcification.csv", sep="")

    res.2 = data.frame(read.csv(nome2))
    sparcification = rbind(sparcification, res.2)

    unlink(nome2)

    f = f + 1
    gc()
  }
  cat("\n#=========================================================")

  retorno$all.silhouette = all.silho
  retorno$sparcification = sparcification

  setwd(parameters$Folders$folderReports)
  write.csv(all.silho, "best-silhouete.csv", row.names = FALSE)

  setwd(parameters$Folders$folderReports)
  write.csv(sparcification, "silho-sparcification.csv", row.names = FALSE)

  n = mean(sparcification$sparc)

  res.4 = data.frame()
  k = 1
  while(k<=n){
    res.1 = data.frame(filter(all.silho, knn == k))
    res.2 = data.frame(count(res.1, part))
    names(res.2) = c("partitions", "frequency")
    res.3 = cbind(knn = k, res.2)
    res.4 = rbind(res.4, res.3)
    k = k + 1
    gc()
  }

  setwd(parameters$Folders$folderReports)
  write.csv(res.4, "silho-frequency-partitions-knn.csv",
            row.names = FALSE)

  return(retorno)

  gc()
  cat("\n#################################################################")
  cat("\n# END BEST PARTITIONS SILHOUETE                                 #")
  cat("\n#################################################################")
  cat("\n\n\n\n")

}


###############################################################################
# CONTACT INFORMATION                                                          #
# In case of errors, contact: elainececiliagatto@gmail.com                     #
# Thank you for your collaboration!                                            #
###############################################################################
