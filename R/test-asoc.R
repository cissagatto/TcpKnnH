##############################################################################
# STANDARD HPML                                                              #
# Copyright (C) 2023                                                         #
#                                                                            #
# This code is free software: you can redistribute it and/or modify it under #
# the terms of the GNU General Public License as published by the Free       #
# Software Foundation, either version 3 of the License, or (at your option)  #
# any later version. This code is distributed in the hope that it will be    #
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty of     #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General   #
# Public License for more details.                                           #
#                                                                            #
# 1 - PhD Elaine Cecilia Gatto | Prof PhD Ricardo Cerri                      #
# 2 - Prof PhD Mauri Ferrandin                                               #
# 3 - Prof PhD Celine Vens | PhD Felipe Nakano Kenji                         #
# 4 - Prof PhD Jesse Read                                                    #
#                                                                            #
# 1 = Federal University of São Carlos - UFSCar - https://www2.ufscar.br     #
# Campus São Carlos | Computer Department - DC - https://site.dc.ufscar.br | #
# Post Graduate Program in Computer Science - PPGCC                          #
# http://ppgcc.dc.ufscar.br | Bioinformatics and Machine Learning Group      #
# BIOMAL - http://www.biomal.ufscar.br                                       #
#                                                                            #
# 2 - Federal University of Santa Catarina Campus Blumenau - UFSC            #
# https://ufsc.br/                                                           #
#                                                                            #
# 3 - Katholieke Universiteit Leuven Campus Kulak Kortrijk Belgium           #
# Medicine Department - https://kulak.kuleuven.be/                           #
# https://kulak.kuleuven.be/nl/over_kulak/faculteiten/geneeskunde            #
#                                                                            #
# 4 - Ecole Polytechnique | Institut Polytechnique de Paris | 1 rue Honoré   #
# d’Estienne d’Orves - 91120 - Palaiseau - FRANCE                            #
#                                                                            #
##############################################################################



##################################################
# SET WORK SPACE
##################################################
FolderRoot = "~/TcpKnnH"
FolderScripts = "~/TcpKnnH/R"


#########################################################################
#
#########################################################################
build.rf.silho <- function(parameters){

  # retorno <-list()

  # f = 1
  bthpkParalel <- foreach(f = 1:parameters$Number.Folds) %dopar% {
  # while(f<=parameters$Number.Folds){

    resultados_fold <- list()

    cat("\n\n=================================================")
    cat("\nFold: ", f)

    #########################################################################
    FolderRoot = "~/TcpKnnH"
    FolderScripts = "~/TcpKnnH/R"

    #########################################################################
    setwd(FolderScripts)
    source("libraries.R")

    setwd(FolderScripts)
    source("utils.R")

    #########################################################################
    cat("\nCreating Folders from Best Partitions and Splits Tests")
    FolderPartitionsSplit = paste(parameters$Folders$folderPartitions,
                                  "/Split-", f, sep = "")
    FolderTestedSplit = paste(parameters$Folders$folderTest,
                                "/Split-", f, sep = "")
    if(dir.create(FolderTestedSplit) == FALSE){dir.create(FolderTestedSplit)}

    ########################################################################
    #cat("\nOpen Train file ", f, "\n")
    nome_arq_tr = paste(parameters$Folders$folderCVTR, "/",
                        parameters$Dataset.Name,
                        "-Split-Tr-", f,".csv", sep="")
    arquivo_tr = data.frame(read.csv(nome_arq_tr))

    #####################################################################
    #cat("\nOpen Validation file ", f, "\n")
    nome_arq_vl = paste(parameters$Folders$folderCVVL,
                        "/", parameters$Dataset.Name,
                        "-Split-Vl-", f,".csv", sep="")
    arquivo_vl = data.frame(read.csv(nome_arq_vl))

    ########################################################################
    #cat("\nOpen Test file ", f, "\n")
    nome_arq_ts = paste(parameters$Folders$folderCVTS, "/",
                        parameters$Dataset.Name,
                        "-Split-Ts-", f, ".csv", sep="")
    arquivo_ts = data.frame(read.csv(nome_arq_ts))

    ########################################################################
    # juntando treino com validação
    arquivo_tr2 = rbind(arquivo_tr, arquivo_vl)

    #########################################################################
    itens = dir(FolderPartitionsSplit, full.names = TRUE)
    pastas = itens[file.info(itens)$isdir]
    knn_pastas = pastas[grep("/knn-", pastas)]
    tamanho = length(knn_pastas)

    # DO KNN 1 ATÉ O ÚLTIMO
    u = 1
    while(u<=tamanho){

      FolderPartitionKnn = paste(FolderPartitionsSplit, "/knn-", u, sep="")

      FolderKnn = paste(FolderTestedSplit, "/Knn-", u, sep="")
      if(dir.create(FolderKnn)==FALSE){dir.create(FolderKnn)}

      file.name = paste(FolderPartitionKnn, "/knn-", u,
                        "-h-partition.csv", sep="")
      particao = data.frame(read.csv(file.name))
      number.clusters = length(unique(particao$groups))

      # DO GRUPO 1 ATÉ O ÚLTIMO
      g = 1
      while(g<=number.clusters){

        FolderClusters = paste(FolderKnn, "/cluster-", g, sep="")
        if(dir.create(FolderClusters)==FALSE){dir.create(FolderClusters)}

        specificGroup = data.frame(filter(particao, groups == g))
        number.labels = nrow(specificGroup)

        fim = parameters$Dataset.Info$LabelStart + (nrow(specificGroup)-1)
        labels.indices = seq(parameters$Dataset.Info$LabelStart, fim, by=1)
        nomes.labels.clusters = specificGroup$label

        #########################################################################
        cat("\nTrain: Mount Group")
        train.attributes = arquivo_tr2[, parameters$Dataset.Info$AttStart:parameters$Dataset.Info$AttEnd]
        train.classes = select(arquivo_tr2, specificGroup$label)
        train.dataset.cluster = cbind(train.attributes, train.classes)

        #########################################################################
        cat("\nTest: Mount Group")
        test.attributes = arquivo_ts[, parameters$Dataset.Info$AttStart:parameters$Dataset.Info$AttEnd]
        test.classes = select(arquivo_ts, specificGroup$label)
        test.dataset.cluster = cbind(test.attributes, test.classes)
        n = ncol(test.dataset.cluster)
        nomes = colnames(test.dataset.cluster)
        label = nomes[n]

        #########################################################################
        cat("\nTrain: Save Group")
        train.name.csv = paste(FolderClusters,"/",
                               parameters$Dataset.Name,
                               "-split-tr-",f, "-group-",g,
                               ".csv",sep = "")

        cat("\nTest: Save Group")
        test.name.csv = paste(FolderClusters, "/",
                              parameters$Dataset.Name,
                              "-split-ts-", f, "-group-", g,
                              ".csv", sep = "" )

        #########################################################################
        write.csv(train.dataset.cluster, train.name.csv, row.names = FALSE)
        write.csv(test.dataset.cluster, test.name.csv, row.names = FALSE)

        # se tiver apenas um rótulo chame o SINGLE
        # caso contrário o MULTILABEL
        if(number.labels==1){

          cat("\n\n#=========================================================#")
          cat("  \n# FOLD = ", f)
          cat("  \n# KNN = ", u)
          cat("  \n# CLUSTER = ", g)
          cat("  \n# SINGLE LABEL")
          cat("  \n#=========================================================#\n\n")

          #######################################################################
          cat("\nExecute PYTHON\n")
          str.execute = paste("python3 ",
                              parameters$Folders$Python,
                              "/standard_single.py ",
                              train.name.csv, " ",
                              test.name.csv, " ",
                              parameters$Dataset.Info$AttEnd, " ",
                              FolderClusters,
                              sep="")
          # EXECUTA
          start <- proc.time()
          res = print(system(str.execute))
          if(res!=0){break}

          tempo = data.matrix((proc.time() - start))
          tempo = data.frame(t(tempo))
          write.csv(tempo,
                    paste(FolderClusters, "/runtime-cluster.csv", sep=""),
                    row.names = FALSE)

          #####################################################################
          y_pred_bin = data.frame(read.csv(paste(FolderClusters, "/y_pred_bin.csv", sep="")))
          y_true = data.frame(read.csv(paste(FolderClusters, "/y_true.csv", sep="")))
          y_pred_proba = data.frame(read.csv(paste(FolderClusters, "/y_pred_proba.csv", sep="")))

          if(nrow(y_pred_proba)!=nrow(test.dataset.cluster)){
            break
          }

          nomescolunas = colnames(y_pred_proba)
          y_pred_proba_final = data.frame(y_pred_proba[,2])
          colnames(y_pred_proba_final) = label
          write.csv(y_pred_proba_final,
                    paste(FolderClusters, "/y_pred_proba.csv", sep=""),
                    row.names = FALSE)

          #####################################################################
          # cat("\nSave original and pruned predictions")
          # pred.bin = paste(nomes.labels.clusters, "-pred-bin", sep="")
          # true.o = paste(nomes.labels.clusters, "-true", sep="")
          # proba.o = paste(nomes.labels.clusters, "-pred-proba", sep="")

          # all.predictions = cbind(y_pred_proba[,2], y_pred_bin, y_true)
          # names(all.predictions) = c(proba.o, pred.bin, true.o)
          #
          # write.csv(all.predictions,
          #           paste(FolderClusters, "/clusters-predictions.csv", sep=""),
          #           row.names = FALSE)

          # Armazenando os resultados na lista
          # resultados_fold[[paste("Fold", f, "KNN", u, "Cluster", g, sep = "_")]] <- list(
          #   y_pred_bin = y_pred_bin,
          #   y_true = y_true,
          #   y_pred_proba = y_pred_proba
          # )

          unlink(test.name.csv)
          unlink(train.name.csv)
          #unlink(paste(FolderClusters, "/y_pred_bin.csv", sep=""))
          #unlink(paste(FolderClusters, "/y_pred_proba.csv", sep=""))
          #unlink(paste(FolderClusters, "/y_true.csv", sep=""))

        } else {

          cat("\n\n#=========================================================#")
          cat("  \n# FOLD = ", f)
          cat("  \n# KNN = ", u)
          cat("  \n# CLUSTER = ", g)
          cat("  \n# MULTI LABEL")
          cat("  \n#=========================================================#\n\n")

          #######################################################################
          train.mldr = mldr_from_dataframe(train.dataset.cluster, labelIndices = labels.indices)
          test.mldr = mldr_from_dataframe(test.dataset.cluster, labelIndices = labels.indices)

          #######################################################################
          cat("\nExecute PYTHON\n")
          str.execute = paste("python3 ",
                              parameters$Folders$Python,
                              "/standard_multilabel.py ",
                              train.name.csv, " ",
                              test.name.csv, " ",
                              parameters$Dataset.Info$AttEnd, " ",
                              FolderClusters,
                              sep="")
          # EXECUTA
          start <- proc.time()
          res = print(system(str.execute))
          if(res!=0){break}

          tempo = data.matrix((proc.time() - start))
          tempo = data.frame(t(tempo))
          write.csv(tempo,
                    paste(FolderClusters, "/runtime-cluster.csv", sep=""),
                    row.names = FALSE)

          #####################################################################
          y_pred_bin = data.frame(read.csv(paste(FolderClusters, "/y_pred_bin.csv", sep="")))
          y_true = data.frame(read.csv(paste(FolderClusters, "/y_true.csv", sep="")))
          y_pred_proba = data.frame(read.csv(paste(FolderClusters, "/y_pred_proba.csv", sep="")))

          #######################################
          nomes = colnames(y_pred_proba)
          nomes.2 = c("")
          m = ncol(y_pred_proba)/2
          a = 1
          while(a<=m){
            nomes.2[a] = paste("prob_", a-1, "_1", sep="")
            a = a + 1
          }

          y_pred_proba = y_pred_proba %>% select(all_of(nomes.2))
          names(y_pred_proba) = nomes.labels.clusters
          write.csv(y_pred_proba,
                    paste(FolderClusters, "/y_pred_proba.csv", sep=""),
                    row.names = FALSE)


          #####################################################################
          cat("\nSave original and pruned predictions")
          # pred.bin = paste(nomes.labels.clusters, "-pred-bin", sep="")
          # true.o = paste(nomes.labels.clusters, "-true", sep="")
          # proba.o = paste(nomes.labels.clusters, "-pred-proba", sep="")
          #
          # all.predictions = cbind(y_pred_proba, y_pred_bin, y_true)
          # names(all.predictions) = c(proba.o, pred.bin, true.o)
          #
          # write.csv(all.predictions,
          #           paste(FolderClusters, "/clusters-predictions.csv", sep=""),
          #           row.names = FALSE)

          # Armazenando os resultados na lista
          # resultados_fold[[paste("Fold", f, "KNN", u, "Cluster", g, sep = "_")]] <- list(
          #   y_pred_bin = y_pred_bin,
          #   y_true = y_true,
          #   y_pred_proba = y_pred_proba
          # )

          unlink(test.name.csv)
          unlink(train.name.csv)
          #unlink(paste(FolderClusters, "/y_pred_bin.csv", sep=""))
          #unlink(paste(FolderClusters, "/y_pred_proba.csv", sep=""))
          #unlink(paste(FolderClusters, "/y_true.csv", sep=""))

        } # fim do else

        g = g + 1
        gc()
      } # fim do grupo

      #return(resultados_fold)

      u = u + 1
      gc()
    } # fim do knn

    # f = f + 1
    gc()
  } # ending folds

  #return(bthpkParalel)

  gc()
  cat("\n############################################################")
  cat("\n# RF SILHOUETTE: End build.python.silho                    #")
  cat("\n############################################################")
  cat("\n\n\n\n")
}


######################################################################
#
######################################################################
gather.preds.rf.silho <- function(parameters) {

  #f = 1
  gatherR <- foreach(f = 1:parameters$Number.Folds) %dopar% {
  # while(f<=parameters$Number.Folds){

    final.runtime = data.frame(fold = numeric(),
                               knn = numeric(),
                               cluster = numeric(),
                               user.self = numeric(),
                               sys.self = numeric(),
                               elapsed = numeric(),
                               user.child = numeric(),
                               sys.child = numeric())

    final.model.size <- data.frame(fold = numeric(),
                                   knn = numeric(),
                                   cluster = numeric(),
                                   Format = character(),
                                   Size.Bytes = numeric(),
                                   Size.KB = numeric(),
                                   Size.MB = numeric(),
                                   Size.GB = numeric())

    labels <- parameters$Config$NamesLabels
    all.proba <- data.frame(matrix(ncol = length(labels) + 2, nrow = 0))
    colnames(all.proba) <- c("Fold", "Knn", labels)

    all.bin <- data.frame(matrix(ncol = length(labels) + 2, nrow = 0))
    colnames(all.bin) <- c("Fold", "Knn", labels)

    all.true <- data.frame(matrix(ncol = length(labels) + 2, nrow = 0))
    colnames(all.true) <- c("Fold", "Knn", labels)

    final.roc.auc = data.frame(fold = numeric(),
                               knn = numeric(),
                               roc_auc = numeric())

    final.roc.auc.macro = data.frame(fold = numeric(),
                                     knn = numeric(),
                                     roc_auc_macro = numeric())

    final.roc.auc.micro = data.frame(fold = numeric(),
                                     knn = numeric(),
                                     roc_auc_micro = numeric())

    #########################################################################
    FolderRoot = "~/TcpKnnH"
    FolderScripts = "~/TcpKnnH/R"

    #########################################################################
    setwd(FolderScripts)
    source("libraries.R")
    source("utils.R")

    #########################################################################
    cat("\nCreating Folders from Best Partitions and Splits Tests")
    FolderPartitionsSplit = paste(parameters$Folders$folderPartitions,
                                  "/Split-", f, sep = "")
    FolderTestedSplit = paste(parameters$Folders$folderTest,
                              "/Split-", f, sep = "")

    #########################################################################
    itens <- dir(FolderPartitionsSplit, full.names = TRUE)
    pastas <- itens[file.info(itens)$isdir]
    knn_pastas <- pastas[grep("/knn-", pastas)]
    tamanho = length(knn_pastas)

    ###################################################################
    u = 1
    while(u<=tamanho){

      FolderPartitionKnn = paste(FolderPartitionsSplit, "/knn-", u, sep="")
      FolderKnn = paste(FolderTestedSplit, "/Knn-", u, sep="")

      file.name = paste(FolderPartitionKnn, "/knn-", u, "-h-partition.csv", sep="")
      particao = data.frame(read.csv(file.name))
      number.clusters <- length(unique(particao$groups))

      ###################################################################
      apagar = c(0)
      y.true.final = data.frame(apagar)
      y.pred.bin.final = data.frame(apagar)
      y.pred.proba.final = data.frame(apagar)

      #################################################################
      g = 1
      while(g <= number.clusters) {

        cat("\n\n#=========================================================#")
        cat("  \n# FOLD = ", f)
        cat("  \n# KNN = ", u)
        cat("  \n# CLUSTER = ", g)
        cat("  \n#=========================================================#\n\n")


        Folder.Group.Test = paste(FolderKnn, "/cluster-", g, sep = "")
        specificGroup = data.frame(filter(particao, groups == g))
        number.labels = nrow(specificGroup)

        setwd(Folder.Group.Test)
        #predictions = data.frame(read.csv("clusters-predictions.csv"))
        runtime = data.frame(read.csv("runtime-cluster.csv"))
        model.size = data.frame(read.csv("model-sizes.csv"))

        y_pred_bin = data.frame(read.csv("y_pred_bin.csv"))
        y_pred_proba = data.frame(read.csv("y_pred_proba.csv"))
        y_true = data.frame(read.csv("y_true.csv"))

        # nomes.pred.proba = c(paste(specificGroup$labels, ".pred.proba", sep=""))
        # nomes.pred.bin = c(paste(specificGroup$labels, ".pred.bin", sep=""))
        # nomes.true = c(paste(specificGroup$labels, ".true", sep=""))

        # y_pred_proba = predictions %>% select(all_of(nomes.pred.proba))
        # y_pred_bin = predictions %>% select(all_of(nomes.pred.bin))
        # y_true = predictions %>% select(all_of(nomes.true))

        y.pred.proba.final = cbind(y.pred.proba.final, y_pred_proba)
        y.pred.bin.final = cbind(y.pred.bin.final, y_pred_bin)
        y.true.final = cbind(y.true.final, y_true)

        runtime = runtime[,-1]
        final.runtime = rbind(final.runtime,
                              data.frame(fold = f,
                                         knn = u,
                                         cluster = g,
                                         runtime))

        colnames(model.size) = c("Type", "Bytes", "Size.KB",
                                 "Size.MB", "Size.GB")
        final.model.size = rbind(final.model.size,
                                 data.frame(fold = f,
                                            knn = u,
                                            cluster = g,
                                            model.size))

        #res = print(system(paste("rm -rf ", Folder.Group.Test, sep="")))
        #if(res!=0){break}

        g = g + 1
        gc()
      } # fim do grupo

      ########################################
      y.pred.bin.final = y.pred.bin.final[, -1]
      y.pred.proba.final = y.pred.proba.final[, -1]
      y.true.final = y.true.final[,-1]

      ########################################
      all.bin = rbind(all.proba,
                        cbind(Fold = f, Knn = u, y.pred.bin.final))

      all.proba = rbind(all.proba,
                        cbind(Fold = f, Knn = u, y.pred.proba.final))

      all.true = rbind(all.proba,
                        cbind(Fold = f, Knn = u, y.true.final))
      #colnames(y.pred.bin.final)
      #colnames(y.pred.proba.final)
      #colnames(y.true.final)

      ########################################
      #names(y.true.final) = parameters$Config$NamesLabels
      #names(y.pred.bin.final) = parameters$Config$NamesLabels
      #names(y.pred.proba.final) = parameters$Config$NamesLabels

      ########################################
      nome.true = paste(FolderKnn, "/y_true.csv", sep="")
      nome.pred.proba = paste(FolderKnn, "/y_pred_proba.csv", sep="")
      nome.pred.bin = paste(FolderKnn, "/y_pred_bin.csv", sep="")

      ########################################
      write.csv(y.true.final, nome.true, row.names = FALSE)
      write.csv(y.pred.proba.final, nome.pred.proba, row.names = FALSE)
      write.csv(y.pred.bin.final, nome.pred.bin, row.names = FALSE)

      ############################################################
      #y.true.2 = data.frame(sapply(y.true.final, function(x) as.numeric(as.character(x))))
      #y.true.3 = mldr_from_dataframe(y.true.2,
      #                               labelIndices = seq(1,ncol(y.true.2)),
      #                               name = "y.true.2")

      ########################################################################
      #y_threshold_05 <- data.frame(as.matrix(fixed_threshold(y.pred.proba.final,
      #                                                       threshold = 0.5)))
      #y_threshold_05 = data.frame(as.matrix(y_threshold_05))
      # write.csv(y_threshold_05,
      #           paste(FolderKnn, "/y_pred_thr05.csv", sep=""),
      #           row.names = FALSE)

      #####################################################################
      #nome.true = paste(FolderKnn, "/y_true.csv", sep="")
      #nome.pred.proba = paste(FolderKnn, "/y_pred_proba.csv", sep="")
      #nome.pred.bin = paste(FolderKnn, "/y_pred_bin.csv", sep="")
      #nome.thr.05 = paste(FolderKnn, "/y_pred_thr05.csv", sep="")

      save.pred.proba = paste(FolderTestedSplit, "/knn-", u,
                              "-pred-proba-auprc.csv", sep="")

      save.pred.bin = paste(FolderTestedSplit, "/knn-", u,
                            "-pred-bin-auprc.csv", sep="")

      #save.thr05 = paste(FolderKnn, "/pred-thr05-auprc.csv", sep="")

      #################################################################
      str.execute = paste("python3 ",
                          parameters$Folders$Python,
                          "/auprc.py ",
                          nome.true, " ",
                          nome.pred.proba, " ",
                          save.pred.proba, " ",
                          sep="")
      res = print(system(str.execute))
      if(res!=0){break}

      #################################################################
      str.execute = paste("python3 ",
                          parameters$Folders$Python,
                          "/auprc.py ",
                          nome.true, " ",
                          nome.pred.bin, " ",
                          save.pred.bin, " ",
                          sep="")
      res = print(system(str.execute))
      if(res!=0){break}

#       #################################################################
#       str.execute = paste("python3 ",
#                           parameters$Folders$Python,
#                           "/auprc.py ",
#                           nome.true, " ",
#                           nome.thr.05, " ",
#                           save.thr05, " ",
#                           sep="")
#       res = print(system(str.execute))
#       if(res!=0){break}

      ####################################################
      # names = paste(parameters$Config$NamesLabels, "-proba", sep="")
      # y.pred.proba.final = data.frame(y.pred.proba.final)
      # names(y.pred.proba.final) = names
      # rm(names)
      #
      # names = paste(parameters$Config$NamesLabels, "-bin", sep="")
      # y.pred.bin.final = data.frame(y.pred.bin.final)
      # names(y.pred.bin.final) = names
      # rm(names)
      #
      # names  = paste(parameters$Config$NamesLabels, "-true", sep="")
      # y.true.final = data.frame(y.true.final)
      # names(y.true.final) = names
      # rm(names)

      # names  = paste(parameters$Config$NamesLabels, "-thr05", sep="")
      # y_threshold_05 = data.frame(y_threshold_05)
      # names(y_threshold_05) = names
      # rm(names)

      # ,
      # y_threshold_05
      # all.predictions = cbind(y.true.final,
      #                         y.pred.bin.final,
      #                         y.pred.proba.final)
      #
      # write.csv(all.predictions,
      #           paste(FolderKnn, "/folder-predictions.csv", sep=""),
      #           row.names = FALSE)


      # ##############################################
      # names(y.true.final) = parameters$Config$NamesLabels
      # names(y.pred.bin.final) = parameters$Config$NamesLabels
      # names(y.pred.proba.final) = parameters$Config$NamesLabels

      # matrix.confusao(true = y.true.final,
      #                 pred = y_threshold_05,
      #                 type = "thr05",
      #                 salva = FolderKnn,
      #                 nomes.rotulos = parameters$Config$NamesLabels)

      matrix.confusao(true = y.true.final,
                      pred = y.pred.bin.final ,
                      type = "pred-bin",
                      salva = FolderKnn,
                      nomes.rotulos = parameters$Config$NamesLabels)

      #########################################################################
      cat("\nOpen Test file")
      test.name.file = paste(parameters$Folders$folderCVTS, "/",
                             parameters$Dataset.Name,
                             "-Split-Ts-", f, ".csv", sep = "")
      test.dataset.original = data.frame(read.csv(test.name.file))
      nrow(test.dataset.original)

      labels.indices = seq(parameters$Dataset.Info$LabelStart,
                           parameters$Dataset.Info$LabelEnd, by=1)

      test.mldr = mldr_from_dataframe(test.dataset.original,
                                      labelIndices = labels.indices)
      nrow(test.mldr$dataset)

      #########################################################################
      cat("\nOpen Train file")
      train.name.file = paste(parameters$Folders$folderCVTR, "/",
                              parameters$Dataset.Name,
                              "-Split-Tr-", f, ".csv", sep = "")
      train.dataset.original = data.frame(read.csv(train.name.file))
      train.mldr = mldr_from_dataframe(train.dataset.original,
                                       labelIndices = labels.indices)

      #########################################################################
      cat("\nOpen Validation file")
      val.name.file = paste(parameters$Folders$folderCVVL, "/",
                            parameters$Dataset.Name,
                            "-Split-Vl-", f, ".csv", sep = "")
      val.dataset.original = data.frame(read.csv(val.name.file))
      val.mldr = mldr_from_dataframe(val.dataset.original,
                                     labelIndices = labels.indices)

      #########################################################################
      tv = rbind(train.dataset.original, val.dataset.original)
      mldr.tv = mldr_from_dataframe(tv, labelIndices = labels.indices)

      #########################################################################
      roc.bin = roc.curva(f = f,
                          test = test.mldr,
                          y_pred = y.pred.bin.final,
                          Folder = FolderTestedSplit,
                          nome = paste("/knn-", u, "-pred-bin", sep=""))

      roc.proba = roc.curva(f = f,
                            test = test.mldr,
                            y_pred = y.pred.proba.final,
                            Folder = FolderTestedSplit,
                            nome = paste("/knn-", u, "-pred-proba", sep=""))

      final.roc.auc = rbind(final.roc.auc,
                            cbind(fold = f,
                                  knn = u,
                                  roc_auc = roc.proba$roc_auc))

      final.roc.auc.macro = rbind(final.roc.auc.macro,
                                  cbind(fold = f,
                                        knn = u,
                                        roc_auc = roc.proba$roc_auc_macro))

      final.roc.auc.micro = rbind(final.roc.auc.micro,
                                  cbind(fold = f,
                                        knn = u,
                                        roc_auc = roc.proba$roc_auc_micro))

      # roc.curva(f = f,
      #           test = test.mldr,
      #           y_pred = y_threshold_05,
      #           Folder = FolderKnn,
      #           nome = "thr05")

     # res = print(system(paste("rm -r ", FolderKnn, sep="")))
     # if(res!=0){break}

      u = u + 1
      gc()
    } # fim do knn

    name = paste(FolderTestedSplit, "/all-roc-auc.csv", sep="")
    write.csv(final.roc.auc, name, row.names = FALSE)

    name = paste(FolderTestedSplit, "/all-roc-auc-macro.csv", sep="")
    write.csv(final.roc.auc.macro, name, row.names = FALSE)

    name = paste(FolderTestedSplit, "/all-roc-auc-micro.csv", sep="")
    write.csv(final.roc.auc.micro, name, row.names = FALSE)

    name = paste(FolderTestedSplit, "/all-proba.csv", sep="")
    write.csv(all.proba, name, row.names = FALSE)

    name = paste(FolderTestedSplit, "/all-bin.csv", sep="")
    write.csv(all.bin, name, row.names = FALSE)

    name = paste(FolderTestedSplit, "/all-true.csv", sep="")
    write.csv(all.true, name, row.names = FALSE)

    name = paste(FolderTestedSplit, "/all-runtime.csv", sep="")
    write.csv(final.runtime, name, row.names = FALSE)

    name = paste(FolderTestedSplit, "/all-model-size.csv", sep="")
    write.csv(final.model.size, name, row.names = FALSE)

    # f = f + 1
    gc()
  } # end do foreach

  gc()
  cat("\n#####################################################")
  cat("\n# RF SILHOUETTE: End gather.preds.python.silho      #")
  cat("\n######################################################")
  cat("\n\n\n\n")

} # end da função


############################################################################
#
############################################################################
evaluate.rf.silho <- function(parameters){

  #f = 1
  avaliaParalel <- foreach (f = 1:parameters$Number.Folds) %dopar%{
  # while(f<=parameters$Config.File$Number.Folds){

    #########################################################################
    FolderRoot = "~/TcpKnnH"
    FolderScripts = "~/TcpKnnH/R"

    #########################################################################
    setwd(FolderScripts)
    source("libraries.R")
    source("utils.R")

    #########################################################################
    cat("\nCreating Folders from Best Partitions and Splits Tests")
    FolderPartitionsSplit = paste(parameters$Folders$folderPartitions,
                                  "/Split-", f, sep = "")
    FolderTestedSplit = paste(parameters$Folders$folderTest,
                              "/Split-", f, sep = "")

    #########################################################################
    itens <- dir(FolderPartitionsSplit, full.names = TRUE)
    pastas <- itens[file.info(itens)$isdir]
    knn_pastas <- pastas[grep("/knn-", pastas)]
    tamanho = length(knn_pastas)

    ###################################################################
    u = 1
    while(u<=tamanho){

      FolderKnn = paste(FolderTestedSplit, "/Knn-", u, sep = "")

      # #####################################################################
      # nome.pred.proba = paste(FolderTestedSplit, "/all-proba.csv", sep="")
      # nome.pred.bin = paste(FolderTestedSplit, "/all-bin.csv", sep="")
      # nome.true = paste(FolderTestedSplit, "/all-true.csv", sep="")

      #####################################################################
      nome.pred.proba = paste(FolderKnn, "/y_pred_proba.csv", sep="")
      nome.pred.bin = paste(FolderKnn, "/y_pred_bin.csv", sep="")
      nome.true = paste(FolderKnn, "/y_true.csv", sep="")

      #####################################################################
      y_pred_proba = data.frame(read.csv(nome.pred.proba))
      y_pred_bin = data.frame(read.csv(nome.pred.bin))
      y_true = data.frame(read.csv(nome.true))

      #####################
      # y_pred_proba <- y_pred_proba[y_pred_proba$Knn == u, ]
      # y_pred_bin <- y_pred_bin[y_pred_bin$Knn == u, ]
      # y_true <- y_true[y_true$Knn == u, ]
      #
      # #####################
      # y_pred_proba = y_pred_proba[,c(-1,-2)]
      # y_pred_bin = y_pred_bin[,c(-1,-2)]
      # y_true = y_true[,c(-1,-2)]
      #
      # class(y_true$amazed.suprised)

      ##########################################################################
      y.true.2 = data.frame(sapply(y_true, function(x) as.numeric(as.character(x))))
      y.true.3 = mldr_from_dataframe(y.true.2,
                                     labelIndices = seq(1,ncol(y.true.2)),
                                     name = "y.true.2")
      y_pred_bin = sapply(y_pred_bin, function(x) as.numeric(as.character(x)))
      y_pred_proba = sapply(y_pred_proba, function(x) as.numeric(as.character(x)))

      ##########################################################################
      avaliacao(f = f,
                y_true = y.true.3,
                y_pred = y_pred_bin,
                salva = FolderTestedSplit,
                nome = paste("/knn-", u, "-pred-bin", sep=""))

      avaliacao(f = f,
                y_true = y.true.3,
                y_pred = y_pred_proba,
                salva = FolderTestedSplit,
                nome = paste("/knn-", u, "-pred-proba", sep=""))

      # res = print(system(paste("rm -r ", FolderKnn, sep="")))
      # if(res!=0){break}

      u = u + 1
      gc()
    } # fim da sparas

    # f = f + 1
    gc()
  } # fim do dor

  gc()
  cat("\n##################################")
  cat("\n# END FUNCTION EVALUATE          #")
  cat("\n##################################")
  cat("\n\n\n\n")
}




###########################################################################
#
###########################################################################
gather.eval.rf.silho <- function(parameters){

  #########################################################################
  FolderRoot = "~/TcpKnnH"
  FolderScripts = "~/TcpKnnH/R"

  #########################################################################
  setwd(FolderScripts)
  source("libraries.R")
  source("utils.R")

  ##########################
  measures = c("accuracy", "average-precision", "clp", "coverage",
               "F1", "hamming-loss", "macro-AUC", "macro-F1",
               "macro-precision", "macro-recall", "margin-loss",
               "micro-AUC","micro-F1", "micro-precision",
               "micro-recall", "mlp", "one-error", "precision",
               "ranking-loss", "recall", "subset-accuracy", "wlp")

    final.model.size = data.frame()
    final.runtime = data.frame()
    final.measures.bin = data.frame()
    final.measures.proba = data.frame()

    final.measures.auprc.proba = data.frame(fold = numeric(),
                                            Knn = numeric(),
                                            Micro.AUPRC = numeric(),
                                            Macro.AUPRC = numeric())

    final.measures.auprc.bin = data.frame(fold = numeric(),
                                          Knn = numeric(),
                                          Micro.AUPRC = numeric(),
                                          Macro.AUPRC = numeric())

    final.measures.roc.auc = data.frame(fold = numeric(),
                                        Knn = numeric(),
                                        roc.auc = numeric())

    final.measures.roc.auc.micro = data.frame(fold = numeric(),
                                              Knn = numeric(),
                                              roc.auc.micro = numeric())

    final.measures.roc.auc.macro = data.frame(fold = numeric(),
                                              Knn = numeric(),
                                              roc.auc.macro = numeric())

    f = 1
    while(f<=parameters$Number.Folds){

      cat("\n FOLD ", f)


      #########################################################################
      #cat("\nCreating Folders from Best Partitions and Splits Tests")
      FolderPartitionsSplit = paste(parameters$Folders$folderPartitions,
                                    "/Split-", f, sep = "")
      FolderTestedSplit = paste(parameters$Folders$folderTest,
                                "/Split-", f, sep = "")

      #########################################################################
      itens <- dir(FolderPartitionsSplit, full.names = TRUE)
      pastas <- itens[file.info(itens)$isdir]
      knn_pastas <- pastas[grep("/knn-", pastas)]
      tamanho = length(knn_pastas)
      names <- paste("knn", 1:tamanho, sep = "")

      #########################################################################
      itens <- dir(FolderTestedSplit, full.names = TRUE)

      pred_proba_files <- itens[grep("pred-proba-evaluated", itens)]
      pred_bin_files <- itens[grep("pred-bin-evaluated", itens)]

      auprc_proba_files <- itens[grep("pred-proba-auprc", itens)]
      auprc_bin_files <- itens[grep("pred-bin-auprc", itens)]

      ##########################
      df_pred_bin <- lapply(pred_bin_files, read.csv)
      second_columns <- lapply(df_pred_bin, function(df) df[, 2])
      combined_df_pred_bin <- do.call(cbind, second_columns)
      combined_df_pred_bin = data.frame(combined_df_pred_bin)
      combined_df_pred_bin = data.frame(measures, combined_df_pred_bin)
      names(combined_df_pred_bin) = c("measures", names)
      resultados = data.frame(fold = f, combined_df_pred_bin)

      final.measures.bin = rbind(final.measures.bin, resultados)

      ##########################
      df_pred_proba <- lapply(pred_proba_files, read.csv)
      second_columns <- lapply(df_pred_proba, function(df) df[, 2])
      combined_df_pred_proba <- do.call(cbind, second_columns)
      combined_df_pred_proba = data.frame(combined_df_pred_proba)
      combined_df_pred_proba = data.frame(measures, combined_df_pred_proba)
      names(combined_df_pred_proba) = c("measures", names)
      resultados = data.frame(fold = f, combined_df_pred_proba)

      final.measures.proba = rbind(final.measures.proba, resultados)

      ##########################
      df_auprc_proba <- lapply(auprc_proba_files, read.csv)
      resultados <- list()
      for (i in 1:length(df_auprc_proba)) {
        df <- df_auprc_proba[[i]]
        df$Fold <- f
        df$Knn <- i
        resultados[[i]] <- df
      }
      auprc_proba <- do.call(rbind, resultados)
      auprc_proba <- auprc_proba[, c("Fold", "Knn", "Micro.AUPRC", "Macro.AUPRC")]

      final.measures.auprc.proba = rbind(final.measures.auprc.proba, auprc_proba)

      ##########################
      df_auprc_bin <- lapply(auprc_bin_files, read.csv)
      resultados <- list()
      for (i in 1:length(df_auprc_bin)) {
        df <- df_auprc_bin[[i]]
        df$Fold <- f
        df$Knn <- i
        resultados[[i]] <- df
      }
      auprc_bin <- do.call(rbind, resultados)
      auprc_bin <- auprc_bin[, c("Fold", "Knn", "Micro.AUPRC", "Macro.AUPRC")]

      final.measures.auprc.bin = rbind(final.measures.auprc.bin, auprc_bin)

      ##########################
      name1 = paste(FolderTestedSplit, "/all-roc-auc.csv", sep="")
      name2 = paste(FolderTestedSplit, "/all-roc-auc-macro.csv", sep="")
      name3 = paste(FolderTestedSplit, "/all-roc-auc-micro.csv", sep="")
      name4 = paste(FolderTestedSplit, "/all-model-size.csv", sep="")
      name5 = paste(FolderTestedSplit, "/all-runtime.csv", sep="")

      auc = data.frame(read.csv(name1))
      auc.macro = data.frame(read.csv(name2))
      auc.micro = data.frame(read.csv(name3))
      model.size = data.frame(read.csv(name4))
      runtime = data.frame(read.csv(name5))

      final.measures.roc.auc = rbind(final.measures.roc.auc, auc)
      final.measures.roc.auc.macro = rbind(final.measures.roc.auc.macro, auc.macro)
      final.measures.roc.auc.micro = rbind(final.measures.roc.auc.micro, auc.micro)
      final.model.size = rbind(final.model.size, model.size)
      final.runtime = rbind(final.runtime, runtime)

      ##################
      unlink(auprc_proba_files)
      unlink(auprc_bin_files)
      unlink(pred_proba_files)
      unlink(pred_bin_files)

      #################################
      # Listar os diretórios KNN
      itens <- dir(FolderTestedSplit, full.names = TRUE)
      pastas <- itens[file.info(itens)$isdir]
      knn_pastas <- pastas[grep("/Knn-", pastas)]

      # Apagar cada pasta KNN encontrada
      for (pasta in knn_pastas) {
        if (dir.exists(pasta)) {
          unlink(pasta, recursive = TRUE, force = TRUE)  # Remove a pasta e todo o conteúdo
          cat("Removido:", pasta, "\n")
        }
      }


      #################################
      f = f + 1
      gc()
    }

    names(final.measures.auprc.proba)[1] = "fold"
    names(final.measures.auprc.bin)[1] = "fold"

    # Chamadas para cada conjunto de dados
    roc.auc = splitKnn(df = final.measures.roc.auc, nome_base = "roc_auc")
    roc.auc.macro = splitKnn(final.measures.roc.auc.macro, "roc_auc_macro")
    roc.auc.micro = splitKnn(final.measures.roc.auc.micro, "roc_auc_micro")
    auprc.proba = splitAuprc(final.measures.auprc.proba, "auprc_proba")
    auprc.bin = splitAuprc(final.measures.auprc.bin, "auprc_bin")
    resultados.proba = splitPerformance(final.measures.proba)
    resultados.bin = splitPerformance(final.measures.bin)

    mergeAndSaveResults(roc_auc_1 =  roc.auc,
                        roc_auc_macro_1 = roc.auc.macro,
                        roc_auc_micro_1 = roc.auc.micro,
                        auprc_proba_1 = auprc.proba,
                        resultados_proba_1 = resultados.proba)

    model.size = splitKnn(final.model.size, "model_size")
    runtime = splitKnn(final.runtime, "runtime")

    write.csv(model.size,
              paste0(parameters$Folders$folderTest,
                    "/model-size.csv"),
              row.names = FALSE)

    write.csv(runtime,
              paste0(parameters$Folders$folderTest,
                     "/runtime-all-clusters.csv"),
              row.names = FALSE)

  gc()
  cat("\n########################################################")
  cat("\n# END EVALUATED                                        #")
  cat("\n########################################################")
  cat("\n\n\n\n")
}


###############################################################################
# CONTACT INFORMATION                                                          #
# In case of errors, contact: elainececiliagatto@gmail.com                     #
# Thank you for your collaboration!                                            #
###############################################################################
