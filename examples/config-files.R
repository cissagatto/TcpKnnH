# Clear workspace
rm(list = ls())


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

# Define root folders
FolderRoot <- "~/TcpKnnH"
FolderScripts <- "~/TcpKnnH/R"

###############################################################################
# LOAD LIBRARIES                                                              #
###############################################################################
setwd(FolderScripts)
source("libraries.R")

###############################################################################
# READ DATASET INFORMATION                                                    #
###############################################################################
setwd(FolderRoot)
datasets <- read.csv("datasets-original.csv")
n <- nrow(datasets)

###############################################################################
# CREATE CONFIGURATION FILES FOLDER                                           #
###############################################################################
FolderCF <- file.path(FolderRoot, "config-files")
if (!dir.exists(FolderCF)) dir.create(FolderCF)

# Define similarity, validation, and classifier methods
similarity.name <- c("jaccard", "rogers")
similarity.nick <- c("j", "ro")

validation.name <- c("Silhouette", "Macro-F1", "Micro-F1", "Hamming-loss")
validation.nick <- c("s", "ma", "mi", "hl")

classifier.name <- c("clus", "random-forests")
classifier.nick <- c("c", "rf")

# Loop through similarities
for (s in seq_along(similarity.name)) {
  FolderSimilarity <- file.path(FolderCF, similarity.name[s])
  if (!dir.exists(FolderSimilarity)) dir.create(FolderSimilarity)

  # Loop through validation methods
  for (v in seq_along(validation.name)) {
    FolderValidation <- file.path(FolderSimilarity, validation.name[v])
    if (!dir.exists(FolderValidation)) dir.create(FolderValidation)

    # Loop through classifiers
    for (c in seq_along(classifier.name)) {
      FolderClassifier <- file.path(FolderValidation, classifier.name[c])
      if (!dir.exists(FolderClassifier)) dir.create(FolderClassifier)

      # Loop through datasets
      for (d in seq_len(n)) {
        ds <- datasets[d, ]

        cat("\n\n===============================================")
        cat("\nSimilarity:\t", similarity.name[s])
        cat("\nValidation:\t", validation.name[v])
        cat("\nClassifier:\t", classifier.name[c])
        cat("\nDataset:\t", ds$Name)
        cat("\n===============================================")

        # Generate filenames
        name <- paste(similarity.nick[s], validation.nick[v], classifier.nick[c], "-", ds$Name, sep="")
        folder_name <- file.path("/tmp", name)
        config_name <- file.path(FolderClassifier, paste0(name, ".csv"))

        # Write configuration file
        output.file <- file(config_name, "wb")
        write("Config, Value", file = output.file, append = TRUE)
        write("Dataset_Path, ~/TcpKnnH/Datasets", file = output.file, append = TRUE)
        write(paste("Temporary_Path,", folder_name), file = output.file, append = TRUE)
        write(paste("Partitions_Path, ~/TcpKnnH/Partitions/", similarity.name[s], sep=""), file = output.file, append = TRUE)
        write(paste("Validation,", validation.name[v]), file = output.file, append = TRUE)
        write(paste("Similarity,", similarity.name[s]), file = output.file, append = TRUE)
        write(paste("Classifier,", classifier.name[c]), file = output.file, append = TRUE)
        write(paste("Dataset_Name,", ds$Name), file = output.file, append = TRUE)
        write(paste("Number_Dataset,", ds$Id), file = output.file, append = TRUE)
        write("Number_Folds, 10", file = output.file, append = TRUE)
        write("Number_Cores, 10", file = output.file, append = TRUE)
        write("R_clone, 0", file = output.file, append = TRUE)
        write("Save_csv_files, 1", file = output.file, append = TRUE)
        close(output.file)
      }
    }
  }
}

# Clear workspace
rm(list = ls())

###############################################################################
# CONTACT INFORMATION                                                          #
# In case of errors, contact: elainececiliagatto@gmail.com                     #
# Thank you for your collaboration!                                            #
###############################################################################
