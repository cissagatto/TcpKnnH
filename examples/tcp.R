rm(list=ls())

cat("\n\n##############################################################")
cat("  \n# START TESTE HYBRID PARTITIONS WITH K-NN SPARSIFICATION     #")
cat("  \n##############################################################\n\n")

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


cat("\n\n##############################################################")
cat("\n# TcpKnnH LOAD SOURCES                                    #")
cat("\n##############################################################\n\n")
setwd(FolderScripts)
source("libraries.R")
source("utils.R")
source("run.R")


cat("\n\n##############################################################")
cat("\n# TcpKnnH OPTIONS CONFIGURATIONS                          #")
cat("\n##############################################################\n\n")
options(java.parameters = "-Xmx64g")
options(show.error.messages = TRUE)
options(scipen=20)


cat("\n\n##############################################################")
cat("\n# TcpKnnH READ DATASETS                                   #")
cat("\n##############################################################\n\n")
datasets <- data.frame(read.csv("~/TcpKnnH/datasets-original.csv"))


cat("\n\n##############################################################")
cat("\n# TcpKnnH GET THE ARGUMENTS COMMAND LINE                  #")
cat("\n##############################################################\n\n")
args <- commandArgs(TRUE)



#############################################################################
# FIRST ARGUMENT: getting specific dataset information being processed      #
# from csv file                                                             #
#############################################################################

config_file <- args[1]

# /home/cissagatto/TcpKnnH/config-files/jaccard/Silhouette/random-forests
# config_file = "~/TcpKnnH/config-files/jaccard/Silhouette/random-forests/jsrf-emotions.csv"


if(file.exists(config_file)==FALSE){
  cat("\n################################################################")
  cat("\n# Missing Config File! Verify the following path:              #")
  cat("\n# ", config_file, "                                            #")
  cat("\n################################################################\n\n")
  break
} else {
  cat("\n########################################")
  cat("\n# Properly loaded configuration file!  #")
  cat("\n########################################\n\n")
}


cat("\n########################################")
cat("\n# PARAMETERS READ                    #\n")
config = data.frame(read.csv(config_file))
print(config)
cat("\n########################################\n\n")

parameters = list()

# DATASET_PATH
dataset_path = toString(config$Value[1])
dataset_path = str_remove(dataset_path, pattern = " ")
parameters$Path.Dataset = dataset_path

# TEMPORARTY_PATH
folderResults = toString(config$Value[2])
folderResults = str_remove(folderResults, pattern = " ")
parameters$Folder.Results = folderResults

# PARTITIONS_PATH
Partitions_Path = toString(config$Value[3])
Partitions_Path = str_remove(Partitions_Path, pattern = " ")
parameters$Path.Partitions = Partitions_Path

# VALIDATION
validation = toString(config$Value[4])
validation = str_remove(validation, pattern = " ")
parameters$Validation = validation

# SIMILARITY
similarity = toString(config$Value[5])
similarity = str_remove(similarity, pattern = " ")
parameters$Similarity = similarity

# CLASSIFEIR
classifier = toString(config$Value[6])
classifier = str_remove(classifier, pattern = " ")
parameters$Classifier = classifier

# DATASET_NAME
dataset_name = toString(config$Value[7])
dataset_name = str_remove(dataset_name, pattern = " ")
parameters$Dataset.Name = dataset_name

# NUMBER DATASET
number_dataset = as.numeric(config$Value[8])
parameters$Number.Dataset = number_dataset

# NUMBER_FOLDS
number_folds = as.numeric(config$Value[9])
parameters$Number.Folds = number_folds

# NUMBER_CORES
number_cores = as.numeric(config$Value[10])
parameters$Number.Cores = number_cores

# NUMBER_CORES
r_clone = as.numeric(config$Value[11])
parameters$Rclone = r_clone

# NUMBER_CORES
Save_csv_files = as.numeric(config$Value[12])
parameters$Save_csv_files = Save_csv_files

# DATASET_INFO
ds = datasets[number_dataset,]
parameters$Dataset.Info = ds

print(parameters)

###############################################################################
# Creating temporary processing folder                                        #
###############################################################################
if(dir.exists(folderResults) == FALSE) {dir.create(folderResults)}



###############################################################################
# Creating all directories that will be needed for code processing            #
###############################################################################
cat("\n#############################")
cat("\n#  ====> Get directories    #")
cat("\n#############################\n")
diretorios <- directories(parameters)
print(diretorios)
cat("\n\n")

parameters$Folders = diretorios
parameters$Folders$Python



###############################################################################
# Copying datasets from ROOT folder on server                                 #
###############################################################################

cat("\n####################################################################")
cat("\n# Checking the dataset tar.gz file                                 #")
cat("\n####################################################################\n\n")
str00 = paste(dataset_path, "/", ds$Name,".tar.gz", sep = "")
str00 = str_remove(str00, pattern = " ")

if(file.exists(str00)==FALSE){

  cat("\n######################################################################")
  cat("\n# The tar.gz file for the dataset to be processed does not exist!    #")
  cat("\n# Please pass the path of the tar.gz file in the configuration file! #")
  cat("\n# The path entered was: ", str00, "                                  #")
  cat("\n######################################################################\n\n")
  break

} else {

  # COPIANDO
  str01 = paste("cp ", str00, " ", diretorios$folderDatasets, sep = "")
  res = system(str01)
  if (res != 0) {
    cat("\nError: ", str01)
    break
  }

  # DESCOMPACTANDO
  str02 = paste("tar xzf ", diretorios$folderDatasets, "/", ds$Name,
                ".tar.gz -C ", diretorios$folderDatasets, sep = "")
  res = system(str02)
  if (res != 0) {
    cat("\nError: ", str02)
    break
  }

  str29 = paste("cp -r ", diretorios$folderDatasets, "/", ds$Name,
                "/CrossValidation/* ", diretorios$folderResults,
                "/datasets/CrossValidation/", sep="")
  res=system(str29)
  #if(res!=0){break}else{cat("\ncopiou")}

  str30 = paste("cp -r ",diretorios$folderDatasets, "/", ds$Name,
                "/LabelSpace/* ", diretorios$folderResults,
                "/datasets/LabelSpace/", sep="")
  res=system(str30)
  #if(res!=0){break}else{cat("\ncopiou")}

  str31 = paste("cp -r ", diretorios$folderDatasets, "/", ds$Name,
                "/NamesLabels/* ", diretorios$folderResults,
                "/datasets/NamesLabels/", sep="")
  res=system(str31)
  #if(res!=0){break}else{cat("\ncopiou")}

  str32 = paste("rm -r ", diretorios$folderResults,
                "/datasets/", ds$Name, sep="")
  print(system(str32))
  #if(res!=0){break}else{cat("\napagou")}

  #APAGANDO
  str03 = paste("rm ", diretorios$folderDatasets, "/", ds$Name,
                ".tar.gz", sep = "")
  res = system(str03)

  cat("\n####################################################################")
  cat("\n# tar.gz file of the DATASET loaded correctly!                     #")
  cat("\n####################################################################\n\n")


}



###############################################################################
# Copying PARTITIONS from ROOT folder on server                               #
###############################################################################

cat("\n####################################################################")
cat("\n# Checking the PARTITIONS tar.gz file                              #")
cat("\n####################################################################\n\n")
str00 = paste(Partitions_Path, "/", ds$Name,".tar.gz", sep = "")
str00 = str_remove(str00, pattern = " ")

if(file.exists(str00)==FALSE){

  cat("\n######################################################################")
  cat("\n# The tar.gz file for the dataset to be processed does not exist!    #")
  cat("\n# Please pass the path of the tar.gz file in the configuration file! #")
  cat("\n# The path entered was: ", str00, "                                  #")
  cat("\n######################################################################\n\n")
  break

} else {

  cat("\n####################################################################")
  cat("\n# tar.gz file of the PARTITION loaded correctly!                   #")
  cat("\n####################################################################\n\n")

  # COPIANDO
  str01 = paste("cp ", str00, " ", diretorios$folderPartitions, sep = "")
  res = system(str01)
  if (res != 0) {
    cat("\nError: ", str01)
    break
  }

  # DESCOMPACTANDO
  str02 = paste("tar xzf ", diretorios$folderPartitions, "/", ds$Name,
                ".tar.gz -C ", diretorios$folderPartitions, sep = "")
  res = system(str02)
  if (res != 0) {
    cat("\nError: ", str02)
    break
  }

  #APAGANDO
  str03 = paste("rm ", diretorios$folderPartitions, "/", ds$Name,
                ".tar.gz", sep = "")
  res = system(str03)
  if (res != 0) {
    cat("\nError: ", str03)
    break
  }

  str31 = paste("cp -r ", diretorios$folderPartitions, "/", ds$Name,
                "/Partitions/* ", diretorios$folderResults,
                "/Partitions/", sep="")
  res=system(str31)

  str31 = paste("cp -r ", diretorios$folderPartitions, "/", ds$Name,
                "/Communities/* ", diretorios$folderResults,
                "/Communities/", sep="")
  res=system(str31)

  system(paste("rm -r ", diretorios$folderPartitions, "/", ds$Name, sep=""))

}


cat("\n\n###################################################################")
cat("  \n# ====> TcpKnnH EXECUTE                                          #")
cat("  \n###################################################################\n\n")
timeTCP = system.time(res <- execute(parameters))


cat("\n\n##################################################################")
cat("  \n# ====> TcpKnnH SAVE RUNTIME                                     #")
cat("  \n##################################################################\n\n")
result_set <- t(data.matrix(timeTCP))


setwd(parameters$Folders$folderTest)
write.csv(result_set, "runtime-final.csv")
print(timeTCP)
cat("\n")


system(paste0("rm -rf ", parameters$Folders$folderDatasets))
system(paste0("rm -rf ", parameters$Folders$folderPartitions))
system(paste0("rm -rf ", parameters$Folders$folderCommunities))
system(paste0("rm -rf ", parameters$Folders$folderReports))
system(paste0("rm -rf ", parameters$Folders$folderValidation))

system(paste("tar -cjf ", parameters$Folders$folderTest, "/",
             parameters$Dataset.Name, "-",
             parameters$Similarity, "-results",
             ".tar.gz -C ",
             parameters$Folders$folderTest,
             " . ", sep=""))

system(paste("cp -v ", parameters$Folders$folderTest, "/",
             parameters$Dataset.Name, "-",
             parameters$Similarity, "-results",
             ".tar.gz ",
             parameters$Folders$folderRootReports, sep=""))

system(paste("rm -r ", parameters$Folders$folderResults, sep=""))



##########################################
# VALIDATION = 1 --> SILHOUETTE
##########################################
if(parameters$Validation=="Silhouette"){



  # cat("\n\n#######################################################")
  # cat("\n# COPY TEST TO GOOGLE DRIVE                           #")
  # cat("\n#########################################################\n\n")
  # origem1 = parameters$Folders$folderTestSilho
  # destino1 = paste("nuvem:Clus/Communities/Test/",
  #                  similarity, "/Silhouette/", dataset_name,
  #                  "/Knn-H/Tested", sep="")
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


  # cat("\n\n#######################################################")
  # cat("\n# COPY REPORTS TO GOOGLE DRIVE                        #")
  # cat("\n#######################################################\n\n")
  # origem1 = parameters$Folders$folderReports
  # destino1 = paste("nuvem:Clus/Communities/Test/",
  #                  similarity, "/Silhouette/", dataset_name,
  #                  "/Knn-H/", sep="")
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



} else if (parameters$Validation==2){

  # cat("\n\n#######################################################")
  # cat("\n# COPY TEST TO GOOGLE DRIVE                           #")
  # cat("\n#########################################################\n\n")
  # origem1 = parameters$Folders$folderTestMaF1
  # destino1 = paste("nuvem:Clus/Communities/Test/",
  #                  similarity, "/Macro-F1/", dataset_name,
  #                  "/Knn-H/Tested", sep="")
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

  # cat("\n\n#######################################################")
  # cat("\n# COPY REPORTS TO GOOGLE DRIVE                        #")
  # cat("\n#######################################################\n\n")
  # origem1 = parameters$Folders$folderReports
  # destino1 = paste("nuvem:Clus/Communities/Test/",
  #                  similarity, "/Macro-F1/", dataset_name,
  #                  "/Knn-H/", sep="")
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

  str = paste(diretorios$folderRepMaF1, "/",
              parameters$Dataset.Name, sep="")

  str2 = paste("cp -r ", diretorios$folderTestMaF1, " ", str, sep="")
  print(system(str2))

  str2 = paste("cp -r ", diretorios$folderReports , "/* ", str , sep="")
  print(system(str2))


  cat("\n\n############################################################")
  cat("\n# DELETING TEST DIR                                        #")
  cat("\n############################################################\n\n")
  system(paste("rm -r ", parameters$Folders$folderTestMaF1, sep=""))


} else {


  ##########################################
  # VALIDATION = 3 --> MICRO F1
  ##########################################

}


cat("\n\n###################################################################")
cat("\n# TcpKnnH END                                              #")
cat("\n###################################################################\n")

# rm(list = ls())

##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################
