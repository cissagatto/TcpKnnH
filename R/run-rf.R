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


##################################################
# SET WORK SPACE
##################################################
FolderRoot = "~/TcpKnnH"
FolderScripts = "~/TcpKnnH/R"


##############################################################################
#
##############################################################################
execute.run.rf <- function(parameters) {

  FolderRoot = "~/TcpKnnH"
  FolderScripts = "~/TcpKnnH/R"

  setwd(FolderScripts)
  source("test-asoc.R")

  setwd(FolderScripts)
  source("libraries.R")


  cat("\n\n#####################################################")
  cat("\n# RUN RF SILHOUETTE: Build and Test Partitions      #")
  cat("\n######################################################\n\n")
  timeBuild = system.time(resBuild <- build.rf.silho(parameters))


  cat("\n\n######################################################")
  cat("\n# RUN RF SILHOUETTE: Matrix Confusion                #")
  cat("\n######################################################\n\n")
  timePreds = system.time(resGather <- gather.preds.rf.silho(parameters))


  cat("\n\n######################################################")
  cat("\n# RUN RF SILHOUETTE: Evaluation                      #")
  cat("\n######################################################\n\n")
  timeEvaluate = system.time(resEval <- evaluate.rf.silho(parameters))


  cat("\n\n######################################################")
  cat("\n# RUN RF SILHOUETTE: Mean 10 Folds                   #")
  cat("\n######################################################\n\n")
  timeGather = system.time(resGE <- gather.eval.rf.silho(parameters))


  cat("\n\n#####################################################")
  cat("\n# RUN RF SILHOUETTE: Save Runtime                   #")
  cat("\n#####################################################\n\n")
  timesExecute = rbind(timeBuild,
                       timePreds,
                       timeEvaluate,
                       timeGather)

  setwd(parameters$Folders$folderTest)
  write.csv(timesExecute, "runtime-run-rf.csv", row.names = FALSE)

}

###############################################################################
# CONTACT INFORMATION                                                          #
# In case of errors, contact: elainececiliagatto@gmail.com                     #
# Thank you for your collaboration!                                            #
###############################################################################
