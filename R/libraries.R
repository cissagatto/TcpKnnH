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

#' Setup Environment and Load Required Libraries
#'
#' This script sets up the root directories and loads the necessary R packages
#' for processing multi-label datasets, performing clustering, and running
#' classification algorithms.
#'
#' @details
#' The script defines two main directories:
#' \itemize{
#'   \item `FolderRoot`: The root directory where all project-related files are stored.
#'   \item `FolderScripts`: The directory where R scripts are stored.
#' }
#'
#' The script also loads multiple libraries required for different tasks:
#' \itemize{
#'   \item `foreign` - Reads data from different statistical software formats.
#'   \item `stringr` - Provides functions for string manipulation.
#'   \item `plyr` and `dplyr` - Used for data wrangling and transformations.
#'   \item `reshape2` - Assists in data reshaping (melting and casting).
#'   \item `AggregateR` - Provides aggregation functions for large datasets.
#'   \item `lme4` - Implements mixed-effects models.
#'   \item `parallel` - Enables parallel computing.
#'   \item `rJava` - Interface for calling Java from R.
#'   \item `RWeka` - Provides an interface to Weka machine learning algorithms.
#'   \item `mldr` and `utiml` - Handle multi-label classification tasks.
#'   \item `foreach` and `doParallel` - Support parallel computing workflows.
#'   \item `cluster` and `pvclust` - Perform clustering analysis.
#'   \item `factoextra` - Visualizes clustering results.
#' }
#'
#' @export
#'
#' @examples
#' # No direct function call needed, simply source this script to set up the environment.
#' source("setup.R")
#'


FolderRoot = "~/TcpKnnH"
FolderScripts = "~/TcpKnnH/R"


library("foreign", quietly = TRUE)
library("stringr", quietly = TRUE)
library("plyr", quietly = TRUE)
library("dplyr", quietly = TRUE)
library("reshape2", quietly = TRUE)
library("AggregateR", quietly = TRUE)
library("lme4", quietly = TRUE)
library("parallel", quietly = TRUE)

# tem que seguir esta ordem se não dá ruim
library("rJava", quietly = TRUE)
library("RWeka", quietly = TRUE)
library("mldr", quietly = TRUE)
library("utiml", quietly = TRUE)

library("foreach", quietly = TRUE)
library("doParallel", quietly = TRUE)
library("cluster", quietly = TRUE)
library("pvclust", quietly = TRUE)
library("factoextra", quietly = TRUE)


###############################################################################
# CONTACT INFORMATION                                                          #
# In case of errors, contact: elainececiliagatto@gmail.com                     #
# Thank you for your collaboration!                                            #
###############################################################################
