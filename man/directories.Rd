% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{directories}
\alias{directories}
\title{Create and Organize Directory Structure}
\usage{
directories(parameters)
}
\arguments{
\item{parameters}{A list containing configuration parameters, including:
\itemize{
\item{\code{Folder.Results}}{The root directory for storing results.}
\item{\code{FolderRoot}}{The main root directory.}
\item{\code{Validation}}{The validation method used (e.g., "Silhouette", "Macro-F1", "Micro-F1", "Hamming-Loss").}
\item{\code{Classifier}}{The name of the classifier being used.}
\item{\code{Similarity}}{The similarity measure used in evaluation.}
}}
}
\value{
A list containing paths to the created directories:
\item{folderResults}{Main results directory.}
\item{folderUtils}{Directory for utility files.}
\item{folderDatasets}{Directory for dataset storage.}
\item{folderLabelSpace}{Subdirectory for label space data.}
\item{folderNamesLabels}{Subdirectory for label names.}
\item{folderCV}{Cross-validation directory.}
\item{folderCVTR}{Training data directory within cross-validation.}
\item{folderCVTS}{Test data directory within cross-validation.}
\item{folderCVVL}{Validation data directory within cross-validation.}
\item{folderPartitions}{Directory for data partitions.}
\item{folderCommunities}{Directory for community detection results.}
\item{Python}{Directory for Python scripts.}
\item{folderValidation}{Directory for validation results.}
\item{folderTest}{Directory for test results.}
\item{folderRootReports}{Root directory for reports.}
\item{folderReports}{Directory for specific reports.}
\item{folderRepSim}{Directory for similarity-based reports.}
\item{folderRepClassifier}{Directory for classifier-specific reports.}
}
\description{
This function generates and organizes the directory structure needed for storing
results, datasets, partitions, reports, and other relevant files in a machine learning workflow.
}
\examples{
\dontrun{
params <- list(Folder.Results = "results", FolderRoot = "root",
               Validation = "Silhouette", Classifier = "SVM", Similarity = "Cosine")
dirs <- directories(params)
print(dirs$folderResults)
}
}
