% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pff_utils.R
\name{pff_find_R_files_from_folder}
\alias{pff_find_R_files_from_folder}
\title{Finds all R related files from a folder}
\usage{
pff_find_R_files_from_folder(folder.in, do.recursive = TRUE)
}
\arguments{
\item{folder.in}{Folder to search for R related files}

\item{do.recursive}{Find files in all subdirectories? (default = TRUE)}
}
\value{
A dataframe with several information about files and packages
}
\description{
Finds all R related files from a folder
}
\examples{
 my.dir <- dirname(system.file('extdata/Example_Script_1.R', package = 'PkgsFromFiles'))
 df.files <- pff_find_R_files_from_folder(my.dir)
 print(df.files)
}
