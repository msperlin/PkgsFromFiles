#' Finds and installs packages used in R related files
#'
#' This function will search for all R related files in input folder.in and find loaded packages with the
#' library() and require() functions.
#'
#' @param folder.in Folder to search for R related files
#' @param do.recursive Find files in all subdirectories? (default = TRUE)
#' @param my.library.path Library to search for installed packages (default = .libPaths()[1])
#' @param my.repository Url of CRAN repository (default = "https://cloud.r-project.org" )
#'
#' @return Messages in prompt and dataframe with the results
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' my.dir <- dirname(system.file('extdata/Example_Script_1.R', package = 'PkgsFromFiles'))
#' df.out <- pff_find_and_install_pkgs(folder.in = my.dir)
#' print(df.out)
#' }
pff_find_and_install_pkgs <- function(folder.in,
                                      do.recursive = TRUE,
                                      my.library.path = .libPaths()[1],
                                      my.repository = "https://cloud.r-project.org" ) {

  # test input
  flag <-  dir.exists(folder.in)
  if (!flag) {
    stop(paste0('Folder ', folder.in, ' does not exists.. check your arguments'))
  }
  
  flag <- is.logical(do.recursive)
  if (!flag) {
    stop(paste0('Folder ', folder.in, ' does not exists.. check your arguments'))
  }
  
  df.files <- pff_find_R_files_from_folder(folder.in, do.recursive)
  
  if (nrow(df.files) == 0) {
    stop(paste0('Found 0 R related files in folder ',folder.in, '\nYou should check your inputs.'))
  }
  
  cat(paste0('\nChecking available pkgs from ', my.repository))
  my.available.packages <- utils::available.packages(repos = my.repository)[, 1]
  
  all.pkgs <- unlist(lapply(df.files$pkgs,
                            FUN = function(x) stringr::str_split(x, ' ; ')[[1]]))
  
  # Only keep a unique list of packages to make them install once
  all.pkgs <- all.pkgs[!duplicated(all.pkgs)]
  
  # Remove NA
  all.pkgs <- all.pkgs[!is.na(all.pkgs)]
  
  
  cat('\nChecking and installing missing pkgs')
  l.out <- lapply(X = all.pkgs,
                  FUN = pff_check_install_pkgs,
                  my.available.packages = my.available.packages)
  
  df.out <- dplyr::bind_rows(l.out)

  cat('\n\nSummary:')
  cat(paste0('\n\t',
             'Found ', sum(df.out$status.message == 'Already installed'), ' packages already installed'))
  cat(paste0('\n\tHad to install ', sum(df.out$status.message == 'Instalation Ok'), ' packages'))
  cat(paste0('\n\tInstalation failed for ', sum(!df.out$installation), ' packages'))
  cat(paste0('\n\t\t',
             sum(df.out$status.message == 'Instalation failed, pkg not in CRAN'), ' due to package not being found in CRAN'))
  cat(paste0('\n\t\t',
             sum(df.out$status.message == 'Instalation Failed (missing external dependencies?)'), ' due to missing dependencies or other problems'))

  cat('\n\nCheck output dataframe for more details about failed packages')

  return(df.out)
}

