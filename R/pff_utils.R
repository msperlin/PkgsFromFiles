#' Finds used packages from a single file
#'
#' This function will search the names of R packages used in the script
#'
#' @param f.in File to search for pattern
#'
#' @return A character object with all packages, separated by ' ; '
#' @export
#'
#' @examples
#'
#' my.f <- system.file('extdata/Example_Script_1.R', package = 'PkgsFromFiles')
#' pkg.out <- pff_find_pkgs_from_file(my.f)
pff_find_pkgs_from_file <- function(f.in) {

  if (length(f.in) > 1) {
    stop('Function pf_find_pkgs_from_file only accepts a single file')
  }

  txt.out <- paste(readLines(f.in, warn = FALSE), collapse = '\n')

  out.req.1 <- stringr::str_match_all(txt.out, pattern = 'require\\(\"(.*?)\"\\)')[[1]]
  out.req.2 <- stringr::str_match_all(txt.out, pattern = 'require\\((.*?)\\)')[[1]]

  pkg.out.require <- as.vector(c(out.req.1[,2], out.req.2[,2]))

  out1 <- stringr::str_match_all(txt.out, pattern = 'library\\(\"(.*?)\"\\)')[[1]]
  out2 <- stringr::str_match_all(txt.out, pattern = 'library\\((.*?)\\)')[[1]]
  pkg.out.library <- as.vector(c(out1[,2], out2[,2]))

  pkg.out <- c(pkg.out.require, pkg.out.library)

  # sanity checking
  pkg.out <- stringr::str_replace_all(pkg.out, pattern =
                                        stringr::fixed("'"), replacement = '' )
  pkg.out <- stringr::str_replace_all(pkg.out,
                                      pattern = stringr::fixed('"'), replacement = '' )

  pkg.out <- stringr::str_trim(pkg.out)
  pkg.out <- pkg.out[pkg.out != '']
  pkg.out <- unique(pkg.out)

  out.str <- paste0(pkg.out, collapse = ' ; ')

  if (out.str == '') out.str <- NA

  return(out.str)
}

#' Finds all R related files from a folder
#'
#' @inheritParams pff_find_and_install_pkgs
#'
#' @return A dataframe with several information about files and packages
#' @export
#'
#' @examples
#'  my.dir <- dirname(system.file('extdata/Example_Script_1.R', package = 'PkgsFromFiles'))
#'  df.files <- pff_find_R_files_from_folder(my.dir)
#'  print(df.files)
pff_find_R_files_from_folder <- function(folder.in, do.recursive = TRUE){


  flag <-  dir.exists(folder.in)
  if (!flag) {
    stop(paste0('Folder ', folder.in, ' does not exists.. check your arguments'))
  }

  cat('\nSearching folder ', folder.in)

  my.searched.ext <- c('.R', '.Rmd', '.Rnw')
  my.pattern <- paste0(paste0('\\', my.searched.ext, '$'),
                       collapse = '|')
  target.files <- list.files(path = folder.in,
                             pattern = my.pattern  ,
                             recursive = do.recursive,
                             full.names = T)

  file.extensions <- tools::file_ext(target.files)
  my.folders <- unique(dirname(target.files))

  cat(paste0('\n\tFound ', length(target.files), ' files in ', length(my.folders), ' folders'))
  cat(paste0('\n\t\t R Scripts: ', sum(file.extensions == 'R'), ' files'))
  cat(paste0('\n\t\t Rmarkdown files: ', sum(file.extensions == 'Rmd'), ' files'))
  cat(paste0('\n\t\t Sweave files: ', sum(file.extensions == 'Rnw'), ' files'))

  # fix for CRAN check message
  files <- pkgs <- NULL
  df.files <- dplyr::data_frame(files = target.files,
                                file.names = basename(files),
                                extensions = file.extensions,
                                pkgs = sapply(X = target.files,
                                              FUN = pff_find_pkgs_from_file),
                                n.pkgs = sapply(pkgs,
                                                function(x) length(stringr::str_split(x, ' ; ')[[1]])) )

  return(df.files)

}

#' Checks and installs a single package
#'
#' This function will check if input package in pkg.in is installed and, if not, installs it from a chosen repository
#'
#' @param pkg.in Name of the package to be installed
#' @param my.available.packages Names of locally available (installed) packages
#' @inheritParams pff_find_and_install_pkgs
#'
#' @return A dataframe with information about the result of the installation
#' @export
#'
#' @examples
#' pff_check_install_pkgs('dplyr')
pff_check_install_pkgs <- function(pkg.in,
                                   my.available.packages,
                                   my.library.path = .libPaths()[1],
                                   my.repository = "https://cloud.r-project.org" ){
  cat('\nInstalling', pkg.in)

  my.installed.pkgs <- utils::installed.packages(lib.loc = my.library.path)[ ,1]

  if ( !(pkg.in %in% my.installed.pkgs) ){

    if (pkg.in %in% my.available.packages) {
      cat('- Pkg in CRAN. Installing ', pkg.in)
      utils::install.packages(pkg.in, lib = my.library.path, repos = my.repository)

      # check if it was intalled properly
      flag <- pkg.in %in% my.installed.pkgs

      if (flag) {
        my.message = 'Installation OK'
        cat(paste0('\t', my.message))
        return(dplyr::data_frame(pkg = pkg.in,
                                 status.message = my.message,
                                 installation =TRUE))
      } else {
        my.message = 'Installation failed (missing external dependencies?)'
        cat(paste0('\t', my.message))
        return(dplyr::data_frame(pkg = pkg.in,
                                 status.message = my.message,
                                 installation = FALSE))
      }

    } else {
      my.message = 'Installation failed, pkg not in CRAN'
      cat(paste0('\t', my.message))
      return(dplyr::data_frame(pkg = pkg.in,
                               status.message = my.message,
                               installation = FALSE))
    }

  } else {
    my.message = 'Already installed'
    cat(paste0('\t', my.message))
    return(dplyr::data_frame(pkg = pkg.in,
                             status.message = my.message,
                             installation = TRUE))
  }

}


