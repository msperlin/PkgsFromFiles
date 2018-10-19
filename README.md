# Introduction

When working with different computers at home or work, one of the problems I have is installing missing packages in R. As an example, a script that works in my work computer may not work in my home computer if a particular package is not installed.  This is specially annoying when I have a fresh install of the operating system. In this case I must manualy install all packages, case by case. 

One of the solutions to this problem is to use package [pacman](https://CRAN.R-project.org/package=pacman). It includes function `p_load` that will check if a package is available and, if not, install it from CRAN. However, for me, I like using `library` and `require` as it is consistent with my code format. Also, in a fresh R install, I rather install all my required packages in a single run, before I need it. 

Package PkgsFromFiles solves this issue by finding and parsing all R related files (.R, .Rmd, .Rnw) from a given folder, finding all calls to library() and require() and installing all unavailable packages.

# Instalation

```{r, eval=FALSE}
# from cran 
install.packages('PkgsFromFiles')

# from github
if (!require(devtools)) install.packages('devtools')
devtools::install_github('msperlin/PkgsFromFiles')
```

# Usage

The main function of the package is `pff_find_and_install_pkgs`, which will search and install missing packages from R files at a given directory. As an example, we'll use the folder from the package, which contains two example scripts. Let's try it out:

```{r, eval=FALSE}
library(PkgsFromFiles)

my.dir <- dirname(system.file('extdata', package = 'PkgsFromFiles'))

df <- pff_find_and_install_pkgs(folder.in = my.dir)
dplyr::glimpse(df)
```

As you can see, function `pff_find_and_install_pkgs` will find all R related files recursivelly in the given folder. A summary in text is shown at the end of execution. 

The output of the function is a dataframe with the details of the operation:

```{r, eval=TRUE}
dplyr::glimpse(df)
```

The package also includes function `pff_plot_summary_pkgs`:

```{r, eval=FALSE}
p <- pff_plot_summary_pkgs(folder.in = my.dir)
print(p)
```


