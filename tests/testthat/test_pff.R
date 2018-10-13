library(testthat)
library(PkgsFromFiles)

test_that(desc = 'Test of main function',{
          expect_true({
            my.dir <- dirname(system.file('extdata/Example_Script_1.R', package = 'PkgsFromFiles'))
            df.files <- pff_find_R_files_from_folder(my.dir)
            nrow(df.files) > 0
            }) } )

