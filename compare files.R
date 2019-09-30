modified_file <- system.file("ThesisProposal_GV.Rmd", package = "reviewer")
reference_file <- system.file("ThesisProposal.Rmd", package = "reviewer")

library(reviewer)
result <- diff_rmd(modified_file, reference_file)