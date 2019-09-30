library(reviewer)
result <- diff_rmd(current_file = "ThesisProposal_GV.Rmd", 
                   reference_file = "ThesisProposal.Rmd", 
                   show = "raw")
result2 <- diff_rmd(current_file = "ThesisProposal_GV.Rmd", 
                   reference_file = "ThesisProposal.Rmd", 
                   show = "rendered")
