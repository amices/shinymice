#get rdata file
get_rdata_file <- function(path){
env <- attach(path)
nm <- ls(name = env)
if (is.mids(env[[nm]])) {
  #imp <- env[[nm]]
  dat <- env[[nm]][["data"]]
} else {
  dat <- env[[nm]]
}
return(dat)}