#get rdata file
get_rdata_file <- function(path, dat_or_imp = "dat") {
  env <- attach(path)
  nm <- ls(name = env)
  if (is.mids(env[[nm]])) {
    imp <- env[[nm]]
    dat <- env[[nm]][["data"]]
  } else {
    dat <- env[[nm]]
  }
  if (dat_or_imp == "imp") {
    return(imp)
  } else {
    return(dat)
  }
}