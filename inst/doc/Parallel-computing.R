## ----nomessages, echo = FALSE-------------------------------------------------
knitr::opts_chunk$set(
  warning = FALSE,
  message = FALSE,
  fig.height = 5,
  fig.width = 5
)
options(digits=4)
par(mar=c(3,3,1,1)+.1)

## ----eval=FALSE---------------------------------------------------------------
#  library(parallel)
#  primary <- '192.168.2.1'
#  IPs <- list(list(host=primary, user='myname', ncore=8), list(host='192.168.2.2', user='myname', ncore=6))
#  spec <- lapply(IPs, function(IP) rep(list(list(host=IP$host, user=IP$user)), IP$ncore))
#  spec <- unlist(spec, recursive=FALSE)
#  cl <- makeCluster(master=primary, spec=spec, type = 'PSOCK')
#  Final <- runSimulation(..., cl=cl)
#  stopCluster(cl)

## ----eval=FALSE---------------------------------------------------------------
#  library(parallel)
#  primary <- 'master'
#  IPs <- list(list(host=primary, ncore=8), list(host='slave', ncore=6))
#  spec <- lapply(IPs, function(IP) rep(list(list(host=IP$host)), IP$ncore))
#  spec <- unlist(spec, recursive=FALSE)
#  cl <- makeCluster(master=primary, spec=spec, type = 'PSOCK')
#  Final <- runSimulation(..., cl=cl)
#  stopCluster(cl)

## ----eval=FALSE---------------------------------------------------------------
#  library(future)
#  plan(multisession, workers = 2)
#  
#  res <- runSimulation(design=Design, replications=1000, generate=Generate,
#                       analyse=Analyse, summarise=Summarise,
#                       parallel = 'future')

## ----eval=FALSE---------------------------------------------------------------
#  library(progressr)
#  
#  # Rstudio style handler (if using RStudio)
#  handlers("rstudio")
#  
#  # or using the cli package for terminal-based progress
#  handlers('cli')
#  
#  # See help(progressr) for additional options and details
#  
#  # to use progressr, wrap/pipe inside with_progress()
#  res <- with_progress(runSimulation(design=Design, replications=1000, generate=Generate,
#                       analyse=Analyse, summarise=Summarise,
#                       parallel = 'future'))

## ----eval=FALSE---------------------------------------------------------------
#  plan(sequential) # release workers

