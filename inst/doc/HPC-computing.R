## ----nomessages, echo = FALSE-------------------------------------------------
knitr::opts_chunk$set(
  warning = FALSE,
  message = FALSE,
  fig.height = 5,
  fig.width = 5
)
options(digits=4)
par(mar=c(3,3,1,1)+.1)

## ----include=FALSE------------------------------------------------------------
library(SimDesign)

## -----------------------------------------------------------------------------
# SimDesign::SimFunctions()
library(SimDesign)

Design <- createDesign(N = c(10, 20, 30))

Generate <- function(condition, fixed_objects = NULL) {
    dat <- with(condition, rnorm(N, 10, 5)) # distributed N(10, 5)
    dat
}

Analyse <- function(condition, dat, fixed_objects = NULL) {
    ret <- c(mean=mean(dat), median=median(dat)) # mean/median of sample data
    ret
}

Summarise <- function(condition, results, fixed_objects = NULL){
    colMeans(results)
}

## ----eval=FALSE---------------------------------------------------------------
#  # standard setup (not ideal for HPC clusters as parallelization
#  #  occurs within the design conditions, not across)
#  res <- runSimulation(design=Design, replications=10000, generate=Generate,
#                       analyse=Analyse, summarise=Summarise, parallel=TRUE,
#                       filename='mysim')

## -----------------------------------------------------------------------------
Design300 <- expandDesign(Design, repeat_conditions = 100L)
Design300

# replications per row in Design300
replications <- rep(100L, nrow(Design300))

## -----------------------------------------------------------------------------
rc <- c(100, 100, 1000)
DesignUnbalanced <- expandDesign(Design, repeat_conditions = rc)
DesignUnbalanced

replicationsUnbalanced <- rep(c(100, 100, 10), times = rc)
head(replicationsUnbalanced)
table(replicationsUnbalanced)

## -----------------------------------------------------------------------------
set.seed(0)
x <- runif(100)
set.seed(1)
y <- runif(100)

plot(x, y)           ## seemingly independent
plot(x[-1], y[-100]) ## subsets perfectly correlated

## -----------------------------------------------------------------------------
# gen_seeds()   # do this once on the main node/home computer and store the number!
iseed <- 1276149341

## ----eval=FALSE---------------------------------------------------------------
#  # get assigned array ID (default uses type = 'slurm')
#  arrayID <- getArrayID()

## ----eval=FALSE---------------------------------------------------------------
#  # run the simulation on subset based on arrayID subset information
#  runArraySimulation(design=Design300, replications=replications,
#                     generate=Generate, analyse=Analyse,
#                     summarise=Summarise, iseed=iseed,
#                     arrayID=arrayID, filename='mysim')

## ----eval=FALSE---------------------------------------------------------------
#  # run the simulation on subset based on arrayID subset information
#  runArraySimulation(design=Design300, replications=replications,
#                     generate=Generate, analyse=Analyse,
#                     summarise=Summarise, iseed=iseed, arrayID=arrayID,
#                     dirname='mysimfiles', filename='mysim')

## ----eval=FALSE---------------------------------------------------------------
#  library(SimDesign)
#  
#  Design <- createDesign(N = c(10, 20, 30))
#  
#  Generate <- function(condition, fixed_objects = NULL) {
#      dat <- with(condition, rnorm(N, 10, 5)) # distributed N(10, 5)
#      dat
#  }
#  
#  Analyse <- function(condition, dat, fixed_objects = NULL) {
#      ret <- c(mean=mean(dat), median=median(dat)) # mean/median of sample data
#      ret
#  }
#  
#  Summarise <- function(condition, results, fixed_objects = NULL){
#      colMeans(results)
#  }
#  
#  # expand the design to create 300 rows with associated replications
#  Design300 <- expandDesign(Design, repeat_conditions = 100L)
#  replications <- rep(100L, nrow(Design300))
#  
#  # gen_seeds() # do this once on the main node/home computer and store the number!
#  iseed <- 1276149341
#  
#  # get assigned array ID (default uses type = 'slurm')
#  arrayID <- getArrayID()
#  
#  # run the simulation on subset based on arrayID subset information
#  runArraySimulation(design=Design300, replications=replications,
#                     generate=Generate, analyse=Analyse,
#                     summarise=Summarise, iseed=iseed, arrayID=arrayID,
#                     dirname='mysimfiles', filename='mysim')

## ----eval=FALSE---------------------------------------------------------------
#  setwd('mysimfiles')
#  Final <- SimDesign::aggregate_simulations(files=dir())
#  Final

## ----eval=FALSE---------------------------------------------------------------
#  # save the aggregated simulation object for subsequent analyses
#  saveRDS(Final, "../final_sim.rds")

## ----eval=FALSE---------------------------------------------------------------
#  # Return successful results up to the 11 hour mark, and terminate early
#  #   if more than 3.5 GB of RAM are required to store the internal results
#  runArraySimulation(design=Design300, replications=replications,
#                     generate=Generate, analyse=Analyse,
#                     summarise=Summarise, iseed=iseed, arrayID=arrayID,
#                     dirname='mysimfiles', filename='mysim',
#                     control=list(max_time="11:00:00", max_RAM="3.5GB"))
#  

## ----eval=FALSE---------------------------------------------------------------
#  Final <- SimDesign::aggregate_simulations(files=dir())
#  Final

## ----eval=FALSE---------------------------------------------------------------
#  Missed <- SimDesign::aggregate_simulations(files=dir(), check.only=TRUE)
#  Missed

## ----include=FALSE------------------------------------------------------------
subDesign <- createDesign(N=c(10,30))
replications_missed <- c(1000, 2000)

## ----eval=FALSE---------------------------------------------------------------
#  subDesign <- subset(Missed, select=N)
#  replications_missed <- subset(Missed, select=MISSED_REPLICATIONS)

## -----------------------------------------------------------------------------
subDesign
replications_missed

## -----------------------------------------------------------------------------
Design_left <- expandDesign(subDesign, 50) # smaller number of reps per array
Design_left

replications_left <- rep(replications_missed/50, each=50)
head(replications_left)
table(replications_left)

# new total design and replication objects
Design_total <- rbind(Design300, Design_left)
nrow(Design_total)
replications_total <- c(replications, replications_left)
table(replications_total)

# this *must* be the same as the original submission
iseed <- 1276149341

## ----eval=FALSE---------------------------------------------------------------
#  # See if any missing still
#  SimDesign::aggregate_simulations(files=dir(), check.only=TRUE)
#  
#  # Obtain complete simulation results
#  Final <- SimDesign::aggregate_simulations(files=dir())

