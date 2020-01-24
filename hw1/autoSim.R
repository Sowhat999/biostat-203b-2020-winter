# autoSim.R

# set sample size and distribution type vector
nVals <- seq(100, 500, by=100)
distTypes = c("gaussian", "t5", "t1")

# run runSim.R with each sample size and type
for (n in nVals) {    # loop for sample sizes
  for (Dt in distTypes){    # loop for distribution types
    
    oFile <- paste("OutputFolder/",Dt,"_n", n, ".txt", sep="")
    sysCall <- paste("nohup Rscript runSim.R n=", n, " Dt=", shQuote(shQuote(Dt))," rep=", 50," seed=", 203," > ", oFile, sep="")
    # print(class(sysCall))
    system(sysCall, wait = FALSE)
    print(paste("sysCall=", sysCall, sep=""))
    
  }
}


