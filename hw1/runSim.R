## parsing command arguments
for (arg in commandArgs(TRUE)) {
  #print(arg)
  #print(class(arg))
  eval(parse(text=arg))
}

## functions --------------

## check if a given integer is prime
isPrime = function(n) {
  if (n <= 3) {
    return (TRUE)
  }
  if (any((n %% 2:floor(sqrt(n))) == 0)) {
    return (FALSE)
  }
  return (TRUE)
}

## estimate mean only using observation with prime indices
estMeanPrimes = function (x) {
  n = length(x)
  ind = sapply(1:n, isPrime)
  return (mean(x[ind]))
}


## main code ------------------

# set seed number for randomness generation
set.seed(seed)

# preallocate variables for MSE of sample average and prime average
mseSamAvg = 0
msePrimAvg = 0

# for loop for simulation replicates
for (r in 1:rep){
  # generate data from specified distribution
  #print(Dt)
  #print(class(Dt))
  #print(class("gaussian"))
  if (Dt == "gaussian"){
    # standard normal distribution
    x = rnorm(n)
  }else if (Dt == "t1"){
    # t distribution df = 1
    x = rt(n, df = 1)
  }else if (Dt == "t5"){
    # t distribution df = 5
    x = rt(n, df = 5)
  }else{
    stop('Error: Unknown distribution assigned')
  }
  
  # calculate Square Error for two estimator
  mseSamAvg = mseSamAvg + mean(x)^2
  msePrimAvg = msePrimAvg + estMeanPrimes(x)^2
}

# Calculate and print MSE two estimator
print(msePrimAvg/rep)
print(mseSamAvg/rep)

# sprintf('The MSE of classical sample average estimator is %.4f', mseSamAvg/rep)
# sprintf('The MSE of primed-indexed average estimator is %.4f', msePrimAvg/rep)

