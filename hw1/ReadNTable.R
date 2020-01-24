# Import library for txt file Reading
library("tidyverse")

# set sample size and distribution type vector
nVals <- seq(100, 500, by=100)
distTypes = c("gaussian", "t5", "t1")

# print the title
cat(sprintf('\n\n%5s%10s%10s%10s%10s\n','n','Method','Gaussian','t_5','t_1'))

# Read txt files with each sample size and type (9 combinations)
for (n in nVals) {    # loop for sample sizes
  
  # Preallocate a tibble and a count variable
  prtTb = tibble(.rows = 2)
  col_n = 1
  
  for (Dt in distTypes){    # loop for distribution types
    
    # read PrimeAvg & SampAvg (in column) for this  combination
    newTb = read_table(paste("OutputFolder/",Dt,"_n", n, ".txt",sep = "") , col_names = FALSE, col_types = 'cd')
    # add the values to the table for printing as new column
    prtTb[col_n] = newTb[2]
    col_n = col_n + 1
  }
  cat(sprintf('%5d%10s%10.3f%10.3f%10.3f\n', n, 'PrimeAvg', prtTb[1,1], prtTb[1,2], prtTb[1,3]))
  cat(sprintf('%5d%10s%10.3f%10.3f%10.3f\n', n, 'SampAvg', prtTb[1,1], prtTb[1,2], prtTb[1,3]))
}
