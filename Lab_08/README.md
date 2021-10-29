Lab 8
================
Christina Lin
10/29/2021

## Problem 1: Examples for Use of Parallel Computing

1.  Weather forecasting
2.  Analysis of single-cell RNAseq data
3.  Analysis of epidemiology data for a certain disease

## Problem 2

``` r
fun1 <- function(n = 100, k = 4, lambda = 4) {
  x <- NULL
  for (i in 1:n)
    x <- rbind(x, rpois(k, lambda))
  x #return(x)
}
#rpois is a vectorized function
#this first function is filling in functions row by row 

#parallel computing not needed here 
fun1alt <- function(n = 100, k = 4, lambda = 4) {
  matrix(rpois(n*k,lambda), nrow=n, ncol=k)
}
#this second function is filling things out by column because using the default byrow=FALSE
#R sorts data column-wise 
#R sees matrix as a vector, so single number indices can be used to extract elements 

# Benchmarking
microbenchmark::microbenchmark(
  fun1(n=1000),
  fun1alt(n=1000), unit="relative"
)
```

    ## Warning in microbenchmark::microbenchmark(fun1(n = 1000), fun1alt(n = 1000), :
    ## less accurate nanosecond times to avoid potential integer overflows

    ## Unit: relative
    ##               expr      min       lq     mean   median       uq    max neval
    ##     fun1(n = 1000) 23.62325 25.30931 27.61338 25.48355 26.18202 11.437   100
    ##  fun1alt(n = 1000)  1.00000  1.00000  1.00000  1.00000  1.00000  1.000   100

``` r
# Data Generating Process (10 x 10,000 matrix)
set.seed(1234)
x <- matrix(rnorm(1e4), nrow=10)

# Find each column's max value
fun2 <- function(x) {
  apply(x, 2, max)
}
#apply function in R is not a vector function (it's a for-loop underneath)

fun2alt <- function(x) {
  idx <- max.col(t(x)) 
  x[cbind(idx, 1:ncol(x))]
}
#transposes matrix x (switch row and column), and use max.col to get the indices (gives the row number of the max value for each column), which is stored in idx
#cbind then gives the matrix index

all(fun2(x)==fun2alt(x))
```

    ## [1] TRUE

``` r
# Benchmarking
microbenchmark::microbenchmark(
  fun2(x),
  fun2alt(x), unit = "relative"
)
```

    ## Unit: relative
    ##        expr     min       lq     mean   median       uq      max neval
    ##     fun2(x) 7.48467 7.414318 6.714142 7.270242 7.093628 1.349958   100
    ##  fun2alt(x) 1.00000 1.000000 1.000000 1.000000 1.000000 1.000000   100

## Problem 3

``` r
library(parallel)

my_boot <- function(dat, stat, R, ncpus = 1L) {
  
  # Getting the random indices
  n <- nrow(dat)
  idx <- matrix(sample.int(n, n*R, TRUE), nrow=n, ncol=R)
 
  # Making the cluster using `ncpus`
  # STEP 1: Create cluster
  cl <- makePSOCKcluster(ncpus)
  
  # STEP 2: Preparing the cluster to export, include all the objects that are not included in the child sessions
  clusterSetRNGStream(cl, 123) 
  clusterExport(cl, c("stat", "dat", "idx"), envir = environment())
  # without setting environment to the current function environment, it will attempt to look for the variables in the global environment 
  
    # STEP 3: THIS FUNCTION NEEDS TO BE REPLACES WITH parLapply
  ans <- parLapply(cl = cl, seq_len(R), function(i) {
    stat(dat[idx[,i], , drop=FALSE])
  })
  
  # Coercing the list into a matrix
  ans <- do.call(rbind, ans)
  
  # STEP 4: Stop the cluster
  stopCluster(cl)
  
  ans
  
}
```

``` r
# Bootstrap of an OLS
my_stat <- function(d) coef(lm(y ~ x, data=d))

# DATA SIM
set.seed(1)
n <- 500; R <- 1e4

x <- cbind(rnorm(n)); y <- x*5 + rnorm(n)

# Checking if we get something similar as lm
ans0 <- confint(lm(y~x))
ans1 <- my_boot(dat = data.frame(x, y), my_stat, R = R, ncpus = 2L)

# You should get something like this
t(apply(ans1, 2, quantile, c(.025,.975)))
```

    ##                   2.5%      97.5%
    ## (Intercept) -0.1386903 0.04856752
    ## x            4.8685162 5.04351239

``` r
##                   2.5%      97.5%
## (Intercept) -0.1372435 0.05074397
## x            4.8680977 5.04539763
ans0
```

    ##                  2.5 %     97.5 %
    ## (Intercept) -0.1379033 0.04797344
    ## x            4.8650100 5.04883353

``` r
##                  2.5 %     97.5 %
## (Intercept) -0.1379033 0.04797344
## x            4.8650100 5.04883353
```

``` r
system.time(my_boot(dat = data.frame(x, y), my_stat, R = 4000, ncpus = 1L))
```

    ##    user  system elapsed 
    ##   0.035   0.006   1.332

``` r
system.time(my_boot(dat = data.frame(x, y), my_stat, R = 4000, ncpus = 2L))
```

    ##    user  system elapsed 
    ##   0.049   0.010   0.782
