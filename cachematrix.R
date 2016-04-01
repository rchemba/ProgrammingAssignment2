
## CacheMatrix consists of two R function that will be able to cache 
## potentially time-consuming computations and return the cached value.
## In this code the time consuming computation that is made is 
## calculating the inverse of a matrix

## makeCacheMatrix function has nested functions and cached variables.
## The nested functions are used to move the data in and out of 
## the cache environment.The variiables and functions are set 
## in memory.
## @Input : A matrix on which other functions will act 
## @Return: A list which consists of functions nested within makeCacheMatrix.

makeCacheMatrix <- function(x = matrix()) {
    
    ## Initialize caacheValue to NULL. This will be updated when the cacheSolve()
    ## function is invoked 
    caacheValue <- NULL
    
    ##Create function to set the matrix passed in the command line call to $set.
    set <- function(y) {
        
        x <<- y
        
        ##make sure the cacheValue is Null. This will be updated when cacheSolve invokes
        ##setCacheValue method
        caacheValue <<- NULL
    }
    
    
    ## Returns the matrix passed in the command line call to $set
    get <- function() {
        x
    }
    

    ## Create function to set the value of the cache
    ## This function is invoked by cacheSolve() function
    setCacheValue <- function(inverseMatrix) {
        caacheValue <<- inverseMatrix
    }
       
    
    ## Returns the cached value
    getCacheValue <- function() {
        caacheValue
    }
    
    ## list of functions to set/get the matrix and 
    ## set/get the cached value
    list(
        set = set,
        get = get,
        setCacheValue = setCacheValue,
        getCacheValue = getCacheValue
    )
}

## cacheSolve function is the main function that evaluates the time consuming 
## computation (Calcuating the inverse of the matrix).
## This function calculates the inversion only on the first execution.
## For all other executions it returns the cached value
## @Input  : "x" is an invertible matrix that has already been defined using
##           makeCacheMatrix$set()
## @Return : Returns the inverted matrix of the input matrix

cacheSolve <- function(x, ...) {
    
    ## Get the cached value for the invertible matrix
    ## using maeCacheMatrix::getCacheValue().
    inverseMatrix <- x$getCacheValue()
    
    ## If the cached value is not null it implies that the inverse has
    ## been calculated. Return the cached value.
    
    if (!is.null(inverseMatrix)) {
        message("Returning cached data")
        return(inverseMatrix)
    }
    ## If the cached value is not null it implies that the inverse
    ## is being executed for the first time
    data <- x$get()
    
    ## Calculate the inverse of a matrix using the built in function
    ## solve()
    inverseMatrix <- solve(data)
    
    ## Set the inverted matrix in the cache so that it can be used for 
    ## future invocations.
    x$setCacheValue(inverseMatrix)
    inverseMatrix
}


## Test results for the code

## create a matrix called startMatrix 
## startMatrix <- makeCacheMatrix()

## set the value of the matrix to be an invertible matrix
##startMatrix$set(matrix(rnorm(81), 9))

##Get the inverse (for the first time)
##cacheSolve(startMatrix)

############################### RESULT ##############################

# [,1]        [,2]        [,3]         [,4]          [,5]
# [1,]  0.10374243 -0.63307422 -0.28715325  0.225445421 -0.8649766415
# [2,] -0.33081213 -0.50522361 -0.44460067  0.008589458 -0.9687646585
# [3,]  0.02465926  0.24581499  0.04739635 -0.091232746  0.5402678605
# [4,]  0.21312323 -0.12790878  0.17122438  0.098191080  0.2101489798
# [5,]  0.79182467 -0.21671465  0.62461725  0.113997848  0.2461397222
# [6,]  0.70843482 -0.60381603  0.65317500 -0.391251487 -0.0847512554
# [7,]  0.36059747  0.86832275  0.36992955  0.230992864  0.8283667751
# [8,] -0.29007240 -0.26702958  0.05671141  0.325079045 -0.3297763278
# [9,]  0.53619392  0.01937718  0.10009667  0.167807118  0.0007825697
# [,6]       [,7]         [,8]       [,9]
# [1,] -0.738136392 -1.1188388 -1.279190267  1.6732625
# [2,] -0.455844402 -1.0728339 -0.976261089  1.7804869
# [3,]  0.025268311  0.5537211  0.037000283 -0.5329659
# [4,]  0.007868355  0.5943736  0.384925437 -0.6474862
# [5,] -0.864198711  1.0358234 -0.285891835 -0.6099803
# [6,] -1.011941975  0.6330420 -0.662152078 -0.2079746
# [7,]  1.135780456  1.6018382  1.553433899 -2.4984499
# [8,] -0.049820481 -0.6517437 -0.483866426  0.8342454
# [9,]  0.092936844  0.4152101 -0.002544245 -0.0696961

##Get the inverse (again)
##cacheSolve(startMatrix)

############################### RESULT ##############################

# Returning cached data

# [,1]        [,2]        [,3]         [,4]          [,5]
# [1,]  0.10374243 -0.63307422 -0.28715325  0.225445421 -0.8649766415
# [2,] -0.33081213 -0.50522361 -0.44460067  0.008589458 -0.9687646585
# [3,]  0.02465926  0.24581499  0.04739635 -0.091232746  0.5402678605
# [4,]  0.21312323 -0.12790878  0.17122438  0.098191080  0.2101489798
# [5,]  0.79182467 -0.21671465  0.62461725  0.113997848  0.2461397222
# [6,]  0.70843482 -0.60381603  0.65317500 -0.391251487 -0.0847512554
# [7,]  0.36059747  0.86832275  0.36992955  0.230992864  0.8283667751
# [8,] -0.29007240 -0.26702958  0.05671141  0.325079045 -0.3297763278
# [9,]  0.53619392  0.01937718  0.10009667  0.167807118  0.0007825697
# [,6]       [,7]         [,8]       [,9]
# [1,] -0.738136392 -1.1188388 -1.279190267  1.6732625
# [2,] -0.455844402 -1.0728339 -0.976261089  1.7804869
# [3,]  0.025268311  0.5537211  0.037000283 -0.5329659
# [4,]  0.007868355  0.5943736  0.384925437 -0.6474862
# [5,] -0.864198711  1.0358234 -0.285891835 -0.6099803
# [6,] -1.011941975  0.6330420 -0.662152078 -0.2079746
# [7,]  1.135780456  1.6018382  1.553433899 -2.4984499
# [8,] -0.049820481 -0.6517437 -0.483866426  0.8342454
# [9,]  0.092936844  0.4152101 -0.002544245 -0.0696961

