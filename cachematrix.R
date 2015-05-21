## This function firstly creates a special matrix, which store four sub-functions. Secondly, it checks if the inverse
## of the matrix already exists returning the cached matrix, otherwise it computes the inverse. 

## The makeCacheMatrix function stores a list of sub-functions. These sub-functions allow working at two different 
## levels locally and in the parent environmment, returning the inverse matrix to memory.
## Args: 
##  x: matrix assumed to be invertable.
## Returns:
##  A list of functions stored in the main function.
## Functions:
##  1. set: stores the value of the matrix. This function is only required if the input of the main function changes. 
##  2. get: returns the value of the matrix stored in x.
##  3. setsolve: stores a value in 'm'.
##  4. getsolve: returns the value stored in 'm'.

makeCacheMatrix <- function(x = matrix()) {                                  
  m <- NULL                                                            ## 'm' is defined as null in the parent environment.
  set <- function(y) {
    x <<- y                                                            ## set assigns 'y' to 'x' in its parent environment.
    m <- NULL                                                          ## Restores to null the value of the solve 'm'.
  }
  get <- function(){                                                   ## get returns the value stored in set.
    x
  } 
    
  setsolve <- function(solve) {
    m <<- solve                                                        ## setsolve assigns the value solve to 'm' in its parent environment.
  }
  getsolve <- function() {                                             ## getsolve returns the value stored in setsolve.
    m
  }
  
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve) ## Return the list of functions.
}

## The cacheSolve function computes de inverse of a given matrix, if the inverse has been already calculated, 
## it returns the inverse cached matrix and skips the rest of the computation. 

## Args: 
##  x: special matrix stored by makeCacheMatrix containing the sub-functions:set, get, setsolve, getsolve.
## Returns:
##  The inverse of a matrix. 
## Functions:
##  The solve function computes the inverse of a matrix.

cacheSolve <- function(x, ...) {
 m <- x$getsolve()                                                    ## Assign to 'm' the value returned by getsolve.
                                                                      ## x$getsolve subsets getsolve sub-function stored in the main function x
  if(!is.null(m)) {                                                   ## If 'm' is not null and exists in memory.
    message("getting cached data")                                    ## Return message.
    return(m)                                                         ## Return cached inverse matrix already stored in memory.
  }                                                                   ## Skips the rest of the computation.
                                                                      ## If evaluation of 'm' is NULL.
  data <- x$get()                                                     ## Assign to 'data' the matrix returned by get.
                                                                      ## x$get subsets get sub-function stored in the main function x
  m <-solve(data, ...)                                                ## Assign 'm' the computation of the inverse matrix stored in data.
  x$setsolve(m)                                                       ## 'm' is assigned to 'm' in the parent environment. 
  m                                                                   ## Return a matrix that is the inverse of 'x'
}
