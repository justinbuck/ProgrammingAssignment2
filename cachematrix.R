##cachematrix.R
## cache matrix defines 2 functions that allow the 
## results of a matrix inverse to be cached so if the
## same calculation is needed again the cached value
## is used instead of recalculating (to reduce amount
## of computation).
## Jusin Buck, 20-Jul-2014 based on the R programming 
## examples makeVector.R and cachemean.R

# Usage example:
# set input matrix
# amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
# Returns original matrix
# amatrix$get()
# Compute, caches, and return matrix inverse
# cacheSolve(amatrix)   
# Return matrix inverse
# amatrix$getinv()      
# Returns cached matrix inverse using previously computed matrix inverse
# cacheSolve(amatrix)   
# Modify existing matrix
# amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) 
# Computes, caches, and returns new matrix inverse
# cacheSolve(amatrix)   
# Returns matrix
# amatrix$get()         
# Returns matrix inverse
# amatrix$getinv()      

## function to initialise the environment with functions to:
# 1.set the value of the matrix (set)
# 2.get the value of the matirx (get)
# 3.set the value of the inverse (setinv)
# 4.get the value of the inverse (getinv)
# These are output as a list so they can be called
# with [values]$[function]([input]) as shown in the example above
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # set the value of the input to the parent environment
  # and a cached inverse in the parent environment to null
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # define the get function that returns the vlue of x
  get <- function() x
  
  # function to cache the inverse (called with a x$setinv(inv))
  setinv <- function(solve) inv <<- solve
  
  # function to get the inverse
  getinv <- function() inv
  
  # output get and set functions as list
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## function to solve the matrix or return cached value if it exists
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # attempt to get the inverse from the cache
  inv <- x$getinv()
  
  # if result is not null i.e. the matrix inverse is
  # cached, then return the cached value and exit 
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # if code has got this far there is no cached value
  # so get the matrix and calculate inverse
  data <- x$get()
  inv <- solve(data, ...)
  
  # cache the inverse just calculated
  x$setinv(inv)
  
  # output the inverse
  inv
}
