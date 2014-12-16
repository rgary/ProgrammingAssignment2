## This function defines two global functions to implement the requirements
## of Programming Assignment 2. 
##
## The first function (makeCacheMatrix(x)) accepts a matrix as its argument and creates a named 
## vector of functions (an object) which stores a copy of a matrix and defines a set of functions 
## (or in Object Oriented Parlance"methods") to manipulate the matrix and to store/retrieve a 
## cached inverse of it.
##
## The methods are:
##    changeMatrix(x):   compare x to the cached matrix. If it is identical, do nothing. If
##                       it is different, replace the original matrix and invalidate the cached
##                       inverse.
##    getCachedMatrix(): return the current value of the cached matrix.
##    setCacheInv(x):    cache x as the inverse of the current matrix. 
##    getCachedInv():    return the value of the current cached inverse or NULL if the cache has
##                       not been set.
##  
##  The second function (cacheSolve(x,...)) returns an inverse of a matrix stored using 
##  makeCacheMatrix. If the inverse is invalid (NULL), then cacheSolve will invoke the solve()
##  and cache the result. If the inverse is valid, then it is returned without recalculating it.
##  
##  The normal invocation of these functions would look something like this:
##    a <- createCacheMatrix(some_matrix)
##    solution <- solveCache(a)
##
##  Notes
##    - the code assumes that assumes the matrix is invertable, reducing the need to error check.
##    - The code produced by makeCacheMatrix doesn't have to be used for the inverse, the result
##      of any function could be cached to prevent recomputation.
##    - To be functionally complete, the extra arguments to solve() should be checked to see if they
##      have changed as well. Changing the extra arguments could result in a different answer and
##      should also be a reason for treating the cache as invalid, but for the purposes of this
##      assignment, that is overkill. 
##

# Return a named vector of functions to implement matrix cacheing.
makeCacheMatrix <- function(x = matrix()) {
  cached_x <- x 
  cached_inv <- NULL
  
  # if matrix has actually changed, store the new value and invalidate the cache
  changeMatrix <- function(y) {
    if (identical(cached_x,y)) {
      return
    }
    cached_x <<- x
    cached_inv <<- NULL  # toss the old invers
  }
  
  getCachedMatrix <- function(...) { # Return cached_x, might be NULL.
    return(cached_x) 
  }
  
  setCachedInv <- function(x) {
    cached_inv <<- x  # Set Cached Inverse
  }
  
  getCachedInv <- function() {
    return(cached_inv)  # Retrieve Cached Value for Inverse Matrix
  }
  
  # Return a list of the created functions
  list(setMatrix=setMatrix, 
       getCachedMatrix=getCachedMatrix, 
       setCachedInv=setCachedInv, 
       getCachedInv=getCachedInv)
}


# Check for a cached inverse and return it, 
# ...if it doesn't exist, calculate it and save if for later.
cacheSolve <- function(x, ...) {
  solution = x$getCachedInv()
  if (is.null(solution)) {  # No Cached Value, so solve and save
    print('Calculating solution because cached value not found.')
    solution = solve(x$getCachedMatrix(), ... )
    x$setCachedInv(solution)   
  }
  return(solution)
}
