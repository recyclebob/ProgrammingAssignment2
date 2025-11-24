## Put comments here that give an overall description of what your
## functions do
##
## These two functions provide the option to cache the inverse of a matrix
## so that a previously calculated inverse does not have to calculated again.
## The makeCacheMatrix function lets the user set and cache a matrix and its
## inverse in a cacheMatrix as well as to retrieve them from it afterwards.
## The cacheSolve function returns the matrix inverse if it is already cached
## within a cacheMatrix. If the inverse is not cached yet the function calculates
## the inverse a saves it in the cache.

## Write a short comment describing this function
##
## Allow to cache matrix by creating a list of four functions to
## Set the value of a matrix, get the value of a matrix
## Set the value of the inverse, get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv_mat <- NULL
  mat <- NULL
  set <- function(y) {
    mat <<- y
    inv_mat <<- NULL
  }
  get <- function() mat
  setInv <- function(inv) {
    inv_mat <<- inv
  }
  getInv <- function() inv_mat
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function
##
## Tries to retrieve value of inverse cached in x. 
## If the cached inverse is not null return its value.
## Else calculates the inverse from the matrix cached within x and returns x.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_mat <- x$getInv()
        if(!is.null(m)) {
                message("getting cached inverse")
                return(inv_mat)
        }
        mat <- x$get()
        inv_mat <- solve(x, ...)
        x$setInv(inv_mat)
        inv_mat
}
