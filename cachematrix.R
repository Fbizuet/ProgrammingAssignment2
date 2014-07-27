## Put comments here that give an overall description of what your
## functions do:
# this functions create a special object that stores a marix and cache's its inverse.

## Write a short comment describing this function
# this function creates a special matrix object that has a list that permits:
# a) set the value of the matrix
# b) get the value of the matrix
# c) set the value of the inverse
# d) get the value of the inverse
# and cached its inverse

makeCacheMatrix <- function(x = matrix()) {
          # debe ser cuadra
        if ( nrow(x) != ncol(x)) {  # chk
           stop ("input must be square")
        } # chk

        minv <- NULL
        set <- function(y) {
                x <<- y
                minv <<- NULL
        }
        get <- function() x
        setinv <- function(inversa) minv <<- inversa
        getinv <- function() minv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}  # end   makeCacheMatrix


## Write a short comment describing this function
# this function back an inverse matrix of memory cached or it calculates the inverse by solve function
# 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        minv <- x$getinv()
        if(!is.null(minv)) {
                message("getting cached data")
                return(minv)
        }
        data <- x$get()
        minv <- solve(data, ...)
        x$setinv(minv)
        minv        
}   # end cacheSolve
