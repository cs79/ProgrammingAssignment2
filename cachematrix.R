## A set of functions to cache the inverse of a matrix.
## makeCacheMatrix is a function that makes a list of other functions to perform
##      caching operations on the inverse of a matrix
## cacheSolve performs the actual matrix inversion, and uses makeCacheMatrix to
##      cache results, as well as for retrieval of previously cached inversion


## function to create a list of other functions that can be called with 
##      cacheSolve
## set: sets x to passed matrix, inverse variable (i) to NULL
## get: returns the matrix (x)
## setinverse: sets inverse (i)
## getinverse: returns the inverse (i)

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() {
                x
        }
        setinverse <- function(inverse) {
                i <<- inverse
        }
        getinverse <- function() {
                i
        }
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)  
}


## Solves for inverse matrices, using caching to avoid duplication of processing
##      effort
## First checks to see if an inverse for a matrix has been cached; if so, 
##      returns from makeCacheMatrix
## If no cache found, solves for the inverse of the matrix stored in a list 
##      created by makeCacheMatrix, and caches it in makeCacheMatrix object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                i
        }
        matrix <- x$get()
        inverse <- solve(matrix)
        x$setinverse(inverse)
        inverse
}
