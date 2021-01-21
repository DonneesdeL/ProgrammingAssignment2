## Assignment: Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        z <- NULL       ## initializing the inverse property
        set <- function(matrix) {    ##method to set the matrix
                x <<- matrix
                z<<- NULL
        }
        get <- function() {  ##method to get the matrix
                x
        }
        setInverse <- function(inverse) { ##setting he inverse of the matrix
                z <<- inverse
        }
        getInverse <-function() {        ##getting the inverse of the matrix
                z
        }
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
               
}


## This function computes the inverse of the special matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        z <- x$getInverse()
        if(!is.null(z)){
                message("getting cached data")
                return(z)
        }
        matr <- x$get() ## get the matrix from our object
        z <- solve(matr) %*% matr       ## using matrix multiplication
        x$setInverse(z) ##setting the inverse to the object
        z ##return value
}
