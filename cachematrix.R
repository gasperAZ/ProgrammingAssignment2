## cachematrix.R
## Gaspar Acevedo Zain, 06 Feb 2018

## Set of functions that can computed and retrieve a cached value of
## a square matrix and its inverse


## USAGE

## mat <- matrix(runif(9), 3, 3) ## 3x3 Square matrix
## cacheMatrix <- makeCacheMatrix(mat)

## cacheSolve(cacheMatrix)  ## Solve matrix's inverse
## cacheSolve(cacheMatrix)  ## Due to its has been called twice, returns the cached inverse
## cacheMatrix$getInverse() ## get the cached inverse

## mat2 <- matrix(runif(16), 4, 4)  ## 4x4 square matrix
## cacheMatrix$set(mat2)            ## Set a new matrix
## cacheMatrix$getInverse()         ## try to retrieve its inverse
## NULL     ## this is because matrix value has been modified with "mat2"


## This function creates a special "matrix" object that can cache its inverse
## Its returns a list of functions whith which you can set & get the matrix and its inverse

makeCacheMatrix <- function(m = matrix()) {
        ## Inverse of a Matrix. Default value is NULL
        inverse <- NULL
    
        ## Set the value of the matrix with which the function works
        ## If it's changed, inverse must be NULL
        setMatrix <- function(myMatrix) {
            m <<- myMatrix
            inverse <<- NULL
        }
    
        ## get the value of the matrix
        getMatrix <- function(){    m    }
    
        ## Set the value of the inverse
        setInverse <- function(inverseMatrix){  inverse <<- inverseMatrix   }
    
        ## return the inverse value
        getInverse <- function(){   inverse   }
    
        ## returns a list of functions wich can set and get the matrix and its inverse
        list(set = setMatrix, get = getMatrix, setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of a special matrix, which was created with the function
## makeCacheMatrix. It store (cache) the inverse, which is very usefull when the function its called
## more than once and the matrix value has not changed

cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()
    
        ## if "inverse" is not null means that the inverse has been cached, so
        ## it is returned
        if(!is.null(inverse)) {
            message("Getting cached data...")
            return(inverse)
        }
    
        ## if it is the first time the inverse is computed, we must save it in the special "matrix"
        ## created by the function makeCacheMatrix
    
        ## First we get the matrix value
        matrix <- x$get()
        ## then compute its inverse
        inverse <- solve(matrix, ...)
        ## Then the inverse is cached
        x$setInverse(inverse)
        ## and finally retrieves the inverse
        inverse
}
