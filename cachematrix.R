## The two functions in this file, "cashmatrix.R", are used to create a special
## object that stores a matrix and caches its inverse matrix.

## This function, makeCacheMatrix, creates a special "matrix" object that can cache its
## inverse. This object is a list containing the following functions:
##      1.cache the value of the matrix
##      2.get the value of the matrix
##      3.cache the value of the inverse matrix
##      4.get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {  
    InvM <- NULL    
    set <- function(y) {
        x <<- y   
        InvM <<- NULL
    }
    get <- function() x
    setinvm <- function(inversematrix) InvM <<- inversematrix
    getinvm <- function() InvM
    list( set = set, get= get, setinvm = setinvm, getinvm = getinvm )
}


## This function computes and caches the inverse matrix of the special objected created 
## with the above function. If it is already computed, the cached inverse matrix is fetched. 
## The checking , fetching and caching are done via functions in makeCacheMatrix.

cacheSolve <- function(x, ...) {
        
    InvM <- x$getinvm()
    if(!is.null(InvM)){
        message("getting cached data")
        return(InvM)
    }
    data <- x$get()
    InvM <- solve(data,...)    
    x$setinvm(InvM)    
    InvM
}

