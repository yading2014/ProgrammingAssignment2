## This program provides an example of using <<-, scope rules in R. It stores variable in a local 
#function and reduce the computation complexity, if a large matrix is given 

## makeCacheMatrix() function creates a cache for matrix, and it has four functions 
## set, get, setInverse, getInverse 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL 
    set <- function(y){
        x <<- y 
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse 
    getInverse <- function() m
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## cacheSolve() returns the inverse of a given matrix x 
## it calls the function makeCacheMatrix, to check whether Inverse of the matrix 
# has been stored in a cache or not
# if yes, we return the cache 
# if not, we calcuate the inverse, and set the inverse Matrix 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()     # query x matrix cache 
    if(!is.null(m)){        # if there is a cache 
        message("getting cached data")   # give a message if cached obtained 
        return(m)           # just return the cache
    }
    data <- x$get()         # if there is no cache 
    m <- solve(data,...)    # use solve() to calcuate the inverse of a matrix 
    x$setInverse(m)      # save the result back to the cache 
    m                       # return the result 
}

## the following codes are used for test followed a thread in the forum 
## special thanks :P 

a <- makeCacheMatrix(matrix(1:4,2))
a$get()
a$getInverse()
a$set(matrix(5:8,2))
a$get()
cacheSolve(a)
cacheSolve(a)
a$getInverse()

#test inverse correctness
b = a$getInverse()
a$get() %*% b  
# it should return the identity matrix 

