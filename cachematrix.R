## "makeCacheMatrix" creates an "object" containing a matrix with 4 "methods":
## get, set, getInverse and setInverse. This object is represented as a list
## containing these 4 functions
## "cacheSolve" returns the inverse of a CacheMatrix "object" created with
## "makeCacheMatrix", re-using a previously calculated inverse if the original
## matrix has not changed since last call
## See end of this file for an example on the intended usage

## Takes a matrix as a parameter and stores it, returning a list of functions containing the interface to operate with it
makeCacheMatrix <- function(x = matrix()) {
    
    inverse<-NULL

    #Change the value of the matrix
    set<-function(m){
        x<<-m
        inverse<<-NULL
    }
    
    #Return the stored matrix
    get<-function() x

    #Change the cached value of the inverse
    setInverse<-function(i) inverse<<-i
    
    #Return the cached value of the inverse
    getInverse<-function() inverse
    
    list(set=set, get=get,setInverse=setInverse, getInverse=getInverse)
}


## Returns a matrix that is the inverse of 'x', using a previously cached value
## if possible. 'x' MUST be an "object" created by the previous "makeCacheMatrix"
## function, and it must store an invertible matrix.
cacheSolve <- function(x, ...) {
    inv<-x$getInverse()
    if(!is.null(inv)){
        message("returning cached inverse")
        return(inv)
    }
    
    inv<-solve(x$get(),...)
    x$setInverse(inv)
    inv
}

##Example usage:
# > m<-makeCacheMatrix(matrix(c(3,7,5,2),2,2))
# > m$get()
# [,1] [,2]
# [1,]    3    5
# [2,]    7    2
# > cacheSolve(m)
# [,1]       [,2]
# [1,] -0.06896552  0.1724138
# [2,]  0.24137931 -0.1034483
# > cacheSolve(m)
# returning cached inverse
# [,1]       [,2]
# [1,] -0.06896552  0.1724138
# [2,]  0.24137931 -0.1034483
# > m$set(matrix(c(3,7,-4,2),2,2))
# > cacheSolve(m)
# [,1]       [,2]
# [1,]  0.05882353 0.11764706
# [2,] -0.20588235 0.08823529
# > cacheSolve(m)
# returning cached inverse
# [,1]       [,2]
# [1,]  0.05882353 0.11764706
# [2,] -0.20588235 0.08823529
# > 
