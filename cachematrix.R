## To compute the inverse of a matrix. It is equivalent to the function isolve() 
## but can handle large data more efficiently.
##  

## Create a function to store the 4 functions: set,get,setinverse, and getinverse.
## This function is actually to make a list of functions

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## This function executes the functions that are returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## if the inverse is not calculated by makeCacheMatrix, then cacheSolve will calculate the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix<-x$get()
        m<-solve(matrix, ...)
        x$setinverse(m)
        m
}   


#test
a<-makeCacheMatrix(matrix(2:5,2,2))
cacheSolve(a)
