## These 2 functions work in tandem to create a cache to hold matrix data, and
## retrieve stored data from it in an efficient way to save processing power  
## when possible.


## This first function creates the matrix cache itself. Within this function are
## other functions which store the matrix data itself, along with indicators 
## that 'cacheSolve' accesses to know what to do (most importantly, object 'm') 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This second function retrieves data from the 'MakeCacheMatrix' function. If
## it sees that m=null (i.e. no inverse matrix has been calculated yet), it will
## calculate, return, and store the inverse matrix in the cache of 
## 'MakeCacheMatrix'. When the function is run again, it sees a new value stored
## in the object 'm', and returns that as the inverse matrix value instead of
## having to recalculate it.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        message("calculating inverse")
        m
}


## Testing that the functions work as intended

m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
n1 <- matrix(c(6,2,8,4), nrow = 2, ncol = 2)

mymatrixobject1 <- makeCacheMatrix(m1)
mymatrixobject2 <- makeCacheMatrix(n1)

cacheSolve(mymatrixobject1) #first run
cacheSolve(mymatrixobject1) #second run

cacheSolve(mymatrixobject2) #first run
cacheSolve(mymatrixobject2) #second run



