##makeCacheMatrix creates a list containing a function to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
# cacheSolve returns the inverse of the matrix. If the inverse has already been computed,
# it will grab the result without computing the answer again. It does this by first checking 
# to see if the inverse has been computed. If it hasn't, it computes it and stores the value in 
# the cache by the "setinverse" function.

# cacheSolve assumes the matrix can always be inverted. 

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

## Sample run:
## >  x <- rbind(c(-.25, 0, .25), c(-1,-2,-3), c(12,25,15))
## > m <- makeCacheMatrix(x)
## > m$get()
##   [,1] [,2]  [,3]
##[1,] -0.25    0  0.25
##[2,] -1.00   -2 -3.00
##[3,] 12.00   25 15.00



## > cacheSolve(m)
##           [,1]       [,2]        [,3]
##[1,] -3.91304348 -0.5434783 -0.04347826
##[2,]  1.82608696  0.5869565  0.08695652
##[3,]  0.08695652 -0.5434783 -0.04347826
