## Our aim in this experiment is to write a pair of functions, namely,
## "makeCacheMatrix" and "cacheSolve" that cache the inverse of a matrix

## makeCacheMatrix is a function which creates a special "matrix" object that can
## cache its inverse for the input (which is an invertible square matrix)

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
 set <- function(y) {
 x <<- y
 inv <<- NULL
 }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
 getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve is a function which computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the
## inverse from the cache


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
 if(!is.null(inv)) {  message("getting cached result")
     return(inv)
     }
 data <- x$get()
 inv <- solve(data, ...)
 x$setinv(inv)
 inv
}

## ---------------Checking the program------------------------
## m <- matrix(rnorm(16),4,4)
## m1 <- makeCacheMatrix(m)
## cacheSolve(m1)

##           [,1]       [,2]       [,3]        [,4]
##[1,]  0.5425217  0.5411156  0.3454654 -0.57915030
##[2,] -0.1642609  0.2586815 -0.3498835  0.05271951
##[3,] -0.5675989 -0.4270384  0.8619502 -0.03219287
##[4,] -0.0471719  0.2441943 -0.3105199 -1.11420722
