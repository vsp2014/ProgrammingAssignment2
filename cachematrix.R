## makeCacheMatrix creates a special vector, with a list of following functions
## 1. Get a matrix.
## 2. Set a matrix.
## 3. Get the inverse of the given matrix.
## 4. Set the inverse of the given matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() {  x } 
    ## cache the inverse matrix
    setinverse <- function(inverse) { m <<- inverse }  
    getinverse <- function()  m 
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)    
}


## cacheSolve calculates the inverse of the given matrix.
## It first checks if the inverse of the matrix is already calculated. 
## If it is, it then retrieves the inverse matrix from the cache.
## Otherwise, it calculates and returns the inverse of the matrix.

## This function assumes that the given matrix is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()  ## get the inverse matrix from Cache
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    ## Calculates the inverse of the given matrix
    m <- solve(data, ...)
    x$setinverse(m)
    m    
}
