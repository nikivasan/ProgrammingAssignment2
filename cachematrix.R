## These functions are used to cache the inverse of a matrix. 

## makeCacheMatrix creates a matrix object that can cache its inverse. The function...
## 1. sets the value of the matrix
## 2. gets the value of the matrix
## 3. sets the value of the inverse
## 4. gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve computes the inverse of the matrix produced by the above function.
## If the inverse has already been calculated (assuming the matrix hasn't changed),
## then the function should retrieve the inverse from cache. 
## If the inverse has not been cached, then the function uses solve() to produce the inverse. 

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

##Check Output
MyMatrix <- matrix(c(2,3,5,6), 2, 2)
MyMatrix
MM1 <- makeCacheMatrix(MyMatrix)
cacheSolve(MM1)
cacheSolve(MM1)

