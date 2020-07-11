## For long vectors or big matrices, 
## simple calculations like mean or inverse 
## may take a long amount of time. 
## Hence its better to read its cached value, 
## if avilable, as it saves computation

## Creating a special "matrix" object that can cache its inverse.

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


## Computing or retrieving cached value of inverse

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

## Using the functions to calculate the inverse

cacheSolve(makeCacheMatrix(matrix(1:4, 2,2)))
