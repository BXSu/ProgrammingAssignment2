## The two functions are used to create a special object that stores a matrix and caches its inverse.

## makeCacheMatrix creates a special "matrix", which is a list containing a function to:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
                i <- NULL
                set <- function(y){
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


## The following function computes the inverse of the special "matrix" created with the above function.
## It first checks if the inverse of the matrix has already been calculated. If so, it gets the inverse and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)){
          message("getting cached data")
          return(i)
        }
        data <- x$get()
        i <- solve(data,...)
        x$setinverse(i)
        i
}
