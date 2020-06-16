## The following functions are able to get a matrix as an input, and then it will
## cache the inverse of the input matrix using the makeCacheMatrix function and 
## then return it through the cacheSolve function.

## This function will create a matrix that cache the inverse of the input matrix 
## x.

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}




## This function will check if the special matrix from the above function exists.
## If the original matrix has a inverse, this function will return it.

cacheSolve <- function(x, ...) {
        
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}


test <- makeCacheMatrix(matrix(c(4, 5, 6, 2, 1, 0, 5, 8, 9), 3, 3))
cacheSolve(test)
