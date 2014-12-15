## makeCacheMatrix
##
## make an object that holds the provided matrix
## it provides methods to get/set the matrix
## and to get/set the cached inverse of the matrix
##
## @var matrix x
## @return list

makeCacheMatrix <- function(x = matrix()) {
    xInv <- NULL
    set <- function(y) {
        x <<- y
        xInv <<- NULL
    }
    get <- function() {
        x
    }
    setInverse <- function(inverse) {
        xInv <<- inverse
    }
    getInverse <- function() {
        xInv
    }
    list(
         set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse
    )
}


## cacheSolve
## 
## solve the inverse of a matrix object created by makeCacheMatrix
## the calculated inverse is cached in the provided matrix object
##
## @var list x
## @return matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached matrix inverse")
        return(inverse)
    }
    matrix <- x$get()
    inverse <- solve(matrix, ...)
    x$setInverse(inverse)
    inverse
}
