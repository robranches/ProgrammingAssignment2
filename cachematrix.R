## generates a matrix for caching functions in each position.

makeCacheMatrix <- function(x = matrix()) {
    invm <- NULL
    set <- function(outer) {
        x <<- outer
        invm <<- NULL
    }
    get <- function() x
    setinverse<- function(inverse) invm <<-inverse
    getinverse <- function() invm
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Returns the inverse matrix if cached, else calculates it

cacheSolve <- function(x, ...) {
    invm <- x$getinverse()
    if (!is.null(invm)) {       ##checks the cache
        message("Inverse Cache GOT")
        return(invm)
    } else {
        invm <- solve(x$get()) ## Solve returns the inverse of the matrix
        x$setinverse(invm)
        return(invm)
    }
}
}