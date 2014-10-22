## These functions allow to cache the value of the inverse of the matrix
## so that when we need it again, it can be looked up in the cache rather
## than recomputed

## This function creates a special matrix that contains functions to
## to set and get the value of the matrix, and set and get the value
## of the mean
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL

	# Sets the values for the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }

	# Gets the values of the matrix
        get <- function() x

	# Sets the inverse of the matrix
        setinverse <- function(inverse) inv <<- inverse

	# Gets the inverse of the matrix
        getinverse <- function() inv

        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function calculates the inverse of the special matrix created with
## the above funcion. It first checks to see if the inverse has already been
## calculated. If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value of 
## the inverse in the cache via the setinverse function

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'

	## Gets the inverse already stored
        inv <- x$getinverse()
        if(!is.null(inv)) {
		## if it had been calculated already, it returns it
                message("getting cached data")
                return(inv)
        }
	## otherwise, it calculates it
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
