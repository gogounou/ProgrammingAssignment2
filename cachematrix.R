## A pair of functions to cache the inverse of a matrix. If a previously 
## calculated matrix inverse is called, it will be retrieved from cache
## as opposed to being recalcuated. Assumes the entered matrix has an inverse.

## Function accepts and stores a matrix, while also setting a method to
## store the inverse of that matrix. That inverse can then be retrieved
## by the cacheSolve function below.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        ## NB - this 'set' segment is not strictly necessary for the operation
        ## of the function as described in the assignment. As it was included
        ## in the structure of the example, I included it here - in case it
        ## proves necessary farther down the road...
        
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

## This function consults makeCacheMatrix to see if the inverse of the matrix
## entered has been previously calculated and stored. If it has, it returns
## that value from cache. If not, it calculates the inverse and stores that
## value before printing it to the terminal.

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