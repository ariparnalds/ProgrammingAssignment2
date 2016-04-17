## Functions to handle matrix inversion where caculations 
## are only performed when needed.

## Creates a matrix structure. 
## Matrix as well as its inverse can be saved and retrieved from the structure.
makeCacheMatrix <- function(x = matrix()) 
{
    x <- NULL
    invX <- NULL
    set <- function(y) 
    {
        x <<- y
        invX <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) invX <<- inverse
    getInverse <- function() invX
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Solves the inverse of the matrix.
## If the inverse is already know it is retrieved instead of performing calculations again.
cacheSolve <- function(x, ...) 
{
    invX <- x$getInverse()
    if(!is.null(invX))
    {
        message("Getting cached data")
        return(invX)
    }
    data <- x$get()
    invX <- solve(data)
    x$setInverse(invX)
    invX
}
