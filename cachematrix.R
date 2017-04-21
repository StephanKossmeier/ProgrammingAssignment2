## these functions take a matrix and check if the inverse of this matrix is already 
## cached. If not, then the inverse is computed and stored in cash. If the inverse 
## exists in the cache then it is NOT recomputed but simply retrieved and returned
## NOTE: the functions need to be nested in the form: cacheSolve(makeCacheMatrix(...))

## makeCasheMatrix does 2 things: it takes the matrix to be tested as an argument and
## it defines a list of 4 functions to be used by its parent function cacheSolve
## as far as I understood, the trick is that some functions use "<<-" to assign 
## variables in the parent environment, which I guess makes them sort of global

makeCacheMatrix <-function(x = matrix()) {
        invx <- NULL
        set <- function(y) {
                x <<- y
                invx <<- NULL
        }
        get <- function() x
        setinv <- function(new_inverse_of_x) invx <<- new_inverse_of_x
        getinv <- function() invx
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve uses the list of functions defined in its "argument" makeCacheMatrix
## to check if an inverse exists, if so then the inverse is retrieved from cache and
## returned. If it does not exist then the inverse is computed (there is not check if the
## matrix is actually invertible) and stored in cache and returned.

cacheSolve <- function(x,...) {
        invx <- x$getinv()
        if(!is.null(invx)) {
                message("getting cached data")
                return(invx)
        }
        data <- x$get()
        invx <- solve(data,...)
        x$setinv(invx)
        invx
}
