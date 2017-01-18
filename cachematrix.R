## These functions calculate the inverse of a matrix
## and then cache the result.

## This function creates a makeCacheMatrix object
## to be used by the following function.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y){
                x<<-y
                m<<-NULL
        }
        get <- function() x
        solvmat <- function(solve) s<<-solve
        getsol <- function() s
        list(set=set, get=get, solvmat=solvmat, getsol=getsol)
}


## This function looks for a cached solution, and if
## one is not found, computes an inverse of a given matrix.

cacheSolve <- function(x, ...) {
        s <- x$getsol()
        if(!is.null(s)){
                message("Getting cached solution")
                return(s)
        }
        data <- x$get()
        s <- solve(data)
        x$solvmat(s)
        s
}



