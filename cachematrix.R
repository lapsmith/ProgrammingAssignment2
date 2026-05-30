## Put comments here that give an overall description of what your
## functions do
##makeCacheMatrix creates an environment that stores: The original matrix x; A cached inverse slot initialized as a zero matrix (using default values)
##cacheSolve : Retrieves both the cached inverse and original matrix from the environment ; If a valid inverse exists in cache, returns it immediately

## Write a short comment describing this function
## Creates a special "matrix" object that can cache its inverse.
## - x: stores the original matrix
## - inv: stores the cached inverse (initialized as zero matrix)
## Provides functions to set, get inverse and check if in cache
makeCacheMatrix <- function(x = matrix()) {
    env <- new.env()
    assign("x", x, envir = env)
    assign("inv", matrix(nrow = nrow(x), ncol = ncol(x)), envir = env)
    return(env)
}

## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'
## If inverse already calculated (stored in x$inv), retrieve it
## Otherwise, compute using solve() and store in x$inv
cacheSolve <- function(x, ...) {
     inv_matrix <- get("inv", envir = x)
    orig_matrix <- get("x", envir = x)
    if (!is.null(inv_matrix) && !all(is.na(inv_matrix))) {
        return(inv_matrix)
    }
    inv_matrix <- solve(orig_matrix, ...)
    assign("inv", inv_matrix, envir = x)
    return(inv_matrix)
}
