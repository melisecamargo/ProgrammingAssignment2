# Matrix inversion is usually a costly computation.
# Caching the inverse of a matrix rather than compute it repeatedly may be of some benefit.
# The following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(mat = matrix()) {
    cached_inverse <- NULL

    # set the value of the matrix
    set <- function(new_mat) {
        if (!identical(new_mat, mat)) { # verifies whether the matrix has been changed
            mat <<- new_mat
            cached_inverse <<- NULL
        }
    }

    # get the value of the matrix
    get <- function() {
        mat
    }

    # set the value of inverse of the matrix
    setinverse <- function(inverse) {
        cached_inverse <<- inverse
    }

    #  get the value of inverse of the matrix
    getinverse <- function() {
        cached_inverse
    }

    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# The following function returns the inverse of the matrix.
# However, it first checks if the inverse has already been computed.

# This function assumes that the matrix is always invertible.

cacheSolve <- function(cache_matrix, ...) {
    inverse <- cache_matrix$getinverse()

    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }

    data <- cache_matrix$get()
    inverse <- solve(data, ...)
    cache_matrix$setinverse(inverse)
    inverse
}
