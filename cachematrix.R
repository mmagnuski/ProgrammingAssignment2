## The following functions allow to create special
## kinds of matrix objects that cache its inverses
## so that once an inverse has been computed it 
## is not computed again but fetched from memory.
## 
## Example:
## # create a simple matrix
## y <- matrix(1:4, nrow=2, ncol=2)
##
## # now, turn this matrix into a cacheMatrix:
## cm <- makeCacheMatrix(y)
##
## # compute inverse of cm:
## cacheSolve(cm)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## # if we try to compute the inverse of cm again
## # we will see that now it is fetched from memory:
## cacheSolve(cm)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5


## makeCacheMatrix takes in an ordinary matrix and 
## returns a special matrix - one that can cache its
## inverse so it is only computed once (as long as
## the matrix stays the same)

makeCacheMatrix <- function(x = matrix()) {
        # initialize inverse to null
        inv <- NULL

        # create main 'methods' of the cacheMatrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get    <- function() x
        setinv <- function(newinv) inv <<- newinv
        getinv <- function() inv

        # return a list of these 'methods'
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve returs a matrix that is the inverse of 'x'
## 'x' has to be a matrix created with makeCacheMatrix

cacheSolve <- function(x, ...) {

        # test if the inverse has been cached
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }

        # the inverse is not cached, get the
        # data and solve
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
