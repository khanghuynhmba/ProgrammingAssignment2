## The first function will cache the matrix while the second function calulate the matrix inverse.

##The first function makeCacheMatrix will create a special matrix
##object that can cache its inverse, the reference is behind the makeVector function
##except that the dimension here is matrix()

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


##The second function will calculate the inverse of that matrix
##The reference is from the cachemean function, now with the solve() function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_mtrx <- x$get_inv_mtrx()
        if (!is.null(inv_mtrx)) {
                message("getting cached data")
                return(inv_mtrx)
        }
        data <- x$get()
        inv_mtrx <- solve(data, ...)
        x$set_inv_mtrx(inv_mtrx)
        inv_mtrx
}
