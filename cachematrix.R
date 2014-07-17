## Put comments here that give an overall description of what your
## functions do
##these functions permit to cache the value of the inverse of a matrix so
##that when we need it again, it can be looked up in the cache rather than 
##recalculated.
##this will be helpful if the inverse of the matrix has to be computed repeatedly
##in a loop. If the matrix doesn't change, the inverse doesn't change...



## Write a short comment describing this function
##The first function, makeCacheMatrix creates a special "vector", which is really 
##a list containing a function to

##set the value of the matrix
##get the value of the matrix
##set the value of the inverse of the matrix
##get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) { i <- NULL
                                             set <- function(y) {
                                               x <<- y
                                               i <<- NULL
                                             }
                                             get <- function() x
                                             setinv <- function(inv) i <<- inv
                                             getinv <- function() i
                                             list(set = set, get = get,
                                                  setinv = setinv,
                                                  getinv = getinv)

}


## Write a short comment describing this function :
## Return a matrix that is the inverse of 'x'
##if this value was already calculated get the cached data without calculation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
