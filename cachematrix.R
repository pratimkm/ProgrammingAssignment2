## Cached Accessor function for a matrix and its inverse

## The following function takes a matrix as argument and returns a
## list of functions set(sets the value of the matrix), get(get the matrix)
## setinverse(sets the inverse to cache) and getinverse(returns from cache)

makeCacheMatrix <- function(x = matrix()) {
	  inverse <- NULL
	  set <- function(y) {
	    x <<- y
	    inverse <<- NULL
	  }
	  get <- function() x
	  setinverse <- function(inv) inverse <<- inv
	  getinverse <- function() inverse
	  list(set = set, get = get,
	       setinverse = setinverse,
       		getinverse = getinverse)
}


## returns an inverse of a matrix
## if already cached return from cache else apply solve function to calculate inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         m <- x$getinverse()
	  if(!is.null(m)) {
	    message("getting cached data")
	    return(m)
	  }
	  data <- x$get()
	  m <- solve(data, ...)
	  x$setinverse(m)
 	  m
}


### Unit Tests
B <- matrix( c(1, 2, 3, 4), 2,2)
invB <- solve(B)
lko <- makeCacheMatrix(B)
z <- cacheSolve(lko)
h <- cacheSolve(lko)

