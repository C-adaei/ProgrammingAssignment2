## makeCacheMatrix stores a matrix, x, in the cache and creates a list of functions 
## that return and manipulate a this matrix and its inverted matrix, that is also stored
## in the cache.
## cacheSolve checks if there is an inverted matrix stored in the cache and returns it. 
## if there is not, it calculates the inverted matrix, cache it and return the inverted matrix

## This function is based on the example given to us. It receives a matrix and
## generates a list containing 4 functions. the first function ia a set function 
## that reassign the matrix received. The second function is a get function that 
## returns the matrix received. The third function, setsolution, sets the inverted
## matrix. The fourth function, getsolution, returns the inverted matrix



makeCacheMatrix <- function(x = matrix()) {
  IM <- NULL
  set <- function(y) {
    x <<- y
    IM <<- NULL
  }
  get <- function() x
  setsolution <- function(Inverted_Matrix) IM <<- Inverted_Matrix
  getsolution <- function() IM
  list(set = set, get = get,
       setsolution = setsolution,
       getsolution = getsolution)
}


## This function is also based on the example given to us. It receives a list and
## returns the inverted matrix inside the list. If there is no inverted matrix inside
## the list it calculates the inverted matrix, store it on the list and return it.

cacheSolve <- function(x, ...) {
  IM <- x$getsolution()
  if(!is.null(IM)) {
    return(IM)
  }
  data <- x$get()
  IM <- solve(data, ...)
  x$setsolution(IM)
  IM
        
}
