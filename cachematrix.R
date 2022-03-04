## The first of these functions create the elements over which the second operates. 
## These elements are "special" type of matrix, that caches its inverse once it has been 
## calculated. 
## Example of how to operate:

## a1 <- c(3, 2, 5)
## a2 <- c(2, 3, 2)
## a3 <- c(5, 2, 4)
## A<-rbind(a1,a2,a3)
## myMatrix<-makeCacheMatrix(A)
## cacheSolve(myMatrix)

######################################################################################
## makeCacheMatrix functions creates a "special"  matrix that caches its inverse. 
## It is, creates a matrix with an embedded "flag" that inform if the inverse 
## has been calculated and provides the value. 
## MATRIX NEEDS TO BE SQUARE!!

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv  <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve requieres as an argument an object of the type "makeCacheMatrix" 
## It checks if the inverse of the matrix has been calculated and decides between two options:
## if the inverse exists it retrieves the stored data, 
## otherwise it calculates the matrix inverse using x$getinv (that calculates and caches the inverse).


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
