#ProgrammingAssignment2
#Caching the Inverse of a Matrix

#Right belov after this line you'll find rows for testing:
#Create matrix
#   mtrx <- matrix(c(1,20,51,3,4,15,30,6,0), nrow=3, ncol=3)
#   solv <- makeCacheMatrix(mtrx)
#   cacheSolve(solv)
#Launch again and see that data is take from cache
#   cacheSolve(solv)

makeCacheMatrix <- function(x = matrix()) {
              m<- NULL
              set <- function(y) {
                  x <<- y
                  m <<- NULL
              }
              #get matrix value
              get <- function() x
              setsolve <- function(solve) m <<- solve
              getsolve <- function() m
              #storing functions in makeCacheMatrix
              list(set = set, get = get,
                   setsolve = setsolve,
                   getsolve = getsolve)
}

## Creating matrix which is a list
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x' variable
              m <- x$getsolve()
              if(!is.null(m)) {
                message("getting cached data")
                return(m)
              }
              #geting
              data <- x$get()
              #solving
              m <- solve(data, ...)
              #setting
              x$setsolve(m)
              #returning
              m
}
