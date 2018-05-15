## to create a matrix, setand get its value and set and get its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## To calculate and cache the inverted matrix

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m  ## Return a matrix that is the inverse of 'x'
}

##makeCacheMatrix will take an input matrix, cacheSolve will evaluate
##the inverse. If a matrix entered is already in cache memory then its 
##cached inverse will be returned along with the "getting cached data" message
##otherwise inverse will be evaluated by cacheSolve and set as inverse and
##returned by getinverse function
