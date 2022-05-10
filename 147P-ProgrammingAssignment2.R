makeCacheMatrix <- function(x = matrix()){
  inv <- NULL ##Initial value of inv has been assigned NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
}

get <- function() x
setInverse <- function(inverse){inv <<- inverse}
getInverse <- function(){inv}
list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...){
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("cached data collected")
    return(inv)
  }
  m <- x$get()
  inv <- solve(m, ...)
  x$setInverse(inv)
  return(inv)
}

