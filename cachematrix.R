## The two functions calculate and store/cache inverses of matrices. 
## Once calculated, the inverse doesn't need to be recalculated, it will be retrieved from memory

## This function creates a list containing four functions to set and get the value of the vector and to set and get the value of the inverse     

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y #sets x to y for the whole function
    m <<- NULL
  }
  get <- function() x #report the value of x in its lexical scope
  setinverse <- function(solve) m <<- solve #works in tandem with the cacheSolve function
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  #creates list of functions defined above
}   


## Returns to inverse of a matrix, if it has already been calculated the inverse is retrieved from the cache and computation is skipped

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached inverse") #returns stored inverse if it has been computed earlier
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...) #calculates inverse if it hasn't been computed earlier
  x$setinverse(m)
  m
}
