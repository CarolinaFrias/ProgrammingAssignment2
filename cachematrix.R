
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()){
  iv <- NULL
  set <- function(matrix){
    x <<- matrix
    iv <<- NULL
  }
  get <- function()m
  setinverse <- function(inverse) iv <<- inverse
  getinverse <- function()iv
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(y, ...){
  iv <- y$getInverse()
  if(!is.null(iv)){
    message("getting cached data")
    return(iv)
  }
  data <- y$get()
  x <- solve(data, ...) %*% data
  y$setInverse(iv)
  iv
}
