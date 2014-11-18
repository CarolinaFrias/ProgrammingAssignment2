## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(m = matrix()){
        im <- NULL
        set <- function(matrix){
                m <<- matrix
                im <<- NULL
        }
        get <- function()m
        setinverse <- function(inverse) im <<- inverse
        getinverse <- function()im
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



## Write a short comment describing this function

cacheSolve <- function(y, ...){
        iy <- y$getInverse()
        if(!is.null(iy)){
                message("getting cached data")
                return(iy)
        }
        data <- y$get()
        x <- solve(data, ...) %*% data
        y$setInverse(iy)
        iy
}
