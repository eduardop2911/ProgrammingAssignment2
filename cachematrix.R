# Assignment 2 : Inverse of a matrix

# First Function

makeMatrix <- function(x = matrix()){
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function(){x}
        setinverse <- function(inverse) {inv <<- inverse}
        getinverse <- function(){inv}
        list (set = set, get = get,
              setinverse = setinverse, 
              getinverse = getinverse)
}


# Second function

cacheMatrix <- function(x, ...){
        inv <- x$getinverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        matr <- x$get()
        inv <- solve(matr, ...)
        x$setinverse(inv)
        inv 
}