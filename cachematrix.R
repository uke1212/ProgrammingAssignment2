## With this example I want to write two functions, the first one, 
##  makeCacheMatrix, is a function which create a "special" matrix and cahe's
##  its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
                }
        get <- function() x
        setinversa <- function(inverse) inv <<- inverse
        getinversa <- function() inv
        list (set = set, get = get, setinversa = setinversa 
              , getinversa = getinversa)
        
        }


## The secon one, casheSolve, calculates the inverse matrix of the special
## matrix, but 
##      if the inverse exist, it gets the inverse from the chache 
##      and skips the computation
##      if the inverse has not been calculated it calculate the inverse and set 
##      it in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinversa()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinversa(inv)
        inv
        }


pippo <- matrix(c(2,3,4,3,5,8,4,9,5),3,3)
pippo
pippo1 <- makeCacheMatrix(pippo)
pippo1$get()
pippo1$getinversa() #inverse is NULL
cacheSolve(pippo1)
cacheSolve(pippo1)  #now the inverse exist, so getting cached data
pippo1$getinversa() #inverse is not NULL
