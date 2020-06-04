##Creating a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        
##initialise the matrix
         inv <- NULL
         
##setting the matrix 
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
##getting the matrix
        get <- function() x
##setting the inverse matrix
        setInverse <- function(inverse) inv <<- inverse
##getting the matrix inverse
        getInverse <- function() inv
##list       
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


##Compute the inverse of the special matrix returned by "makeCacheMatrix"

cacheSolve <- function(x, ...) {

## Return a matrix that is the inverse of 'x'
         inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
         
##getting the matrix        
        mat <- x$get()
        
##calculating the inverse       
        inv <- solve(mat, ...)
        
##setting the inverse to the object        
        x$setInverse(inv)
        
#returning the matrix       
        inv
}

