## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#In this function we have setter and getter for the matrix, plus setter and getter for cached inverse matrix
#If the matrix is modified, cached inverse matrix will be destroyed

makeCacheMatrix <- function(x = matrix()) {
 	inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
#Return a matrix that is the inverse of 'x', where 'x' must be produced by makeCacheMatrix function
#It verify if there is already an existent Inverse of the current matrix,if this test is negative, it will compute the inversion.
#It returns the inverted matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if(!is.null(inv)){
		message("getting cached inverse")
		return (inv)
	}
	matr <- x$get()
	inv <- solve(matr, ...)
	x$setinverse(inv)
	inv
}
