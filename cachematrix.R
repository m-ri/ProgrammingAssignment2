# Matrix inversion is a costly computation(especially for big matricies), so it is desiderable to cache the result, in order to don't perform again the computation on the same matrix

#'makeCacheMatrix' return a list containing functions which:
# -set value of the matrix
# -get the value of the matrix
# -set the computed inverse of matrix
# -get the cached inverse of matrix

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


#'cacheSolve' return a matrix that is the inverse of 'x', where 'x' must be produced by 'makeCacheMatrix' function
#It verify if there is already an existent inverse of the current matrix,if this test is negative, it will compute and cache the inverse.
#It returns the inverted matrix

#Be careful, because is assumed that input matricies are invertible
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if(!is.null(inv)){
		#message("getting cached inverse")
		return (inv)
	}
	matr <- x$get()
	inv <- solve(matr, ...)
	x$setinverse(inv)
	inv
}
#Example:
#remove the comment symbol from the line 35,if you want to see the caching
#
#matr <- matrix(1:4,2,2)
#matr_cached <- makeCacheMatrix(matr)
#> cacheSolve(matr_cached)
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> 
#> cacheSolve(matr_cached)
#getting cached inverse
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
