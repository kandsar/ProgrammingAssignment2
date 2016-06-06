## Here, we are creating functions within functions to execute 4 primary tasks,
## namely,
## 1. Create a matrix/ Set the values of a matrix
## 2. Retrieve the matrix/ Get the values of the matrix
## 3. Create inverse of the matrix
## 4. Retrieve the value of inverse of the matrix/ Get the values of the inverse 
##    matrix

makeCacheMatrix <- function(x = matrix()) {
		invers <- NULL
		set <- function(y){
			x <<- y
			invers <<- NULL
		}
		get <- function() x
		setInverse <- function(inverse)   invers <<- inverse
		getInverse <- function() invers
		list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Here in 2nd function "invers" is the variable in which the inverse of the matrix is stored. So, we
## first check if "invers" is empty and hence accordingly print the output showing if ## the inverse matrix is cached.


cacheSolve <- function(x, ...) {
		invers <- x$getInverse()
		if(!is.null(invers)){
			message("getting cached data")
			return(invers)
		}
		outmat <- x$get()
		invers <- solve(outmat,...)
		x$setInverse(invers)
		invers
}
