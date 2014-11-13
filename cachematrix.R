## The code written in this file creates a matrix wrapper 
## which holds a cached version of the matrix inverse

## The makeCacheMatrix is a function which is practically a list
## that holds a matrix inside, a few operators on that matrix, 
## and the matrix inverse (when it is calculated first)

makeCacheMatrix <- function(x = matrix()) {
	## the inverse cache
	inv <- NULL

	## matrix value setting
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	
	## matrix value getting
	get <- function(){
		x
	}
	
	## setting the matrix inverse
	setInverse <- function(newInv){
		inv <<- newInv
	}

	## getting the matrix inverse
	getInverse <- function(){
		inv
	}
	
	list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)


} 


## This method gets a matrix wrapper as an argument and gets the matrix inverse.
## If the inverse was not calculated before, the method calculates it, stores it inside the wrapper
## and returns the inverse.

cacheSolve <- function(x, ...) {
	i <- x$getInverse()
	if(!is.null(i)){
		return(i)
	}
	
	i <- solve(x$get())
	x$setInverse(i)
	i}
