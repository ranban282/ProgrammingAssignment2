
## This is a helper function which creates a matrix 'object', and allows 
## the user to get and set its internal matrix, as well as get and set the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
	cachedMatrixInverse <- NULL
	set <- function(y)
	{
		x <<- y
		cachedMatrixInverse <<- NULL
	}		
	get <- function () x
	setInvertedMatrix <- function(invertedMatrix) cachedMatrixInverse <<- invertedMatrix
	getInvertedMatrix <- function() cachedMatrixInverse
	list(set = set, get=get,setInvertedMatrix=setInvertedMatrix,getInvertedMatrix=getInvertedMatrix)
}


## This function calls the above helper function to check 
## if the inverse of the matrix exists. If so, that matrix is returned.
## Otherwise, the inverse of the matrix is computed, x's inverted matrix is set, and the inverted matrix is returned.
cacheSolve <- function(x, ...) {
	invertedMatrix <- x$getInvertedMatrix()
	if(!is.null(invertedMatrix))
	{
		message("Getting cached data.")
		return(invertedMatrix)

	}
	matrixToInvert=x$get()
	invertedMatrix <- solve(matrixToInvert,...)
	x$setInvertedMatrix(invertedMatrix)
	invertedMatrix
}
