## These functions you so save some computing cycles by caching the inverse
## of a matrix

## This creates a matrix that can be used to cache the inverse of a given matrix

makeCacheMatrix <- function(x = matrix()) {
	inver<-NULL
	set<-function(y){
		x<<-y
		inverse<<-NULL
}
	get<-function()x
	setInv<-function(inverse) inver<<-inverse
	getInv<-function()inver
	list( set=set,
		get=get,
		setInv=setInv,
		getInv=getInv)
}

## This function computes the inverse of the matrix created in makeCacheMatrix.
## If inverse has been calculated and remained the same, then it will get the inverse form the cache.

cacheSolve <- function(x, ...) {
        inver<-x$getInv()
	if(!is.null(inver)){
		message("getting cached data")
		return(inver)
	}
	matx<-x$get()
	inver<-solve(matx, ...)
	x$setInv(inver)
	inver
}
