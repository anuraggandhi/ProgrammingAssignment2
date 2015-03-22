## cachematrix.R
## R Programming, Assignment 2


## The below function allow the result of a potentially costly
##matrix inversion to be cached
##so that the next time the inverse of the matrix is required,
##it returns the precomputed value which saves time

## Create matrix cache. Returns a list of function to get/set the 
## matrix and get/set the cached result of a computation on the
##matrix.  An object for use with the cacheSolve function will be 
## created

createCacheMatrix <- function (x=matrix()){
	resultcache <- NULL  # cached value of the computed result

## set function to set the source matrix, which invalidates the
##result. 

set <- function (y) {
	x<<-y
	resultcache <<- NULL
}

#get function to return the source matrix
get <- function(){
	x
}

#setresult function to set the result cache

setresult <- function (result){
	resultcache <<-result
}

#getresult function to get the cached result
getresult <- function (){
	resultcache
}

##package the function as a list. We name the elements so we can 
##easily call these function by name like x$getmean rather than
##x[[1]]

list (set=set,get=get,setresult=setresult,getresult=getresult)
}

## cacheSolve
## returns the inverse of a matrix x, fetching it from the cache
## if we have already computed it, or computing it if we haven't
## already done this

cacheSolve <- function (x,...){
##return a matrix that is the inverse of the matrix mat
## used to create 'x'

#access the cached result
result <- x$getresult()

#if the cached result is not NULL, return it
if(!is.null(result)){
	message("returning cached result")
	return(result)
}

#if the cache was NULL, we need to calculate and cache the matrix
#before returning it

mat <- x$get()              #fetch the original matrix
invmat <- solve(mat,...)    #invert it
x$setresult(invmat)         #cache the result for next time
invmat						#return the inverse
}
