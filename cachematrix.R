## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## used to calculate the inverse of matrices

makeCacheMatrix <- function(x = matrix()) {
  s<-NULL          # initializing inverse as NULL
  set<-function(y){
    x<<-y
    s<<-NULL
  }
  get<-function()x #function to get matrix x
  setinver <- function(inverse) s<<-inverse
  getinver<-function(){
    inv<-ginv(x)
    inv%*%x }    #function to obtain inverse of the matrix
  list(set=set, get=get, setinver=setinver,getinver=getinver)
}




## Write a short comment describing this function
## It is used to get the cache data
cacheSolve <- function(x, ...) ## gets the cache data
{
  ## Return a matrix that is the inverse of 'x'
  s<- x$getinver()
  if(!is.null(s)){
    #checking whether inverse is NULL
    message("getting cached data!")
    return(s)  #returns inverse value
  }
  data <- x$get()
  s <- solve(data,...)  #calculates inverse value
  X$setinver(s)
  s   ## return a matrix that is inverse of x
}
