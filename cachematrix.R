## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##library(MASS) is used to calculate inverse  for non squared as well as  square matrices.
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL      ##initializing inverse is NULL
    set<-function(y){
        x<<-y
        inv<<-NULL
    }
    get<-function()x        ##function to get matrix x
    setinv<-function(inverse)inv<<-inverse
    getinv<-function(){
        inver<-ginv(x)
        inver%*%x           ##function to obtain inverse of the matrix
    } 
    list(set = set ,get = get,setinv = setinv,getinv = getinv)
}


## Write a short comment describing this function
##this is used to get the cache data
cacheSolve <- function(x, ...) ##get the cache data
    {
    inv<-x$getinv()
    if(!is.null(inv)){         ## checking whether inverse is the null
                    message("getting cached data!")
                    return(inv)    ##return inverse value
    }
    data<-x$get()
    inv<-solve(data,...)       ##calculate inverse value
    x$setinv(inv)
    inv ## Return a matrix that is the inverse of 'x'
}
