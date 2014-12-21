##This pair of  functions will be used to cache the inverse of a matrix rather than compute it repeatedly.

##This function creates a special “matrix’ object that can cache it inverse.

makeCacheMatrix<-function(x=matrix()){
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function() x
    setsolve<-function(solve) m<<-solve
    getsolve<-function()m
    list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
}

##This function computes the inverse of the special matrix returned by makeCacheMatrix
and if the inverse has alreasy been calculated with no changes in the matrix will retrieve
the inverse from the cache.

cacheSolve<-function(x, …) {
    m<-x$getsolve()
    if(!is.null(m)) {
        message(“getting cached data”)
        return(m)
    }
    data<-x$get()
    m<- solve(data,…)
    x$setsolve(m)
    m
}

