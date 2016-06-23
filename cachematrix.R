## Functions create a cache object matrix and calculate the invers of the matrix. 
## If the calculation exists in the cache object then the cached object is retrived
## if the cache is empty the inverse is calculated and stored in the cache object.
## It is important to initialise the cache object first by calling the makeCacheMatrix
## as otherwise error will be produced by cacheSolve fn.

##########example of the call of the function #################
###                                                         ###        
###    set.seed(100)                                        ###
###    mat <- matrix(rnorm(16),4,4)                         ###
###    mat1 <- makeCacheMatrix(mat)                         ###
###    cacheSolve(mat1)                                     ###        
###                                                         ###
### --------------------------------------------------------###
###                     Expected results                    ###
###                                                         ###
###              [,1]       [,2]       [,3]       [,4]      ###
###   [1,]  0.4586793 -0.1390702  1.9595383  1.5824339      ###
###   [2,] -0.4043093  0.2443747 -2.2364384 -0.4642357      ###
###   [3,] -1.4039377 -0.1709849 -1.4833501 -0.9017054      ###
###   [4,] -0.5903023  1.1879533 -0.1067059 -0.5199898      ###
###                                                         ###         
###                                                         ###
###############################################################

## builds a cache object.

makeCacheMatrix <- function(x=matrix()){
        inv<-NULL
        set<-function(y){
                x<<-y
                inv<<-NULL
        }
        get <- function() x
        setinvers <- function(solve) inv <<- solve
        getinvers <- function() inv
        list(set=set, get=get, setinvers=setinvers, getinvers=getinvers)
}


## retrives cached values or calculates the invers and stores it in the cache object

cacheSolve <- function(x,...){
        inv <- x$getinvers()
        if(!is.null(inv))
        {
                message("getting cached inverse")
                return(inv)
        }
        data <-x$get()
        inv <-solve(data,...)
        x$setinvers(inv)
        inv;
}
