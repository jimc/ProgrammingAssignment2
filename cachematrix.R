## overall description: 

# computing a matrix inverse can be expensive, keeping a copy of the inverse is
# possible, but fiddly.  These 2 functions work together to handle all the
# fiddling.

## requirements
# not a universal cache, not transparent
# user makes a diferent kind of matrix, using a different costructor
# inverse may not ever be needed, be lazy
# cache must invalidate when matrix is changed.

## api description:

# makeCacheMatrix( matrix ) returns an "object" which wraps/hides a matrix, 
# providing instead a vector of 4 accessors & mutators; get retrieves the
# matrix, set replaces the current value (and clears any cached inverse of the
# previous value).  The 4 methods are closures on (x, inv) variables


## design

# code uses <<- in the setter methods (set,setinv) to update x, inv symbols in 
# the outer scope, ie not in the scope of the methods themselves, but that of 
# makeCacheMatrix.  Since that environment are the same variables that the getter functions
# close over, the getters share them, and thus make them available to the user.

# this code is basically identical to cacheVectorMean code
# but with var-name changes (with requisite break-fix)

makeCacheMatrix <- function(mtx = matrix()) {
        inv <- NULL
        set <- function(nv) {
                mtx <<- nv     # save
                inv <<- NULL   # drop now-invalid cache
        }
        get <- function()
                mtx
        setinv <- function(nval)
                inv <<- nval
        getinv <- function()
                inv
        list(
                set = set, get = get,
                setinv = setinv,
                getinv = getinv
        )
}


## cacheSolve( CMO ) 
# expects a Cached-Matrix-Object, as returned from 
# makeCacheMatrix(), and either computes, remembers and returns the matrix 
# inverse, or it retrieves the value previously computed - obviously doing 
# latter 1st.  since the setter clears the stored inverse 1st, no stale results
# are possible.

cacheSolve <- function(cmo_, ..., msg = "") {
        ## Return a matrix that is the inverse of 'x'
        # accept a msg arg to add to the message() calls
        
        mat <- cmo_$getinv()
        if (!is.null(mat)) {
                message("getting cached ", msg)
                return(mat)
        }
        
        message("computing ", msg)
        data <- cmo_$get()
        library(MASS)
        mat <- ginv(data, ...)
        cmo_$setinv(mat)
        mat
}
