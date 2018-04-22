# Slew-rate limiter
srlimit <- function(x, r=1) {
	lx <- length(x)
	lr <- length(r)
	
	if (lr != 1 & lr != lx) {
		stop("r must be of length 1 or of length equal to length(x)")
	}
	
    y <- vector(length=lx)
    y[1] <- x[1]
    
    if (lr == 1) {
        for (i in 2:length(x)) {
		    d <- y[i - 1] - x[i]
		    if (abs(d) <= r) {
		        y[i] <- x[i]
		    } else {
			    y[i] <- y[i - 1] - (sign(d)*r)
		    }
        }
    } else {
        for (i in 2:length(x)) {
        	ri <- r[i]
		    d <- y[i - 1] - x[i]
		    if (abs(d) <= ri) {
		        y[i] <- x[i]
		    } else {
			    y[i] <- y[i - 1] - (sign(d)*ri)
		    }
        }
    }
    y
}
