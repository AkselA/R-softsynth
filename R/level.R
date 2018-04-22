fitrange <- function(W, lower=-1, upper=1) {
	if (lower > upper) warning("upper bound must be strictly larger than lower bound")
	if (length(W) == 0) return(numeric(0))
	newrange <- upper - lower
	oldrange <- max(W, na.rm=TRUE) - min(W, na.rm=TRUE)
	if (oldrange == 0) {
		d <- abs(W - lower) < abs(W - upper)
		ifelse(d, lower, upper)
	} else {
	    (W - min(W, na.rm=TRUE)) * (newrange/oldrange) + lower
    }
}

fit01 <- function(W) {
	oldrange <- max(W) - min(W)
	(W - min(W)) / oldrange
}

fit11 <- function(W) {
	oldrange <- max(W) - min(W)
	(W - min(W)) * (2/oldrange) + -1
}

norm11 <- function(x) {
	r <- max(abs(range(x)))
	x / r
}

medcentre <- function(x) {
	x - median(x)
}

medcenter <- medcentre

meancentre <- function(x) {
	x - mean(x)
}

meancenter <- medcentre

comp.v <- function(x, thr=0.8, ratio=1/2) {
	
	lx <- length(x)
	lt <- length(thr)
	lr <- length(ratio)
	
	if (lt == 1 & lr == 1) {
		
        hi <- x >  thr
	    lo <- x < -thr
	    sc <- thr - thr*ratio

	    x[hi] <-  sc + ratio * x[hi]
        x[lo] <- -sc + ratio * x[lo]

	} else {

		if (lt == 1) {
			thr <- rep(thr, lx)
		}
	
		if (lr == 1) {
			ratio <- rep(ratio, lx)
		}
		
		hi <- x >  thr
		lo <- x < -thr
		sc <- thr - thr*ratio
	    
        x[hi] <-  sc[hi] + ratio[hi] * x[hi]
        x[lo] <- -sc[lo] + ratio[lo] * x[lo]
    }
    x
}

fadein <- function(x, length=441) {
	
	lx <- length(x)
	run <- 1:length
	weights <- ((1:length) - 1) / length
	
	x[run] <- x[run]*weights
	x
}

fadeout <- function(x, length=441) {
	
	lx <- length(x)
	run <- (lx - length + 1):lx
	weights <- ((length:1) - 1) / length
	
	x[run] <- x[run]*weights
	x
}

