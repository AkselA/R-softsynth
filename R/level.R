fitrange <- function(W, lower=-1, upper=1) {
	if (length(W) == 0) {
		W <- NA
	}
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
	if (length(W) == 0) {
		W <- NA
	}
	oldrange <- max(W) - min(W)
	(W - min(W)) / oldrange
}

fit11 <- function(W) {
	if (length(W) == 0) {
		W <- NA
	}
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

fadeinl <- function(x, length=441) {
	
	lx <- length(x)
	run <- 1:length
	weights <- ((1:length) - 1) / length
	
	x[run] <- x[run]*weights
	x
}

fadeoutl <- function(x, length=441) {
	
	lx <- length(x)
	run <- (lx - length + 1):lx
	weights <- ((length:1) - 1) / length
	
	x[run] <- x[run]*weights
	x
}

fade <- function(x, len=441, shape=3, lg=100, len2=len, shape2=shape, lg2=lg) {
	
	if (len+len2 > length(x)) {
		len <- len2 <- floor(length(x)/2)
		warning("fade length longer than input vector")
	}
	
	if (len2 == len & shape2 == shape & lg2 == lg) {
		asym <- FALSE
	} else {
		asym <- TRUE
	}

	slope <- dbeta(seq(0, 1, length.out=len*2), shape, shape)
	slope <- log(slope[1:len] + lg)
	slope <- fit01(slope)	
    slope <- slope[!is.na(slope)]

    if (asym) {
		slope2 <- dbeta(seq(0, 1, length.out=len2*2), shape2, shape2)
		slope2 <- log(slope2[(len2+1):length(slope2)] + lg2)
		slope2 <- fit01(slope2)
        slope2 <- slope2[!is.na(slope2)]
    } else {
    	slope2 <- rev(slope)
    }
    
    l1 <- length(x) - length(slope) - length(slope2)
    ones <- rep(1, l1)
    svec <- c(slope, ones, slope2)
    
    x * svec
}

fadein <- function(x, len=441, shape=3, lg=100) {
	fade(x=x, len=len, shape=shape, len2=1)
}

fadeout <- function(x, len=441, shape=3, lg=100) {
	fade(x=x, len2=len, shape2=shape, lg2=lg, len=1)
}

dcrem <- function(x, extra=TRUE, span=1) {
	if (extra) {
	    mod <- loess(x ~ seq_along(x), span=span)
	    x <- medcentre(resid(mod))
    }
    
    ft <- fft(x)
    ft[c(1, length(ft))] <- 0
    
    if (extra) {
        ft[c(2, length(ft) - 1)] <- 0.5 * ft[c(2, length(ft) - 1)]
    }
    
    ifft(ft)/length(ft)
}
