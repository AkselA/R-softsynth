fitrange <- function(W, lower=-1, upper=1) {
	if (lower > upper) warning("upper bound must be strictly larger than lower bound")
	if (length(W) == 0 | all(is.na(W))) {
		return(numeric(0))
	}
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
	if (length(W) == 0 | all(is.na(W))) {
		return(numeric(0))
	}
	oldrange <- max(W) - min(W)
	if (oldrange == 0) {
		d <- abs(W) < abs(W - 1)
		ifelse(d, 0, 1)
	} else {
		(W - min(W)) / oldrange
	}
}

fit11 <- function(W) {
	if (length(W) == 0 | all(is.na(W))) {
		return(numeric(0))
	}
	oldrange <- max(W) - min(W)
	if (oldrange == 0) {
		d <- abs(W - -1) < abs(W - 1)
		ifelse(d, -1, 1)
	} else {
		(W - min(W)) * (2/oldrange) + -1
	}
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

    if (asym) {
		slope2 <- dbeta(seq(0, 1, length.out=len2*2), shape2, shape2)
		slope2 <- log(slope2[(len2+1):length(slope2)] + lg2)
		slope2 <- fit01(slope2)
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

tidyends <- function(x, len=441, shape=1.8, lg=0.05, len2=len, shape2=shape, lg2=lg) {

	if (len+len2 > length(x)) {
		len <- len2 <- floor(length(x)/2)
		warning("fade length longer than input vector")
	}

    l1 <- abs(x[1])
    s1 <- sign(x[1])
	slope <- dbeta(seq(0, 1, length.out=len*2), shape, shape)
	slope <- log(slope[1:len] + lg)
	slope <- fitrange(slope, 0, l1) - l1
	slope <- slope * s1	
    slope <- slope[!is.na(slope)]

    l2 <- abs(x[length(x)])
    s2 <- sign(x[length(x)])
	slope2 <- dbeta(seq(0, 1, length.out=len2*2), shape2, shape2)
	slope2 <- log(slope2[(len2+1):(len2*2)] + lg2)
	slope2 <- fitrange(slope2, 0, l2) - l2
	slope2 <- slope2 * s2
    slope2 <- slope2[!is.na(slope2)]
    
    lz <- length(x) - length(slope) - length(slope2)
    zeroes <- rep(0, lz)
    svec <- c(slope, zeroes, slope2)
    
    x + svec
}

tidyin <- function(x, len=441, shape=1.8, lg=0.05) {
	tidyends(x=x, len=len, shape=shape, len2=0)
}

tidyout <- function(x, len=441, shape=1.8, lg=0.05) {
	tidyends(x=x, len2=len, shape2=shape, lg2=lg, len=0)
}


loopfix <- function(x, len=441, shape=1.8, lg=100, len2=len, shape2=shape, lg2=lg, vk=0.5) {

    lx <- length(x)
    xA <- x[1]
    xZ <- x[lx]
    
	if (len+len2 > length(x)) {
		len <- len2 <- floor(length(x)/2)
		warning("fade length longer than input vector")
	}
	
	step0 <- fadein(x-xA, shape=1.8, len=len/1.5, lg=10)+xA
	step0 <- fadeout(step0-xZ, shape=1.8, len=len/1.5, lg=10)+xZ
	
	x <- step0

    l1 <- xA - (xA*vk + xZ*(1-vk))
    s1 <- sign(l1)
    l1 <- abs(l1)

	slope <- dbeta(seq(0, 1, length.out=len*2), shape, shape)
	slope <- log(slope[1:len] + lg)
	slope <- fitrange(slope, 0, l1) - l1
    slope <- slope * s1
    slope <- slope[!is.na(slope)]

    
    l2 <- xZ - (xA*(1-vk) + xZ*vk)
    s2 <- sign(l2)
    l2 <- abs(l2)

	slope2 <- dbeta(seq(0, 1, length.out=len2*2), shape2, shape2)
	slope2 <- log(slope2[(len2+1):(len2*2)] + lg2)
	slope2 <- fitrange(slope2, 0, l2) - l2
	slope2 <- slope2 * s2
    slope2 <- slope2[!is.na(slope2)]
    
    lz <- lx - length(slope) - length(slope2)
    zeroes <- rep(0, lz)
    svec <- c(slope, zeroes, slope2)
    
    step1 <- x + svec
    
    flip <- step1[lx:1]
    flipw <- fade(rep(-0.5, lx), len=len/2, shape=3, lg=100) + 0.5
    flipw <- flipw*0.8
    
    step2 <- step1*(1-flipw) + flip*flipw
    
    step2


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

