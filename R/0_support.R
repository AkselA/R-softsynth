# Imports
library(rollfun)
library(seewave)
library(pipeR)
library(tuneR)
library(signal)
library(dplyr)


ifft <- function(z) {
	Re(fft(z, inv=TRUE))
}

pol2sq <- function(r, p) {
	
	if (length(r) == 1) {
		r <- rep(r, length(p))
	}

	if (length(p) == 1) {
		p <- rep(p, length(r))
	}
	
	if (!all(dim(r) == dim(p)) | length(r) != length(p)) {
	    stop("r and p must be of same shape")
	}
	
	z <- complex(, r * cos(p), r * sin(p))
	if (is.matrix(r)) {
        z <- matrix(z, nrow(r))
    }
    z
}

all.identical <- function(l) {
    all(sapply(2:length(l), FUN=function(x) identical(l[1], l[x])))
}

na.fill <- function(x, y) {
	if (length(x) != length(y)) {
		stop("x and y must be of equal length")
	}
	x[is.na(x)] <- y[is.na(x)]
	x
}


rwav <- function(...) {
	wave <- readWave(...)
	samp.rate <- wave@samp.rate
	bit <- wave@bit
	out <- wave@left / 2^(bit-1)
	attr(out, "samp.rate") <- samp.rate
	out
}

wwav <- function(object, filename, fs=44100, bit=16, norm=TRUE) {
	object <- unclass(object)
	if (norm) {
		object <- norm11(object)
	}
	maxs <- 2^(bit-1) - 1
	quant <- trunc(object*maxs)
	wave <- Wave(left=quant, right=quant, samp.rate=fs, bit=bit)
	writeWave(wave, filename)
}


abspow <- function(x, p) {
	s <- sign(x)
	a <- abs(x)^p
	a*s
}

sloop <- function(start, stop, l, p=2) {
    if (start < stop) {
	    x <- seq(1, 10, length.out=l)
        fitrange(x^p, start, stop)
	} else {
        x <- seq(10, 1, length.out=l)
        fitrange(x^p, stop, start)
	}
}
