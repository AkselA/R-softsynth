


noise <- function(l, type=c("uniform", "triangle", "binary", 
  "pink", "red", "brownian", "impulse", "pulse"), fs=44100) {
	type <- match.arg(type)
	outp <- switch(type, 
	  uniform  = runif(l, -1, 1),
	  triangle = runif(l, -0.5, 0.5) + runif(l, -0.5, 0.5),
	  binary   = sample(c(-1, 1), l, replace=TRUE),
	  pink     = {
	  	  fr <- 1/sqrt(seq(1, fs/2, length.out=l))
	  	  ph <- runif(l, 10, 1000)
	  	  re <- ifft(pol2sq(fr, ph))
	  	  re/sqrt(l*fs/2)*2600
	  },
	  red      =,
	  brownian = {
	  	  tr <- c((runif(l, -1, 1) * runif(l, -1, 1) * 0.05))
	  	  cs <- cumsum(tr)
	  	  md <- lm(cs ~ seq_along(cs))
	  	  medcentre(resid(md))
	  })
    outp <- as.vector(outp)
	attr(outp, "samp.rate") <- fs
	outp
}

set.seed(1)
noise(80, "bin") %>>% 
  (~ plot(., pch=16, cex=0.6)) %>>% 
  lines(col="#00000066")

