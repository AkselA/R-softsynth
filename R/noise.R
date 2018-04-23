


noise <- function(l=10, type=c("uniform", "triangle", "binary", 
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
	  	  re <- medcentre(re)*2600
	  	  re/sqrt(l*fs/2)
	  },
	  red      = {
	  	  fr <- 1/seq(1, fs/2, length.out=l)
	  	  ph <- runif(l, 10, 1000)
	  	  re <- ifft(pol2sq(fr, ph))
	  	  re <- medcentre(re)*20000	
	  	  re/sqrt(l*fs/2)  	  
	  },
	  brownian = {
	  	  tr <- c((runif(l, -1, 1) * runif(l, -1, 1) * 0.05))
	  	  cs <- cumsum(tr)
	  	  md <- lm(cs ~ seq_along(cs))
	  	  meancentre(resid(md)) * 10
	  })
    outp <- as.vector(outp)
	attr(outp, "samp.rate") <- fs
	outp
}






