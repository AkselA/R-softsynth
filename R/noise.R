


noise <- function(l=10, type=c("uniform", "triangle", "binary", 
  "pink", "red", "brownian"), fs=44100) {
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
	  	  ph <- runif(l, 0, pi*4)
	  	  re <- ifft(pol2sq(fr, ph))
	  	  re <- medcentre(re)*pi*2*10000	
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

impulsenoise.v <- function(l, f, type=c("poisson", "sample"), fs=44100) {
	type <- match.arg(type)
	outp <- switch(type, 
	  poisson = {
	  	  s <- fs/rep(f, length.out=l) - sample.int(2, size=l, replace=TRUE) + 1
	  	  s <- c(s, s[l])
	  	  v <- vector()
	  	  while (length(v) < l) {
		      v <- c(v, 1, rep(0, rpois(1, s[length(v)+1])))
	      }
	      v[1:l]
	  },
	  sample  = {
	  	  s <- fs/rep(f, length.out=l) - sample.int(2, size=l, replace=TRUE) + 1
	  	  s <- c(s, s[l])
	      v <- vector()
          while (length(v) < l) {
		      v <- c(v, 1, rep(0, s[length(v)+1]))
          }
	      v[1:l]
	  })
    attr(outp, "samp.rate") <- fs
    outp
}

set.seed(1)
im1 <- rbinom(44100, 3, seq(0.3, 0.9, length.out=44100)) %>>% 
  "+"(1) %>>% 
  "*"(55/2) %>>% 
  (~ plot(., type="o", pch=16, cex=0.2, lwd=0.5, col="#00000066")) %>>%
  (x ~ impulsenoise.v(44100*1, x, type="sample"))

im2 <- rbinom(44100, 2, seq(0.1, 0.9, length.out=44100)) %>>% 
  "+"(1) %>>% 
  "*"(110) %>>% 
  (~ plot(., type="o", pch=16, cex=0.2, lwd=0.5, col="#00000066")) %>>%
  (x ~ impulsenoise.v(44100*1, x, type="sample"))

im3 <- rbinom(44100, 3, seq(0.2, 0.8, length.out=44100)) %>>% 
  "+"(1) %>>% 
  "*"(55) %>>% 
  (~ plot(., type="o", pch=16, cex=0.2, lwd=0.5, col="#00000066")) %>>%
  (x ~ impulsenoise.v(44100*1, x, type="sample"))

im4 <- rbinom(44100, 3, seq(0.1, 0.9, length.out=44100)) %>>% 
  "+"(1) %>>% 
  "*"(110) %>>% 
  (~ plot(., type="o", pch=16, cex=0.2, lwd=0.5, col="#00000066")) %>>%
  (x ~ impulsenoise.v(44100*1, x, type="sample"))

imA <- c(im1, im2, im3, im4)
imB <- c(im1, im2, im3, rev(im4))

im <- c(imA, imB)
im <- chebylp(ema.v(im), 880*12, r=10)

plot(im, type="l")
wwav(rep(im, 5), "im.wav")


