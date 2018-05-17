chebylp <- function(x, f=880*2, r=15, fs=44100, gain=2, dirty=FALSE) {
	corr <- -0.0025 + 1.152708 + 3.448012e-04*sqrt(f) +
            -6.281090e-06*f + -1.602532e-10*f^2
    fcorr <- corr*f
    ch1 <- cheby1(3, r, fcorr/(fs*0.5), "low")
    ch2 <- cheby1(3, 1.2*r, 0.99*fcorr/(fs*0.5), "low")
    ch3 <- cheby1(3, 0.8*r, 1.02*fcorr/(fs*0.5), "low")
    
    t1 <- signal::filter(ch1, x)
    t1 <- t1 + (abs(min(t1)) - max(t1))/2
    t1 <- abspow(t1, max(1, 0.4+(sqrt(gain)/2))) * gain
    t1 <- t1 - median(t1)
    if (dirty) {
        t1[abs(t1) > 0.5] <- t1[abs(t1) > 0.5]*0.99
        t1[abs(t1) > 0.6] <- t1[abs(t1) > 0.6]*0.98
        t1[abs(t1) > 0.8] <- t1[abs(t1) > 0.8]*0.97
    } else {
        t1 <- comp.v(t1, 0.6, 0.9)
        t1 <- comp.v(t1, 0.7, 0.9)
    }
    t1 <- t1 + x*gain*0.2
    t1 <- comp.v(t1, 0.9, 0.9)
    t1 <- comp.v(t1, 1.5, 0.6)
    t1 <- comp.v(t1, 2.0, 0.5)
# plot(t1, type="l"); abline(h=0, col="red")
# summary(t1)
    t2 <- signal::filter(ch1, t1)
    t2 <- t2 * sqrt(gain)
    if (dirty) {
    	t2 <- 0.5*t2 + 0.5*gain*(t2^3)
        t2[abs(t2) > 0.6] <- t2[abs(t2) > 0.7]*0.99
        t2[abs(t2) > 0.8] <- t2[abs(t2) > 0.8]*min(c((0.99 + (0.01/gain)), 1))
        t2[abs(t2) > 0.9] <- t2[abs(t2) > 0.9]*min(c((0.98 + (0.02/gain)), 1))
    } else {
        t2 <- comp.v(t2, 0.7, 0.9)
        t2 <- comp.v(t2, 0.8, 0.9)
    }
    t2 <- t2 + t1*gain*0.1
# plot(t2, type="l"); abline(h=0, col="red")
# summary(t2)    
    t3 <- signal::filter(ch2, t2)
    t3 <- t3 - median(t3)
    t3 <- t3 + t2*gain*0.1
    if (dirty) {
        t3[abs(t3) > 0.5] <- t3[abs(t3) > 0.5]*0.9
        t3[abs(t3) > 0.7] <- t3[abs(t3) > 0.7]*0.9
        t3[abs(t3) > 0.8] <- t3[abs(t3) > 0.8]*0.9
    } else {
        t3 <- comp.v(t3, 0.7, 0.9)
        t3 <- comp.v(t3, 0.8, 0.9)
    }
    t3 <- comp.v(t3, 0.9, 0.8)
    t3 <- comp.v(t3, 1.0, 0.6)
# plot(t3, type="l"); abline(h=0, col="red")
# summary(t3)
    t4 <- signal::filter(ch3, t3)
    t4 <- t4 + c(t2[c(-1, -2)], 0, 0)*0.05
# plot(t4, type="l"); abline(h=0, col="red")
# summary(t4)
    t4
}


chebyhp <- function(x, f=220*2, r=15, fs=44100, gain=2, dirty=TRUE) {
    corr <- 0.8728129 + -2.074105e-03*log(f) + 
            4.175484e-06*f + 1.365524e-10*f^2 
    fcorr <- corr*f
    ch1 <- cheby1(3, r, fcorr/(fs*0.5), "high")
    ch2 <- cheby1(3, 0.6*r, 0.99*fcorr/(fs*0.5), "high")
    ch3 <- cheby1(3, 1.1*r, 1.02*fcorr/(fs*0.5), "high")
    
    t1 <- signal::filter(ch1, x)
    t1 <- t1 + (abs(min(t1)) - max(t1))/2
    t1 <- abspow(t1*gain, max(1.1, 0.8+(sqrt(gain)/3))) * gain * 2 
    if (dirty) {
    	t1 <- t1 - median(t1)
    	t1 <- t1*2*gain
        t1[abs(t1) > 0.11] <- t1[abs(t1) > 0.11]*0.90
        t1[abs(t1) > 0.20] <- t1[abs(t1) > 0.20]*0.90
        t1[abs(t1) > 0.31] <- t1[abs(t1) > 0.31]*0.90
        t1[abs(t1) > 0.40] <- t1[abs(t1) > 0.40]*0.90
        t1[abs(t1) > 0.51] <- t1[abs(t1) > 0.51]*0.90
        t1[abs(t1) > 0.60] <- t1[abs(t1) > 0.60]*0.91
        t1[abs(t1) > 0.71] <- t1[abs(t1) > 0.71]*0.91
        t1[abs(t1) > 0.80] <- t1[abs(t1) > 0.80]*0.91
        t1[abs(t1) > 0.91] <- t1[abs(t1) > 0.91]*0.92
        t1[abs(t1) > 0.93] <- t1[abs(t1) > 0.93]*0.92
        t1[abs(t1) > 0.95] <- t1[abs(t1) > 0.95]*0.93
        t1[abs(t1) > 0.97] <- t1[abs(t1) > 0.97]*0.93
        t1[abs(t1) > 0.99] <- t1[abs(t1) > 0.99]*0.95
        t1[abs(t1) > 1.00] <- t1[abs(t1) > 1.00]*0.95
        t1[abs(t1) > 1.10] <- t1[abs(t1) > 1.10]*0.95
    	t1 <- comp.v(t1, 1.5, 0.5)
    	t1 <- comp.v(t1, 1.6, 0.4)
    } else {
    	t1 <- comp.v(t1, 0.5, 0.9)
    	t1 <- comp.v(t1, 0.6, 0.9)
    	t1 <- comp.v(t1, 0.8, 0.8)
    	t1 <- comp.v(t1, 1.0, 0.6)
    }
    t1 <- comp.v(t1, 0.8, 0.8)
    t1 <- comp.v(t1, 1.5, 0.6)
    t1 <- t1 + x*gain*0.1
    t1 <- t1 - median(t1)
# plot(t1, type="l"); abline(h=0, col="red")
# summary(t1)
    t2 <- signal::filter(ch2, t1)
    t2 <- t2 * sqrt(gain) * 2
    if (dirty) {
        t2[abs(t2) > 0.7] <- t2[abs(t2) > 0.7]*0.95
        t2[abs(t2) > 0.8] <- t2[abs(t2) > 0.8]*min(c((0.95 + (0.05/gain)), 1))
        t2[abs(t2) > 0.9] <- t2[abs(t2) > 0.9]*min(c((0.95 + (0.05/gain)), 1))
    	t2 <- comp.v(t2, 1.5, 0.6)
    	t2 <- comp.v(t2, 1.7, 0.4)
    } else {
        t2[abs(t2) > 0.5] <- t2[abs(t2) > 0.5]*0.95
    	t2 <- comp.v(t2, 0.4, 0.9)
    	t2 <- comp.v(t2, 0.6, 0.9)
    }
    t2 <- t2 + t1*gain*0.15
    t2 <- comp.v(t2, 0.9, 0.9)
    t2 <- comp.v(t2, 1.0, 0.8)
    t2 <- comp.v(t2, 1.5, 0.6)
# plot(t2, type="l"); abline(h=0, col="red")
# summary(t2)    
    t3 <- signal::filter(ch3, t2)
    t3 <- t3 * sqrt(gain)
    if (dirty) {
        t3[abs(t3) > 0.7] <- t3[abs(t3) > 0.7]*0.9
        t3[abs(t3) > 0.8] <- t3[abs(t3) > 0.8]*0.9
        t3[abs(t3) > 0.9] <- t3[abs(t3) > 0.9]*0.9
    } else {
        t3 <- comp.v(t3, 0.7, 0.9)
        t3 <- comp.v(t3, 0.8, 0.8)
    }
    t3 <- comp.v(t3, 0.9, 0.8)
    t3 <- comp.v(t3, 1.0, 0.6)
    t3 <- 0.8*t3 + 0.2*t1
# plot(t3, type="l"); abline(h=0, col="red")
# summary(t3)
    t3
}


ema.v <- function (x, a=2) {
	b <- a
	b[a >  0.5] <- 1 /    a[a >  0.5]
	b[a <= 0.5] <- 1 / (1-a[a <= 0.5])
	a <- b

    x <- c(0, x)
    
    if (length(a) == length(x) - 1) {
    	a <- c(a[1], a)
        for (i in 2:length(x)) {
            x[i] <- (1 - a[i]) * x[i - 1] + a[i] * x[i]
        }
    } else {
        for (i in 2:length(x)) {
            x[i] <- (1 - a) * x[i - 1] + a * x[i]
        }
    }
    x[-1]
}


# ww <- rwav("~/Desktop/mer.wav")

# x <- ww
# # set.seed(1)
# # x <- runif(44100*10, -1, 1)
# # x <- c(rep(0, 2), rep(c(1, rep(0, 30)), 50))
# s1 <- seq(pi*1.5, pi*2*10 + pi*1.5, length.out=length(x)) %>>% 
  # sin %>>% 
  # "+"(1) %>>% 
  # "*"(3) %>>% 
  # "+"(0)
# s2 <- seq(pi*1.5, pi*2*10*4 + pi*1.5, length.out=length(x)) %>>% 
  # sin %>>% 
  # "+"(1) %>>% 
  # "*"(5) %>>% 
  # "+"(0)
# s <- (s1+s2+1) %>>% "^"(2) %>>% fitrange(1, 100) %>>% (? summary(.))
# plot(s, type="l")

# xe <- ema.v(x, s) %>>% fadein(441) %>>% fadeout(441)
# wwav(xe, "xe.wav", norm=TRUE)
# plot(xe, type="l")

