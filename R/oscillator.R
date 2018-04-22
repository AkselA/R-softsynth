

fs <- 44100
f <- 220
l <- 5

xsin <- seq(0, 2*pi*f*l, length.out=fs*l) %>>%
  (? length(.)) %>>%
  (? summary(.)) %>>%
  (? diff(.) %>>% median) %>>%
  replace(. > mean(.)*0.9, mean(.)*0.9) %>>%
  (~ temp1) %>>%
  # rolliter(40000, 2) %>>%
  # na.fill(temp1) %>>%
  (x ~ signal::filter(cheby1(3, 5, 5/(fs*0.5), "low"), x)) %>>%
  (~ temp1) %>>%
  sin %>>%
  (x ~ signal::filter(cheby2(5, 3, (1.1*f)/(fs*0.5), "low"), x)) %>>%
  jitter(amount=0.2) %>>%
  (x ~ signal::filter(cheby1(3, 50, (6*f)/(fs*0.5), "low"), x)) %>>%
  (x ~ signal::filter(butter(2, (0.2*f)/(fs*0.5), "high"), x)) %>>%
  (x ~ signal::filter(butter(3, (0.5*f)/(fs*0.5), "high"), x)) %>>%
  fit11 %>>%
  (? summary(.)) %>>%
  (~ plot(., type="l"))

wwav(xsin, "t6.wav")

###

