
directsearch <- function(x,y) {
  maxvalue = -Inf;
  for (meanlog in seq(0,5,0.01)) {
    for (sdlog in seq(0.05,5,0.01)) {
      L =   sum(log(dlnorm(y,meanlog,sdlog))) + sum(log(1.0-plnorm(x,meanlog,sdlog)))
      if (maxvalue < L) {
        maxvalue = L
        theta = c(meanlog,sdlog)
      }
    }
  }
  return(theta)
}

#TODO: log(F(t_i + a) - F(t_i - a)) í stað log(f(t_i)) í log-sennileikafallinu þar sem a er 0.5 eða önnur tala sem er háð því hvernig er klippt niður í daga. Ég var t.d. að spá hvað t_i = 1 þýðir? Þýðir það í raun að t_i sé á bilinu [0, 1.5]? Og þýðir t_i = 2 að t_i sé á bilinu [1.5, 2.5]?    
fitlognormal <- function(df, thestate) {
  df <- filter(df, state == thestate)
  x <- filter(df, censored == TRUE)$state_duration
  y <- filter(df, censored == FALSE)$state_duration
  print(x)
  print(y)
  theta = directsearch(x,y)
  h <- hist(df$state_duration, breaks=seq(0,21,1),plot=FALSE)
  plot(h, col="grey") #plot hist
  xlines <-seq(min(h$breaks),max(h$breaks),length.out=100) #seq of x for pdf
  lines(x = xlines,y=dlnorm(xlines,theta[1],theta[2])*length(x)*diff(h$breaks)[1])
  return(theta)
}

impute_lognormal <- function(df, thestate, theta) {
  idx <- which(df$state == thestate & df$censored == TRUE)
  for (i in idx) {
    z <- ceiling(rlnorm(1000,theta[1],theta[2]));
    sdur <- df$state_duration[i]
    z <- z[(z > sdur) & (z < 28)]
    df$state_duration[i] = z[1]
  }
  return(df)
}

impute_empirical <- function(df, thestate) {
  idx <- which(df$state == thestate & df$censored == TRUE)
  duration_completed <- filter(df, state == thestate & censored == FALSE)$state_duration
  for (i in idx) {
    duration = df$state_duration[i] + 1 # default the next day 
    for (j in c(1:100)) { # you get 100 tries before we give up to find a greater value!
      imputed_duration <- sample(duration_completed,1)
      if (imputed_duration >= duration) {
         df$state_duration[i] = imputed_duration;
         break
      }
    }
  }
  return(df)
}

  