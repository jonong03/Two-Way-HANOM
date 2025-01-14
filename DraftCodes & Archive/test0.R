# Task 1: Write out algorithm of HANOM, compare with existing package

pacman::p_load(data.table, dplyr)

## First Stage
s1<- c(95.39, 96.07, 96.58, 95.67, 96.73, 98.07, 95.32, 97.41, 95.81, 97.79)
s2<- c(92.15, 92.16, 94.75, 94.69, 93.53, 94.25, 91.13, 92.09, 96.07, 96.15)
s3<- c(95.15, 90.75, 91.97, 93.39, 86.97, 94.28, 91.14, 91.45, 92.35, 94.92)
s4<- c(95.99, 97.12, 97.07, 96.23, 97.19, 96.68, 95.67, 96.49, 97.52, 95.30)
s<- list(s1,s2,s3,s4)


## Second Stage
ss1<- c(94.98, 95.12, 96.37, 99.03, 95.16, 97.21, 94.45, 95.68, 96.67, 94.27, 95.78)
ss2<- c(96.61, 96.95, 94.39,94.17,95.58,92.34,92.75,93.64,98.64,94.07,94.23,95.68,92.25,94.98,97.71,93.44,96.99,93.01)
ss3<- c(94.21,90.86,93.59,93.62,97.01,91.12,94.72,90.82,91.56,93.47,93.35,92.01,93.22,95.01,94.46,96.27,94.77,95.53,94.07,95.21,95.39,91.68,93.05,94.43,95.09,93.57,95.05,99.39,90.73,94.68,92.25,92.24,92.25,95.55)
ss4<- c(96.50,96.85,97.54,95.56,98.04,97.73,98.00,97.35,97.14,96.77,97.30)
ss<- list(ss1,ss2,ss3,ss4)

dt<- data.table(y= c(s1,s2,s3,s4), solvent = rep(paste0("s", c(1:4)), each= length(s1)))
dt[,.(mean(y), var(y)), by=solvent]


# To specify --------------------------------------------------------------
n0 = 10   #initial sample size
alpha = 0.05   #significance level (for difference)
power = 0.85   #test power
amount = 2.5   #diff size (delta)


# Calculated --------------------------------------------------------------
df = length(n0)-1
I = 4
w = 6 # need to refer to the table (build internally)

ni_f <- function(n0, w, amount, ss) {
  a = n0 + 1
  b = 1 + floor((w/amount)^2 * var(ss))
  max(a, b)
}

# Sample ni from each group, for second set of observations
ni<- mapply(ni_f, n0, w, amount, ss= list(s1,s2,s3,s4))

# Apply ni_f to second set of observations

lapply(list(ss1,ss2,ss3,ss4), mean)
lapply(ss, length)

# Calculate bi

bi_f <- function(ni, n0, amount, w, s1) {
  bi = ((ni[1]- n0)/ni[1])*(1+sqrt(n0/(ni[1]-n0) * (((amount/w)^2)*(ni[1]/var(s1))-1) ))
  return(bi)
}

bi_f <- function(amount, w, s0) {
  n0 = length(s0)
  a = 1 + floor((w/amount)^2 * var(s0))
  ni = max(n0+1, a)
  bi = ((ni- n0)/ni)*(1+sqrt(n0/(ni-n0) * (((amount/w)^2)*(ni/var(s0))-1) ))
  return(bi)
}

bi_f(amount= amount, w= w, s0= s2)
bi<- mapply(bi_f, amount, w, s0= s); bi

ybar_s <- sapply(s,mean)
ybar_ss <- sapply(ss, mean)

f2 <- function(bi, sbar, ssbar) {
  (1-bi)*sbar + bi*ssbar
}
ybar_telda <- mapply(f2, bi, ybar_s, ybar_ss)
mean(ybar_telda)


#Decision Lines

hval = 2.55
dl = c(mean(ybar_telda) - hval*amount/w, mean(ybar_telda) + hval*amount/w)

ybar_ss < dl[2] & ybar_ss > dl[1]




# Assemble ----------------------------------------------------------------

## First Stage
s1<- c(95.39, 96.07, 96.58, 95.67, 96.73, 98.07, 95.32, 97.41, 95.81, 97.79)
s2<- c(92.15, 92.16, 94.75, 94.69, 93.53, 94.25, 91.13, 92.09, 96.07, 96.15)
s3<- c(95.15, 90.75, 91.97, 93.39, 86.97, 94.28, 91.14, 91.45, 92.35, 94.92)
s4<- c(95.99, 97.12, 97.07, 96.23, 97.19, 96.68, 95.67, 96.49, 97.52, 95.30)
s<- list(s1,s2,s3,s4)


## Second Stage
ss1<- c(94.98, 95.12, 96.37, 99.03, 95.16, 97.21, 94.45, 95.68, 96.67, 94.27, 95.78)
ss2<- c(96.61, 96.95, 94.39,94.17,95.58,92.34,92.75,93.64,98.64,94.07,94.23,95.68,92.25,94.98,97.71,93.44,96.99,93.01)
ss3<- c(94.21,90.86,93.59,93.62,97.01,91.12,94.72,90.82,91.56,93.47,93.35,92.01,93.22,95.01,94.46,96.27,94.77,95.53,94.07,95.21,95.39,91.68,93.05,94.43,95.09,93.57,95.05,99.39,90.73,94.68,92.25,92.24,92.25,95.55)
ss4<- c(96.50,96.85,97.54,95.56,98.04,97.73,98.00,97.35,97.14,96.77,97.30)
ss<- list(ss1,ss2,ss3,ss4)

dt<- data.table(y= c(s1,ss1, s2,ss2, s3,ss3, s4,ss4), treatment = rep(paste0("s", c(1:4)), times= c(length(s1)+length(ss1), length(s2)+length(ss2), length(s3)+length(ss3), length(s4)+length(ss4))))

ANOM_f<- function(data, n0=10, alpha=0.05, power = 0.85, amount = 2.5){
  data= dt
  setnames(data, new=c("y","treatment"))
  tr<- data[,2] %>% unique()

  ni_f <- function(n0, w, amount, ss) {
    a = n0 + 1
    b = 1 + floor((w/amount)^2 * var(ss))
    max(a, b)
  }
  bi_f <- function(amount, w, s0) {
    n0 = length(s0)
    a = 1 + floor((w/amount)^2 * var(s0))
    ni = max(n0+1, a)
    bi = ((ni- n0)/ni)*(1+sqrt(n0/(ni-n0) * (((amount/w)^2)*(ni/var(s0))-1) ))
    return(bi)
  }
  
  
  #d1<- data[,.SD[sample(.N, max(2,n0))],by = treatment]
  d1<- data[,.SD[1:max(2,n0)],by = treatment]
  d1l<- split(d1, f=d1$treatment, keep.by=FALSE) %>% lapply(., function(x) x$y)
  
  ni<- mapply(ni_f, n0, w, amount, d1l)
  bi<- mapply(bi_f, amount, w, s0= d1l); bi
  

  d2<- data[.SD[(1+max(2,n0)):n1], by=]
  
  ybar_s <- sapply(s,mean)
  ybar_ss <- sapply(ss, mean)
  
  f2 <- function(bi, sbar, ssbar) {
    (1-bi)*sbar + bi*ssbar
  }
  ybar_telda <- mapply(f2, bi, ybar_s, ybar_ss)
  mean(ybar_telda)
  
  
  
}
?split
