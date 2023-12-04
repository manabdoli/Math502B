#' fft analysis
rm(list = ls())
library(Math502B)
# 0. Load Data ####
## Load mask2m
if(!grepl(pattern = 'maskPrj$', getwd())) setwd('maskPrj')
if(!exists('mask2m')) load('mask2m.rda')
str(mask2m)
x <- mask2m[304,304,]
plot(x)
plot(fft(x))
grid()
x1 <- Math502B::shiftK(x, length(x)/4)
points(fft(x1), pch=2, col=adjustcolor('blue', .5), cex=.5)

x2 <- Math502B::shiftK(x, length(x)/2)
points(fft(x2), pch=3, col=adjustcolor('red', .5), cex=.5)

x3 <- Math502B::shiftK(x, length(x)/4*3)
points(fft(x3), pch=4, col=adjustcolor('green', .5), cex=.5)

plot(x)
points((x1), pch=2, col=adjustcolor('blue', .5), cex=.5)
points((x2), pch=3, col=adjustcolor('red', .5), cex=.5)
points((x3), pch=4, col=adjustcolor('green', .5), cex=.5)


# Shift ####
{
library(Math502B)
set.seed(123)
x <- rnorm(20)
y <- fft(x); plot(Re(y), Im(y), pch=paste0(0:9), cex=1,
             xlim = c(-1,1)*max(abs(Re(y))),
             col=c(rep('#ff0000aa', 10), rep('#0000ffaa', 10)))
k <- 13
y <- fft(shiftK(x, k)); points(Re(y)+.1, Im(y), pch=paste0(0:9), cex=1,
                                    col=c(rep('#44ff00aa', 10), rep('#00ffffaa', 10)))
}
