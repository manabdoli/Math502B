#x <- read.csv(xf <- file.choose())
#x <- read.csv("\\Users\\manab\\Downloads\\Model Evaluation Log - Entry Log (Ren).csv")

#xf <- file.choose()
#xf <- "C:\\SharedDocs\\GitHub\\Math502B\\ClusterPrediction\\AllTestResults.mat"
#x <- R.matlab::readMat(xf)

library(R.matlab)

x0 <-  read.csv("C:\\SharedDocs\\GitHub\\Math502B\\ClusterPrediction\\TestResults_NoiseNon.csv")
x45 <-  read.csv("C:\\SharedDocs\\GitHub\\Math502B\\ClusterPrediction\\TestResults_Noise45.csv")
x90 <-  read.csv("C:\\SharedDocs\\GitHub\\Math502B\\ClusterPrediction\\TestResults_Noise90.csv")
x180 <-  read.csv("C:\\SharedDocs\\GitHub\\Math502B\\ClusterPrediction\\TestResults_Noise180.csv")

x <- rbind(
  cbind(OrientationNoise='None', x0),
  cbind(OrientationNoise='45Deg', x45),
  cbind(OrientationNoise='90Deg', x90),
  cbind(OrientationNoise='180Deg', x180))

rm('x0', 'x45', 'x90', 'x180')

library(tidyverse)
library(patchwork)
fOrder <- c('FULL', 'PCA', 'FFT', 'KEY')
mOrder <- c('LDA', 'QDA', 'GLM', 'NNet', 'CNNet')
oOrder <- c('None', '45Deg', '90Deg', '180Deg')
x <- x %>% rename(Noise=Noise.Y.N.) %>%
  mutate(Classification=factor(Classification, paste0('Kmean-', fOrder)),
         Features=factor(Features, fOrder),
         Methods=factor(Methods, mOrder),
         OrientationNoise=factor(OrientationNoise, oOrder)
  )

#############
{
  png(filename = 'img/RMSEc2c_dtl.png', width = 900, height = 500)
  x %>% ggplot(aes(x=Methods, y=RMSE_XY_c2c, color=OrientationNoise))+
    geom_boxplot()+
    facet_grid(Classification~Features)+
    theme(axis.text.x = element_text(angle = 45, hjust = .9))
  dev.off()

  png(filename = 'img/RMSEc2c.png', width = 900, height = 500)
  x %>% ggplot(aes(x=Methods, y=RMSE_XY_c2c, color=Noise))+
    geom_boxplot()+
    facet_grid(Classification~Features)+
    theme(axis.text.x = element_text(angle = 45, hjust = .9))
  dev.off()

  png(filename = 'img/RMSEt2c.png', width = 900, height = 500)
  x %>% ggplot(aes(x=Methods, y=RMSE_XY_t2c, color=Noise))+
    geom_boxplot()+
    facet_grid(Classification~Features)+
    theme(axis.text.x = element_text(angle = 45, hjust = .9))
  dev.off()

  png(filename = 'img/RMSE_Deg_c2c.png', width = 900, height = 500)
  x %>% ggplot(aes(x=Methods, y=RMSE_DEG_c2c, color=Noise))+
    geom_boxplot()+
    facet_grid(Classification~Features)+
    theme(axis.text.x = element_text(angle = 45, hjust = .9))
  dev.off()

  png(filename = 'img/RMSE_Deg_t2c.png', width = 900, height = 500)
  x %>% ggplot(aes(x=Methods, y=RMSE_DEG_t2c, color=Noise))+
    geom_boxplot()+
    facet_grid(Classification~Features)+
    theme(axis.text.x = element_text(angle = 45, hjust = .9))
  dev.off()

  png(filename = 'img/MCR.png', width = 900, height = 500)
  x %>% ggplot(aes(x=Methods, y=MCR, color=Noise))+
    geom_boxplot()+
    facet_grid(Classification~Features)+
    theme(axis.text.x = element_text(angle = 45, hjust = .9))
  dev.off()

  png(filename = 'img/CPUtime.png', width = 900, height = 500)
  x %>% ggplot(aes(x=Methods, y=log10(CPUtime), color=Noise))+
    geom_boxplot()+
    facet_grid(Classification~Features)+
    theme(axis.text.x = element_text(angle = 45, hjust = .9))
  dev.off()
}
