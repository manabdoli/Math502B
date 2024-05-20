# fn <- file.choose()
# x <- read.csv(fn)
x <- read.csv("/Users/mabdoli/Documents/GitHub/Math502B/ClusterPrediction/ResultsTable.csv")

library(tidyverse)
library(patchwork)
fOrder <- c('FULL', 'PCA', 'FFT', 'KEY')
mOrder <- c('LDA', 'QDA', 'GLM', 'NNet', 'CNNet')
x <- x %>% rename(Noise=Noise.Y.N.) %>%
  mutate(Classification=factor(Classification, paste0('Kmean-', fOrder)),
         Features=factor(Features, fOrder),
         Methods=factor(Methods, mOrder)
  )

#############
{
png(filename = 'img/RMSEc2c.png', width = 720, height = 432)
x %>% ggplot(aes(x=Methods, y=RMSE_XY_c2c, color=Noise))+
  geom_boxplot()+
  facet_grid(Classification~Features)+
  theme(axis.text.x = element_text(angle = 45, hjust = .9))
dev.off()

png(filename = 'img/RMSEt2c.png', width = 720, height = 432)
x %>% ggplot(aes(x=Methods, y=RMSE_XY_t2c, color=Noise))+
  geom_boxplot()+
  facet_grid(Classification~Features)+
  theme(axis.text.x = element_text(angle = 45, hjust = .9))
dev.off()

png(filename = 'img/RMSE_Deg_c2c.png', width = 720, height = 432)
x %>% ggplot(aes(x=Methods, y=RMSE_DEG_c2c, color=Noise))+
  geom_boxplot()+
  facet_grid(Classification~Features)+
  theme(axis.text.x = element_text(angle = 45, hjust = .9))
dev.off()

png(filename = 'img/RMSE_Deg_t2c.png', width = 720, height = 432)
x %>% ggplot(aes(x=Methods, y=RMSE_DEG_t2c, color=Noise))+
  geom_boxplot()+
  facet_grid(Classification~Features)+
  theme(axis.text.x = element_text(angle = 45, hjust = .9))
dev.off()

png(filename = 'img/MCR.png', width = 720, height = 432)
x %>% ggplot(aes(x=Methods, y=MCR, color=Noise))+
  geom_boxplot()+
  facet_grid(Classification~Features)+
  theme(axis.text.x = element_text(angle = 45, hjust = .9))
dev.off()

png(filename = 'img/CPUtime.png', width = 720, height = 432)
x %>% ggplot(aes(x=Methods, y=log10(CPUtime), color=Noise))+
  geom_boxplot()+
  facet_grid(Classification~Features)+
  theme(axis.text.x = element_text(angle = 45, hjust = .9))
dev.off()
}
