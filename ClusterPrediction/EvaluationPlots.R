#x <- read.csv(xf <- file.choose())
x <- read.csv("\\Users\\manab\\Downloads\\Model Evaluation Log - Entry Log (Ren).csv")

str(x)
library(tidyverse)
x %>%
  pivot_longer(cols = c(MSE, MCR, Time..sec.),
               values_to = 'Value', names_to = 'Measures') %>%
  ggplot(aes(x=Features, y=Value))+
  geom_boxplot()+
  facet_wrap(.~Measures, scales = 'free_y')

x %>%
  pivot_longer(cols = c(MSE, MCR, Time..sec.),
               values_to = 'Value', names_to = 'Measures') %>%
  ggplot(aes(x=Methods, y=Value))+
  geom_boxplot()+
  facet_wrap(.~Measures, scales = 'free_y')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

x %>%
  pivot_longer(cols = c(MSE, MCR, Time..sec.),
               values_to = 'Value', names_to = 'Measures') %>%
  ggplot(aes(x=Methods, y=Value, color=Features))+
  geom_boxplot()+
  facet_grid(Measures~Features, scales = "free_y")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
