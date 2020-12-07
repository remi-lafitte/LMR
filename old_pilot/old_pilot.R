library(here)
library(tidyverse)
here()  

d<-read.csv(here::here("data", "papa.csv"), header=T)
d<-d[-c(1:14),] %>% 
  select(offset, jitter_x_norm, resp.keys) %>%
  #90 trials
  mutate(bias = ifelse(resp.keys == "right",1,0))
ggplot(d)+
  aes(x = offset, y = bias)+
  geom_jitter(size = 2, alpha = 0.5,
              height=0.02)+
  scale_x_continuous(limits=c(-8,8),breaks = seq(-8,8,1))+
  geom_smooth(method = "glm", method.args = list(family = "binomial"))+
  geom_hline(yintercept = 0.50, lty = "dashed", col = "red")+
  geom_vline(xintercept = 0, lty = "dashed", col = "red")+
  labs(y="Prop. of right responses", x= "Offset (mm)")+
  theme_bw(base_size = 20)
