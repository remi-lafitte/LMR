library(here)
library(tidyverse)
library(broom)
result = list() 
for(i in list.files(path = here::here("data"))) {
d<-read.csv(here::here("data", i), header=T)
d$trials.thisN<-as.character(d$trials.thisN)
d<-d[is.na(d$trials.thisN) ==F,] %>%
  select(offset, resp.keys) %>%
  #90 trials
  mutate(bias = ifelse(resp.keys == "e",0,1))

mod<-glm(bias ~ offset, family = binomial(link = "logit"), data = d)
sum<-summary(mod)
coeff<-t(as.matrix(sum$coefficients[, "Estimate"]))
colnames(coeff)<-c("intercept", "offset")
coeff<-as.data.frame(coeff)
pse <- -coeff$intercept/coeff$offset
slope <- coeff$offset
jnd <- log(3/slope)


p<-ggplot(d)+
  aes(x = offset, y = bias)+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0.50, lty = "dashed", col = "blue", size=1)+
  geom_vline(xintercept = pse, lty = "dashed", col = "blue", size=1)+
  scale_x_continuous(limits=c(-10.5,10.5),breaks = seq(-10,10,1))+
  geom_smooth(method = "glm", method.args = list(family = "binomial"),
              se = F, size=2)+
  geom_jitter(size = 3, alpha = 0.7, width = 0.2,height=0.02, col = "black",fill="red",
              stroke = 1.2, shape = 21)+
  geom_point(aes(x=pse, y=0.5), fill="red", size=5, col = "black", shape=24,
             stroke = 1.5)+
  scale_y_continuous(limits=c(-0.1,1.1),breaks = seq(0,1,0.5),
                     labels = c("0%\nreponse gauche",
                                paste0("50%\nvotre milieu\nsubjectif est\ndecalee de\n",
                                       round(pse,1), "mm"), 
                                       "100%\nreponse droite"))+
  labs(y="% de reponse 'ligne coupee a droite'", x= "Decalage de la ligne verticale \npar rapport au milieu (mm)")+
  theme_bw(base_size = 16) 

name_subject<-substr(i, start = 1, stop = 8)
mat<-as.matrix(c(pse, slope, jnd)) %>% t
colnames(mat)<-c("pse", "slope", "jnd")
mat<-as.data.frame(mat)
result[[i]]<- mat

png(filename = paste0("plot",name_subject,".png"))
print({
p
})
dev.off()
}

df<-bind_rows(result)
mean(df$pse)
t.test(pse ~ 1, df)
