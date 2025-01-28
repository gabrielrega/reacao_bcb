####################################
###### Curva de Reação BCB #########
####################################

library(forecast)
library(ggplot2)
library(ggthemes)
library(easyGgplot2)
library(xtable)
library(TStools)
library(png)
library(grid)
library(mFilter)
library(dynlm)
library(lmtest)
library(car)
library(xts)
library(stargazer)

data <- ts(read.csv2('data.csv', header=T, sep=';', dec=',')[,-1],
           start=c(2001,11), freq=12)


hp <- hpfilter(data[,4], type='lambda', freq=14400)
hiato <- hp$cycle
data <- ts.intersect(data[,1:3], hiato)
colnames(data) <- c('SELIC', 'EXPINF', 'META', 'HIATO')


img <- readPNG('logo.png')
g <- rasterGrob(img, interpolate=TRUE)


g1 <- autoplot(data[,1])+
  geom_line(colour="darkblue", size=1)+
  xlab('')+ylab('% a.a.')+
  ggtitle('Taxa Selic')+
  theme_economist()

g2 <- autoplot(data[,2])+
  geom_line(colour="red", size=1)+
  xlab('')+ylab('% a.a.')+
  ggtitle('Expectativa da Inflação')+
  theme_economist()

g3 <- autoplot(data[,3])+
  geom_line(colour="darkgreen", size=1)+
  xlab('')+ylab('% a.a.')+
  ggtitle('Meta de Inflação')+
  theme_economist()


g4 <- autoplot(data[,4])+
  geom_line(colour="black", size=1)+
  xlab('')+ylab('índice')+
  ggtitle('Hiato do Produto')+
  theme_economist()

ggplot2.multiplot(g1, g2, g3, g4, cols=2)




DESVIO <- data[,2]-data[,3]
data <- ts.intersect(data[,1], DESVIO, data[,4])
colnames(data) <- c('SELIC', 'DESVIO', 'HIATO')

data <- window(data, start=c(2004,01))

reacao <- dynlm(SELIC~lag(SELIC,-1)+lag(SELIC,-2)+
                  DESVIO+lag(HIATO,-1), data=data)

neutra <- coef(reacao)[1]/(1-coef(reacao)[2]-coef(reacao)[3])

taylor <- coef(reacao)[4]/(1-coef(reacao)[2]-coef(reacao)[3])

stargazer(reacao, type='html', title='Curva de Reação do Banco Central')

modelo <- cbind(data[,1], fitted(reacao))

colnames(modelo) <- c('Selic', 'Modelo')

autoplot(modelo) +
  geom_line(size=0.8) +
  ylab('% a.a.') +
  xlab('') +
  scale_x_discrete(limits=2004:2016) +
  ggtitle('SELIC vs. Modelo') +
  theme_economist(base_size = 11) +
  scale_color_economist(stata=FALSE) +
  labs(colour = "")

autoplot(resid(reacao))+theme_economist()

dwtest(reacao)
