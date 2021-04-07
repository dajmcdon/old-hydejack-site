library(tidyverse)
circol = '#ff4900'
bkg = 'white'
mjcol = '#aed6f1'
micol = '#aed6f1'
polygon = data.frame(x = cospi(seq(0,330,length.out = 12)/180),
                     y = sinpi(seq(0,330,length.out = 12)/180))
d = ggplot(polygon,aes(x,y)) + geom_polygon(fill=NA,color=circol,size=1) +
  theme_void() + theme(panel.background = element_rect(fill=bkg))
d = d + geom_polygon(data=polygon[c(1,4,7,10),],fill=NA,color=mjcol) +
  geom_polygon(data=polygon[c(2,5,8,11),],fill=NA,color=mjcol) +
  geom_polygon(data=polygon[c(3,6,9,12),],fill=NA,color=mjcol) +
  geom_polygon(data=polygon[c(1,5,9),],fill=NA,color=micol) +
  geom_polygon(data=polygon[c(2,6,10),],fill=NA,color=micol) +
  geom_polygon(data=polygon[c(3,7,11),],fill=NA,color=micol) +
  geom_polygon(data=polygon[c(4,8,12),],fill=NA,color=micol) 
## M
mcol = '#0B62a4'
mw = 1
m = (polygon[1,2] - polygon[5,2])/(polygon[1,1]-polygon[5,1])
yend = - m
d = d + geom_path(data=polygon[c(12,3),],color=mcol,size=mw) +
  geom_path(data=polygon[c(8,5),],color=mcol,size=mw) +
  geom_path(data=rbind(polygon[5,],c(0,yend)),color=mcol,size=mw) + 
  geom_path(data=rbind(polygon[3,],c(0,yend)),color=mcol,size=mw)

## D
dcol = '#0B62a4'
dw = 1
v1 = .5/m+1 # x-coordinate
v2x = (1+m) / (m-1/m)
v3y = .5/m + 1
d + geom_path(data=data.frame(x=c(-.5,-.5),y=c(-.5,.5)),colour=dcol,size=dw) +
  geom_path(data=data.frame(x=c(-.5,v1),y=c(.5,.5)),color=dcol,size=dw) +
  geom_path(data=data.frame(x=c(-.5,v1),y=c(-.5,-.5)),color=dcol,size=dw) +
  geom_path(data=data.frame(x=c(v1,v2x),y=c(.5,v2x)),color=dcol,size=dw) +
  geom_path(data=data.frame(x=c(v1,v2x),y=c(-.5,-v2x)),color=dcol,size=dw) + 
  geom_path(data=data.frame(x=c(v2x,.5),y=c(v2x,v3y)),color=dcol,size=dw) +
  geom_path(data=data.frame(x=c(v2x,.5),y=c(-v2x,-v3y)),color=dcol,size=dw) +
  geom_path(data=data.frame(x=c(.5,.5),y=c(v3y,-v3y)),color=dcol,size=dw)

ggsave(file='../assets/icons/icon@3x.png',width=576/300,height=576/300)

## Now, you need to rescale this into 
## icon@2x.png 384x384
## icon.png	192x192
## icon@0,75x.png	144x144
## icon@0,5x.png	96x96
## icon@0,25x.png	48x48
## and favicon.ico (32x32) using some website