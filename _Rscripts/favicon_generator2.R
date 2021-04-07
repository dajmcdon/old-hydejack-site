gr='#00884e'
bl='#084b74'
re='#c63800'
or='#ff9200'

d=data.frame(x=c(-.2,-1.1,.9),y=c(.3,.1,.1),v=c('J','D','M'))
ggplot(d,aes(x,y)) + 
  geom_text(aes(label=v),color=c(or,bl,bl),size=c(45,30,30),family='Palatino')+
  xlim(-2,2) + ylim(-2,2) + coord_fixed() +
  theme_void()
ggsave('~/Documents/Work/Website/academic-jekyll-site/assets/icons/icon@3x.png',
       width = 1.92, height=1.92)  
ggplot(d,aes(x,y)) + 
  geom_text(aes(label=v),color=c(or,bl,bl),size=c(20,40/3,40/3),family='Palatino')+
  xlim(-2,2) + ylim(-2,2) + 
  theme_void()
ggsave('~/Documents/Work/Website/academic-jekyll-site/assets/icons/tile-wide.png',
       width=1.86,height=.9)
