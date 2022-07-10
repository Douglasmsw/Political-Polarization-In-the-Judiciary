library(ggplot2)
library(grid)
library(gridExtra)

###############################################################################
##FIGURE 1
###############################################################################
v.dem <- c(rnorm(100000,-1,.5))
v.rep <- c(rnorm(100000,1,.5))
v.atty <- c(rnorm(90000,-1,.5),rnorm(30000,1,.5))

gg <- data.frame(rbind(cbind(v.dem,'Democrats'),
                       cbind(v.rep,'Republicans'),
                       cbind(v.atty,'Attorneys')))
colnames(gg) <- c('x1','type')
gg$type <- factor(gg$type)
gg$x1 <- as.numeric(as.character(gg$x1))
p <- ggplot(data=gg) + geom_density(aes(x = x1, color=type,fill=type),adjust =2, alpha=.2) + xlim(-2.5,2.5)
p <- p + theme_bw()
p <- p + scale_color_manual('',values=c('Democrats' = 'blue',
                                   'Republicans' = 'red',
                                   'Attorneys' = 'black'))
p <- p + scale_fill_manual('',values=c('Democrats' = 'blue',
                                   'Republicans' = 'red',
                                   'Attorneys' = 'black'))
p <- p + xlab('Conservatism')
p <- p + theme(legend.position=c(.1,.75))

pdf(file='figures/figure_1_stylized_distribution.pdf',height=4, width =9)
print(p)
dev.off()



###############################################################################
##FIGURE 1 V2
###############################################################################
v.dem <- c(rnorm(100000,-1,.5))
v.rep <- c(rnorm(100000,1,.5))
v.atty <- c(rnorm(90000,-1,.5),rnorm(30000,1,.5))

gg <- data.frame(rbind(cbind(v.dem,'Democrats'),
                       cbind(v.rep,'Republicans'),
                       cbind(v.atty,'Attorneys')))
colnames(gg) <- c('x1','type')
gg$type <- factor(gg$type)
gg$x1 <- as.numeric(as.character(gg$x1))
p <- ggplot(data=gg) + geom_density(aes(x = x1, color=type,fill=type,linetype=type,alpha=type),adjust =2 ) + xlim(-2.5,2.5)
p <- p + theme_bw()

p <- p + scale_color_manual('',values=c('Republicans' = 'darkgrey','Democrats' = 'black','Attorneys'='black'))
p <- p + scale_fill_manual('',values=c('Republicans' = 'darkgrey','Democrats' = 'black','Attorneys'='black'))
p <- p + scale_linetype_manual('',values=c('Republicans' = 1,'Attorneys' = 2,'Democrats'=1))
p <- p + scale_alpha_manual('',values=c('Democrats' = .2,'Republicans' = .2,'Attorneys'=.4))
p <- p + xlab('Conservatism')
p <- p + theme(legend.position=c(.1,.75))


pdf(file='figures/figure_1_stylized_distribution_bw.pdf',height=4, width =9)
print(p)
dev.off()



###############################################################################
##FIGURE 2 v2
###############################################################################
v.dem <- c(rnorm(100000,-1,.5))
v.rep <- c(rnorm(100000,1,.5))
v.pol <- c(rnorm(100000,-1,.5),rnorm(100000,1,.5))
v.atty <- c(rnorm(100000,-1,.5),rnorm(round(100000/3),1,.5))
d.pol <- density(c(rnorm(100000,-1,.5),rnorm(100000,1,.5)),from=-2.5,to=2.5,n=512)
d.atty <- density(c(rnorm(100000,-1,.5),rnorm(round(100000/3),1,.5)),from=-2.5,to=2.5,n=512)



gg <- data.frame(rbind(cbind(v.dem,'Democrats'),
                       cbind(v.rep,'Republicans'),
                       cbind(v.atty,'Attorneys'),
                       cbind(v.atty,'Judges'))
                 )
colnames(gg) <- c('x1','type')
gg$type <- factor(gg$type)
gg$x1 <- as.numeric(as.character(gg$x1))
p <- ggplot(data=gg) + geom_density(aes(x = x1, color=type,fill=type,alpha=type,linetype=type),adjust=2)
p <- p + theme_bw()

p <- p + scale_color_manual('',values=c('Democrats' = 'blue',
                                   'Republicans' = 'red',
                                   'Attorneys' = 'black',
                                   'Judges'='black'
                                   ))
p <- p + scale_fill_manual('',values=c('Democrats' = 'blue',
                                  'Republicans' = 'red',
                                  'Attorneys' = 'darkgrey',
                                  'Judges'='black'
                                  ))



p <- p + scale_linetype_manual('',values=c('Democrats' = 1, 'Republicans'=1,'Attorneys' = 2,'Judges'=1))
p <- p + scale_alpha_manual('',values=c('Democrats' = 0.2,'Republicans' = 0.2,'Attorneys' = 0,'Judges'=.7))
p <- p + theme(legend.position='none')
p <- p + ggtitle(bquote(omega == .(0)))
p <- p + xlab(' ')
p1 <- p


gg <- data.frame(rbind(cbind(v.dem,'Democrats'),
                       cbind(v.rep,'Republicans'),
                       cbind(v.atty,'Attorneys'),
                       cbind(c(v.atty,v.pol),'Judges'))
                 )
colnames(gg) <- c('x1','type')
gg$type <- factor(gg$type)
gg$x1 <- as.numeric(as.character(gg$x1))
p <- ggplot(data=gg) + geom_density(aes(x = x1, color=type,fill=type,alpha=type,linetype=type),adjust=2)
p <- p + theme_bw()

p <- p + scale_color_manual('',values=c('Democrats' = 'blue',
                                   'Republicans' = 'red',
                                   'Attorneys' = 'black',
                                   'Judges'='black'
                                   ))
p <- p + scale_fill_manual('',values=c('Democrats' = 'blue',
                                  'Republicans' = 'red',
                                  'Attorneys' = 'darkgrey',
                                  'Judges'='black'
                                  ))


p <- p + scale_linetype_manual('',values=c('Democrats' = 1, 'Republicans'=1,'Attorneys' = 2,'Judges'=1))
p <- p + scale_alpha_manual('',values=c('Democrats' = 0.2,'Republicans' = 0.2,'Attorneys' = 0,'Judges'=.7))
p <- p + theme(legend.position='none')
p <- p + ggtitle(bquote(omega == .(0)))
p <- p + theme(legend.position='none')
p <- p + ggtitle(bquote(omega == .(.5)))
p <- p + xlab('Conservatism')+ ylab('')


p2 <- p
gg <- data.frame(rbind(cbind(v.dem,'Democrats'),
                       cbind(v.rep,'Republicans'),
                       cbind(v.atty,'Attorneys'),
                       cbind(c(v.pol),'Judges'))
                 )
colnames(gg) <- c('x1','type')
gg$type <- factor(gg$type)
gg$x1 <- as.numeric(as.character(gg$x1))
p <- ggplot(data=gg) + geom_density(aes(x = x1, color=type,fill=type,alpha=type,linetype=type),adjust =2)
p <- p + theme_bw()

p <- p + scale_color_manual('',values=c('Democrats' = 'blue',
                                   'Republicans' = 'red',
                                   'Attorneys' = 'black',
                                   'Judges'='black'
                                   ))
p <- p + scale_fill_manual('',values=c('Democrats' = 'blue',
                                  'Republicans' = 'red',
                                  'Attorneys' = 'darkgrey',
                                  'Judges'='black'
                                  ))

p <- p + scale_linetype_manual('',values=c('Democrats' = 1, 'Republicans'=1,'Attorneys' = 2,'Judges'=1))
p <- p + scale_alpha_manual('',values=c('Democrats' = .2,'Republicans' = .2,'Attorneys' = 0,'Judges'=.7))
p <- p + ggtitle(bquote(omega == .(1)))
p <- p + xlab(' ') + ylab('')

p3 <- p


g_legend<-function(p){
    tmp <- ggplotGrob(p)
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
}

legend <- g_legend(p3)
lwidth <- sum(legend$width)

pdf(file='figures/figure_2_varying_omega_values.pdf',height=3, width =12)
p4 <- grid.arrange(arrangeGrob(p1 + theme(legend.position="none"),
                               p2 + theme(legend.position="none"),
                               p3 + theme(legend.position="none"),
                               left = "",nrow=1), legend, 
                   widths=unit.c(unit(1, "npc") - lwidth, lwidth), nrow=1)
dev.off()



###############################################################################
##FIGURE 2- V2
###############################################################################
v.dem <- c(rnorm(100000,-1,.5))
v.rep <- c(rnorm(100000,1,.5))
v.pol <- c(rnorm(100000,-1,.5),rnorm(100000,1,.5))
v.atty <- c(rnorm(100000,-1,.5),rnorm(round(100000/3),1,.5))
d.pol <- density(c(rnorm(100000,-1,.5),rnorm(100000,1,.5)),from=-2.5,to=2.5,n=512)
d.atty <- density(c(rnorm(100000,-1,.5),rnorm(round(100000/3),1,.5)),from=-2.5,to=2.5,n=512)



gg <- data.frame(rbind(cbind(v.dem,'Democrats'),
                       cbind(v.rep,'Republicans'),
                       cbind(v.atty,'Attorneys'),
                       cbind(v.atty,'Judges'))
                 )
colnames(gg) <- c('x1','type')
gg$type <- factor(gg$type)
gg$x1 <- as.numeric(as.character(gg$x1))
p <- ggplot(data=gg) + geom_density(aes(x = x1, color=type,fill=type,alpha=type,linetype=type),adjust=2)
p <- p + theme_bw()

p <- p + scale_color_manual('',values=c('Democrats' = 'darkgrey',
                                   'Republicans' = 'black',
                                   'Attorneys' = 'black',
                                   'Judges'='black'
                                   ))
p <- p + scale_fill_manual('',values=c('Democrats' = 'darkgrey',
                                  'Republicans' = 'black',
                                  'Attorneys' = 'black',
                                  'Judges'='black'
                                  ))
p <- p + scale_linetype_manual('',values=c('Republicans' = 1,'Attorneys' = 2,'Democrats'=1,'Judges'=1))
p <- p + scale_alpha_manual('',values=c('Democrats' = .2,'Republicans' = .2,'Attorneys'=0,'Judges'=.4))
p <- p + theme(legend.position='none')
p <- p + ggtitle(bquote(omega == .(0)))
p <- p + xlab(' ')
p1 <- p


gg <- data.frame(rbind(cbind(v.dem,'Democrats'),
                       cbind(v.rep,'Republicans'),
                       cbind(v.atty,'Attorneys'),
                       cbind(c(v.atty,v.pol),'Judges'))
                 )
colnames(gg) <- c('x1','type')
gg$type <- factor(gg$type)
gg$x1 <- as.numeric(as.character(gg$x1))
p <- ggplot(data=gg) + geom_density(aes(x = x1, color=type,fill=type,alpha=type,linetype=type),adjust=2)
p <- p + theme_bw()


p <- p + scale_color_manual('',values=c('Democrats' = 'darkgrey',
                                   'Republicans' = 'black',
                                   'Attorneys' = 'black',
                                   'Judges'='black'
                                   ))
p <- p + scale_fill_manual('',values=c('Democrats' = 'darkgrey',
                                  'Republicans' = 'black',
                                  'Attorneys' = 'black',
                                  'Judges'='black'
                                  ))
p <- p + scale_linetype_manual('',values=c('Republicans' = 1,'Attorneys' = 2,'Democrats'=1,'Judges'=1))
p <- p + scale_alpha_manual('',values=c('Democrats' = .2,'Republicans' = .2,'Attorneys'=0,'Judges'=.4))


p <- p + theme(legend.position='none')
p <- p + ggtitle(bquote(omega == .(.5)))
p <- p + xlab('Conservatism')+ ylab('')
p2 <- p

gg <- data.frame(rbind(cbind(v.dem,'Democrats'),
                       cbind(v.rep,'Republicans'),
                       cbind(v.atty,'Attorneys'),
                       cbind(c(v.pol),'Judges'))
                 )
colnames(gg) <- c('x1','type')
gg$type <- factor(gg$type)
gg$x1 <- as.numeric(as.character(gg$x1))
p <- ggplot(data=gg) + geom_density(aes(x = x1, color=type,fill=type,alpha=type,linetype=type),adjust =2)
p <- p + theme_bw()

p <- p + scale_color_manual('',values=c('Democrats' = 'darkgrey',
                                   'Republicans' = 'black',
                                   'Attorneys' = 'black',
                                   'Judges'='black'
                                   ))
p <- p + scale_fill_manual('',values=c('Democrats' = 'darkgrey',
                                  'Republicans' = 'black',
                                  'Attorneys' = 'black',
                                  'Judges'='black'
                                  ))
p <- p + scale_linetype_manual('',values=c('Republicans' = 1,'Attorneys' = 2,'Democrats'=1,'Judges'=1))
p <- p + scale_alpha_manual('',values=c('Democrats' = .2,'Republicans' = .2,'Attorneys'=0,'Judges'=.4))
p <- p + ggtitle(bquote(omega == .(1)))
p <- p + xlab('') + ylab('')
p3 <- p


g_legend<-function(p){
    tmp <- ggplotGrob(p)
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
}

legend <- g_legend(p3)
lwidth <- sum(legend$width)


pdf(file='figures/figure_2_varying_omega_values_bw.pdf',height=3, width =12)
p4 <- grid.arrange(arrangeGrob(p1 + theme(legend.position="none"),
                               p2 + theme(legend.position="none"),
                               p3 + theme(legend.position="none"),
                               left = "",nrow=1), legend, 
                   widths=unit.c(unit(1, "npc") - lwidth, lwidth), nrow=1)
dev.off()
