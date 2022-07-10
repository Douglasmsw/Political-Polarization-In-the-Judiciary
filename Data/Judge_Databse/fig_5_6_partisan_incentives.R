###############################################################################
##DENSITY OVERLAP BY STATE
###############################################################################
library(ggplot2)
library(grid)
library(overlap)
library(gridExtra)
library(gridBase)
library(ggplot2)
library(moments)
library(Matrix)
library(xtable)

source('src/func_sc.R')

##LOADING DATA
load("data/dime_mh_merged.Rdata")
load('data/jud_selection_data.Rdata')
m <- match(lawyers.all$dime.cid,Xfm$dime.cid,incomparables=c(NA,''))
m1 <- match(lawyers.all$isln,Xfm$isln,incomparables=c(NA,''))
m[is.na(m)] <- m1[is.na(m)]
lawyers.all$judge.hierarchy[!is.na(m) & lawyers.all$judge.hierarchy ==0 ] <- 1

load('data/dime_cands.Rdata')
cands <- cbind(cycle=cands[,'fecyear'],cands)


lawyers.all$is.judge <- lawyers.all$judge.hierarchy
lawyers.all$is.judge[is.na(lawyers.all$is.judge)] <- ''
state.subset <- c('US','ST',"AK", "AL", "AR","DC",
                  "AZ", "CA", "CO",
                  "CT", "DE", "FL", "GA","HI", "IA",
                  "ID", "IL", "IN", "KS", "KY", "LA",
                  "MA", "MD", "ME", "MI", "MN", "MO",
                  "MS", "MT", "NC", "ND", "NE", "NH",
                  "NJ", "NM", "NV", "NY", "OH", "OK",
                  "OR", "PA", "RI", "SC", "SD", "TN",
                  "TX", "UT", "VA", "VT", "WA", "WI",
                  "WV", "WY")
use <- lawyers.all$is.judge == 0
gg <- data.frame(ip=as.numeric(lawyers.all[use,'cfscore']),
                 st = as.factor(lawyers.all[use,'state']),
                 is.judge= 'lawyer',
                 jtype = 'lawyer',
                 bid = paste(as.numeric(lawyers.all[use,'dime.cid']),lawyers.all[use,'state'],sep='_'),
                 lyear= lawyers.all$lyear[use]
                 )

lawyers.all$jtype <- ifelse(lawyers.all$judge.hierarchy == 4,'U.S. Circuit Courts',
                            ifelse(lawyers.all$judge.hierarchy == 3,'U.S. District Courts',
                                   ifelse(lawyers.all$judge.hierarchy == 2,'State High Courts',
                                          ifelse(lawyers.all$judge.hierarchy == 1,'State Lower Courts','lawyer'))))

use <- lawyers.all$is.judge >= 1
gg2 <- data.frame(ip=as.numeric(lawyers.all[use,'cfscore']),
                  st = as.factor(lawyers.all[use,'state']),
                  is.judge= 'judge',
                  jtype = lawyers.all$jtype[use],
                  bid = paste(as.numeric(lawyers.all[use,'dime.cid']),lawyers.all[use,'state'],sep='_'),
                  lyear= lawyers.all$lyear[use]
                  )
gg2$st <- as.character(gg2$st)
gg2[which(gg2$jtype %in% c('U.S. Circuit Courts','U.S. District Courts')),'st'] <- 'US'
gg <- rbind(gg,gg2)

##WINDSORIZING
gg$ip[gg$ip > 2] <- 2
gg$ip[gg$ip < -2] <- -2
gg <- gg[gg$st %in% state.subset,]
gg <- gg[complete.cases(gg),]
gg <- gg[which(gg$lyear >=5 | gg$is.judge == 'judge'),]

###############################################################################
##CALCULATE DENSITIES
###############################################################################
out <- get.density.plots(state.subset[!(state.subset %in% c('DC'))],weighted.pols=FALSE)

###############################################################################
##OUTPUT PARTISAN OVERLAP SUMMARY TABLE
###############################################################################
tab.out <- out$rval[,c('V1',"judge.to.atty.ks.pval","judge.to.atty.overlap.Dhat1","judge.to.cand.ks.pval","judge.to.cand.overlap.Dhat1")]
oov <- as.matrix(tab.out[,-1])
mode(oov) <- 'numeric'
tab.out[,-1] <- round(oov,2)
rownames(tab.out) <- tab.out[,1]
tab.out <- tab.out[,-1]
colnames(tab.out) <- c('KS P-value','Overlap Coef',
                       'KS P-value','Overlap Coef')

out1 <- xtable(tab.out)
print(out1,type='latex',file='tables/table_A2.tex',size='scriptsize')

###############################################################################
##PLOT MEANS BY STATE
###############################################################################
wm.all <- out$wm.all
rownames(wm.all) <- out$wm.all[,1]
rownames(wm.all)[rownames(wm.all) == 'US'] <- 'Fed. Courts'
rownames(wm.all)[rownames(wm.all) == 'ST'] <- 'State Courts'
nn <- state.name[match(rownames(wm.all),state.abb)]
rownames(wm.all)[!is.na(nn)] <- nn[!is.na(nn)]
wm.all <- wm.all[,-1]
mode(wm.all) <- 'numeric'
wm.use <- wm.all[(wm.all[,1] < wm.all[,3] & wm.all[,3] < wm.all[,2]) | (wm.all[,1] > wm.all[,3] & wm.all[,3] > wm.all[,2]),]


out$rval[,1] <- as.character(out$rval[,1])
out$rval[(out$rval[,1]) == 'US',1] <- 'Fed. Courts'
out$rval[(out$rval[,1]) == 'ST',1] <- 'State Courts'
nn <- state.name[match((out$rval[,1]),state.abb)]
out$rval[!is.na(nn),1] <- nn[!is.na(nn)]

gpp <- rbind(cbind(rownames(wm.all),mip.a=wm.all[,2],mip.p=wm.all[,1],mip.j=wm.all[,3],type='atty'))

gpp <- as.data.frame(gpp)
colnames(gpp) <- c('st','mip.a','mip.p','mip.j','type')
gpp$mip.a<- as.numeric(as.character(gpp$mip.a))
gpp$mip.p<- as.numeric(as.character(gpp$mip.p))
gpp$mip.j<- as.numeric(as.character(gpp$mip.j))

gpp$mip.a[gpp$st == 'State Courts'] <- gpp$mip.a[gpp$st == 'State Courts'] -0.001

mz <- match(gpp$st,(out$rval[,1]))
gpp$ksv.cand <- ifelse(as.numeric(as.character(out$rval$judge.to.cand.ks.pval[mz])) < .05,'sig','nsig')
gpp$ksv <- ifelse(as.numeric(as.character(out$rval$judge.to.atty.ks.pval[mz])) < .05,'sig','nsig')
gpp$ksv2 <- ifelse(as.numeric(as.character(out$rval$judge.to.atty.ks.pval2[mz])) < .05,'sig','nsig')

gpp$ksv = ifelse(gpp$ksv =='sig' & gpp$ksv2 =='sig','sig','nsig')
gpp$pval <- ifelse(as.numeric(as.character(out$rval$judge.to.atty.t.test)) < .05,'sig','nsig')
gpp$st <- reorder(gpp$st,gpp$mip.a)
gpp$sig.type <-ifelse(((gpp$mip.j < gpp$mip.a & gpp$mip.j < gpp$mip.p) |
                       (gpp$mip.j > gpp$mip.a & gpp$mip.j > gpp$mip.p))
                      & gpp$ksv == 'sig' & gpp$ksv.cand=='sig','Strong Evidence of Ideological Selection',
               ifelse(gpp$ksv == 'sig','Strong Evidence of Ideological Selection',
                      'Weak or No Evidence of Ideological Selection'))
gpp$st <- reorder(gpp$st,
                  as.numeric(as.factor(gpp$sig.type)))
gpp$sig.type <- reorder(gpp$sig.type,
                        rev(as.numeric(as.factor(gpp$sig.type))))

p <- ggplot(gpp) + geom_segment(aes(x=mip.p,xend=mip.a,y=factor(st),yend=factor(st),
                                    colour = factor(sig.type),
                                    linetype = factor(sig.type))
                                    ,size=.75)

p <- p + geom_point(aes(y=st,x=mip.j),shape='J',size=5,colour='darkgreen',alpha=.75)
p <- p + geom_point(aes(y=st,x=mip.a),shape='A',size=5,colour='blue',alpha=.75)
p <- p + geom_point(aes(y=st,x=mip.p),shape='P',size=5,colour='red',alpha=.75)


p <- p + scale_shape_identity()
p <- p + scale_size_identity()
p <- p + scale_colour_manual('',
                             values = c('Strong Evidence of Ideological Selection'='darkgrey',
                                        'out of bounds' = 'darkgrey',
                                        'Weak or No Evidence of Ideological Selection'='darkgrey'))
p <- p + scale_linetype_manual('',
                               values = c('Strong Evidence of Ideological Selection'=2,'out of bounds' = 2,
                                          'Weak or No Evidence of Ideological Selection'=2))
p <- p + ylab('') + xlab('DIME score (Conservatism)')
p <- p + theme_bw()
p <- p + theme(legend.position='FALSE',
               strip.text=element_text(size=12))
p <-  p + facet_wrap(~sig.type,ncol=1,as.table=FALSE,drop=FALSE,scales = "free_y")


pdf(file='figures/figure_6_st_means_for_pol_atty_jud.pdf', width = 9, height = 11)
print(p)
dev.off()

p2 <- ggplot(gpp) + geom_segment(aes(x=mip.p,xend=mip.a,y=factor(st),yend=factor(st),
                                    colour = factor(sig.type),
                                    linetype = factor(sig.type))
                                    ,size=.75)

p2 <- p2 + geom_point(aes(y=st,x=mip.j),shape='J',size=5,colour='black',alpha=.75)
p2 <- p2 + geom_point(aes(y=st,x=mip.a),shape='A',size=5,colour='darkgrey',alpha=1)
p2 <- p2 + geom_point(aes(y=st,x=mip.p),shape='P',size=5,colour='darkgrey',alpha=1)
p2 <- p2 + scale_shape_identity()
p2 <- p2 + scale_size_identity()
p2 <- p2 + scale_colour_manual('',
                             values = c('Strong Evidence of Ideological Selection'='darkgrey',
                                        'out of bounds' = 'darkgrey',
                                        'Weak or No Evidence of Ideological Selection'='darkgrey'))
p2 <- p2 + scale_linetype_manual('',
                               values = c('Strong Evidence of Ideological Selection'=2,'out of bounds' = 2,
                                          'Weak or No Evidence of Ideological Selection'=2))
p2 <- p2 + ylab('') + xlab('DIME score (Conservatism)')
p2 <- p2 + theme_bw()
p2 <- p2 + theme(legend.position='FALSE',
               strip.text=element_text(size=12))
p2 <-  p2 + facet_wrap(~sig.type,ncol=1,as.table=FALSE,drop=FALSE,scales = "free_y")


pdf(file='figures/figure_6_st_means_for_pol_atty_jud_bw.pdf', width = 9, height = 11)
print(p2)
dev.off()


###############################################################################
##PLOT OMEGA ON OVERLAP BY PARTY
###############################################################################

gpq <- NULL
for(xx in 1:length(out$p.ov.all)){
    gpq <- rbind(gpq,cbind(names(out$wsd.all)[xx],out$p.ov.all[[xx]]))

    dt <- round(out$p.ov.all[[xx]][,2],2)
    rt <- round(out$p.ov.all[[xx]][,3],2)
    d.npos <- sum(dt[-1] > dt[1])
    r.npos <- sum(rt[-1] > rt[1])
    if(d.npos > 0 & r.npos > 0){
        print(c(xx,names(out$wsd.all)[xx],d.npos,r.npos))
    }

}

gpq <- as.data.frame(gpq)
colnames(gpq) <- c('st','omega','dov','rov')
gpq$st <- as.factor(as.character(gpq$st))
gpq$omega <- as.numeric(as.character(gpq$omega))
gpq$dov <- as.numeric(as.character(gpq$dov))
gpq$rov <- as.numeric(as.character(gpq$rov))


ov <-  -1 * (((gpq[gpq$omega ==1,'rov']-gpq[gpq$omega ==0,'rov'])))##/(gpq[gpq$omega ==0,'rov']))
mmzq <- match(gpq$st,gpq[gpq$omega ==0,'st'])
gpq$st <- reorder(gpq$st,ov[mmzq])


p1 <- ggplot(data=gpq,aes(x=omega,y=dov,group=st))
p1 <- p1 + geom_line(aes(x=omega,y=dov,group=st),color='blue')
p1 <- p1 + geom_line(aes(x=omega,y=rov,group=st),color='red')
p1 <- p1 + ylab('Overlap Coefficient') + xlab(expression(omega))
p1 <- p1 + theme_bw() + ylim(.05,.95)
p1 <- p1 + theme(axis.text.x=element_text(size=5),
                 axis.text.y=element_text(size=7),
                 strip.text=element_text(size=10),##,face='bold'),
                 axis.title.x=element_text(size=16),
                 axis.title.y=element_text(size=16),
                 plot.title=element_text(size=0)
                 )
p1 <- p1 + facet_wrap(~st,ncol=9)
p1 <- p1 + scale_x_continuous(breaks=c(0,.5,1),labels=c('0','0.5','1'))

pdf(file='figures/figure_5_st_omega_overlap_mappings.pdf', width = 8, height = 10)
print(p1)
dev.off()

p2 <- p1
p2 <- p2 + geom_line(aes(x=omega,y=dov,group=st),color='black')
p2 <- p2 + geom_line(aes(x=omega,y=rov,group=st),color='grey')

pdf(file='figures/figure_5_st_omega_overlap_mappings_bw.pdf', width = 8, height = 10)
print(p2)
dev.off()


