########################################
## packages
########################################
library(micEcon)
library(sampleSelection)
library(stargazer)
library(mfx)
library(ggplot2)
library(xtable)

########################################
## loading the data
########################################
load('data/dime_mh_merged.Rdata')
load('data/judges_all.Rdata')

all.judges <- all.judges[order(all.judges$jtype),]
m <- match(all.judges$isln,lawyers.all$isln,incomparables=c('',NA))
m1 <- match(all.judges$dime.cid,lawyers.all$dime.cid,incomparables=c('',NA))
m[is.na(m)] <- m1[is.na(m)]
all.judges$p.to.reps<- lawyers.all$p.to.reps[m]

lawyers.all$jtype <- as.character(all.judges$jtype[match(lawyers.all$isln,all.judges$isln,incomparables=c('',NA))])
lawyers.all$jtype[is.na(lawyers.all$jtype)] <- 'Attorneys'
lawyers.all$jtype2 <- ifelse(grepl('State',lawyers.all$jtype),'State Judges',
                             ifelse(grepl('U\\.',lawyers.all$jtype),'Federal Judges',
                                    'Attorneys'))
lawyers.all$jtype <- factor(lawyers.all$jtype,level=rev(c('Attorneys','State Lower Courts','State High Courts','U.S. Circuit Courts','U.S. District Courts')))

lawyers.all$bipart <- with(lawyers.all,ifelse(abs(p.to.reps -.5) < 0.5 ,1,0))
lawyers.all$bipart2 <- with(lawyers.all,ifelse(abs(p.to.reps -.5) < 0.3 ,1,0))

bb <- with(lawyers.all,aggregate(bipart,list(jtype2),mean,na.rm=T))
bb2 <- with(lawyers.all,aggregate(bipart2,list(jtype2),mean,na.rm=T))
bb2[,2] <- round(bb2[,2],3)
rn <- bb2[,1]
bb3 <- data.frame(bb[,2],bb2[,2])
rownames(bb3) <- rn
colnames(bb3) <- c('Strict Partisans','Less than 80% to One Party')
xtable(bb3,digits=3)


tt <- with(lawyers.all,table(jtype,bipart))
aa <- with(lawyers.all,aggregate(bipart,list(jtype),mean,na.rm=T))


aa <- with(lawyers.all,aggregate(bipart,list(jtype),mean,na.rm=T))
aa[,2] <- round(aa[,2],3)
xtable(aa)

aa2 <- with(lawyers.all,aggregate(bipart2,list(jtype),mean,na.rm=T))
aa2[,2] <- round(aa2[,2],3)
rn <- aa2[,1]
aa3 <- data.frame(aa[,2],aa2[,2])
rownames(aa3) <- rn
colnames(aa3) <- c('Strict Partisans','Less than 80% to One Party')
xtable(aa3)

###############################################################################
##FIGURES A2 and A3
###############################################################################
p <- qplot(x=p.to.reps,data=lawyers.all,geom='histogram')
p <- p + xlab('Percentage Donated to Republicans')
p <- p + ylab('Number of Lawyers')
p <- p + theme_bw()
pdf(file='figures/figure_A2_partisan_giving_dist.pdf',width = 8, height=6)
print(p)
dev.off()


use <- which(lawyers.all$jtype %in% c('Attorneys','State High Courts', 'State Lower Courts','U.S. Circuit Courts','U.S. District Courts'))
p <- qplot(x=p.to.reps,data=lawyers.all[use,],geom='histogram') 
p <- p + xlab('Percentage Donated to Republicans')
p <- p + ylab('Count')
p <- p + theme_bw()
p <- p+facet_wrap(~jtype,scale='free_y',ncol=1)
p
pdf(file='figures/figure_A3_partisan_giving_dists_judges.pdf',width = 10, height=2.5*5)
print(p)
dev.off()

