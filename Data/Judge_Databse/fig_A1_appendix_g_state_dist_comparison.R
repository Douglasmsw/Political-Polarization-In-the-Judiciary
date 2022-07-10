###############################################################################
##FIGURE A1: COMPARE STATE DISTRIBUTIONS
###############################################################################
library(ggplot2)
library(grid)
library(plyr)
load("data/dime_mh_merged.Rdata")


state.subset <- c("AK", "AL", "AR","DC","AZ", "CA", "CO",
                  "CT", "DE", "FL", "GA","HI", "IA",
                  "ID", "IL", "IN", "KS", "KY", "LA",
                  "MA", "MD", "ME", "MI", "MN", "MO",
                  "MS", "MT", "NC", "ND", "NE", "NH",
                  "NJ", "NM", "NV", "NY", "OH", "OK",
                  "OR", "PA", "RI", "SC", "SD", "TN",
                  "TX", "UT", "VA", "VT", "WA", "WI",
                  "WV", "WY")
gg <- data.frame(ip=as.numeric(lawyers.all[,'cfscore']),
                 st = as.factor(lawyers.all[,'state']),
                 bid = paste(as.numeric(lawyers.all[,'dime.cid']),lawyers.all[,'state'],sep='_')
                 )
gg$ip[gg$ip > 1.5] <- 1.5
gg$ip[gg$ip < -1.8] <- -1.8
gg <- gg[gg$st %in% state.subset,]
gg <- gg[!duplicated(gg[,'bid']),]


sn <- state.name[match(gg$st,state.abb)]
sn[is.na(sn)] <- 'Washington D.C.'
gg$st <- sn

cdf <- ddply(gg, "st", summarise, ip.mean=median(ip,na.rm=T))


p <- ggplot(data=gg,x=ip)
p <- p + geom_histogram(aes(x=ip),binwidth=.2,colour='black',fill='white')
p <- p + xlim(-1.800,1.50)
p <- p + facet_wrap(~ st, scales = "free_y",nrow=7)
p <- p + theme_bw()
p <- p + theme(strip.text.x=element_text(size=18),
               axis.title = element_text(size=24)
               )
p <- p + xlab('DIME Score (Conservatism)')  + ylab('')

pdf('figures/figure_A1_st_lawyer_dist.pdf',width = 25+ 4,height= 25 *(6/9 ))
print(p)
dev.off()

