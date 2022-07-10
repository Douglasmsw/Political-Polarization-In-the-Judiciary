library(stargazer)
library(ggplot2)

##LOADING DATA
load('data/flvh_mh_linked.Rdata')

##FIGURE A6
flvf_mh$party2 <- with(flvf_mh,ifelse(party=='DEM','D',
                              ifelse(party=='REP','R','I')))
q <- qplot(data=flvf_mh[which(flvf_mh$score >= 5 &
                              flvf_mh$party %in% c('DEM','REP','NPA')),],
           x=cfscore.fed.only,colour=party,geom='density')
q <- q + xlim(-2,1.8)
q <- q + scale_color_manual('',values=c('DEM'='darkblue','REP'='red','NPA'='orange'))
q <- q + xlab('DIME score (Conservatism)')

q <- q + theme_bw()
q <- q + theme(legend.position=c(.9,.85))
pdf(file='figures/figure_A6_appendix_cfscore_by_fl_party_reg.pdf',height = 5,width= 10 )
print(q)
dev.off()

