##LOADING LIBRARIES
library(ggplot2)

##LOADING SCRIPTS
source('src/plot_dist_combo.R')

##LOADING DATA
load("data/dime_mh_merged.Rdata")
load('data/dime_cands.Rdata')


##REMOVE ROWS WITH MISSING CFSCORES 
lawyers <- lawyers.all[!is.na(lawyers.all$cfscore),]
gg <- data.frame(ip=lawyers$cfscore,st=rep('Lawyers',nrow(lawyers)))
qq1 <- c(-1.75,1.6)

bw =.125
gg$ip[gg$ip > qq1[2]] <- qq1[2]
gg$ip[gg$ip < qq1[1]] <- qq1[1]

xr <- abs(qq1[1] - qq1[2]) * (1/bw)

xr1 <- abs(qq1[1]) /bw
xr2 <- abs(qq1[2]) /bw
dd1 <- density(gg$ip[gg$ip <0],n=floor(xr1),from=qq1[1],to=0)
dd2 <- density(gg$ip[gg$ip >= 0],n=floor(xr2),from=0,to=qq1[2])
max.y <- 0
for(i in 1:(ceiling(xr1))){
    m1 <- sum(gg$ip >= (bw * (i-1)) &
              gg$ip <= (bw * (i)))
    if(m1 > max.y){max.y <-  m1}
}

for(i in 1:(ceiling(xr2))){
    m1 <- sum(gg$ip <= ((-1*bw) * (i-1)) &
              gg$ip >= ((-1*bw) * (i)))
    if(m1 > max.y){max.y <-  m1}
}
max.y <- max.y  * 1.1


p <- ggplot(data=gg,aes(x=ip))
p <- p + geom_histogram(aes(x=ip),binwidth=bw,fill='white',colour='black')

p <- p + xlab(NULL)
p <- p + ylab('Number of Donors')
p <- p + theme_bw()
p <- p + theme(axis.text.y = element_text(size=10,colour='black'),
               axis.text.x = element_text(size=8,colour='black'),
               title = NULL,
               legend.position='none',
               strip.text.x=element_text(size=17,face='bold'),
               strip.background = element_rect(fill = 'white'),
               plot.title=element_blank(),
               panel.border = element_rect(fill = NA, colour = "grey50"),
               plot.margin =  unit(c(.05, .25, 0.05, 0.25), "lines"),
               axis.title.y = element_text(size=12,colour='black'),
               axis.title.x = element_text(size=12,colour='black'))
p <- p + xlab('DIME score (Conservatism)')  + ylab('')


ttext <- rbind(c(x=-1.72,label='Alan Grayson','20908'),
               c(x=-1.58,label='Bernie Sanders','29147'),
               c(x=-1.28,label='Barack Obama','99911'),
               c(x=-0.95,label='Hillary Clinton','40105'),
               c(x=-0.95,label='Bill Clinton','99909'),
               c(x=-0.64,label='Andrew Cuomo','NY13066'),
               c(x=-0.33,label='Max Baucus','14203'),
               c(x=-0.02,label='Joe Manchin','40915'),
               c(x=0.21,label='Olympia Snowe','14661'),
               c(x=0.44,label='Chris Christie',"NJ90695"),
               c(x=0.65,label='Mitt Romney','P80003353'),
               c(x=0.98,label='Paul Ryan','29939'),
               c(x=1.1,label = 'Michelle Bachmann','20728'),
               c(x=1.37,label='Ron Paul','14290'))
m <- match(ttext[,3],cands[,'ICPSR2'])
ttext[,1] <- as.numeric(cands[m,'oc1'])
ttext <- ttext[order(as.numeric(ttext[,1])),]
cols <- ifelse(cands[m,'Party'] == 100,'blue',
               ifelse(cands[m,'Party'] == 200,'red','blue'))
cols[!is.na(cols)] <- 'black'

count <- 1
for(x in 1:nrow(ttext)){
    shift <- ifelse(count %% 2 == 1,.05 * (max.y),0)
    p <- p + annotate(geom='text',x=as.numeric(ttext[x,1]),y=(-.08*(max.y))+shift,label=ttext[x,2],
                      colour=cols[x],size=4,hjust=.5,family=c("serif"))
    p <- p + annotate(geom='point',x=as.numeric(ttext[x,1]),y=(-0.10*( max.y))+shift,size=1.25,colour=cols[x])
    count <- count+1
}

pdf(file='figures/figure_3_lawyer_dist_with_cands.pdf',height = 5,width= 10 )
print(p)
dev.off()
