library(ggplot2)
library(Matrix)
library(gridExtra)
library(texreg)

source('src/plot_dist_combo.R')
load("data/dime_mh_merged.Rdata")
lawyers <- lawyers.all[!is.na(lawyers.all$cfscore),]

###############################################################################
##JUDICIAL BRANCH
###############################################################################
load('data/judges_all.Rdata')
all.judges <- all.judges[order(all.judges$jtype),]

###############################################################################
##PLOT TO COMPARE TYPES OF JUDGES
###############################################################################
bid <- as.numeric(as.character(lawyers[,'dime.cid']))
laws <- lawyers[!(bid %in% c(all.judges[,2])),]
laws <- laws[laws$is.judge==0,]

gg <- rbind(cbind(all.judges[,'cfscore'],328,as.character(all.judges[,'jtype']),'Judges',
                  as.character(all.judges$judge),as.character(all.judges$judge.type),
                  as.character(all.judges[,4]),as.numeric(as.character(all.judges[,2]))),
            cbind(as.numeric(laws[,'cfscore']),328,
                  'Attorneys','Attorneys','Attorneys','',laws[,'state'],as.character(laws[,'dime.cid'])))

colnames(gg) <- c('ip','party','jtype','seat','judge','judge.type','state','bid')
rownames(gg) <- 1:nrow(gg)
gg[gg[,2] ==100,2] = 'Dems'
gg[gg[,2] ==200,2] = 'Reps'
gg[!(gg[,2] %in% c('Dems','Reps')),2] = 'Non-Partisan'
gg <- as.data.frame(gg)
colnames(gg) <- c('ip.value','party','jtype','seat','judge','judge.type','state','bid')
gg$type <- factor(gg[,2])
gg$seat <- factor(gg[,4])
gg$ip.value <- as.numeric(as.character(gg$ip.value))
gg$jtype <- factor(gg[,3])


state.subset <- c("DC","AK", "AL", "AR",
                  "AZ", "CA", "CO",
                  "CT", "DE", "FL", "GA","HI", "IA",
                  "ID", "IL", "IN", "KS", "KY", "LA",
                  "MA", "MD", "ME", "MI", "MN", "MO",
                  "MS", "MT", "NC", "ND", "NE", "NH",
                  "NJ", "NM", "NV", "NY", "OH", "OK",
                  "OR", "PA", "RI", "SC", "SD", "TN",
                  "TX", "UT", "VA", "VT", "WA", "WI",
                  "WV", "WY","US")
gg$state[!(gg$state %in% state.subset)] <- NA


jud <- rep('',nrow(gg))
jud1 <- as.character(gg$jtype)
jud2 <- as.character(gg$judge)
jud3 <- as.character(gg$judge.type)
tt <- table(as.character(jud2))
tt[rev(order(tt))[1:50]]
jud[grepl('u\\.s\\.|us|u\\. s\\.|fed',jud2,ignore.case=T)] <- 'Federal Lower'
jud[jud1=='State Lower Courts' | jud1 ==''] <- 'State and Local Courts'
jud[jud1=='' & jud3 == 'state'] <- 'State and Local Courts'
jud[jud1=='State High Courts'] <- 'State High Courts'
jud[jud1 == 'Fed District' | jud2 == 'Fed District' | jud2 == 'U.S. Dist. J.' |
    grepl('U\\. S\\. Dis',jud2,ignore.case=T)] <- 'U.S. District Courts'
jud[jud1 == 'Fed Circuit' | jud2 == 'Fed Circuit' | grepl('U\\. S\\. Court of A',jud2,ignore.case=T)|
    (grepl('U\\.S\\.',jud2,ignore.case=T) & grepl('App',jud2,ignore.case=T))
    ] <- 'U.S. Circuit Courts'

gg$jtype <- as.character(gg$jtype)
gg$jtype[jud == 'U.S. District Courts'] <- 'U.S. District Court Judges'
gg$jtype[jud == 'State High Courts'] <- 'State High Court Judges'
gg$jtype[jud == 'State and Local Courts'] <- 'State Lower Court Judges'
gg$jtype[jud == 'U.S. Circuit Courts' | gg$jtype == 'Fed Circuit' | gg$jtype == 'federal'] <- 'U.S. Circuit Court of Appeals Judges'
gg$jtype[gg$jtype == 'U.S. Circuit Courts' | gg$jtype == 'Fed Circuit'] <- 'U.S. Circuit Court of Appeals Judges'
gg$jtype[jud == 'Federal Lower' | jud1 == 'U.S. Magistrate'] <- 'U.S. Administrative and Magistrate Judges'
gg$jtype[gg$jtype == 'State Administrative'] <- 'State Lower Court Judges'
gg$jtype <- factor(gg$jtype)
 
gg2 <- data.frame(ip=gg$ip.value,st=gg$jtype)
gg2 <- gg2[!is.na(gg2$ip),]
 
qq1 <- c(-1.75,1.75)

ord <- c('U.S. Circuit Court of Appeals Judges',
         'U.S. District Court Judges',
         'U.S. Administrative and Magistrate Judges',
         'State High Court Judges',
         'State Lower Court Judges',
         'Attorneys')

pdf(file=paste('figures/figure_4_judge_comp_all.pdf',sep=''),width = 10, height=2.5*length(ord))
pushViewport(viewport(layout=grid.layout(length(ord),1)))
count <- 1
for(xx in ord){
    gg1 <- gg2[gg2$st == xx,]
    gg1$st <- factor(gg1$st)
    pout <- plot.combo(gg1,qq1=qq1,ttle=xx,bw=.1,last=ifelse(count==length(ord),TRUE,FALSE),brat1=.3,brat2=1,mean.or.median='median')
    print(pout,vp=viewport(layout.pos.row = count, layout.pos.col = 1))
          count <- count +1
}
dev.off()


