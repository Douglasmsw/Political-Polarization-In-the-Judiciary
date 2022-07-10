########################################
## packages
########################################
library(stargazer)
library(foreign)
library(plyr)

load("data/dime_mh_merged.Rdata")
load("data/judges_all.Rdata")

########################################
## adding judges into lawyers' data
########################################
all.judges <- subset(all.judges,!grepl('ret',judge,ignore.case=T))
all.judges <- subset(all.judges,!grepl('sr',judge,ignore.case=T))
all.judges$inCF <- ifelse(!is.na(all.judges$dime.cid),1,0) 

## adjusting variables
lawyers.all$lyear[lawyers.all$lyear < 0 | lawyers.all$lyear > 100] <- NA
lawyers.all$female <- ifelse(lawyers.all[,'gender'] == 'F',1,
                 ifelse(lawyers.all[,'gender'] == 'M',0,NA))

lawyers.all$inCF <- 0
lawyers.all$inCF[which(!is.na(lawyers.all$cfscore) |
                       !is.na(lawyers.all$dime.cid))] <- 1
bid <- as.numeric(as.character(c(lawyers.all[,'dime.cid'],lawyers.all[,'bonica.id2.2'])))
all.judges$dime.cid[is.na(all.judges$dime.cid)] <- -9
all.judges$isln[is.na(all.judges$isln)] <- -9

missing <- all.judges[which(!(all.judges$dime.cid %in% na.omit(c(bid,bid+5e10)))),]
missing <- missing[which(!(missing$isln %in% na.omit(lawyers.all$isln))),]
lawyers.all$inCF = ifelse(!is.na(lawyers.all$dime.cid),1,0)

aggfun <- function(z,...){
    return(c(mean(z,na.rm=T),median(z,na.rm=T),length(z)))
}

a1 <- rbind(all=with(lawyers.all,aggfun(inCF,na.rm=T)),
            female=with(lawyers.all,aggfun(inCF[female==1],na.rm=T)),
            male=with(lawyers.all,aggfun(inCF[female==0],na.rm=T)),
            state.lower.judge=with(all.judges,aggfun(inCF[ jtype %in% c('','State Lower Courts')],na.rm=T)),
            state.higher.judge=with(all.judges,aggfun(inCF[ jtype == "State High Courts"],na.rm=T)),
            fed.district.judge=with(all.judges,aggfun(inCF[ jtype == "U.S. District Courts"],na.rm=T)),
            fed.circuit.judge=with(all.judges,aggfun(inCF[ jtype == "U.S. Circuit Courts"],na.rm=T)),
            fed.maj.judge=with(all.judges,aggfun(inCF[ jtype == "U.S. Magistrate"],na.rm=T)),
            admin.judges=with( all.judges,aggfun(inCF[ jtype %in% c('U.S. Administrative','State Administrative')],na.rm=T)),
            gov=with(lawyers.all,aggfun(inCF[gov==1],na.rm=T)),
            corp=with(lawyers.all,aggfun(inCF[corp==1],na.rm=T)),
            lawprof=with(lawyers.all,aggfun(inCF[etype == 'lawprof'],na.rm=T)),
            partner=with(lawyers.all,aggfun(inCF[partner == 1],na.rm=T)),
            is.big.law=with(lawyers.all,aggfun(inCF[is.big.law == 1],na.rm=T)),
            prosecutor=with(lawyers.all,aggfun(inCF[prosecutor.or.da == 1],na.rm=T)),
            pubdef=with(lawyers.all,aggfun(inCF[public.defender == 1],na.rm=T)),
            top.14=with(lawyers.all,aggfun(inCF[top.14 == 1],na.rm=T)),
            mid.rank=with(lawyers.all,aggfun(inCF[not.top.100 == 0 & top.14==0],na.rm=T)),
            not.top.100=with(lawyers.all,aggfun(inCF[not.top.100 == 1],na.rm=T)),
            inpract10=with(lawyers.all,aggfun(inCF[lyear <= 10],na.rm=T)),
            inpract10to20=with(lawyers.all,aggfun(inCF[lyear > 10 & lyear <=20],na.rm=T)),
            inpract20to30=with(lawyers.all,aggfun(inCF[lyear > 20 & lyear <=30],na.rm=T)),
            inpract30to40=with(lawyers.all,aggfun(inCF[lyear > 30 & lyear <=40],na.rm=T)),
            inpract40=with(lawyers.all,aggfun(inCF[lyear > 40],na.rm=T))
            )


a2 <- rbind(all=with(lawyers.all,aggfun(cfscore,na.rm=T)),
            female=with(lawyers.all,aggfun(cfscore[female==1],na.rm=T)),
            male=with(lawyers.all,aggfun(cfscore[female==0],na.rm=T)),
            state.lower.judge=with(all.judges,aggfun(cfscore[ jtype %in% c('','State Lower Courts')],na.rm=T)),
            state.higher.judge=with(all.judges,aggfun(cfscore[ jtype == "State High Courts"],na.rm=T)),
            fed.district.judge=with(all.judges,aggfun(cfscore[ jtype == "U.S. District Courts"],na.rm=T)),
            fed.circuit.judge=with(all.judges,aggfun(cfscore[ jtype == "U.S. Circuit Courts"],na.rm=T)),
            fed.maj.judge=with(all.judges,aggfun(cfscore[ jtype == "U.S. Magistrate"],na.rm=T)),
            admin.judges=with( all.judges,aggfun(cfscore[ jtype %in% c('U.S. Administrative','State Administrative')],na.rm=T)),
            gov=with(lawyers.all,aggfun(cfscore[gov==1],na.rm=T)),
            corp=with(lawyers.all,aggfun(cfscore[corp==1],na.rm=T)),
            lawprof=with(lawyers.all,aggfun(cfscore[etype == 'lawprof'],na.rm=T)),
            partner=with(lawyers.all,aggfun(cfscore[partner == 1],na.rm=T)),
            is.big.law=with(lawyers.all,aggfun(cfscore[is.big.law == 1],na.rm=T)),
            prosecutor=with(lawyers.all,aggfun(cfscore[prosecutor.or.da == 1],na.rm=T)),
            pubdef=with(lawyers.all,aggfun(cfscore[public.defender == 1],na.rm=T)),
            top.14=with(lawyers.all,aggfun(cfscore[top.14 == 1],na.rm=T)),
            mid.rank=with(lawyers.all,aggfun(cfscore[not.top.100 == 0 & top.14==0],na.rm=T)),
            not.top.100=with(lawyers.all,aggfun(cfscore[not.top.100 == 1],na.rm=T)),
            inpract10=with(lawyers.all,aggfun(cfscore[lyear <= 10],na.rm=T)),
            inpract10to20=with(lawyers.all,aggfun(cfscore[lyear > 10 & lyear <=20],na.rm=T)),
            inpract20to30=with(lawyers.all,aggfun(cfscore[lyear > 20 & lyear <=30],na.rm=T)),
            inpract30to40=with(lawyers.all,aggfun(cfscore[lyear > 30 & lyear <=40],na.rm=T)),
            inpract40=with(lawyers.all,aggfun(cfscore[lyear > 40],na.rm=T))
      )
a3 <- cbind(round(a1[,1],3),round(a2[,1],3),round(a2[,2],3),a1[,3])

rownames(a3) <- c('All',
                  'Female',
                  'Male',
                  'State Lower Court Judge',
                  'State High Court Judge',
                  'Fed. District Court Judge',
                  'Fed. Circuit Court Judge',
                  'Fed. Magistrate Judge',
                  'Administrative Judge',
                  'Employed by Government',
                  'In-House',
                  'Law Professor',
                  'Partner',
                  'Big Law',
                  'Prosecutor',
                  'Public Defender',
                  'Top 14 Law School',
                  'Top 15-100 Law School',
                  '>=Top 100 Law School',
                  'Years since Admittance (<10)',
                  'Years since Admittance (11-20)',
                  'Years since Admittance (21-30)',
                  'Years since Admittance (31-40)',
                  'Years since Admittance (>40)'
                  )
colnames(a3) <- c('Prop. Donors','Mean CFscore','Median CFscore','N')
print(a3)
star <- stargazer(a3)
writeLines(star,'tables/table_1.tex')
