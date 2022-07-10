library(stargazer)
library(visreg)
library(texreg)
library(Zelig)
library(GGally)
library(lattice)

load('data/jud_selection_data.Rdata')

Xfm$cfdist <- with(Xfm,cfscore - st.lawyer.mean)
Xfm$lc.dist <- with(Xfm,st.cand.mean-st.lawyer.mean)
Xfm$log.year <- log(Xfm$lyear)
Xfm$lyear.sq <- (Xfm$lyear)^2
Xfm$female <- ifelse(Xfm$gender == 'F',1,0)

Xfm$cand.pe <- ifelse(Xfm$jud.sel == 'PE',1,0) * Xfm$st.cand.mean
Xfm$cand.npe <- ifelse(Xfm$jud.sel == 'NPE',1,0) * Xfm$st.cand.mean
Xfm$cand.gc <- ifelse(Xfm$jud.sel == 'GC',1,0) * Xfm$st.cand.mean
Xfm$cand.g <- ifelse(Xfm$jud.sel == 'G',1,0) * Xfm$st.cand.mean
Xfm$lawyer.pe <- ifelse(Xfm$jud.sel == 'PE',1,0) * Xfm$st.lawyer.mean
Xfm$lawyer.npe <- ifelse(Xfm$jud.sel == 'NPE',1,0) * Xfm$st.lawyer.mean
Xfm$lawyer.gc <- ifelse(Xfm$jud.sel == 'GC',1,0) * Xfm$st.lawyer.mean
Xfm$lawyer.g <- ifelse(Xfm$jud.sel == 'G',1,0) * Xfm$st.lawyer.mean

###############################################################################
##REGRESSION MODELS
###############################################################################
m1 <- lm(cfscore~ -1 +   jud.sel +
             lawyer.pe + lawyer.npe + lawyer.gc + lawyer.g  +
                 cand.pe + cand.npe + cand.gc + cand.g,
         data=Xfm)
summary(m1)

m2 <- lm(cfscore~ -1 + jud.sel +
             lawyer.pe + lawyer.npe + lawyer.gc + lawyer.g  +
                  cand.pe + cand.npe + cand.gc + cand.g +
                     lyear + lyear.sq +
                          female + top.14 + not.top.100+attended.in.state.ls,
         data=Xfm)


###############################################################################
##INTERIM APPOINTMENS
###############################################################################
Xfm$jud.sel.tmp <- Xfm$jud.sel
Xfm$jud.sel <- Xfm$jud.sel.interim
Xfm$jud.sel[is.na(Xfm$jud.sel)] <- Xfm$jud.sel.tmp[is.na(Xfm$jud.sel)]
Xfm$cand.pe <- ifelse(Xfm$jud.sel == 'PE',1,0) * Xfm$st.cand.mean
Xfm$cand.npe <- ifelse(Xfm$jud.sel == 'NPE',1,0) * Xfm$st.cand.mean
Xfm$cand.gc <- ifelse(Xfm$jud.sel == 'GC',1,0) * Xfm$st.cand.mean
Xfm$cand.g <- ifelse(Xfm$jud.sel == 'G',1,0) * Xfm$st.cand.mean
Xfm$lawyer.pe <- ifelse(Xfm$jud.sel == 'PE',1,0) * Xfm$st.lawyer.mean
Xfm$lawyer.npe <- ifelse(Xfm$jud.sel == 'NPE',1,0) * Xfm$st.lawyer.mean
Xfm$lawyer.gc <- ifelse(Xfm$jud.sel == 'GC',1,0) * Xfm$st.lawyer.mean
Xfm$lawyer.g <- ifelse(Xfm$jud.sel == 'G',1,0) * Xfm$st.lawyer.mean

m3 <- lm(cfscore~  -1+   jud.sel +
             lawyer.pe + lawyer.npe + lawyer.gc + lawyer.g +
                 cand.pe + cand.npe + cand.gc + cand.g +
                     lyear + lyear.sq +
                         female + top.14 + not.top.100+attended.in.state.ls,data=Xfm)
summary(m3)
star <- stargazer(m2,m3,
                  digits = 3, style = "ajps",
                  star.cutoffs = c(0.05, 0.01),
                  ##star.char = c("", "*"),
                  font.size = "scriptsize",
                  title = "Judicial Selection",
                  covariate.labels=c('Appointed','Merit','Partisan Election','Non-Partisan Election',
                                     'Avg. Lawyer$\\times$Partisan Election',
                                     'Avg. Lawyer$\\times$Non-Partisan Election',
                                     'Avg. Lawyer$\\times$Merit',
                                     'Avg. Lawyer$\\times$Appointed',
                                     'Avg. Politician$\\times$Partisan Election',
                                     'Avg. Politician$\\times$Non-Partisan Election',
                                     'Avg. Politician$\\times$Merit',
                                     'Avg. Politician$\\times$Appointed',
                                     'Years Since Admitted',
                                     'Years Since Admitted$^2$',
                                     'Female',
                                     'Top 14 Law School',
                                     '$>$ 100 Ranked Law School',
                                     'In-State Law School'))
writeLines(star,'tables/table_4.tex')


###############################################################################
##FIGURE 7
###############################################################################
m4 <- lm(cfscore~ -1 +   jud.sel * (st.cand.mean + st.lawyer.mean) +
             lyear + lyear.sq +
                 female + top.14 + not.top.100+attended.in.state.ls,data=Xfm)

pdf(file='figures/figure_7_jud_selection_cand_mean.pdf',width=11,height=4)
visreg(m4,'st.cand.mean',by='jud.sel',
       ylim=c(-.75,.75),
       xlim=c(-.75,.75),
       scales=list(alternating =1,tck=c(1,0)),
       partial=F,mfrows=1,rug=FALSE,
       line.par=list(col=c('red')),
       xlab='Avg. Politician in State (P) (DIME score)',
       ylab='Judicial Ideology (DIME score)',
       strip.names=c('Appointed','Merit','Partisan Elections','Non-partisan Elections'))
dev.off()

pdf(file='figures/figure_7_jud_selection_lawyer_mean.pdf',width=11,height=4)
visreg(m4,'st.lawyer.mean',by='jud.sel',
        ylim=c(-.75,.75),xlim=c(-.75,.75),
       scales=list(alternating =1,tck=c(1,0)),
       partial=F,mfrows=1,rug=FALSE,
       xlab='Avg. Lawyer in State (A) (DIME score)',
       ylab='Judicial Ideology (DIME score)',
       strip.names=c('Appointed','Merit','Partisan Elections','Non-partisan Elections'))
dev.off()


pdf(file='figures/figure_7_jud_selection_cand_mean_bw.pdf',width=11,height=4)
trellis.par.set(strip.background=list(col="gray90"))
visreg(m4,'st.cand.mean',by='jud.sel',
       ylim=c(-.75,.75),
       xlim=c(-.75,.75),
       scales=list(alternating =1,tck=c(1,0)),
       partial=F,mfrows=1,rug=FALSE,
       line.par=list(col=c('black')),
       xlab='Avg. Politician in State (P) (DIME score)',
       ylab='Judicial Ideology (DIME score)',
       strip.names=c('Appointed','Merit','Partisan Elections','Non-partisan Elections'))
dev.off()

pdf(file='figures/figure_7_jud_selection_lawyer_mean_bw.pdf',width=11,height=4)
trellis.par.set(strip.background=list(col="gray90"))
visreg(m4,'st.lawyer.mean',by='jud.sel',
        ylim=c(-.75,.75),xlim=c(-.75,.75),
       scales=list(alternating =1,tck=c(1,0)),
       line.par=list(col=c('black')),
       partial=F,mfrows=1,rug=FALSE,
       xlab='Avg. Lawyer in State (A) (DIME score)',
       ylab='Judicial Ideology (DIME score)',
       strip.names=c('Appointed','Merit','Partisan Elections','Non-partisan Elections'))
dev.off()


###############################################################################
##FIRST DIFFS
###############################################################################
Xfm2 <- Xfm
Xfm2$jud.sel <- Xfm2$jud.sel.tmp
Xfm2 <- Xfm[,c('cfscore',
               'jud.sel',
               'st.cand.mean',
               'st.lawyer.mean',
               'lyear','lyear.sq',
               'female',
               'top.14','not.top.100',
               'attended.in.state.ls',
               'state')]
Xfm2 <- Xfm2[complete.cases(Xfm2),]

z.out <- zelig(cfscore~
                -1 + jud.sel * (st.cand.mean + st.lawyer.mean) +
                lyear + lyear.sq +
                female + top.14 + not.top.100+attended.in.state.ls,
            model='ls',data=Xfm2)



for(js in c('G','GC','PE','NPE')){
    x.high <- setx(z.out, jud.sel=js,st.lawyer.mean = 0.5)
    x.low <- setx(z.out, jud.sel=js,st.lawyer.mean = -0.5)
    s.out <-  sim(z.out, x = x.low, x1 = x.high)
    print(summary(s.out))
}

for(js in c('G','GC','PE','NPE')){
    x.high <- setx(z.out, jud.sel=js,st.cand.mean = 0.5)
    x.low <- setx(z.out, jud.sel=js,st.cand.mean = -0.5)
    s.out <-  sim(z.out, x = x.low, x1 = x.high)
    print(js)
    print(summary(s.out))
}


