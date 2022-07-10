library(stargazer)
library(visreg)
library(GGally)
library(Matrix)
library(ggplot2)

load('data/alt_state_measures_mat.Rdata')
load('data/jud_selection_data.Rdata')

##FIGURE A5
zz <- ggpairs(comp.st.measures)
zz <- zz + theme_bw()
zz <- zz+ theme(strip.text=element_text(size=8))
zz$xAxisLabels <- gsub('_',' ',zz$xAxisLabels)
zz$yAxisLabels <- gsub('_',' ',zz$yAxisLabels)

pdf(file='figures/figure_A4_state_policy_mean_comparisons.pdf',width=8,height=8)
print(zz)
dev.off()

###############################################################################
##FIGURE A5
###############################################################################
Xfm$st.cand.mean <- comp.st.measures$Berry_et_al_Government[match(Xfm$state,rownames(comp.st.measures))]
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
##
###############################################################################
m1 <- lm(cfscore~ -1 +   jud.sel +
             lawyer.pe + lawyer.npe + lawyer.gc + lawyer.g  +
                 cand.pe + cand.npe + cand.gc + cand.g,
         data=Xfm)

m2 <- lm(cfscore~ -1 + jud.sel +
             lawyer.pe + lawyer.npe + lawyer.gc + lawyer.g  +
                  cand.pe + cand.npe + cand.gc + cand.g +
                     lyear + lyear.sq +
                          female + top.14 + not.top.100+attended.in.state.ls,
         data=Xfm)



###############################################################################
##INTERIM APPOINTMENTS
###############################################################################
Xfm$jud.sel.tmp <- Xfm$jud.sel
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
        
###############################################################################
##FIGURE A5
###############################################################################
m4 <- lm(cfscore~ -1 +jud.sel * (st.cand.mean + st.lawyer.mean) +
             lyear + lyear.sq +
                 female + top.14 + not.top.100+attended.in.state.ls,data=Xfm)
 
pdf(file='figures/figure_A5_jud_selection_cand_mean_berry.pdf',width=11,height=4)
visreg(m4,'st.cand.mean',by='jud.sel',
       scales=list(alternating =1,tck=c(1,0)),
       partial=F,mfrows=1,rug=FALSE,
       line.par=list(col=c('red')),

       xlab='Berry Et. Al State Insitutional Ideology',
       ylab='Judicial Ideology (DIME score)',
       strip.names=c('Appointed','Merit','Partisan Elections','Non-partisan Elections'))
dev.off()

pdf(file='figures/figure_A5_jud_selection_lawyer_mean_berry.pdf',width=11,height=4)
visreg(m4,'st.lawyer.mean',by='jud.sel',
       ylim=c(-.5,.5),xlim=c(-.5,.5),
       scales=list(alternating =1,tck=c(1,0)),
       partial=F,mfrows=1,rug=FALSE,
       xlab='Avg. Lawyer in State (A) (DIME score)',
       ylab='Judicial Ideology (DIME score)',
       strip.names=c('Appointed','Merit','Partisan Elections','Non-partisan Elections'))
dev.off()
