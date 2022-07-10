########################################
## packages
########################################
library(micEcon)
library(sampleSelection)
library(stargazer)
library(mfx)
library(mlogit)
library(Zelig)
library(visreg)
library(lattice)
########################################
## loading the data
########################################
load("data/dime_mh_merged.Rdata")
lawyers.all <- lawyers.all[which(!is.na(lawyers.all$cfscore) & lawyers.all$top.14 == 1 & lawyers.all$lyear >= 15),]


#########################################
## Analysis: What predicts being a judge?
#########################################
re.out1 <- relogit(fed.circuit~ cfscore+lyear + I(lyear^2) ,data=lawyers.all)
c1 <- visreg(re.out1,"cfscore", scale="response", rug=FALSE,partial=FALSE,xlim=c(-1.5,1.5))
c1$fit[101,5]/c1$fit[1,5]
re.out2 <- relogit(fed.district~ cfscore+lyear + I(lyear^2),data=lawyers.all)
c2 <- visreg(re.out2,"cfscore", scale="response", rug=FALSE,partial=FALSE,xlim=c(-1.5,1.5))
c2$fit[101,5]/c2$fit[1,5]
re.out3 <- relogit(state.higher~ cfscore+lyear + I(lyear^2),data=lawyers.all)
c3 <- visreg(re.out3,"cfscore", scale="response", rug=FALSE,partial=FALSE,xlim=c(-1.5,1.5))
c3$fit[101,5]/c3$fit[1,5]
re.out4 <- relogit(state.lower~ cfscore+lyear + I(lyear^2),data=lawyers.all)
c4 <- visreg(re.out4,"cfscore", scale="response", rug=FALSE,partial=FALSE,xlim=c(-1.5,1.5))
c4$fit[101,5]/c4$fit[1,5]

pdf(file='figures/figure_8_pred_prob_judge.pdf',width=10,height=10)
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
visreg(re.out1, "cfscore", scale="response",line.par=list(col=c('black')), rug=FALSE,partial=FALSE,xlim=c(-1.5,1.5),xlab='DIME Score',ylab='Pr(Y=1)',main='Federal Court of Appeals')
visreg(re.out2, "cfscore", scale="response",line.par=list(col=c('black')), rug=FALSE,partial=FALSE,xlim=c(-1.5,1.5),xlab='DIME Score',ylab='Pr(Y=1)',main='Federal District Courts')
visreg(re.out3, "cfscore", scale="response",line.par=list(col=c('black')), rug=FALSE,partial=FALSE,xlim=c(-1.5,1.5),xlab='DIME Score',ylab='Pr(Y=1)',main='State High Courts')
visreg(re.out4, "cfscore", scale="response",line.par=list(col=c('black')), rug=FALSE,partial=FALSE,xlim=c(-1.5,1.5),xlab='DIME Score',ylab='Pr(Y=1)',main='State Lower Courts')
dev.off()

star <- stargazer(re.out1,re.out2,re.out3,re.out4,
                  digits = 3, style = "ajps",
                  star.cutoffs = c(0.05, 0.01),
                  title = "Probability of Judgeship for Graduates of Top 14 Law Schools ($>=$ 15 Years since Bar Admission)",
                  omit.stat = c("rsq","f","ser"), label = "t:judge_outcome",
                  dep.var.labels=c('Fed. CoA','Fed. District','State High Court','State Lower Court'),
                  model.names = NULL,
                  covariate.labels=c('DIME score','Years since Admitted','Years since Admitted$^2$'))
writeLines(star,'tables/table_5.tex')

