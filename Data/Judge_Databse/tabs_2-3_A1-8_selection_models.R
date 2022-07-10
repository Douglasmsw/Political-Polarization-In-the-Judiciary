########################################
##loading R  packages
########################################
library(micEcon)
library(sampleSelection)
library(stargazer)
library(mfx)


########################################
## loading the data
########################################
load("data/dime_mh_merged.Rdata")
load('data/judges_all.Rdata')




###############################################################################
## EXCLUSION RESTRICTION: NUMBER OF TOP-BALLOT STATE EXECUTIVES ARE ELECTED
###############################################################################
##SECSTATE APPOINTED
secstate <- c('DE','FL','MD','NY','OK','PA','TX','VA','ME','NH','TN','DC','ZZ','AK','HI','NJ','UT')
##AG APPOINTED
ag <-  c('AK','HI','NJ','WY','ME','TN','DC','ZZ')
##TREASURER
treasurer <- unique(c('MT','MN','MI','ME','NH','NJ','MD','VA','TN','GA','AK','HI','DC','ZZ'))
##LTGOV
ltgov <- c('OR','WY','AZ','TN','WV','NH','ME','DC','ZZ')
## ##AUDITOR
auditor <- c('OR','CA','NV','ID','AZ','AK','HI','CO','KS',
             'TX','LA','GA','SC','FL','VA','IL','WI','MI',
             'NJ','MD','RI','CT','NH','ME','TN','DC','ZZ')

appointed.execs <- table(c(ag,treasurer,secstate,ltgov,auditor))
oo <- data.frame(unique(lawyers.all$state),5)
m <- match(oo[,1],names(appointed.execs))
oo[!is.na(m),2] <- as.numeric(oo[!is.na(m),2]) - appointed.execs[m[!is.na(m)]]
m <- match(lawyers.all$state,oo[,1])
lawyers.all$num.elected.execs <- oo[m,2]

cor(lawyers.all$cfscore,lawyers.all$num.elected.execs,use='complete.obs')
summary(glm(lawyers.all$cfscore~lawyers.all$num.elected.execs))
cor(lawyers.all$inCF,lawyers.all$num.elected.execs,use='complete.obs')
summary(lm(lawyers.all$inCF~lawyers.all$num.elected.execs))
lawyers.all$fed.circuit[which(lawyers.all$court.type == 'USCA')] = 1

###############################################################################
##SELECTION MODEL RESULTS
###############################################################################
##MODEL 1
m0 <- heckit(inCF ~ female + lyear + I(lyear^2) +  gov + corp+ is.big.law + solo.practice + law.prof +
             partner +  prosecutor.or.da + public.defender + top.14 + not.top.100 +
             num.elected.execs,
             ##outcome
             cfscore ~ female+ lyear + I(lyear^2) +  gov + corp+ is.big.law + solo.practice + law.prof +
             partner +  prosecutor.or.da + public.defender + top.14 + not.top.100 ,
             data=lawyers.all, method='2step')

##MODEL 2
m1 <- heckit(inCF ~ female+ lyear + I(lyear^2) + gov + corp+ is.big.law + solo.practice + law.prof +
             partner +  prosecutor.or.da + public.defender + top.14 + not.top.100 + cd.pvs +  num.elected.execs,
             ##outcome
             cfscore ~  female+ lyear + I(lyear^2) +gov+ corp + is.big.law + solo.practice +  law.prof +
             partner +  prosecutor.or.da + public.defender + top.14 + not.top.100 +  cd.pvs,
             data=lawyers.all,method='2step')
 
##STAGE 1 TABLE
star <- stargazer(m0$probit, m1$probit,
                  digits = 3, style = "ajps", 
                  star.cutoffs = c(0.05, 0.01),
                  selection.equation=TRUE,
                  font.size = "footnotesize",
                  title ="First-stage Results: Probit regression, whether individual contributes (is in DIME database) as outcome variable.",
                  covariate.labels=c('Constant','Female','Years since Admitted','Years since Admitted$^2$',
                      'Government Lawyer','Corporate (in house counsel)',
                      'Big Law Firm (top 100)','Solo-practice','Law Professor',
                      'Partner','Prosecutor/District Attorney','Public Defender',
                      'Top 14 Law School','$>100$ Ranked Law School',
                      'CD Dem. Pres. Vote Share','N. Elected State Execs.'),
                  label = "t:heckit_probit")
writeLines(star,'tables/table_A3.tex')

##STAGE 2 TABLE
star <- stargazer(m0, m1,digits = 3, style = "ajps",
                  star.cutoffs = c(0.05, 0.01),
                  title = "Second-stage Results: OLS, Contributor DIME score as outcome variable",
                  font.size = "footnotesize",
                  label = "t:heckit_outcome",
                  covariate.labels=c('Female','Years since Admitted','Years since Admitted$^2$',
                                     'Government Lawyer','Corporate (in house counsel)',
                                     'Big Law Firm (top 100)','Solo-practice','Law Professor',
                                     'Partner','Prosecutor/District Attorney','Public Defender',
                                     'Top 14 Law School','$>100$ Ranked Law School',
                                     'CD Dem. Pres. Vote Share'))
writeLines(star,'tables/table_2.tex')



##
##F-TEST for EXCLUSION RESTRICTION
##
f1 <- glm(inCF ~ num.elected.execs + female+
              lyear + I(lyear^2) +  gov + corp+ is.big.law + solo.practice + law.prof +
              partner +  prosecutor.or.da + public.defender + top.14 + not.top.100,
             
          data=lawyers.all,family = binomial(link = "probit"))
drop1(f1,test='F')

 
f2 <- glm(inCF ~  num.elected.execs + female + lyear + I(lyear^2) +  gov + corp+ is.big.law + solo.practice + law.prof +
          partner +  prosecutor.or.da + public.defender + top.14 + not.top.100 + cd.pvs,
          data=lawyers.all,family = binomial(link = "probit"))
drop1(f2,test='F')

rm(m0,m1,f1,f2);gc()

########################################
## Doing some K-S tests
########################################
ks.test(lawyers.all$cfscore, lawyers.all$cfscore[lawyers.all$is.judge==1])
ks.test(lawyers.all$cfscore, lawyers.all$cfscore[which(lawyers.all$judge.hierarchy == "1")])
ks.test(lawyers.all$cfscore, lawyers.all$cfscore[which(lawyers.all$judge.hierarchy == "2")])
ks.test(lawyers.all$cfscore, lawyers.all$cfscore[which(lawyers.all$judge.hierarchy == "3")])
ks.test(lawyers.all$cfscore, lawyers.all$cfscore[which(lawyers.all$judge.hierarchy == "4")])

ks.test(lawyers.all$cfscore[which(lawyers.all$judge.hierarchy == "1")],
        lawyers.all$cfscore[which(lawyers.all$judge.hierarchy == "4")])
ks.test(lawyers.all$cfscore[which(lawyers.all$judge.hierarchy == "1")],
        lawyers.all$cfscore[which(lawyers.all$judge.hierarchy == "2")])
ks.test(lawyers.all$cfscore[which(lawyers.all$judge.hierarchy == "3")],
        lawyers.all$cfscore[which(lawyers.all$judge.hierarchy == "4")])
 

########################################
##t-tests give the same results...
########################################
t.test(lawyers.all$cfscore, lawyers.all$cfscore[lawyers.all$is.judge==1])
t.test(lawyers.all$cfscore, lawyers.all$cfscore[which(lawyers.all$judge.hierarchy == "1")])
t.test(lawyers.all$cfscore, lawyers.all$cfscore[which(lawyers.all$judge.hierarchy == "2")])
t.test(lawyers.all$cfscore, lawyers.all$cfscore[which(lawyers.all$judge.hierarchy == "3")])
t.test(lawyers.all$cfscore, lawyers.all$cfscore[which(lawyers.all$judge.hierarchy == "4")])

t.test(lawyers.all$cfscore[which(lawyers.all$judge.hierarchy == "1")],
        lawyers.all$cfscore[which(lawyers.all$judge.hierarchy == "4")])
t.test(lawyers.all$cfscore[which(lawyers.all$judge.hierarchy == "1")],
        lawyers.all$cfscore[which(lawyers.all$judge.hierarchy == "2")])
t.test(lawyers.all$cfscore[which(lawyers.all$judge.hierarchy == "3")],
        lawyers.all$cfscore[which(lawyers.all$judge.hierarchy == "4")])


###############################################################################
##JUDGE ANALYSIS
###############################################################################
lawyers.all$st.dummies <- lawyers.all$state
lawyers.all$inCF <- ifelse(!is.na(lawyers.all$cfscore),1,0)
lawyers.all$is.judge[lawyers.all$fed.mag ==1 | lawyers.all$fed.admin==1 | lawyers.all$state.admin==1] <- 0

##MODEL 1
aa1 <- heckit(##selection
              inCF ~ is.judge +  fed.mag + fed.admin + state.admin +female +
                  lyear + I(lyear^2) +
                  top.14 + not.top.100 +  num.elected.execs,
              ##outcome
              cfscore ~ is.judge  + fed.mag + fed.admin + state.admin + female +
                  lyear + I(lyear^2) +
                  top.14 + not.top.100 ,
              data = lawyers.all, method='2step')
summary(aa1)
 
##MODEL 2
aa2 <- heckit(##selection
              inCF ~ is.judge  +  fed.mag +  fed.admin + state.admin +female + lyear + I(lyear^2) +
              top.14 + not.top.100 +  st.dummies,
              ##outcome
              cfscore ~ is.judge + fed.mag + fed.admin +  state.admin + female + lyear + I(lyear^2) +
              top.14 + not.top.100 + st.dummies,
              data = lawyers.all, method='2step')
summary(aa2)
 


##MODEL 3
aa3 <- heckit(##selection
              inCF ~ fed.circuit+ fed.district +  state.higher + state.lower + fed.mag + fed.admin + state.admin + 
                  female + lyear + I(lyear^2) + top.14 + not.top.100 + 
                          num.elected.execs,
              ##outcome
              cfscore ~ fed.circuit+ fed.district + state.higher + state.lower + fed.mag +  fed.admin + state.admin + 
                  female +  lyear + I(lyear^2) +  top.14 + not.top.100,
    data = lawyers.all,
    method='2step',
    ys=TRUE,yo=TRUE)
summary(aa3)
 

##MODEL 4
aa4 <- heckit(##selection
    inCF ~ fed.circuit+ fed.district + state.higher + state.lower + fed.mag +  fed.admin + state.admin + 
              female + lyear + I(lyear^2) +
              top.14 + not.top.100 + 
              st.dummies,
              ##outcome
              cfscore ~ fed.circuit+ fed.district + state.higher + state.lower + fed.mag +  fed.admin + state.admin + 
              female + lyear + I(lyear^2) +
              top.14 + not.top.100+
              st.dummies,
              data = lawyers.all, method='2step')
summary(aa4)



##MODEL 2

##STAGE 1 TABLE
sx <- stargazer(aa1$probit, aa2$probit, aa3$probit, aa4$probit,
                omit = "st.dummies", digits = 3, style = "ajps",
                star.cutoffs = c(0.05, 0.01),
                font.size = "footnotesize",
                title = "Probit regression, whether individual contributes (is in DIME database) as outcome variable.",
                label = "t:judge_probit",
                covariate.labels=c('Constant',
                                   'Judge',
                                   'Fed. CoA',
                                   'Fed. District Court',
                                   'State Higher Court',
                                   'State Lower Court',
                                   'Fed. Mag.',
                                   'Fed. Admin. Judge',
                                   'State Admin. Judge',
                                   'Female',
                                   'Years since Admitted',
                                   'Years since Admitted$^2$',
                                   'Top 14 Law School',
                                   '\\textgreater100 Ranked Law School',
                                   'Num Elected Execs'
                                   ))
cat(paste0(sx,'\n'),file='tables/table_A1.tex')


##STAGE 2 TABLE
sx <- stargazer(aa1, aa2, aa3, aa4,
                omit = "st.dummies", digits = 3, style = "ajps",
                star.cutoffs = c(0.05, 0.01),
                font.size = "footnotesize",
                title = "Second-stage Results: OLS, Contributor DIME score as outcome variable",
                label = "t:judge_outcome",
                covariate.labels=c('Judge',
                                   'Fed. CoA',
                                   'Fed. District Court',
                                   'State Higher Court',
                                   'State Lower Court',
                                   'Fed. Mag.',
                                   'Fed. Admin. Judge',
                                   'State Admin. Judge',
                                   'Female','Years since Admitted','Years since Admitted$^2$',
                                   'Top 14 Law School','\\textgreater100 Ranked Law School'
                                   ))
cat(paste0(sx,'\n'),file='tables/table_3.tex')


rm(aa1,aa2,aa3,aa4);gc()



###############################################################################
##F-TEST for EXCLUSION RESTRICTION
###############################################################################
f1 <- glm(inCF ~ female + lyear + I(lyear^2) +  gov + corp+ is.big.law + solo.practice + law.prof +
          partner +  prosecutor.or.da + public.defender + top.14 + not.top.100 + num.elected.execs,
          data=lawyers.all,family = binomial(link = "probit"))
drop1(f1,test='F')
f2 <- glm(inCF ~ fed.admin + state.admin + fed.mag +  state.lower + state.higher + fed.district + fed.circuit+
              female + lyear + I(lyear^2) + top.14 + not.top.100 +
              num.elected.execs,
          data=lawyers.all,family = binomial(link = "probit"))
drop1(f2,test='F')
rm(f1,f2);gc()


###############################################################################
##RESULTS FOR UNCORRECTED MODEL FOR APPENDIX
###############################################################################
lm0 <- lm(cfscore ~ is.judge + fed.admin + state.admin + fed.mag + female + lyear + I(lyear^2) +
          top.14 + not.top.100 , data = lawyers.all)

lm1 <- lm(cfscore ~ is.judge + fed.admin +  state.admin + fed.mag +  female + lyear + I(lyear^2) +
          top.14 + not.top.100  + st.dummies, data = lawyers.all)

lm2 <- lm(cfscore ~ fed.admin + state.admin + fed.mag + state.lower + state.higher + fed.district +
          fed.circuit+ female + lyear + I(lyear^2) + top.14 + not.top.100 ,
          data = lawyers.all)
 
lm3 <- lm(cfscore ~ fed.admin + state.admin + fed.mag + state.lower + state.higher + fed.district +
          fed.circuit+ female + lyear + I(lyear^2) + top.14 + not.top.100 + st.dummies,
          data = lawyers.all)


star <- stargazer(lm0, lm1,lm2,lm3, omit = "st.dummies", digits = 3, style = "ajps",
                  star.cutoffs = c(0.05, 0.01),
                  title = "Model Results Without Selection Bias Correction: OLS, Contributor DIME score as outcome variable",
                  font.size = "footnotesize",
                  label = "t:outcome",
                  covariate.labels=c('Judge',
                                     'Fed. Admin. Judge','State Admin. Judge',
                                     'Fed. Mag.',
                                     'State Lower Courts','State High Courts','Fed. District Courts','Fed. CoA',
                                     'Female','Years since Admitted','Years since Admitted$^2$',
                                     'Top 14 Law School',
                                     '\\textgreater100 Ranked Law School',
                                     'Constant'
                                     ),
                  summary=FALSE)
writeLines(star,'tables/table_A4.tex')

rm(lm1,lm2,lm3,lm4);gc()



###############################################################################
##JUDGE ANALYSIS--15 years out
###############################################################################

##MODEL 1
aaa1 <- heckit(##selection
              inCF ~ is.judge + fed.admin + state.admin + fed.mag +  female +
                  lyear + I(lyear^2) +
                      top.14 + not.top.100 +  num.elected.execs,
              ##outcome
              cfscore ~ is.judge + fed.admin + state.admin + fed.mag + female +
                  lyear + I(lyear^2) +
                      top.14 + not.top.100 ,
              data = lawyers.all[lawyers.all$lyear>=15,], method='2step')
summary(aaa1)

##MODEL 2
aaa2 <- heckit(##selection
              inCF ~ is.judge + fed.admin + state.admin + fed.mag +  female + lyear + I(lyear^2) +
              top.14 + not.top.100 +  st.dummies,
              ##outcome
              cfscore ~ is.judge + fed.admin +  state.admin + fed.mag + female + lyear + I(lyear^2) +
              top.14 + not.top.100 + st.dummies,
    data = lawyers.all[lawyers.all$lyear>=15,], method='2step')
summary(aaa2)
 

##MODEL 3
aaa3 <- heckit(##selection
              inCF ~ fed.admin + state.admin + fed.mag +  state.lower + state.higher + fed.district + fed.circuit+
                  female + lyear + I(lyear^2) + top.14 + not.top.100 + 
                          num.elected.execs,
              ##outcome
              cfscore ~ fed.admin + state.admin + fed.mag +  state.lower + state.higher + fed.district + fed.circuit+
                  female +  lyear + I(lyear^2) +  top.14 + not.top.100,
    data = lawyers.all[lawyers.all$lyear>=15,],
    method='2step')
summary(aaa3)


##MODEL 4
aaa4 <- heckit(##selection
              inCF ~ fed.admin + state.admin + fed.mag +  state.lower + state.higher + fed.district + fed.circuit+
              female + lyear + I(lyear^2) +
              top.14 + not.top.100 + 
              st.dummies,
              ##outcome
              cfscore ~ fed.admin + state.admin + fed.mag +  state.lower + state.higher + fed.district + fed.circuit+
              female + lyear + I(lyear^2) +
              top.14 + not.top.100+
              st.dummies,
    data = lawyers.all[lawyers.all$lyear>=15,], method='2step')
summary(aaa4)


##STAGE 1 TABLE
star <- stargazer(aaa1$probit, aaa2$probit, aaa3$probit, aaa4$probit,
                  star.cutoffs = c(0.05, 0.01),
                  omit = "st.dummies", digits = 3, style = "ajps",
                  font.size = "footnotesize",
                  title = "Probit regression, whether individual contributes (is in DIME database) as outcome variable (>= 15 Years since Bar Admission)",
                  label = "t:judge_probit_15y",
                  covariate.labels=c('Constant','Judge',
                                     'Fed. Admin. Judge','State Admin. Judge', 'Fed. Mag.',
                                     'State Lower Court','State Higher Court',
                                     'Fed. District Court','Fed. CoA',
                                     'Female','Years since Admitted','Years since Admitted$^2$',
                                     'Top 14 Law School',
                                     '\\textgreater100 Ranked Law School',
                                     'Num Elected Execs'
                                     ),
                  summary=FALSE)
writeLines(star,'tables/table_A5.tex')


##STAGE 2 TABLE
star <- stargazer(aaa1, aaa2, aaa3, aaa4,
                  star.cutoffs = c(0.05, 0.01),
                  omit = "st.dummies", digits = 3, style = "ajps",
                  font.size = "footnotesize",
                  title = "Second-stage Results: OLS, Contributor DIME score as outcome variable (>= 15 Years since Bar Admission)",
                  label = "t:judge_outcome_15y",
                  covariate.labels=c('Judge',
                                     'Fed. Admin. Judge','State Admin. Judge',
                                     'Fed. Mag.',
                                     'State Lower Courts','State High Courts','Fed. District Courts','Fed. CoA',
                                     'Female','Years since Admitted','Years since Admitted$^2$',
                                     'Top 14 Law School',
                                     '\\textgreater100 Ranked Law School',
                                     'Constant'
                                     ))
writeLines(star,'tables/table_A6.tex')



###############################################################################
##EXCLUDING SETS OF RECIPIENTS WHEN ESTIMATING DIME SCORES
###############################################################################

##ALL RECIPIENTS
lawyers.all$inCF <- as.numeric(!is.na(lawyers.all$cfscore))
ccc1 <- heckit(##selection
    inCF ~ fed.circuit+ fed.district + state.higher + state.lower + fed.mag +  fed.admin + state.admin + 
              female + lyear + I(lyear^2) +
              top.14 + not.top.100 + 
              st.dummies,
              ##outcome
              cfscore ~ fed.circuit+ fed.district + state.higher + state.lower + fed.mag +  fed.admin + state.admin + 
              female + lyear + I(lyear^2) +
              top.14 + not.top.100+
              st.dummies,
              data = lawyers.all, method='2step')
summary(ccc1)

##NO JUDGES
lawyers.all$inCF <- as.numeric(!is.na(lawyers.all$cfscore.no.judges))
ccc2 <- heckit(##selection
               inCF ~ fed.circuit+ fed.district + state.higher + state.lower + fed.mag +  fed.admin + state.admin + 
                   female + lyear + I(lyear^2) +
                   top.14 + not.top.100 + 
                   st.dummies,
                ##outcome
                cfscore.no.judges ~ fed.circuit+ fed.district + state.higher + state.lower + fed.mag +  fed.admin + state.admin + 
                    female + lyear + I(lyear^2) +
                    top.14 + not.top.100+
                    st.dummies,
    data = lawyers.all, method='2step')
summary(ccc2)

##FED ONLY
lawyers.all$inCF <- as.numeric(!is.na(lawyers.all$cfscore.fed.only))
ccc3 <- heckit(##selection
               inCF ~ fed.circuit+ fed.district + state.higher + state.lower + fed.mag +  fed.admin + state.admin + 
                   female + lyear + I(lyear^2) +
                   top.14 + not.top.100 + 
                   st.dummies,
                ##outcome
                cfscore.fed.only ~ fed.circuit+ fed.district + state.higher + state.lower + fed.mag +  fed.admin + state.admin + 
                    female + lyear + I(lyear^2) +
                    top.14 + not.top.100+
                    st.dummies,
    data = lawyers.all, method='2step')
summary(ccc3)

sx <- stargazer(ccc1, ccc2, ccc3,
                omit = "st.dummies", digits = 3, style = "ajps",
                star.cutoffs = c(0.05, 0.01),
                font.size = "footnotesize",
                title = "Second-stage Results: OLS, Contributor DIME score  as outcome variable",
                label = "t:judge_outcome",
                covariate.labels=c('Fed. CoA',
                                   'Fed. District Court',
                                   'State Higher Court',
                                   'State Lower Court',
                                   'Fed. Mag.',
                                   'Fed. Admin. Judge',
                                   'State Admin. Judge',
                                   'Female','Years since Admitted','Years since Admitted$^2$',
                                   'Top 14 Law School',
                                   '\\textgreater100 Ranked Law School'
                                   ))
cat(paste0(sx,'\n'),file='tables/table_A8.tex')



###############################################################################
##JUDGE ANALYSIS--PARTY INDICATOR
###############################################################################

##INCLUDE BINARY SPECIFICATION FOR APPENDIX?
##Note: This can take several hours to run...  
run.binary.spec <- FALSE

if(run.binary.spec){
    lawyers.all$inCF <- as.numeric(!is.na(lawyers.all$cfscore))
    
    ##MODEL 1
    bbb1 <- selection(##selection
        inCF ~ is.judge + fed.mag + fed.admin + state.admin +  female +
            lyear + I(lyear^2) +
            top.14 + not.top.100 +  num.elected.execs,
        ##outcome
        I(cfscore>0) ~ is.judge + fed.mag + fed.admin + state.admin + female +
            lyear + I(lyear^2) +
            top.14 + not.top.100 ,
        data = lawyers.all)
    summary(bbb1)

    
    ##MODEL 2
    bbb2 <- selection(##selection
        inCF ~ is.judge + fed.mag + fed.admin + state.admin +   female + lyear + I(lyear^2) +
            top.14 + not.top.100 +  st.dummies,
        ##outcome
        I(cfscore>0) ~ is.judge + fed.mag + fed.admin +  state.admin  + female + lyear + I(lyear^2) +
            top.14 + not.top.100 + st.dummies,
        data = lawyers.all)
    summary(bbb2)

    ##MODEL 3
    bbb3 <- selection(##selection
        inCF ~ fed.circuit+ fed.district + state.higher + state.lower + fed.mag +  fed.admin + state.admin +
            female + lyear + I(lyear^2) + top.14 + not.top.100 + 
            num.elected.execs,
        ##outcome
        I(cfscore>0) ~  fed.circuit+ fed.district + state.higher + state.lower + fed.mag +  fed.admin + state.admin + 
            female +  lyear + I(lyear^2) +  top.14 + not.top.100,
        data = lawyers.all)
    summary(bbb3)

    ##MODEL 4
    bbb4 <- selection(##selection
        inCF ~ fed.circuit+ fed.district + state.higher + state.lower + fed.mag +  fed.admin + state.admin +
            female + lyear + I(lyear^2) +
            top.14 + not.top.100 + 
            st.dummies,
        ##outcome
        I(cfscore>0) ~  fed.circuit+ fed.district + state.higher + state.lower + fed.mag +  fed.admin + state.admin + 
            female +  lyear + I(lyear^2) +  top.14 + not.top.100 + st.dummies,
        data = lawyers.all)
    summary(bbb4)


    ##STAGE 2 TABLE
    star <- stargazer(bbb1, bbb2, bbb3, bbb4,
                      omit = "st.dummies", digits = 3, style = "ajps",
                      star.cutoffs = c(0.05, 0.01),
                      font.size = "footnotesize",
                      title = "Second-stage Results: Binary Indicator for Donor is Conservative (DIME score > 0) as outcome variable",
                      label = "t:judge_outcome_binary",
                      covariate.labels=c('Judge',
                                         'Fed. CoA',
                                         'Fed. District Court',
                                         'State Higher Court',
                                         'State Lower Court',
                                         'Fed. Mag.',
                                         'Fed. Admin. Judge',
                                         'State Admin. Judge',
                                         'Female','Years since Admitted','Years since Admitted$^2$',
                                         'Top 14 Law School','\\textgreater Ranked Law School'
                                         ))
    writeLines(star,'tables/table_A7.tex')
}
