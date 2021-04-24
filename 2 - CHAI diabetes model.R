setwd("~/Downloads")
rm(list=ls())
library(tidyverse)
library(tableone)
load("dfimp.RData")
cost = read_csv("cost.csv")
cnt_reg = read_csv("cnt_reg.csv")

dfimp = left_join(dfimp,cost,by="Country")
dfimp = dfimp %>%
  filter(Country!="Swaziland" & Country!="South Africa DHS") 

dfrep = do.call("rbind", replicate(50, dfimp, simplify = FALSE))


dxdf = dfrep %>%
  mutate(detected=(und_dia==0)) %>%
  group_by(Country) %>%
  mutate(positionInCategory = 1:n(),
         current_level = sum(und_dia==0)/n()) %>%
  ungroup()


start_time <- Sys.time()

screens = 1
screening_goal = ifelse(screens==1,0,ifelse(screens==2,0.6,0.8))
newscreening  = (screening_goal-dxdf$current_level)
newscreening[newscreening<0]= 0

dxdf$newlydetected = 1*(dxdf$detected==1)+newscreening*(dxdf$detected==0)


ncon  = length(table(dxdf$Country))

outmat_cvdevents = NULL
outmat_chfevents = NULL
outmat_nephevents = NULL
outmat_retinevents = NULL
outmat_neuroevents = NULL
outmat_hypogevents = NULL
outmat_bmievents = NULL
outmat_genitevents = NULL
outmat_dkaevents = NULL
outmat_ampevents = NULL
outmat_gidevents = NULL

outmat_cvddeaths = NULL
outmat_chfdeaths = NULL
outmat_nephdeaths = NULL
outmat_retindeaths = NULL
outmat_neurodeaths = NULL
outmat_hypogdeaths = NULL
outmat_bmideaths = NULL
outmat_genitdeaths = NULL
outmat_dkadeaths = NULL
outmat_ampdeaths = NULL
outmat_giddeaths = NULL

outmat_rxbpcosts = NULL
outmat_rxdmcosts = NULL
outmat_rxstatincosts = NULL

outmat_cvdcosts = NULL
outmat_chfcosts = NULL
outmat_nephcosts = NULL
outmat_retincosts = NULL
outmat_neurocosts = NULL
outmat_hypogcosts = NULL
outmat_bmicosts = NULL
outmat_genitcosts = NULL
outmat_dkacosts = NULL
outmat_ampcosts = NULL
outmat_gidcosts = NULL

outmat_cvddalys = NULL
outmat_chfdalys = NULL
outmat_nephdalys = NULL
outmat_retindalys = NULL
outmat_neurodalys = NULL
outmat_hypogdalys = NULL
outmat_bmidalys = NULL
outmat_genitdalys = NULL
outmat_dkadalys = NULL
outmat_ampdalys = NULL
outmat_giddalys = NULL

K = 1
C = 0.1658 
r = 0.03
D_cvd = mean(c(0.422,0.056,0.021,0.076,0.312,0.539,0.567)) 
D_chf = mean(c(0.037, 0.070, 0.186)) 
D_neph = mean(c(0.573, 0.105)) 
D_retin = mean(c(0.195)) 
D_neuro = mean(c(0.099)) 
D_hypog = mean(c(0.05))
D_bmi = mean(c(0.01))
D_genit = mean(c(0.06))
D_dka = mean(c(0.06))
D_amp = mean(c(0.09))
D_gid = mean(c(0.07))
b = 0.04

iters = 10000
 for (iter in 1:iters) {
  print(paste("On iteration", iter))
  
  
  
  dxdf$rxbpcosts_base = 1.5*10*dxdf$bprx*dxdf$rxbp_cost*dxdf$detected/(1.03^10)
  dxdf$rxdmcosts_base = 1.5*10*dxdf$oralrx*dxdf$rxdm_cost*dxdf$detected/(1.03^10)
  dxdf$rxstatincosts_base =10*(as.numeric(dxdf$statin)-1)*dxdf$rxstatin_cost*dxdf$detected/(1.03^10)
  
  
  set.seed(iter)
  dxdf$cvdevents_base = rbinom(length(dxdf$cvdrisk),1,dxdf$cvdrisk/100)
  dxdf$cvdcosts_base = dxdf$cvdevents_base*dxdf$cvdevents_cost/(1.03^10)
  dxdf$cvddeaths_base = dxdf$cvdevents_base*0.2 #20% case fatality, https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3819990/
  
  set.seed(iter)
  dxdf$chfevents_base = rbinom(length(dxdf$chfrisk),1,dxdf$chfrisk/100)
  dxdf$chfcosts_base = dxdf$chfevents_base*dxdf$chfevents_cost/(1.03^10)
  dxdf$chfdeaths_base = dxdf$chfevents_base*0.5 #50% case fatality at 5 years, https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3033496/
  
  set.seed(iter)
  dxdf$nephevents_base = rbinom(length(dxdf$nephrisk),1,dxdf$nephrisk/100)
  dxdf$nephcosts_base = dxdf$nephevents_base*dxdf$nephevents_cost/(1.03^10)
  dxdf$nephdeaths_base = dxdf$nephevents_base*1 #100% case fatality
  
  set.seed(iter)
  dxdf$retinevents_base = rbinom(length(dxdf$retinrisk),1,dxdf$retinrisk/100)
  dxdf$retincosts_base = dxdf$retinevents_base*dxdf$retinevents_cost/(1.03^10)
  dxdf$retindeaths_base = dxdf$retinevents_base*0 
  
  set.seed(iter)
  dxdf$neuroevents_base = rbinom(length(dxdf$neurorisk),1,dxdf$neurorisk/100)
  dxdf$neurocosts_base = dxdf$neuroevents_base*dxdf$neuroevents_cost/(1.03^10)
  dxdf$neurodeaths_base = dxdf$neuroevents_base*0.1 #10% case fatality at 10 yrs, https://care.diabetesjournals.org/content/28/3/617#T4
  
  set.seed(iter)
  dxdf$hypogevents_base = rbinom(length(dxdf$hypogrisk),1,dxdf$hypogrisk/100)
  dxdf$hypogcosts_base = dxdf$hypogevents_base*dxdf$hypogevents_cost/(1.03^10)
  dxdf$hypogdeaths_base = dxdf$hypogevents_base*0.25 # 25 % case fatality at 3-5 yrs, https://pubmed.ncbi.nlm.nih.gov/29127240/ and https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3425013/
  
  set.seed(iter)
  dxdf$bmievents_base = dxdf$bmirisk #kg/m^2 bmi gain/loss
  dxdf$bmicosts_base = dxdf$bmievents_base*dxdf$bmievents_cost/(1.03^10)
  dxdf$bmideaths_base = dxdf$bmievents_base*0
  
  set.seed(iter)
  dxdf$genitevents_base = rbinom(length(dxdf$genitrisk),1,dxdf$genitrisk/100)
  dxdf$genitcosts_base = dxdf$genitevents_base*dxdf$genitevents_cost/(1.03^10)
  dxdf$genitdeaths_base = dxdf$genitevents_base*0.02  #2% case fatality, https://www.cdc.gov/infectioncontrol/guidelines/cauti/background.html
  
  set.seed(iter)
  dxdf$dkaevents_base = rbinom(length(dxdf$dkarisk),1,dxdf$dkarisk/100)
  dxdf$dkacosts_base = dxdf$dkaevents_base*dxdf$dkaevents_cost/(1.03^10)
  dxdf$dkadeaths_base = dxdf$dkaevents_base*0.04 # 4% case fatality,  https://pubmed.ncbi.nlm.nih.gov/8485963/
  
  set.seed(iter)
  dxdf$ampevents_base = rbinom(length(dxdf$amprisk),1,dxdf$amprisk/100)
  dxdf$ampcosts_base = dxdf$ampevents_base*dxdf$ampevents_cost/(1.03^10)
  dxdf$ampdeaths_base = dxdf$ampevents_base*0.1  #10% case fatality, https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3329303/ and http://www.diabetesincontrol.com/death-after-lower-limb-amputation/
  
  set.seed(iter)
  dxdf$gidevents_base = rbinom(length(dxdf$gidrisk),1,dxdf$gidrisk/100)
  dxdf$gidcosts_base = dxdf$gidevents_base*dxdf$gidevents_cost/(1.03^10)
  dxdf$giddeaths_base = dxdf$gidevents_base*0 
  
  
  
  
  
  
  dxdf$a = dxdf$age+5
  dxdf$L = 57.44-dxdf$age
  dxdf$L[dxdf$L<0]=5
  
  dxdf$YLL_cvd = ((K*C*(exp(r*dxdf$a)))/((r+b)^2))*(((exp(-(r+b)*(dxdf$L+dxdf$a)))*(-(r+b)*(dxdf$L+dxdf$a)-1))-
                                                      (exp(-(r+b)*dxdf$a))*(-(r+b)*dxdf$a-1))+(((1-K)/r)*(1-exp(-r*dxdf$L)))
  dxdf$YLD_cvd = D_cvd*(((K*C*(exp(r*dxdf$a)))/((r+b)^2))*(((exp(-(r+b)*(dxdf$L+dxdf$a)))*(-(r+b)*(dxdf$L+dxdf$a)-1))-
                                                             (exp(-(r+b)*dxdf$a))*(-(r+b)*dxdf$a-1))+(((1-K)/r)*(1-exp(-r*dxdf$L))))
  dxdf$DALYS_cvd_base = (dxdf$cvddeaths_base*dxdf$YLL_cvd+dxdf$cvdevents_base*dxdf$YLD_cvd)/(1.03^10)
  
  
  dxdf$YLL_chf = ((K*C*(exp(r*dxdf$a)))/((r+b)^2))*(((exp(-(r+b)*(dxdf$L+dxdf$a)))*(-(r+b)*(dxdf$L+dxdf$a)-1))-
                                                      (exp(-(r+b)*dxdf$a))*(-(r+b)*dxdf$a-1))+(((1-K)/r)*(1-exp(-r*dxdf$L)))
  dxdf$YLD_chf = D_chf*(((K*C*(exp(r*dxdf$a)))/((r+b)^2))*(((exp(-(r+b)*(dxdf$L+dxdf$a)))*(-(r+b)*(dxdf$L+dxdf$a)-1))-
                                                             (exp(-(r+b)*dxdf$a))*(-(r+b)*dxdf$a-1))+(((1-K)/r)*(1-exp(-r*dxdf$L))))
  dxdf$DALYS_chf_base = (dxdf$chfdeaths_base*dxdf$YLL_chf+dxdf$chfevents_base*dxdf$YLD_chf)/(1.03^10)
  
  dxdf$YLL_neph = ((K*C*(exp(r*dxdf$a)))/((r+b)^2))*(((exp(-(r+b)*(dxdf$L+dxdf$a)))*(-(r+b)*(dxdf$L+dxdf$a)-1))-
                                                       (exp(-(r+b)*dxdf$a))*(-(r+b)*dxdf$a-1))+(((1-K)/r)*(1-exp(-r*dxdf$L)))
  dxdf$YLD_neph = D_neph*(((K*C*(exp(r*dxdf$a)))/((r+b)^2))*(((exp(-(r+b)*(dxdf$L+dxdf$a)))*(-(r+b)*(dxdf$L+dxdf$a)-1))-
                                                               (exp(-(r+b)*dxdf$a))*(-(r+b)*dxdf$a-1))+(((1-K)/r)*(1-exp(-r*dxdf$L))))
  dxdf$DALYS_neph_base = (dxdf$nephdeaths_base*dxdf$YLL_neph+dxdf$nephevents_base*dxdf$YLD_neph)/(1.03^10)
  
  
  dxdf$YLL_retin = ((K*C*(exp(r*dxdf$a)))/((r+b)^2))*(((exp(-(r+b)*(dxdf$L+dxdf$a)))*(-(r+b)*(dxdf$L+dxdf$a)-1))-
                                                        (exp(-(r+b)*dxdf$a))*(-(r+b)*dxdf$a-1))+(((1-K)/r)*(1-exp(-r*dxdf$L)))
  dxdf$YLD_retin = D_retin*(((K*C*(exp(r*dxdf$a)))/((r+b)^2))*(((exp(-(r+b)*(dxdf$L+dxdf$a)))*(-(r+b)*(dxdf$L+dxdf$a)-1))-
                                                                 (exp(-(r+b)*dxdf$a))*(-(r+b)*dxdf$a-1))+(((1-K)/r)*(1-exp(-r*dxdf$L))))
  dxdf$DALYS_retin_base = (dxdf$retindeaths_base*dxdf$YLL_retin+dxdf$retinevents_base*dxdf$YLD_retin)/(1.03^10)
  
  dxdf$YLL_neuro = ((K*C*(exp(r*dxdf$a)))/((r+b)^2))*(((exp(-(r+b)*(dxdf$L+dxdf$a)))*(-(r+b)*(dxdf$L+dxdf$a)-1))-
                                                        (exp(-(r+b)*dxdf$a))*(-(r+b)*dxdf$a-1))+(((1-K)/r)*(1-exp(-r*dxdf$L)))
  dxdf$YLD_neuro = D_neuro*(((K*C*(exp(r*dxdf$a)))/((r+b)^2))*(((exp(-(r+b)*(dxdf$L+dxdf$a)))*(-(r+b)*(dxdf$L+dxdf$a)-1))-
                                                                 (exp(-(r+b)*dxdf$a))*(-(r+b)*dxdf$a-1))+(((1-K)/r)*(1-exp(-r*dxdf$L))))
  dxdf$DALYS_neuro_base = (dxdf$neurodeaths_base*dxdf$YLL_neuro+dxdf$neuroevents_base*dxdf$YLD_neuro)/(1.03^10)
  
  
  dxdf$YLL_hypog = ((K*C*(exp(r*dxdf$a)))/((r+b)^2))*(((exp(-(r+b)*(dxdf$L+dxdf$a)))*(-(r+b)*(dxdf$L+dxdf$a)-1))-
                                                        (exp(-(r+b)*dxdf$a))*(-(r+b)*dxdf$a-1))+(((1-K)/r)*(1-exp(-r*dxdf$L)))
  dxdf$YLD_hypog = D_hypog*(2/365) # 2 day disability duration
  dxdf$DALYS_hypog_base = (dxdf$hypogdeaths_base*dxdf$YLL_hypog+dxdf$hypogevents_base*dxdf$YLD_hypog)/(1.03^10)
  
  dxdf$YLL_bmi = ((K*C*(exp(r*dxdf$a)))/((r+b)^2))*(((exp(-(r+b)*(dxdf$L+dxdf$a)))*(-(r+b)*(dxdf$L+dxdf$a)-1))-
                                                      (exp(-(r+b)*dxdf$a))*(-(r+b)*dxdf$a-1))+(((1-K)/r)*(1-exp(-r*dxdf$L)))
  dxdf$YLD_bmi = D_bmi*(((K*C*(exp(r*dxdf$a)))/((r+b)^2))*(((exp(-(r+b)*(dxdf$L+dxdf$a)))*(-(r+b)*(dxdf$L+dxdf$a)-1))-
                                                             (exp(-(r+b)*dxdf$a))*(-(r+b)*dxdf$a-1))+(((1-K)/r)*(1-exp(-r*dxdf$L))))
  dxdf$DALYS_bmi_base = (dxdf$bmideaths_base*dxdf$YLL_bmi+dxdf$bmievents_base*dxdf$YLD_bmi)/(1.03^10)
  
  
  dxdf$YLL_genit = ((K*C*(exp(r*dxdf$a)))/((r+b)^2))*(((exp(-(r+b)*(dxdf$L+dxdf$a)))*(-(r+b)*(dxdf$L+dxdf$a)-1))-
                                                        (exp(-(r+b)*dxdf$a))*(-(r+b)*dxdf$a-1))+(((1-K)/r)*(1-exp(-r*dxdf$L)))
  dxdf$YLD_genit = D_genit*(5/365) # 5 day disability duration
  dxdf$DALYS_genit_base = (dxdf$genitdeaths_base*dxdf$YLL_genit+dxdf$genitevents_base*dxdf$YLD_genit)/(1.03^10)
  
  
  dxdf$YLL_dka = ((K*C*(exp(r*dxdf$a)))/((r+b)^2))*(((exp(-(r+b)*(dxdf$L+dxdf$a)))*(-(r+b)*(dxdf$L+dxdf$a)-1))-
                                                      (exp(-(r+b)*dxdf$a))*(-(r+b)*dxdf$a-1))+(((1-K)/r)*(1-exp(-r*dxdf$L)))
  dxdf$YLD_dka = D_dka*(5/365) # 5 day disability duration
  dxdf$DALYS_dka_base = (dxdf$dkadeaths_base*dxdf$YLL_dka+dxdf$dkaevents_base*dxdf$YLD_dka)/(1.03^10)
  
  dxdf$YLL_amp = ((K*C*(exp(r*dxdf$a)))/((r+b)^2))*(((exp(-(r+b)*(dxdf$L+dxdf$a)))*(-(r+b)*(dxdf$L+dxdf$a)-1))-
                                                      (exp(-(r+b)*dxdf$a))*(-(r+b)*dxdf$a-1))+(((1-K)/r)*(1-exp(-r*dxdf$L)))
  dxdf$YLD_amp = D_amp*(((K*C*(exp(r*dxdf$a)))/((r+b)^2))*(((exp(-(r+b)*(dxdf$L+dxdf$a)))*(-(r+b)*(dxdf$L+dxdf$a)-1))-
                                                             (exp(-(r+b)*dxdf$a))*(-(r+b)*dxdf$a-1))+(((1-K)/r)*(1-exp(-r*dxdf$L))))
  dxdf$DALYS_amp_base = (dxdf$ampdeaths_base*dxdf$YLL_amp+dxdf$ampevents_base*dxdf$YLD_amp)/(1.03^10)
  
  
  
  dxdf$YLL_gid = ((K*C*(exp(r*dxdf$a)))/((r+b)^2))*(((exp(-(r+b)*(dxdf$L+dxdf$a)))*(-(r+b)*(dxdf$L+dxdf$a)-1))-
                                                      (exp(-(r+b)*dxdf$a))*(-(r+b)*dxdf$a-1))+(((1-K)/r)*(1-exp(-r*dxdf$L)))
  dxdf$YLD_gid = D_gid*(5/365)*0.11 + D_gid  # 5 day disability duration for the 11% with GI distress + perpetual for those with injection
  dxdf$DALYS_gid_base = (dxdf$giddeaths_base*dxdf$YLL_gid+dxdf$gidevents_base*dxdf$YLD_gid)/(1.03^10)
  
  
  
  
  
  
  
  parsed = dxdf  %>%
    add_count(Country) %>%
    group_by(Country) %>%
    summarise(cvdevents_base = sum(cvdevents_base)/n,
              chfevents_base = sum(chfevents_base)/n,
              nephevents_base = sum(nephevents_base)/n,
              retinevents_base = sum(retinevents_base)/n,
              neuroevents_base = sum(neuroevents_base)/n,
              hypogevents_base = sum(hypogevents_base)/n,
              bmievents_base = sum(bmievents_base)/n,
              genitevents_base = sum(genitevents_base)/n,
              dkaevents_base = sum(dkaevents_base)/n,
              ampevents_base = sum(ampevents_base)/n,
              gidevents_base = sum(gidevents_base)/n,
              cvddeaths_base = sum(cvddeaths_base)/n,
              chfdeaths_base = sum(chfdeaths_base)/n,
              nephdeaths_base = sum(nephdeaths_base)/n,
              retindeaths_base = sum(retindeaths_base)/n,
              neurodeaths_base = sum(neurodeaths_base)/n,
              hypogdeaths_base = sum(hypogdeaths_base)/n,
              bmideaths_base = sum(bmideaths_base)/n,
              genitdeaths_base = sum(genitdeaths_base)/n,
              dkadeaths_base = sum(dkadeaths_base)/n,
              ampdeaths_base = sum(ampdeaths_base)/n,
              giddeaths_base = sum(giddeaths_base)/n,
              rxbpcosts_base = sum(rxbpcosts_base)/n,
              rxdmcosts_base = sum(rxdmcosts_base)/n,
              rxstatincosts_base = sum(rxstatincosts_base)/n,
              cvdcosts_base = sum(cvdcosts_base)/n,
              chfcosts_base = sum(chfcosts_base)/n,
              nephcosts_base = sum(nephcosts_base)/n,
              retincosts_base = sum(retincosts_base)/n,
              neurocosts_base = sum(neurocosts_base)/n,
              hypogcosts_base = sum(hypogcosts_base)/n,
              bmicosts_base = sum(bmicosts_base)/n,
              genitcosts_base = sum(genitcosts_base)/n,
              dkacosts_base = sum(dkacosts_base)/n,
              ampcosts_base = sum(ampcosts_base)/n,
              gidcosts_base = sum(gidcosts_base)/n,
              cvddalys_base = sum(DALYS_cvd_base)/n,
              chfdalys_base = sum(DALYS_chf_base)/n,
              nephdalys_base = sum(DALYS_neph_base)/n,
              retindalys_base = sum(DALYS_retin_base)/n,
              neurodalys_base = sum(DALYS_neuro_base)/n,
              hypogdalys_base = sum(DALYS_hypog_base)/n,
              bmidalys_base = sum(DALYS_bmi_base)/n,
              genitdalys_base = sum(DALYS_genit_base)/n,
              dkadalys_base = sum(DALYS_dka_base)/n,
              ampdalys_base = sum(DALYS_amp_base)/n,
              giddalys_base = sum(DALYS_gid_base)/n) %>%
    distinct()
  
  
  cvdevents_mat = parsed %>% select(Country, cvdevents_base)
  cvdevents_mat = as.matrix(cvdevents_mat)
  chfevents_mat = parsed %>% select(Country, chfevents_base)
  chfevents_mat = as.matrix(chfevents_mat)
  nephevents_mat = parsed %>% select(Country, nephevents_base)
  nephevents_mat = as.matrix(nephevents_mat)
  retinevents_mat = parsed %>% select(Country, retinevents_base)
  retinevents_mat = as.matrix(retinevents_mat)
  neuroevents_mat = parsed %>% select(Country, neuroevents_base)
  neuroevents_mat = as.matrix(neuroevents_mat)
  hypogevents_mat = parsed %>% select(Country, hypogevents_base)
  hypogevents_mat = as.matrix(hypogevents_mat)
  bmievents_mat = parsed %>% select(Country, bmievents_base)
  bmievents_mat = as.matrix(bmievents_mat)
  genitevents_mat = parsed %>% select(Country, genitevents_base)
  genitevents_mat = as.matrix(genitevents_mat)
  dkaevents_mat = parsed %>% select(Country, dkaevents_base)
  dkaevents_mat = as.matrix(dkaevents_mat)
  ampevents_mat = parsed %>% select(Country, ampevents_base)
  ampevents_mat = as.matrix(ampevents_mat)
  gidevents_mat = parsed %>% select(Country, gidevents_base)
  gidevents_mat = as.matrix(gidevents_mat)
  
  cvddeaths_mat = parsed %>% select(Country, cvddeaths_base)
  cvddeaths_mat = as.matrix(cvddeaths_mat)
  chfdeaths_mat = parsed %>% select(Country, chfdeaths_base)
  chfdeaths_mat = as.matrix(chfdeaths_mat)
  chfdeaths_mat = parsed %>% select(Country, chfdeaths_base)
  nephdeaths_mat = parsed %>% select(Country, nephdeaths_base)
  nephdeaths_mat = as.matrix(nephdeaths_mat)
  retindeaths_mat = parsed %>% select(Country, retindeaths_base)
  retindeaths_mat = as.matrix(retindeaths_mat)
  neurodeaths_mat = parsed %>% select(Country, neurodeaths_base)
  neurodeaths_mat = as.matrix(neurodeaths_mat)
  hypogdeaths_mat = parsed %>% select(Country, hypogdeaths_base)
  hypogdeaths_mat = as.matrix(hypogdeaths_mat)
  bmideaths_mat = parsed %>% select(Country, bmideaths_base)
  bmideaths_mat = as.matrix(bmideaths_mat)
  genitdeaths_mat = parsed %>% select(Country, genitdeaths_base)
  genitdeaths_mat = as.matrix(genitdeaths_mat)
  dkadeaths_mat = parsed %>% select(Country, dkadeaths_base)
  dkadeaths_mat = as.matrix(dkadeaths_mat)
  ampdeaths_mat = parsed %>% select(Country, ampdeaths_base)
  ampdeaths_mat = as.matrix(ampdeaths_mat)
  giddeaths_mat = parsed %>% select(Country, giddeaths_base)
  giddeaths_mat = as.matrix(giddeaths_mat)
  
  rxbpcosts_mat = parsed %>% select(Country, rxbpcosts_base)
  rxbpcosts_mat = as.matrix(rxbpcosts_mat)
  rxdmcosts_mat = parsed %>% select(Country, rxdmcosts_base)
  rxdmcosts_mat = as.matrix(rxdmcosts_mat)
  rxstatincosts_mat = parsed %>% select(Country, rxstatincosts_base)
  rxstatincosts_mat = as.matrix(rxstatincosts_mat)
  
  cvdcosts_mat = parsed %>% select(Country, cvdcosts_base)
  cvdcosts_mat = as.matrix(cvdcosts_mat)
  chfcosts_mat = parsed %>% select(Country, chfcosts_base)
  chfcosts_mat = as.matrix(chfcosts_mat)
  nephcosts_mat = parsed %>% select(Country, nephcosts_base)
  nephcosts_mat = as.matrix(nephcosts_mat)
  retincosts_mat = parsed %>% select(Country, retincosts_base)
  retincosts_mat = as.matrix(retincosts_mat)
  neurocosts_mat = parsed %>% select(Country, neurocosts_base)
  neurocosts_mat = as.matrix(neurocosts_mat)
  hypogcosts_mat = parsed %>% select(Country, hypogcosts_base)
  hypogcosts_mat = as.matrix(hypogcosts_mat)
  bmicosts_mat = parsed %>% select(Country, bmicosts_base)
  bmicosts_mat = as.matrix(bmicosts_mat)
  genitcosts_mat = parsed %>% select(Country, genitcosts_base)
  genitcosts_mat = as.matrix(genitcosts_mat)
  dkacosts_mat = parsed %>% select(Country, dkacosts_base)
  dkacosts_mat = as.matrix(dkacosts_mat)
  ampcosts_mat = parsed %>% select(Country, ampcosts_base)
  ampcosts_mat = as.matrix(ampcosts_mat)
  gidcosts_mat = parsed %>% select(Country, gidcosts_base)
  gidcosts_mat = as.matrix(gidcosts_mat)
  
  cvddalys_mat = parsed %>% select(Country, cvddalys_base)
  cvddalys_mat = as.matrix(cvddalys_mat)
  chfdalys_mat = parsed %>% select(Country, chfdalys_base)
  chfdalys_mat = parsed %>% select(Country, chfdalys_base)
  nephdalys_mat = parsed %>% select(Country, nephdalys_base)
  nephdalys_mat = as.matrix(nephdalys_mat)
  retindalys_mat = parsed %>% select(Country, retindalys_base)
  retindalys_mat = as.matrix(retindalys_mat)
  neurodalys_mat = parsed %>% select(Country, neurodalys_base)
  neurodalys_mat = as.matrix(neurodalys_mat)
  hypogdalys_mat = parsed %>% select(Country, hypogdalys_base)
  hypogdalys_mat = as.matrix(hypogdalys_mat)
  bmidalys_mat = parsed %>% select(Country, bmidalys_base)
  bmidalys_mat = as.matrix(bmidalys_mat)
  genitdalys_mat = parsed %>% select(Country, genitdalys_base)
  genitdalys_mat = as.matrix(genitdalys_mat)
  dkadalys_mat = parsed %>% select(Country, dkadalys_base)
  dkadalys_mat = as.matrix(dkadalys_mat)
  ampdalys_mat = parsed %>% select(Country, ampdalys_base)
  ampdalys_mat = as.matrix(ampdalys_mat)
  giddalys_mat = parsed %>% select(Country, giddalys_base)
  giddalys_mat = as.matrix(giddalys_mat)

  dxdf$statin = as.numeric(dxdf$statin)-1
  
for (i  in 1:8) {

    #sulfa, sglt2, glp1, dpp4, tzd, analogue, sglt2+glp1, all
    
    
      dxdf$rxbp0 = dxdf$bprx
      dxdf$rxstatin0 = dxdf$statin
      dxdf$rxdm0 = dxdf$oralrx
      
      dxdf$ins_diff_cost = (dxdf$ana_cost - dxdf$nph_cost)/1000*0.64*(dxdf$bmi*(1.66^2))*365.25  # $/1000 iu / 1000 * 0.64 IU/kg/day * kg {=bmi * m^2} * 365.25 days/yr = $/yr
      dxdf$rxdm_cost_updated =  (i==1)*dxdf$rxdm_cost+
        (i==2)*(dxdf$rxdm_cost-dxdf$sulfo_cost+dxdf$sglt_cost)+
        (i==3)*(dxdf$rxdm_cost-dxdf$sulfo_cost+dxdf$glp_cost)+
        (i==4)*(dxdf$rxdm_cost-dxdf$sulfo_cost+dxdf$dpp_cost)+
        (i==5)*(dxdf$rxdm_cost-dxdf$sulfo_cost+dxdf$tzd_cost)+
        (i==6)*(dxdf$rxdm_cost+0.10*dxdf$ins_diff_cost)+
        (i==7)*(dxdf$rxdm_cost-dxdf$sulfo_cost+dxdf$combo_cost)+
        (i==8)*(dxdf$rxdm_cost-dxdf$sulfo_cost+dxdf$combo_cost+0.035*dxdf$ins_diff_cost)  #sulfa, sglt2, glp1, dpp4, tzd, analogue, sglt2+glp1

      
      set.seed(iter)
      dxdf$rxbp = dxdf$bprx
      dxdf$rxbp_costs = 1.5*10*dxdf$rxbp*dxdf$rxbp_cost/(1.03^10) # over 10 yrs; titrated average between starting dose and max dose = 1.5*
      set.seed(iter)
      dxdf$rxdm = dxdf$oralrx
      dxdf$rxdm_costs = 1.5*10*dxdf$rxdm*dxdf$rxdm_cost_updated/(1.03^10) 
      set.seed(iter)
      dxdf$rxstatin = dxdf$statin
      dxdf$rxstatin_costs = 10*dxdf$rxstatin*dxdf$rxstatin_cost/(1.03^10)
      
      dxdf$newsbp = dxdf$sbp
      dxdf$deltasbp = (dxdf$newsbp - dxdf$sbp)

      dxdf$newa1c = dxdf$a1c

      set.seed(iter)
      # half of those on orals are on sulfonylureas, and new effect dependent on new drug
      dxdf$onsulfa = (dxdf$oralrx==1)*rbinom(dxdf$oralrx,1,0.5)
      
      set.seed(iter)
      dxdf$hocvd = rbinom(dxdf$cvdrisk, 1, dxdf$cvdrisk/100)
      dxdf$hocvd[dxdf$mi==1] = 1
      set.seed(iter)
      dxdf$hockd = rbinom(dxdf$nephrisk,1,dxdf$nephrisk/100)
      set.seed(iter)
      dxdf$hohypog = rbinom(dxdf$hypogrisk,1,dxdf$hypogrisk/100)
      set.seed(iter)
      dxdf$hochf = rbinom(dxdf$chfrisk,1,dxdf$chfrisk/100)
      
      dxdf$altmed = ((dxdf$cvdhx==1)| (dxdf$hockd==1)| (dxdf$hohypog==1)| (dxdf$hochf==1))&(dxdf$onsulfa==1)
      dxdf$lowegfr = rbinom(length(dxdf$altmed),1,.04182)
      dxdf$altmedsglt = ((((dxdf$cvdhx==1)| (dxdf$hockd==1)| (dxdf$hohypog==1)| (dxdf$hochf==1))&(dxdf$lowegfr==0)))&(dxdf$onsulfa==1)
      dxdf$altmed = (i==2)*(dxdf$altmedsglt)+(i!=2)*(dxdf$altmed)
      dxdf$classeff = (dxdf$altmed==0)*0.57+(dxdf$altmed==1)*(ifelse(i==1,0.57,ifelse(i==2,0.51,ifelse(i==3,0.80,ifelse(i==4,0.53,ifelse(i==5,0.60,ifelse(i==6,0.57,ifelse(i==7,0.53,0.53))))))))  #sulfa, sglt2, glp1, dpp4, tzd, analogue, sglt2+glp1
      
      dxdf$newa1c[dxdf$onsulfa==1] = dxdf$a1c[dxdf$onsulfa==1]+0.57-dxdf$classeff[dxdf$onsulfa==1]
      dxdf$deltaa1c = (dxdf$newa1c - dxdf$a1c)

      
       
      dxdf$cvdrisk_opt = dxdf$cvdrisk*(1-dxdf$rxbp)*(1-dxdf$rxstatin)+
        dxdf$cvdrisk*(2^(dxdf$deltasbp*(-0.0000184775*dxdf$age^2+0.001584*dxdf$age+0.028672)))*dxdf$rxbp*(1-dxdf$rxstatin)+
        dxdf$cvdrisk*(1-0.261-0.056)*dxdf$rxstatin*(1-dxdf$rxbp)+
        dxdf$cvdrisk*(2^(dxdf$deltasbp*(-0.0000184775*dxdf$age^2+0.001584*dxdf$age+0.028672)))*(1-0.261)*dxdf$rxbp*dxdf$rxstatin
      
      dxdf$cvdrisk_rr = (dxdf$altmed==0)*1+(dxdf$altmed==1)*(ifelse(i==1,1,ifelse(i==2,0.87,ifelse(i==3,0.92,ifelse(i==4,1,ifelse(i==5,0.84,ifelse(i==6,1,ifelse(i==7,0.873,0.873))))))))  #sulfa, sglt2, glp1, dpp4, tzd, analogue, sglt2+glp1
      dxdf$cvdrisk_opt = dxdf$cvdrisk_opt * dxdf$cvdrisk_rr
      dxdf$cvdrisk_opt[dxdf$cvdrisk_opt>100] = 100
      dxdf$deltacvdrisk = dxdf$cvdrisk -  dxdf$cvdrisk_opt
      
      #dxdf$chfrisk_opt = dxdf$chfrisk*(1-dxdf$rxbp)+dxdf$chfrisk*(1-0.22)*dxdf$rxbp # http://www.nejm.org/doi/full/10.1056/NEJM199108013250501
      
      dxdf$chfrisk_rr = (dxdf$altmed==0)*1+(dxdf$altmed==1)*ifelse(i==1,1,ifelse(i==2,0.70,ifelse(i==3,1,ifelse(i==4,1,ifelse(i==5,1.33,ifelse(i==6,1,ifelse(i==7,0.72,0.72)))))))  #sulfa, sglt2, glp1, dpp4, tzd, analogue, sglt2+glp1
      dxdf$chfrisk_opt = dxdf$chfrisk * dxdf$chfrisk_rr
      dxdf$chfrisk_opt[dxdf$chfrisk_opt>100] = 100
      
      dxdf$deltachfrisk =  dxdf$chfrisk-dxdf$chfrisk_opt
      
      
      dxdf$nephrisk_opt = dxdf$nephrisk*(1-dxdf$rxbp)*(1-dxdf$rxdm)+
        dxdf$nephrisk*((dxdf$newsbp/dxdf$sbp)^2.5640)*dxdf$rxbp*(1-dxdf$rxdm)+ # JAMA Vijan beta coef
        dxdf$nephrisk*((dxdf$newa1c/dxdf$a1c)^1.1025)*dxdf$rxdm*(1-dxdf$rxbp)+
        dxdf$nephrisk*((dxdf$newsbp/dxdf$sbp)^2.5640)*dxdf$rxbp*((dxdf$newa1c/dxdf$a1c)^1.4325)*dxdf$rxbp*dxdf$rxdm
      dxdf$nephrisk_opt[is.na(dxdf$nephrisk_opt)==T]=dxdf$nephrisk[is.na(dxdf$nephrisk_opt)==T]
      
      dxdf$nephrisk_rr = (dxdf$altmed==0)*1+(dxdf$altmed==1)*ifelse(i==1,1,ifelse(i==2,0.71,ifelse(i==3,0.78,ifelse(i==4,1,ifelse(i==5,1,ifelse(i==6,1,ifelse(i==7,0.714,0.714)))))))  #sulfa, sglt2, glp1, dpp4, tzd, analogue, sglt2+glp1
      dxdf$nephrisk_opt = dxdf$nephrisk_opt * dxdf$nephrisk_rr
      
      dxdf$deltanephrisk = dxdf$nephrisk - dxdf$nephrisk_opt 
      
      
      dxdf$retinrisk_opt = dxdf$retinrisk*(1-dxdf$rxbp)*(1-dxdf$rxdm)+
        dxdf$retinrisk*((dxdf$newsbp/dxdf$sbp)^6.4249)*dxdf$rxbp*(1-dxdf$rxdm)+ # JAMA Vijan beta coef
        dxdf$retinrisk*((dxdf$newa1c/dxdf$a1c)^2.5144)*dxdf$rxdm*(1-dxdf$rxbp)+
        dxdf$retinrisk*((dxdf$newsbp/dxdf$sbp)^6.4249)*dxdf$rxbp*((dxdf$newa1c/dxdf$a1c)^2.5144)*dxdf$rxbp*dxdf$rxdm
      dxdf$retinrisk_opt[is.na(dxdf$retinrisk_opt)==T]=dxdf$retinrisk[is.na(dxdf$retinrisk_opt)==T]
           dxdf$deltaretinrisk =  dxdf$retinrisk - dxdf$retinrisk_opt 
      
      
      dxdf$neurorisk_opt = dxdf$neurorisk*(1-dxdf$rxdm)+
        dxdf$neurorisk*((dxdf$newa1c/dxdf$a1c)^1.4325)*dxdf$rxdm
      dxdf$neurorisk_opt[is.na(dxdf$neurorisk_opt)==T]=dxdf$neurorisk[is.na(dxdf$neurorisk_opt)==T]
      dxdf$deltaneurorisk = dxdf$neurorisk - dxdf$neurorisk_opt 
      
      
      dxdf$hypogrisk_opt = dxdf$hypogrisk
      dxdf$hypogrisk_rr = (dxdf$altmed==0)*1+(dxdf$altmed==1)*ifelse(i==1,1,ifelse(i==2,0.12,ifelse(i==3,0.29,ifelse(i==4,0.24,ifelse(i==5,0.37,ifelse(i==6,1,ifelse(i==7,0.13,.13*(1-.035)-.68*.035)))))))  #sulfa, sglt2, glp1, dpp4, tzd, analogue, sglt2+glp1
      dxdf$hypogrisk_opt[dxdf$insulin!=1] = dxdf$hypogrisk_opt[dxdf$insulin!=1] * dxdf$hypogrisk_rr[dxdf$insulin!=1]
      dxdf$hypogrisk_opt[dxdf$insulin==1 & i==6] = dxdf$hypogrisk_opt[dxdf$insulin==1 & i==6] * 0.68 
      dxdf$deltahypogrisk = dxdf$hypogrisk - dxdf$hypogrisk_opt 
      
      dxdf$bmirisk_opt = dxdf$bmirisk
      dxdf$bmirisk_rr = (dxdf$altmed==0)*0+(dxdf$altmed==1)*ifelse(i==1,0,ifelse(i==2,-4.4/(1.66^2),ifelse(i==3,-1.9,ifelse(i==4,-1.9/(1.66^2),ifelse(i==5,3.2/(1.66^2),ifelse(i==6,0,ifelse(i==7,-1.5,-1.5)))))))  #sulfa, sglt2, glp1, dpp4, tzd, analogue, sglt2+glp1
      dxdf$bmirisk_opt = dxdf$bmirisk_opt + dxdf$bmirisk_rr #kg/m^2 gain/loss
      dxdf$deltabmirisk = dxdf$bmirisk - dxdf$bmirisk_opt # in kg/m^2
      
      dxdf$genitrisk_opt = dxdf$genitrisk
      dxdf$genitrisk_rr = (dxdf$altmed==0)*1+(dxdf$altmed==1)*ifelse(i==1,1,ifelse(i==2,2.1,ifelse(i==3,1,ifelse(i==4,1,ifelse(i==5,1,ifelse(i==6,1,ifelse(i==7,2.0,2.0)))))))  #sulfa, sglt2, glp1, dpp4, tzd, analogue, sglt2+glp1
      dxdf$genitrisk_opt = dxdf$genitrisk_opt * dxdf$genitrisk_rr
      dxdf$deltagenitrisk = dxdf$genitrisk - dxdf$genitrisk_opt 
      
      dxdf$dkarisk_opt = dxdf$dkarisk
      dxdf$dkarisk_rr = (dxdf$altmed==0)*1+(dxdf$altmed==1)*ifelse(i==1,1,ifelse(i==2,2.85,ifelse(i==3,1,ifelse(i==4,1,ifelse(i==5,1,ifelse(i==6,2.74,2.74))))))  #sulfa, sglt2, glp1, dpp4, tzd, analogue, sglt2+glp1
      dxdf$dkarisk_opt = dxdf$dkarisk_opt * dxdf$dkarisk_rr
      dxdf$deltadkarisk = dxdf$dkarisk - dxdf$dkarisk_opt 
      
      dxdf$amprisk_opt = dxdf$amprisk
      dxdf$amprisk_rr = (dxdf$altmed==0)*1+(dxdf$altmed==1)*ifelse(i==1,1,ifelse(i==2,1.91,ifelse(i==3,1,ifelse(i==4,1,ifelse(i==5,1,ifelse(i==6,1.86,1.86))))))  #sulfa, sglt2, glp1, dpp4, tzd, analogue, sglt2+glp1
      dxdf$amprisk_opt = dxdf$amprisk_opt * dxdf$amprisk_rr
      dxdf$deltaamprisk = dxdf$amprisk - dxdf$amprisk_opt 
      
      # absolute increase in events
      dxdf$gidrisk_opt = dxdf$gidrisk
      dxdf$gidrisk_rr = (dxdf$altmed==0)*0+(dxdf$altmed==1)*ifelse(i==1,0,ifelse(i==2,0,ifelse(i==3,100,ifelse(i==4,0,ifelse(i==5,0,ifelse(i==6,0,ifelse(i==7,0.66,0.66)))))))  #sulfa, sglt2, glp1, dpp4, tzd, analogue, sglt2+glp1
      dxdf$gidrisk_opt = dxdf$gidrisk_opt + dxdf$gidrisk_rr # absolute risk increase, %
      dxdf$deltagidrisk = dxdf$gidrisk - dxdf$gidrisk_opt 
      
      
      
      
      
      
      
      
      set.seed(iter)
      dxdf$cvdevents_opt = rbinom(length(dxdf$cvdrisk),1,dxdf$cvdrisk_opt/100)  
      dxdf$cvdcosts_opt = dxdf$cvdevents_opt*dxdf$cvdevents_cost/(1.03^10)
      set.seed(iter)
      dxdf$cvddeaths_opt = dxdf$cvdevents_opt*0.2 #rbinom(length(dxdf$cvdrisk_opt),1,0.2) #20% case fatality, https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3819990/
      
      set.seed(iter)
      dxdf$chfevents_opt = rbinom(length(dxdf$chfrisk),1,dxdf$chfrisk_opt/100) #rbinom(length(dxdf$chfrisk_opt),1,dxdf$chfrisk_opt/100)
      dxdf$chfcosts_opt = dxdf$chfevents_opt*dxdf$chfevents_cost/(1.03^10)
      set.seed(iter)
      dxdf$chfdeaths_opt = dxdf$chfevents_opt*0.5  # rbinom(length(dxdf$chfrisk_opt),1,0.5) #50% case fatality at 5 years, https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3033496/
      
      set.seed(iter)
      dxdf$nephevents_opt = rbinom(length(dxdf$nephrisk),1,dxdf$nephrisk_opt/100)  #rbinom(length(dxdf$nephrisk_opt),1,dxdf$nephrisk_opt/100)
      dxdf$nephcosts_opt = dxdf$nephevents_opt*dxdf$nephevents_cost/(1.03^10)
      set.seed(iter)
      dxdf$nephdeaths_opt = dxdf$nephevents_opt*1 #rbinom(length(dxdf$nephrisk_opt),1,1) #100% case fatality
      
      set.seed(iter)
      dxdf$retinevents_opt = rbinom(length(dxdf$retinrisk),1,dxdf$retinrisk_opt/100)  #rbinom(length(dxdf$retinrisk_opt),1,dxdf$retinrisk_opt/100)
      dxdf$retincosts_opt = dxdf$retinevents_opt*dxdf$retinevents_cost/(1.03^10)
      set.seed(iter)
      dxdf$retindeaths_opt = dxdf$retinevents_opt*0 #rbinom(length(dxdf$retinrisk_opt),1,0) 
      
      set.seed(iter)
      dxdf$neuroevents_opt = rbinom(length(dxdf$neurorisk),1,dxdf$neurorisk_opt/100)  #rbinom(length(dxdf$neurorisk_opt),1,dxdf$neurorisk_opt/100)
      dxdf$neurocosts_opt = dxdf$neuroevents_opt*dxdf$neuroevents_cost/(1.03^10)
      set.seed(iter)
      dxdf$neurodeaths_opt = dxdf$neuroevents_opt*0.1 #rbinom(length(dxdf$neurorisk_opt),1,0.1) #10% case fatality at 10 yrs, https://care.diabetesjournals.org/content/28/3/617#T4
      
      set.seed(iter)
      dxdf$hypogevents_opt = rbinom(length(dxdf$hypogrisk),1,dxdf$hypogrisk_opt/100)
      dxdf$hypogcosts_opt = dxdf$hypogevents_opt*dxdf$hypogevents_cost/(1.03^10)
      dxdf$hypogdeaths_opt = dxdf$hypogevents_opt*0.25 # 25 % case fatality at 3-5 yrs, https://pubmed.ncbi.nlm.nih.gov/29127240/ and https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3425013/
      
      set.seed(iter)
      dxdf$bmievents_opt = dxdf$bmirisk_opt #kg/m^2 bmi gain/loss
      dxdf$bmicosts_opt = dxdf$bmievents_opt*dxdf$bmievents_cost/(1.03^10)
      dxdf$bmideaths_opt = dxdf$bmievents_opt*0
      
      set.seed(iter)
      dxdf$genitevents_opt = rbinom(length(dxdf$genitrisk),1,dxdf$genitrisk_opt/100)
      dxdf$genitcosts_opt = dxdf$genitevents_opt*dxdf$genitevents_cost/(1.03^10)
      dxdf$genitdeaths_opt = dxdf$genitevents_opt*0.02  #2% case fatality, https://www.cdc.gov/infectioncontrol/guidelines/cauti/background.html
      
      set.seed(iter)
      dxdf$dkaevents_opt = rbinom(length(dxdf$dkarisk),1,dxdf$dkarisk_opt/100)
      dxdf$dkacosts_opt = dxdf$dkaevents_opt*dxdf$dkaevents_cost/(1.03^10)
      dxdf$dkadeaths_opt = dxdf$dkaevents_opt*0.04 # 4% case fatality,  https://pubmed.ncbi.nlm.nih.gov/8485963/
      
      set.seed(iter)
      dxdf$ampevents_opt = rbinom(length(dxdf$amprisk),1,dxdf$amprisk_opt/100)
      dxdf$ampcosts_opt = dxdf$ampevents_opt*dxdf$ampevents_cost/(1.03^10)
      dxdf$ampdeaths_opt = dxdf$ampevents_opt*0.1  #10% case fatality, https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3329303/ and http://www.diabetesincontrol.com/death-after-lower-limb-amputation/
      
      set.seed(iter)
      dxdf$gidevents_opt = rbinom(length(dxdf$gidrisk),1,dxdf$gidrisk_opt/100)
      dxdf$gidcosts_opt = dxdf$gidevents_opt*dxdf$gidevents_cost/(1.03^10)
      dxdf$giddeaths_opt = dxdf$gidevents_opt*0 
      
      
      
      
      
      dxdf$DALYS_cvd_opt = (dxdf$cvddeaths_opt*dxdf$YLL_cvd+dxdf$cvdevents_opt*dxdf$YLD_cvd)/(1.03^10)
      
      dxdf$DALYS_chf_opt = (dxdf$chfdeaths_opt*dxdf$YLL_chf+dxdf$chfevents_opt*dxdf$YLD_chf)/(1.03^10)
      
      dxdf$DALYS_neph_opt = (dxdf$nephdeaths_opt*dxdf$YLL_neph+dxdf$nephevents_opt*dxdf$YLD_neph)/(1.03^10)
      
      dxdf$DALYS_retin_opt = (dxdf$retindeaths_opt*dxdf$YLL_retin+dxdf$retinevents_opt*dxdf$YLD_retin)/(1.03^10)
      
      dxdf$DALYS_neuro_opt = (dxdf$neurodeaths_opt*dxdf$YLL_neuro+dxdf$neuroevents_opt*dxdf$YLD_neuro)/(1.03^10)
      
      dxdf$DALYS_hypog_opt = (dxdf$hypogdeaths_opt*dxdf$YLL_hypog+dxdf$hypogevents_opt*dxdf$YLD_hypog)/(1.03^10)
      
      dxdf$DALYS_bmi_opt = (dxdf$bmideaths_opt*dxdf$YLL_bmi+dxdf$bmievents_opt*dxdf$YLD_bmi)/(1.03^10)
      
      dxdf$DALYS_genit_opt = (dxdf$genitdeaths_opt*dxdf$YLL_genit+dxdf$genitevents_opt*dxdf$YLD_genit)/(1.03^10)
      
      dxdf$DALYS_dka_opt = (dxdf$dkadeaths_opt*dxdf$YLL_dka+dxdf$dkaevents_opt*dxdf$YLD_dka)/(1.03^10)
      
      dxdf$DALYS_amp_opt = (dxdf$ampdeaths_opt*dxdf$YLL_amp+dxdf$ampevents_opt*dxdf$YLD_amp)/(1.03^10)
      
      dxdf$DALYS_gid_opt = (dxdf$giddeaths_opt*dxdf$YLL_gid+dxdf$gidevents_opt*dxdf$YLD_gid)/(1.03^10)
      
      parsed = dxdf  %>%
        add_count(Country) %>%
        group_by(Country) %>%
        summarise(cvdevents_opt = sum(cvdevents_opt)/n,
                  chfevents_opt = sum(chfevents_opt)/n,
                  nephevents_opt = sum(nephevents_opt)/n,
                  retinevents_opt = sum(retinevents_opt)/n,
                  neuroevents_opt = sum(neuroevents_opt)/n,
                  hypogevents_opt = sum(hypogevents_opt)/n,
                  bmievents_opt = sum(bmievents_opt)/n,
                  genitevents_opt = sum(genitevents_opt)/n,
                  dkaevents_opt = sum(dkaevents_opt)/n,
                  ampevents_opt = sum(ampevents_opt)/n,
                  gidevents_opt = sum(gidevents_opt)/n,
                  cvddeaths_opt = sum(cvddeaths_opt)/n,
                  chfdeaths_opt = sum(chfdeaths_opt)/n,
                  nephdeaths_opt = sum(nephdeaths_opt)/n,
                  retindeaths_opt = sum(retindeaths_opt)/n,
                  neurodeaths_opt = sum(neurodeaths_opt)/n,
                  hypogdeaths_opt = sum(hypogdeaths_opt)/n,
                  bmideaths_opt = sum(bmideaths_opt)/n,
                  genitdeaths_opt = sum(genitdeaths_opt)/n,
                  dkadeaths_opt = sum(dkadeaths_opt)/n,
                  ampdeaths_opt = sum(ampdeaths_opt)/n,
                  giddeaths_opt = sum(giddeaths_opt)/n,
                  rxbpcosts_opt = sum(rxbp_costs)/n,
                  rxdmcosts_opt = sum(rxdm_costs)/n,
                  rxstatincosts_opt = sum(rxstatin_costs)/n,
                  cvdcosts_opt = sum(cvdcosts_opt)/n,
                  chfcosts_opt = sum(chfcosts_opt)/n,
                  nephcosts_opt = sum(nephcosts_opt)/n,
                  retincosts_opt = sum(retincosts_opt)/n,
                  neurocosts_opt = sum(neurocosts_opt)/n,
                  hypogcosts_opt = sum(hypogcosts_opt)/n,
                  bmicosts_opt = sum(bmicosts_opt)/n,
                  genitcosts_opt = sum(genitcosts_opt)/n,
                  dkacosts_opt = sum(dkacosts_opt)/n,
                  ampcosts_opt = sum(ampcosts_opt)/n,
                  gidcosts_opt = sum(gidcosts_opt)/n,
                  cvddalys_opt = sum(DALYS_cvd_opt)/n,
                  chfdalys_opt = sum(DALYS_chf_opt)/n,
                  nephdalys_opt = sum(DALYS_neph_opt)/n,
                  retindalys_opt = sum(DALYS_retin_opt)/n,
                  neurodalys_opt = sum(DALYS_neuro_opt)/n,
                  hypogdalys_opt = sum(DALYS_hypog_opt)/n,
                  bmidalys_opt = sum(DALYS_bmi_opt)/n,
                  genitdalys_opt = sum(DALYS_genit_opt)/n,
                  dkadalys_opt = sum(DALYS_dka_opt)/n,
                  ampdalys_opt = sum(DALYS_amp_opt)/n,
                  giddalys_opt = sum(DALYS_gid_opt)/n) %>%
        distinct()
      
      
      
      cvdevents_mat = cbind(cvdevents_mat,as.double(parsed$cvdevents_opt))
      chfevents_mat = cbind(chfevents_mat,as.double(parsed$chfevents_opt))
      nephevents_mat = cbind(nephevents_mat,as.double(parsed$nephevents_opt))
      retinevents_mat = cbind(retinevents_mat,as.double(parsed$retinevents_opt))
      neuroevents_mat = cbind(neuroevents_mat,as.double(parsed$neuroevents_opt))
      hypogevents_mat = cbind(hypogevents_mat,as.double(parsed$hypogevents_opt))
      bmievents_mat = cbind(bmievents_mat,as.double(parsed$bmievents_opt))
      genitevents_mat = cbind(genitevents_mat,as.double(parsed$genitevents_opt))
      dkaevents_mat = cbind(dkaevents_mat, as.double(parsed$dkaevents_opt))
      ampevents_mat = cbind(ampevents_mat, as.double(parsed$ampevents_opt))
      gidevents_mat = cbind(gidevents_mat,as.double(parsed$gidevents_opt))
      
      rxbpcosts_mat = cbind(rxbpcosts_mat,as.double(parsed$rxbpcosts_opt))
      rxdmcosts_mat = cbind(rxdmcosts_mat,as.double(parsed$rxdmcosts_opt))
      rxstatincosts_mat = cbind(rxstatincosts_mat,as.double(parsed$rxstatincosts_opt))
      
      cvdcosts_mat = cbind(cvdcosts_mat,as.double(parsed$cvdcosts_opt))
      chfcosts_mat = cbind(chfcosts_mat,as.double(parsed$chfcosts_opt))
      nephcosts_mat = cbind(nephcosts_mat,as.double(parsed$nephcosts_opt))
      retincosts_mat = cbind(retincosts_mat,as.double(parsed$retincosts_opt))
      neurocosts_mat = cbind(neurocosts_mat,as.double(parsed$neurocosts_opt))
      hypogcosts_mat = cbind(hypogcosts_mat,as.double(parsed$hypogcosts_opt))
      bmicosts_mat = cbind(bmicosts_mat,as.double(parsed$bmicosts_opt))
      genitcosts_mat = cbind(genitcosts_mat,as.double(parsed$genitcosts_opt))
      dkacosts_mat = cbind(dkacosts_mat, as.double(parsed$dkacosts_opt))
      ampcosts_mat = cbind(ampcosts_mat, as.double(parsed$ampcosts_opt))
      gidcosts_mat = cbind(gidcosts_mat,as.double(parsed$gidcosts_opt))
      
      cvddeaths_mat = cbind(cvddeaths_mat,as.double(parsed$cvddeaths_opt))
      chfdeaths_mat = cbind(chfdeaths_mat,as.double(parsed$chfdeaths_opt))
      nephdeaths_mat = cbind(nephdeaths_mat,as.double(parsed$nephdeaths_opt))
      retindeaths_mat = cbind(retindeaths_mat,as.double(parsed$retindeaths_opt))
      neurodeaths_mat = cbind(neurodeaths_mat,as.double(parsed$neurodeaths_opt))
      hypogdeaths_mat = cbind(hypogdeaths_mat,as.double(parsed$hypogdeaths_opt))
      bmideaths_mat = cbind(bmideaths_mat,as.double(parsed$bmideaths_opt))
      genitdeaths_mat = cbind(genitdeaths_mat,as.double(parsed$genitdeaths_opt))
      dkadeaths_mat = cbind(dkadeaths_mat, as.double(parsed$dkadeaths_opt))
      ampdeaths_mat = cbind(ampdeaths_mat, as.double(parsed$ampdeaths_opt))
      giddeaths_mat = cbind(giddeaths_mat,as.double(parsed$giddeaths_opt))
      
      cvddalys_mat = cbind(cvddalys_mat,as.double(parsed$cvddalys_opt))
      chfdalys_mat = cbind(chfdalys_mat,as.double(parsed$chfdalys_opt))
      nephdalys_mat = cbind(nephdalys_mat,as.double(parsed$nephdalys_opt))
      retindalys_mat = cbind(retindalys_mat,as.double(parsed$retindalys_opt))
      neurodalys_mat = cbind(neurodalys_mat,as.double(parsed$neurodalys_opt))
      hypogdalys_mat = cbind(hypogdalys_mat,as.double(parsed$hypogdalys_opt))
      bmidalys_mat = cbind(bmidalys_mat,as.double(parsed$bmidalys_opt))
      genitdalys_mat = cbind(genitdalys_mat,as.double(parsed$genitdalys_opt))
      dkadalys_mat = cbind(dkadalys_mat, as.double(parsed$dkadalys_opt))
      ampdalys_mat = cbind(ampdalys_mat, as.double(parsed$ampdalys_opt))
      giddalys_mat = cbind(giddalys_mat,as.double(parsed$giddalys_opt))
      
      
      
      dxdf_tab = dxdf
      varsToFactor <- c("onsulfa","hockd","hohypog","hochf","altmed", "hocvd")
      dxdf_tab[varsToFactor] <- lapply(dxdf_tab[varsToFactor], factor)


      vars =c("onsulfa","hockd","hohypog","hochf","hocvd","altmed", "hba1c_p","cvdrisk_opt", "chfrisk_opt", "nephrisk_opt", "retinrisk_opt", "neurorisk_opt", "hypogrisk_opt","bmirisk_opt","genitrisk_opt","dkarisk_opt","amprisk_opt","gidrisk_opt")
      tableTwo <- CreateTableOne(vars = vars, data = dxdf_tab, strata="Country")
      tab2c = print(tableTwo, nonnormal = c("onsulfa","hockd","hohypog","hochf","altmed", "hba1c_p","cvdrisk_opt", "chfrisk_opt", "nephrisk_opt", "retinrisk_opt", "neurorisk_opt", "hypogrisk_opt","bmirisk_opt","genitrisk_opt","dkarisk_opt","amprisk_opt","gidrisk_opt"))

      dxdf_tabr = dxdf_tab %>%
        group_by(Region)

      tableTwor <- CreateTableOne(vars = vars, data = dxdf_tabr, strata="Region")
      tab2r = print(tableTwor, nonnormal = c("onsulfa","hockd","hohypog","hochf","hocvd","altmed", "hba1c_p","cvdrisk_opt", "chfrisk_opt", "nephrisk_opt", "retinrisk_opt", "neurorisk_opt", "hypogrisk_opt","bmirisk_opt","genitrisk_opt","dkarisk_opt","amprisk_opt","gidrisk_opt"))

      tableTwoa <- CreateTableOne(vars = vars, data = dxdf_tab)
      tab2a = print(tableTwoa, nonnormal = c("onsulfa","hockd","hohypog","hochf","hocvd","altmed", "hba1c_p","cvdrisk_opt", "chfrisk_opt", "nephrisk_opt", "retinrisk_opt", "neurorisk_opt", "hypogrisk_opt","bmirisk_opt","genitrisk_opt","dkarisk_opt","amprisk_opt","gidrisk_opt"))

      
}
  
  
  #sulfa, sglt2, glp1, dpp4, tzd, analogue, sglt2+glp1
  labels = c("Country",
             "Baseline",
             "Sulfonylurea", 
             "SGLT2i",
             "GLP1 RA",
             "DPP-4i",
             "TZD",
             "Analogue insulin",
             "SGLT2i+GLP1 RA",
             "All")
  
  colnames(rxbpcosts_mat) = labels
  colnames(rxdmcosts_mat) = labels
  colnames(rxstatincosts_mat) = labels
  colnames(cvdcosts_mat) = labels
  colnames(chfcosts_mat) = labels
  colnames(nephcosts_mat) = labels
  colnames(retincosts_mat) = labels
  colnames(neurocosts_mat) = labels
  colnames(hypogcosts_mat) = labels
  colnames(bmicosts_mat) = labels
  colnames(genitcosts_mat) = labels
  colnames(dkacosts_mat) = labels
  colnames(ampcosts_mat) = labels
  colnames(gidcosts_mat) = labels
  
  colnames(cvdevents_mat) = labels
  colnames(chfevents_mat) = labels
  colnames(nephevents_mat) = labels
  colnames(retinevents_mat) = labels
  colnames(neuroevents_mat) = labels
  colnames(hypogevents_mat) = labels
  colnames(bmievents_mat) = labels
  colnames(genitevents_mat) = labels
  colnames(dkaevents_mat) = labels
  colnames(ampevents_mat) = labels
  colnames(gidevents_mat) = labels
  
  colnames(cvddeaths_mat) = labels
  colnames(chfdeaths_mat) = labels
  colnames(nephdeaths_mat) = labels
  colnames(retindeaths_mat) = labels
  colnames(neurodeaths_mat) = labels
  colnames(hypogdeaths_mat) = labels
  colnames(bmideaths_mat) = labels
  colnames(genitdeaths_mat) = labels
  colnames(dkadeaths_mat) = labels
  colnames(ampdeaths_mat) = labels
  colnames(giddeaths_mat) = labels
  
  colnames(cvddalys_mat) = labels
  colnames(chfdalys_mat) = labels
  colnames(nephdalys_mat) = labels
  colnames(retindalys_mat) = labels
  colnames(neurodalys_mat) = labels
  colnames(hypogdalys_mat) = labels
  colnames(bmidalys_mat) = labels
  colnames(genitdalys_mat) = labels
  colnames(dkadalys_mat) = labels
  colnames(ampdalys_mat) = labels
  colnames(giddalys_mat) = labels
  
  
  outmat_cvdevents = bind_rows(outmat_cvdevents,as_tibble(cvdevents_mat))
  outmat_chfevents = bind_rows(outmat_chfevents,as_tibble(chfevents_mat))
  outmat_nephevents = bind_rows(outmat_nephevents,as_tibble(nephevents_mat))
  outmat_retinevents = bind_rows(outmat_retinevents,as_tibble(retinevents_mat))
  outmat_neuroevents = bind_rows(outmat_neuroevents,as_tibble(neuroevents_mat))
  outmat_hypogevents = bind_rows(outmat_hypogevents,as_tibble(hypogevents_mat))
  outmat_bmievents = bind_rows(outmat_bmievents,as_tibble(bmievents_mat))
  outmat_genitevents = bind_rows(outmat_genitevents,as_tibble(genitevents_mat))
  outmat_dkaevents = bind_rows(outmat_dkaevents,as_tibble(dkaevents_mat))
  outmat_ampevents = bind_rows(outmat_ampevents,as_tibble(ampevents_mat))
  outmat_gidevents = bind_rows(outmat_gidevents,as_tibble(gidevents_mat))
  
  outmat_cvddeaths = bind_rows(outmat_cvddeaths,as_tibble(cvddeaths_mat))
  outmat_chfdeaths = bind_rows(outmat_chfdeaths,as_tibble(chfdeaths_mat))
  outmat_nephdeaths = bind_rows(outmat_nephdeaths,as_tibble(nephdeaths_mat))
  outmat_retindeaths = bind_rows(outmat_retindeaths,as_tibble(retindeaths_mat))
  outmat_neurodeaths = bind_rows(outmat_neurodeaths,as_tibble(neurodeaths_mat))
  outmat_hypogdeaths = bind_rows(outmat_hypogdeaths,as_tibble(hypogdeaths_mat))
  outmat_bmideaths = bind_rows(outmat_bmideaths,as_tibble(bmideaths_mat))
  outmat_genitdeaths = bind_rows(outmat_genitdeaths,as_tibble(genitdeaths_mat))
  outmat_dkadeaths = bind_rows(outmat_dkadeaths,as_tibble(dkadeaths_mat))
  outmat_ampdeaths = bind_rows(outmat_ampdeaths,as_tibble(ampdeaths_mat))
  outmat_giddeaths = bind_rows(outmat_giddeaths,as_tibble(giddeaths_mat))
  
  outmat_rxbpcosts = bind_rows(outmat_rxbpcosts,as_tibble(rxbpcosts_mat))
  outmat_rxdmcosts = bind_rows(outmat_rxdmcosts,as_tibble(rxdmcosts_mat))
  outmat_rxstatincosts = bind_rows(outmat_rxstatincosts,as_tibble(rxstatincosts_mat))
  
  outmat_cvdcosts = bind_rows(outmat_cvdcosts,as_tibble(cvdcosts_mat))
  outmat_chfcosts = bind_rows(outmat_chfcosts,as_tibble(chfcosts_mat))
  outmat_nephcosts = bind_rows(outmat_nephcosts,as_tibble(nephcosts_mat))
  outmat_retincosts = bind_rows(outmat_retincosts,as_tibble(retincosts_mat))
  outmat_neurocosts = bind_rows(outmat_neurocosts,as_tibble(neurocosts_mat))
  outmat_hypogcosts = bind_rows(outmat_hypogcosts,as_tibble(hypogcosts_mat))
  outmat_bmicosts = bind_rows(outmat_bmicosts,as_tibble(bmicosts_mat))
  outmat_genitcosts = bind_rows(outmat_genitcosts,as_tibble(genitcosts_mat))
  outmat_dkacosts = bind_rows(outmat_dkacosts,as_tibble(dkacosts_mat))
  outmat_ampcosts = bind_rows(outmat_ampcosts,as_tibble(ampcosts_mat))
  outmat_gidcosts = bind_rows(outmat_gidcosts,as_tibble(gidcosts_mat))
  
  outmat_cvddalys = bind_rows(outmat_cvddalys,as_tibble(cvddalys_mat))
  outmat_chfdalys = bind_rows(outmat_chfdalys,as_tibble(chfdalys_mat))
  outmat_nephdalys = bind_rows(outmat_nephdalys,as_tibble(nephdalys_mat))
  outmat_retindalys = bind_rows(outmat_retindalys,as_tibble(retindalys_mat))
  outmat_neurodalys = bind_rows(outmat_neurodalys,as_tibble(neurodalys_mat))
  outmat_hypogdalys = bind_rows(outmat_hypogdalys,as_tibble(hypogdalys_mat))
  outmat_bmidalys = bind_rows(outmat_bmidalys,as_tibble(bmidalys_mat))
  outmat_genitdalys = bind_rows(outmat_genitdalys,as_tibble(genitdalys_mat))
  outmat_dkadalys = bind_rows(outmat_dkadalys,as_tibble(dkadalys_mat))
  outmat_ampdalys = bind_rows(outmat_ampdalys,as_tibble(ampdalys_mat))
  outmat_giddalys = bind_rows(outmat_giddalys,as_tibble(giddalys_mat))
  
}

setwd("~/Box/Research/Research projects/WHO diabetes/Results matrices/")

save(outmat_rxbpcosts,file=paste0("outmat_rxbpcosts",screens))
save(outmat_rxdmcosts,file=paste0("outmat_rxdmcosts",screens))
save(outmat_rxstatincosts,file=paste0("outmat_rxstatincosts",screens))

save(outmat_cvdcosts,file=paste0("outmat_cvdcosts",screens))
save(outmat_chfcosts,file=paste0("outmat_chfcosts",screens))
save(outmat_nephcosts,file=paste0("outmat_nephcosts",screens))
save(outmat_retincosts,file=paste0("outmat_retincosts",screens))
save(outmat_neurocosts,file=paste0("outmat_neurocosts",screens))
save(outmat_hypogcosts,file=paste0("outmat_hypogcosts",screens))
save(outmat_bmicosts,file=paste0("outmat_bmicosts",screens))
save(outmat_genitcosts,file=paste0("outmat_genitcosts",screens))
save(outmat_dkacosts,file=paste0("outmat_dkacosts",screens))
save(outmat_ampcosts,file=paste0("outmat_ampcosts",screens))
save(outmat_gidcosts,file=paste0("outmat_gidcosts",screens))

save(outmat_cvdevents,file=paste0("outmat_cvdevents",screens))
save(outmat_chfevents,file=paste0("outmat_chfevents",screens))
save(outmat_nephevents,file=paste0("outmat_nephevents",screens))
save(outmat_retinevents,file=paste0("outmat_retinevents",screens))
save(outmat_neuroevents,file=paste0("outmat_neuroevents",screens))
save(outmat_hypogevents,file=paste0("outmat_hypogevents",screens))
save(outmat_bmievents,file=paste0("outmat_bmievents",screens))
save(outmat_genitevents,file=paste0("outmat_genitevents",screens))
save(outmat_dkaevents,file=paste0("outmat_dkaevents",screens))
save(outmat_ampevents,file=paste0("outmat_ampevents",screens))
save(outmat_gidevents,file=paste0("outmat_gidevents",screens))

save(outmat_cvddeaths,file=paste0("outmat_cvddeaths",screens))
save(outmat_chfdeaths,file=paste0("outmat_chfdeaths",screens))
save(outmat_nephdeaths,file=paste0("outmat_nephdeaths",screens))
save(outmat_retindeaths,file=paste0("outmat_retindeaths",screens))
save(outmat_neurodeaths,file=paste0("outmat_neurodeaths",screens))
save(outmat_hypogdeaths,file=paste0("outmat_hypogdeaths",screens))
save(outmat_bmideaths,file=paste0("outmat_bmideaths",screens))
save(outmat_genitdeaths,file=paste0("outmat_genitdeaths",screens))
save(outmat_dkadeaths,file=paste0("outmat_dkadeaths",screens))
save(outmat_ampdeaths,file=paste0("outmat_ampdeaths",screens))
save(outmat_giddeaths,file=paste0("outmat_giddeaths",screens))

save(outmat_cvddalys,file=paste0("outmat_cvddalys",screens))
save(outmat_chfdalys,file=paste0("outmat_chfdalys",screens))
save(outmat_nephdalys,file=paste0("outmat_nephdalys",screens))
save(outmat_retindalys,file=paste0("outmat_retindalys",screens))
save(outmat_neurodalys,file=paste0("outmat_neurodalys",screens))
save(outmat_hypogdalys,file=paste0("outmat_hypogdalys",screens))
save(outmat_bmidalys,file=paste0("outmat_bmidalys",screens))
save(outmat_genitdalys,file=paste0("outmat_genitdalys",screens))
save(outmat_dkadalys,file=paste0("outmat_dkadalys",screens))
save(outmat_ampdalys,file=paste0("outmat_ampdalys",screens))
save(outmat_giddalys,file=paste0("outmat_giddalys",screens))



end_time <- Sys.time()
end_time - start_time


