
setwd("~/Downloads")

library(tidyverse)
library(ggplot2)
library(robustbase)
cnt_reg = read_csv("~/Downloads/cnt_reg.csv")
cost = read_csv("cost.csv")
gdp = read_csv("gdp.csv")


load("outmat_cvdevents1")
load("outmat_chfevents1")
load("outmat_nephevents1")
load("outmat_retinevents1")
load("outmat_neuroevents1")
load("outmat_hypogevents1")
load("outmat_bmievents1")
load("outmat_genitevents1")
load("outmat_dkaevents1")
load("outmat_ampevents1")
load("outmat_gidevents1")

load("outmat_cvddeaths1")
load("outmat_chfdeaths1")
load("outmat_nephdeaths1")
load("outmat_retindeaths1")
load("outmat_neurodeaths1")
load("outmat_hypogdeaths1")
load("outmat_bmideaths1")
load("outmat_genitdeaths1")
load("outmat_dkadeaths1")
load("outmat_ampdeaths1")
load("outmat_giddeaths1")

load("outmat_cvddalys1")
load("outmat_chfdalys1")
load("outmat_nephdalys1")
load("outmat_retindalys1")
load("outmat_neurodalys1")
load("outmat_hypogdalys1")
load("outmat_bmidalys1")
load("outmat_genitdalys1")
load("outmat_dkadalys1")
load("outmat_ampdalys1")
load("outmat_giddalys1")

load("outmat_rxbpcosts1")
load("outmat_rxdmcosts1")
load("outmat_rxstatincosts1")

load("outmat_cvdcosts1")
load("outmat_chfcosts1")
load("outmat_nephcosts1")
load("outmat_retincosts1")
load("outmat_neurocosts1")
load("outmat_hypogcosts1")
load("outmat_bmicosts1")
load("outmat_genitcosts1")
load("outmat_dkacosts1")
load("outmat_ampcosts1")
load("outmat_gidcosts1")



##### DALYS #####
cvddalys = outmat_cvddalys %>% mutate_at(2:10,as.numeric) %>% group_by(Country) %>% summarise_all(median) %>% mutate(outcome = "cvd")
chfdalys = outmat_chfdalys %>% mutate_at(2:10,as.numeric) %>% group_by(Country) %>% summarise_all(median) %>% mutate(outcome = "chf")
nephdalys = outmat_nephdalys %>% mutate_at(2:10,as.numeric) %>% group_by(Country) %>% summarise_all(median) %>% mutate(outcome = "neph")
neurodalys = outmat_neurodalys %>% mutate_at(2:10,as.numeric) %>% group_by(Country) %>% summarise_all(median) %>% mutate(outcome = "neuro")
retindalys = outmat_retindalys %>% mutate_at(2:10,as.numeric) %>% group_by(Country) %>% summarise_all(median)  %>% mutate(outcome = "retin")
hypogdalys = outmat_hypogdalys %>% mutate_at(2:10,as.numeric) %>% group_by(Country) %>% summarise_all(median)  %>% mutate(outcome = "hypog")
bmidalys = outmat_bmidalys %>% mutate_at(2:10,as.numeric) %>% group_by(Country) %>% summarise_all(median)  %>% mutate(outcome = "bmi")
genitdalys = outmat_genitdalys %>% mutate_at(2:10,as.numeric) %>% group_by(Country) %>% summarise_all(median)  %>% mutate(outcome = "genit")
dkadalys = outmat_dkadalys %>% mutate_at(2:10,as.numeric) %>% group_by(Country) %>% summarise_all(median)  %>% mutate(outcome = "dka")
ampdalys = outmat_ampdalys %>% mutate_at(2:10,as.numeric) %>% group_by(Country) %>% summarise_all(median)  %>% mutate(outcome = "amp")
giddalys = outmat_giddalys %>% mutate_at(2:10,as.numeric) %>% group_by(Country) %>% summarise_all(median)  %>% mutate(outcome = "gid")
# ampdalys$SGLT2i = ampdalys$Sulfonylurea
dalys = rbind(cvddalys,chfdalys,nephdalys,neurodalys,retindalys,hypogdalys,bmidalys,genitdalys,dkadalys,ampdalys,giddalys)
dalys = dalys %>%
  filter(Country!="Swaziland" & Country!="South Africa DHS")
dalys_by_country = dalys  %>%
  group_by(Country) %>%
  select(2:10)%>%
  summarize_all(sum)
summary(dalys_by_country)
dalys_by_outcome=   dalys %>%
  group_by(outcome) %>%
  select(2:10) %>%
  summarize_all(median)
summary(dalys_by_outcome)
dalys_by_region = dalys %>%
  group_by(Country) %>%
  summarise(across(where(is.numeric), sum)) %>%
  left_join(cnt_reg) %>%
  group_by(Region) %>%
  summarise(across(where(is.numeric), median))
summary(dalys_by_region)



##### COSTS #####

rxdmcosts = outmat_rxdmcosts %>% mutate_at(2:10,as.numeric) %>% group_by(Country) %>% summarise_all(median, na.rm = T) %>% mutate(outcome = "rxdm")
cvdcosts = outmat_cvdcosts %>% mutate_at(2:10,as.numeric) %>% group_by(Country) %>% summarise_all(median, na.rm = T) %>% mutate(outcome = "cvd")
chfcosts = outmat_chfcosts %>% mutate_at(2:10,as.numeric) %>% group_by(Country) %>% summarise_all(median, na.rm = T) %>% mutate(outcome = "chf")
nephcosts = outmat_nephcosts %>% mutate_at(2:10,as.numeric) %>% group_by(Country) %>% summarise_all(median, na.rm = T) %>% mutate(outcome = "neph")
neurocosts = outmat_neurocosts %>% mutate_at(2:10,as.numeric) %>% group_by(Country) %>% summarise_all(median, na.rm = T) %>% mutate(outcome = "neuro")
retincosts = outmat_retincosts %>% mutate_at(2:10,as.numeric) %>% group_by(Country) %>% summarise_all(median, na.rm = T)  %>% mutate(outcome = "retin")
hypogcosts = outmat_hypogcosts %>% mutate_at(2:10,as.numeric) %>% group_by(Country) %>% summarise_all(median, na.rm = T)  %>% mutate(outcome = "hypog")
bmicosts = outmat_bmicosts %>% mutate_at(2:10,as.numeric) %>% group_by(Country) %>% summarise_all(median, na.rm = T)  %>% mutate(outcome = "bmi")
genitcosts = outmat_genitcosts %>% mutate_at(2:10,as.numeric) %>% group_by(Country) %>% summarise_all(median, na.rm = T)  %>% mutate(outcome = "genit")
dkacosts = outmat_dkacosts %>% mutate_at(2:10,as.numeric) %>% group_by(Country) %>% summarise_all(median, na.rm = T)  %>% mutate(outcome = "dka")
ampcosts = outmat_ampcosts %>% mutate_at(2:10,as.numeric) %>% group_by(Country) %>% summarise_all(median, na.rm = T)  %>% mutate(outcome = "amp")
gidcosts = outmat_gidcosts %>% mutate_at(2:10,as.numeric) %>% group_by(Country) %>% summarise_all(median, na.rm = T)  %>% mutate(outcome = "gid")
# ampcosts$SGLT2i = ampcosts$Sulfonylurea
costs = rbind(rxdmcosts,cvdcosts,chfcosts,nephcosts,neurocosts,retincosts,hypogcosts,bmicosts,genitcosts,dkacosts,ampcosts,gidcosts)
costs = costs %>%
  filter(Country!="Swaziland" & Country!="South Africa DHS")
costs_by_country = costs  %>%
  group_by(Country) %>%
  select(2:10)%>%
  summarize_all(sum)
summary(costs_by_country)
costs_by_outcome =  costs %>%
  group_by(outcome) %>%
  select(2:10) %>%
  summarize_all(median)
summary(costs_by_outcome)
costs_by_region = costs %>%
  group_by(Country) %>%
  summarise(across(where(is.numeric), sum)) %>%
  left_join(cnt_reg) %>%
  group_by(Region) %>%
  summarise(across(where(is.numeric), median))
summary(costs_by_region)


#### ICER ####

inc_daly_by_country = -cbind((dalys_by_country[,4]-dalys_by_country[,3]),(dalys_by_country[,5]-dalys_by_country[,3]),(dalys_by_country[,6]-dalys_by_country[,3]),(dalys_by_country[7]-dalys_by_country[3]),(dalys_by_country[8]-dalys_by_country[3]),(dalys_by_country[9]-dalys_by_country[3]),(dalys_by_country[10]-dalys_by_country[3]))
inc_daly_by_country[inc_daly_by_country<0]=0.0001
summary(inc_daly_by_country*1000)

inc_cost_by_country = cbind((costs_by_country[,3]-costs_by_country[,2]),(costs_by_country[,4]-costs_by_country[,2]),(costs_by_country[,5]-costs_by_country[,2]),(costs_by_country[,6]-costs_by_country[,2]),(costs_by_country[7]-costs_by_country[2]),(costs_by_country[8]-costs_by_country[2]),(costs_by_country[9]-costs_by_country[2]))
colnames(inc_cost_by_country) = colnames(inc_daly_by_country)
inc_cost_by_country[inc_cost_by_country<0]=0.0001
summary(inc_cost_by_country*1000)

icer_by_country = inc_cost_by_country/inc_daly_by_country
colnames(inc_cost_by_country) = colnames(inc_daly_by_country)
icer_by_country = cbind(dalys_by_country[,1],icer_by_country)
summary(icer_by_country)


inc_daly_by_region = -cbind((dalys_by_region[,4]-dalys_by_region[,3]),(dalys_by_region[,5]-dalys_by_region[,3]),(dalys_by_region[,6]-dalys_by_region[,3]),(dalys_by_region[7]-dalys_by_region[3]),(dalys_by_region[8]-dalys_by_region[3]),(dalys_by_region[9]-dalys_by_region[3]),(dalys_by_region[10]-dalys_by_region[3]))
inc_daly_by_region[inc_daly_by_region<0]=0.0001
summary(inc_daly_by_region*1000)

inc_cost_by_region = cbind((costs_by_region[,3]-costs_by_region[,2]),(costs_by_region[,4]-costs_by_region[,2]),(costs_by_region[,5]-costs_by_region[,2]),(costs_by_region[,6]-costs_by_region[,2]),(costs_by_region[7]-costs_by_region[2]),(costs_by_region[8]-costs_by_region[2]),(costs_by_region[9]-costs_by_region[2]))
colnames(inc_cost_by_region) = colnames(inc_daly_by_region)
inc_cost_by_region[inc_cost_by_region<0]=0.0001
summary(inc_cost_by_region*1000)

icer_by_region = inc_cost_by_region/inc_daly_by_region
colnames(inc_cost_by_region) = colnames(inc_daly_by_region)
icer_by_region = cbind(dalys_by_region[,1],icer_by_region)
summary(icer_by_region)

df_d = dalys_by_region %>%
  mutate_if(is.numeric,function(x, na.rm = FALSE) 1000*x) %>%
  rename_all(function(x){paste0("d.", x)})
df_c = costs_by_region %>%
  mutate_if(is.numeric,function(x, na.rm = FALSE) 1000*x) %>%
  rename_all(function(x){paste0("c.", x)})
df_i = icer_by_region %>%
  rename_all(function(x){paste0("i.", x)})
df = data.frame(df_d,df_c,df_i)
library(tableone)
tableTwo <- CreateTableOne(vars = colnames(df), data = df)
tab2a = print(tableTwo)#, nonnormal = colnames(df))





#### TARGET PRICES ####

# 3 times GDP per cap
ibc = icer_by_country %>%
  left_join(gdp) %>%
  mutate(gdp_thres = 3*gdp) 
  
# costs too much already
summary(ibc[,2:8]>ibc$gdp_thres)

# how much reduction
cost_redux = as.matrix((ibc[,2:8]-ibc$gdp_thres)/ibc[,2:8])
cost_redux[cost_redux<0]=NA
summary(cost_redux)

cost_redux_by_country_perc_g1 = cbind(icer_by_country[,1],cost_redux)
cost_redux_by_country_perc_g1 = as_tibble(cost_redux_by_country_perc_g1) %>%
  mutate_at(2:8,as.numeric) %>%
  rename("Country" = "V1") %>%
  left_join(cnt_reg,by="Country")
cost_redux_by_region_perc_g1 = cost_redux_by_country_perc_g1 %>%
#  group_by(Region) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

cost_dol_by_country_perc_g1 = cost_redux_by_country_perc_g1 %>%
  left_join(cost,by=c("Country" = "Country")) %>%
  mutate(dol_sglt_redux  = SGLT2i * sglt_cost,
         dol_glp_redux = `GLP1 RA` * glp_cost,
         dol_dpp_redux = `DPP-4i` * dpp_cost,
         dol_tzd_redux = TZD * tzd_cost,
         dol_ana_redux = `Analogue insulin`*ana_cost) %>%
  select(dol_sglt_redux,
         dol_glp_redux,
         dol_dpp_redux,
         dol_tzd_redux,
         dol_ana_redux)



# to be cost saving
cost_save_thr_by_country = cbind(icer_by_country[,1],costs_by_country[,4:7]-cbind(costs_by_country[,3],
                              costs_by_country[,3],
                              costs_by_country[,3],
                              costs_by_country[,3]),
                              costs_by_country[,8]-costs_by_country[,2])
labs = colnames(cost_save_thr_by_country)
labs[1] = "Country"
colnames(cost_save_thr_by_country) = labs
per_yr_costs_to_save_by_country = cost_save_thr_by_country
per_yr_costs_to_save_by_country[,2:6] = per_yr_costs_to_save_by_country[,2:6]/10/1.5/(1.03^10)
per_yr_costs_to_save_by_country = per_yr_costs_to_save_by_country %>%
  left_join(cbind(cost[,1],cost[,17:25]))
per_yr_costs_to_save_by_country[,16] = (per_yr_costs_to_save_by_country[,13]-per_yr_costs_to_save_by_country[,12])/1000*0.64*28.50*(1.66^2)*365.25

cost_redux_by_country_perc_g2 = cbind(per_yr_costs_to_save_by_country[,1],
                                      per_yr_costs_to_save_by_country[,2]/per_yr_costs_to_save_by_country[,8],
                                      per_yr_costs_to_save_by_country[,3]/per_yr_costs_to_save_by_country[,9],
                                      per_yr_costs_to_save_by_country[,4]/per_yr_costs_to_save_by_country[,10],
                                      per_yr_costs_to_save_by_country[,5]/per_yr_costs_to_save_by_country[,11],
                                      per_yr_costs_to_save_by_country[,6]/per_yr_costs_to_save_by_country[,16])
     
colSums(cost_redux_by_country_perc_g2>0)
cost_redux_by_country_perc_g2[cost_redux_by_country_perc_g2<0]=NA
cost_redux_by_country_perc_g2 = as_tibble(cost_redux_by_country_perc_g2)
cost_redux_by_country_perc_g2 = cost_redux_by_country_perc_g2 %>%
  mutate_at(2:6,as.numeric)
cost_redux_by_country_perc_g2[cost_redux_by_country_perc_g2<0] = NA
summary(cost_redux_by_country_perc_g2)

cost_redux_by_country_perc_g2 = as_tibble(cost_redux_by_country_perc_g2) %>%
  mutate_at(2:6,as.numeric) %>%
  rename("Country" = "V1") %>%
  left_join(cnt_reg,by="Country")

cost_redux_by_region_perc_g2 = cost_redux_by_country_perc_g2 %>%
#  group_by(Region) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

cost_dol_by_country_perc_g2 = cost_redux_by_country_perc_g2 %>%
  left_join(cost,by=c("Country" = "Country")) %>%
  mutate(dol_sglt_redux = V2 * sglt_cost,
         dol_glp_redux = V3 * glp_cost,
         dol_dpp_redux = V4 * dpp_cost,
         dol_tzd_redux = V5* tzd_cost,
         dol_ana_redux = V6*ana_cost) %>%
  select(dol_sglt_redux,
         dol_glp_redux,
         dol_dpp_redux,
         dol_tzd_redux,
         dol_ana_redux)


##### FIG 2 #####

library(ggplot2)

df1 = cost_redux_by_country_perc_g1 %>% rename("Glargine" = "Analogue insulin") %>% pivot_longer(cols = 2:6, names_to = "Agent", values_to = "Price reduction")   %>% select("Country","Region","Agent","Price reduction")
df2 = cost_redux_by_country_perc_g2 %>% rename("SGLT2i" = "V2", "GLP1 RA" = "V3", "DPP-4i" = "V4", "TZD" = "V5", "Glargine" = "V6") %>% pivot_longer(cols = 2:6, names_to = "Agent", values_to = "Price reduction")
df1$Goal = "<3 x GDP"
df2$Goal = "Cost saving"
df1$`Price reduction` = 100*df1$`Price reduction`
df2$`Price reduction` = 100*df2$`Price reduction`

df = rbind(df1,df2) %>% replace_na(list("Price reduction" = 0)) 
df$`Price reduction`[df$`Price reduction`>100] = 100
df$`Price reduction`  = as.numeric(df$`Price reduction`)
Price = as.numeric(df$`Price reduction`)
Agent = df$Agent
Goal = df$Goal
data=data.frame(Price, Agent, Goal)
data$Agent <- factor(data$Agent, levels=c("SGLT2i","GLP1 RA","DPP-4i","TZD","Glargine"))

# grouped boxplot
ggplot(data, aes(x=Agent, y=Price, fill=Goal)) + 
  geom_boxplot() + 
  ylim(0,100) + 
  ylab("Price reduction (%)") +
  theme_classic() + 
  scale_fill_brewer(palette="Accent")
  


