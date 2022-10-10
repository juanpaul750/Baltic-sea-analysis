library(readxl)
library(mgcv)
library(tidyverse)
library(lme4)
library(ggplot2)
library(ggpubr)
library(zoo)

#Section 1 code:

phos_t = read_csv("phost.csv")

library(ggplot2)

ggplot(phos_t) +
 aes(x = year, y = phos) +
 geom_line(size = 0.5, colour = "#112446") +
 labs(title = "Anual avarage of phosphorous load") +
 theme_minimal() + theme(plot.title = element_text(size = 17)) +labs(x = "YEAR", y = "P(10,000 ton / year)")



# Danish straight:
# monthy seasonal pattern:

land_oxy1 = read.delim('w land1.txt')
id = 1:nrow(land_oxy1)

land_oxy1_new = data.frame(year = land_oxy1$Year, month = land_oxy1$Month, DO2 =land_oxy1$Dissolved.Oxygen..ml.l., Temp = land_oxy1$Temperature..degC.,
                           salinity = land_oxy1$Practical.Salinity..dmnless., DO = land_oxy1$Dissolved.Oxygen..ml.l.,
                           PO4 = land_oxy1$Phosphate.Phosphorus..PO4.P...umol.l., total_PO4 = land_oxy1$Total.Phosphorus..P...umol.l.,
                           si04 = land_oxy1$Silicate.Silicon..SiO4.Si...umol.l.,
                           NO2_NO3 = land_oxy1$Nitrate.Nitrogen..NO3.N...umol.l. + land_oxy1$Nitrite.Nitrogen..NO2.N...umol.l.,
                           NH4 = land_oxy1$Ammonium.Nitrogen..NH4.N...umol.l., chl = land_oxy1$Chlorophyll.a..ug.l.,
                           Total_nitrogen = land_oxy1$Total.Nitrogen..N...umol.l.,id)
summary(land_oxy1_new)

# monthly avarage:

month_avg = land_oxy1_new %>% 
  group_by(month ) %>%
  summarise(across(-id,mean, na.rm = TRUE))

month_max = land_oxy1_new %>% 
  group_by(month ) %>%
  summarise(across(DO2,max, na.rm = TRUE))

month_min = land_oxy1_new %>% 
  group_by(month ) %>%
  summarise(across(DO2,min, na.rm = TRUE))

month_avg$DO_max = month_max$DO2
month_avg$DO_min = month_min$DO2

ggplot(month_avg)+
  geom_line(aes(x = month, y = DO2, colour = "mean disolved oxygen"),size = 1)+
  geom_line(aes(x =month, y = DO_max ,colour = "CI"),size = 0.5,linetype = "dashed")+
  geom_line(aes(x =month, y = DO_min ),size = 0.5, colour = "red",linetype = "dashed")+
  geom_abline(intercept = 2, slope =0,linetype = "dashed",colour = "red", size = 1.5)+
  geom_line(aes(x = month, y = Temp,colour = "mean temperature"),size = 1, ) + theme(panel.grid.major = element_line(colour = "gray95"),
    panel.background = element_rect(fill = "gray97")) +labs(title = "Seasonal pattern of disolved oxygen",
    x = "Month", y = "Disolved oxygen (ml/L)")


# every year monthly pattern:
yt = seq(1990,2021)
year_month_land_oxy1 = land_oxy1_new %>% filter ( year == yt)
year_month_land_oxy1['day'] = 1
year_month_land_oxy1$Date = as.Date(with(year_month_land_oxy1,paste(year,month,day,sep='-')),"%Y-%m-%d")

# total oxygen time series:
# simple gam:
data_date_inflow = data.frame(date = year_month_land_oxy1$Date,DO = year_month_land_oxy1$DO)

data_date_inflow = data_date_inflow%>%drop_na()
data_date_inflow = data_date_inflow %>% arrange(date)
id_inflow1 = 1:nrow(data_date_inflow)
data_date_inflow$timestep = id_inflow1


oxygot_model = gam(DO  ~ s(timestep, k = 20, bs = "cs"),data =data_date_inflow)
gam.check(oxygot_model)
data_date_inflow$fit = oxygot_model$fitted.values

ggplot(data_date_inflow) +
  aes(x = date, y = fit) +
  geom_line(size = 0.5, colour = "#112446") +
  theme_minimal()+geom_abline(intercept = 2, slope =0,linetype = "dashed",colour = "red")



# INFLOW PATTERN:
yt = seq(1983,2000)
year_month_land_oxy1 = land_oxy1_new %>% filter ( year == yt)
year_month_land_oxy1['day'] = 1
year_month_land_oxy1$Date = as.Date(with(year_month_land_oxy1,paste(year,month,day,sep='-')),"%Y-%m-%d")

# total oxygen time series:
# simple gam:
data_date_inflow = data.frame(date = year_month_land_oxy1$Date,DO = year_month_land_oxy1$DO)

data_date_inflow = data_date_inflow%>%drop_na()
data_date_inflow = data_date_inflow %>% arrange(date)
id_inflow1 = 1:nrow(data_date_inflow)
data_date_inflow$timestep = id_inflow1


oxygot_model = gam(DO  ~ s(timestep, k = 10, bs = "cs"),data =data_date_inflow)
gam.check(oxygot_model)
data_date_inflow$fit = oxygot_model$fitted.values

q1 = ggplot(data_date_inflow)+
  geom_line(aes(x = date, y = fit, colour = "trend"),size = 1) +
  theme_minimal()+geom_abline(intercept = 2, slope =0,linetype = "dashed",colour = "red")+
  labs(title = "Danish straits",x = "Year", y = "Disolved oxygen (ml/L)")












# GOTland basin:
gotland_oxy1 = read.delim('gotland deep.txt')


id_got = 1:nrow(gotland_oxy1)

gotland_oxy1_new = data.frame(year = gotland_oxy1$Year, month = gotland_oxy1$Month, DO2 =gotland_oxy1$Dissolved.Oxygen..ml.l.,
                           Temp = gotland_oxy1$Temperature..degC.,
                           salinity = gotland_oxy1$Practical.Salinity..dmnless., DO = gotland_oxy1$Dissolved.Oxygen..ml.l.,
                           PO4 = gotland_oxy1$Phosphate.Phosphorus..PO4.P...umol.l., total_PO4 = gotland_oxy1$Total.Phosphorus..P...umol.l.,
                           si04 = gotland_oxy1$Silicate.Silicon..SiO4.Si...umol.l.,
                           NO2_NO3 = gotland_oxy1$Nitrate.Nitrogen..NO3.N...umol.l. + gotland_oxy1$Nitrite.Nitrogen..NO2.N...umol.l.,
                           NH4 = gotland_oxy1$Ammonium.Nitrogen..NH4.N...umol.l., chl = gotland_oxy1$Chlorophyll.a..ug.l.,
                           Total_nitrogen = gotland_oxy1$Total.Nitrogen..N...umol.l.,id_got,
                           h2s = -( gotland_oxy1$Hydrogen.Sulphide..H2S.S...umol.l. )*(10/213))


gotland_oxy1_new$oxy_avg = rowSums(gotland_oxy1_new[,c('DO','h2s')], na.rm = T)
gotland_oxy1_new_neg = na_if(gotland_oxy1_new$oxy_avg, 0)
gotland_oxy1_new$new_neg = gotland_oxy1_new_neg



# avg monthly pattern:
month_avg_got = gotland_oxy1_new %>% 
  group_by(month ) %>%
  summarise(across(-id_got,mean, na.rm = TRUE))

month_max_got = gotland_oxy1_new %>% 
  group_by(month ) %>%
  summarise(across(new_neg,max, na.rm = TRUE))

month_min_got = gotland_oxy1_new %>% 
  group_by(month ) %>%
  summarise(across(new_neg,min, na.rm = TRUE))

month_avg_got$DO_max = month_max_got$new_neg
month_avg_got$DO_min = month_min_got$new_neg

ggplot(month_avg_got)+
  geom_line(aes(x = month, y = new_neg, colour = "mean disolved oxygen"),size = 1)+
  geom_line(aes(x =month, y = DO_max ,colour = "CI"),size = 0.5,linetype = "dashed")+
  geom_line(aes(x =month, y = DO_min ),size = 0.5, colour = "red",linetype = "dashed")+
  geom_abline(intercept = 2, slope =0,linetype = "dashed",colour = "red", size = 1.5)+
  theme(panel.grid.major = element_line(colour = "gray95"),
        panel.background = element_rect(fill = "gray97")) +
  labs(title = "Seasonal pattern of disolved oxygen",x = "Month", y = "Disolved oxygen (ml/L)")

# every year monthly pattern:
yt = seq(1990,2021)
year_month_got_oxy1 = gotland_oxy1_new %>% filter ( year == yt)
year_month_got_oxy1['day'] = 1
year_month_got_oxy1$Date = as.Date(with(year_month_got_oxy1,paste(year,month,day,sep='-')),"%Y-%m-%d")

# total oxygen time series:
# simple gam:

data_date_D0 = data.frame(D02 = year_month_got_oxy1$new_neg, date =year_month_got_oxy1$Date )
data_date_D0 = data_date_D0%>%drop_na()
id_g = 1:nrow(data_date_D0)

data_date_D0 = data_date_D0%>%arrange(date)
data_date_D0$id = id_g



oxygot_model = gam(D02~ s(id, k = 7, bs = "cs"),
                   data =data_date_D0)

gam.check(oxygot_model)

data_gam_got = data.frame(fit = oxygot_model$fitted.values, date = data_date_D0$date)


ggplot(year_month_got_oxy1) +
  aes(x = Date, y = new_neg) +
  geom_line(size = 0.5, colour = "#112446") +
  theme_minimal()+geom_abline(intercept = 2, slope =0,linetype = "dashed",colour = "red",size = 1)+
  geom_abline(intercept = 0, slope =0,linetype = "dashed",colour = "blue",size = 1)+
  geom_line(data = data_gam_got,aes(y = fit , x = date, colour = "trend"),size = 1)+
  labs(title = "Time Series of dissolved oxygen",x = "DATE", y = "Disolved oxygen (ml/L)")


########################################################################
yt = seq(1983,2000)
year_month_got_oxy1 = gotland_oxy1_new %>% filter ( year == yt)
year_month_got_oxy1['day'] = 1
year_month_got_oxy1$Date = as.Date(with(year_month_got_oxy1,paste(year,month,day,sep='-')),"%Y-%m-%d")

# total oxygen time series:
# simple gam:

data_date_D0 = data.frame(D02 = year_month_got_oxy1$new_neg, date =year_month_got_oxy1$Date )
data_date_D0 = data_date_D0%>%drop_na()
id_g = 1:nrow(data_date_D0)

data_date_D0 = data_date_D0%>%arrange(date)
data_date_D0$id = id_g

oxygot_model = gam(D02~ s(id, k = 20, bs = "cs"),
                   data =data_date_D0)

gam.check(oxygot_model)

data_gam_got = data.frame(fit = oxygot_model$fitted.values, date = data_date_D0$date)

q2 = ggplot(year_month_got_oxy1)+
  geom_line(aes(x = Date, y = salinity))+
  theme_minimal()+geom_abline(intercept = 2, slope =0,linetype = "dashed",colour = "red",size = 1)+
  geom_abline(intercept = 0, slope =0,linetype = "dashed",colour = "blue",size = 1)+
  geom_line(data = data_gam_got,aes(y = fit , x = date, colour = "trend"),size = 1)+
  labs(title = "Gotland Basin",x = "DATE", y = "Disolved oxygen (ml/L)")

################################################################






#limfjord
  
lim_oxy1 = read.delim('lim2.txt')
  
  
id_lim = 1:nrow(lim_oxy1)
  
lim_oxy1_new = data.frame(year = lim_oxy1$Year, month = lim_oxy1$Month, DO2 =lim_oxy1$Dissolved.Oxygen..ml.l.,
                                Temp = lim_oxy1$Temperature..degC.,
                                salinity = lim_oxy1$Practical.Salinity..dmnless.,
                                PO4 = lim_oxy1$Phosphate.Phosphorus..PO4.P...umol.l., total_PO4 = lim_oxy1$Total.Phosphorus..P...umol.l.,
                                si04 = lim_oxy1$Silicate.Silicon..SiO4.Si...umol.l.,chl = lim_oxy1$Chlorophyll.a..ug.l.,
                                Total_nitrogen = lim_oxy1$Total.Nitrogen..N...umol.l.,id_lim)




# every year monthly pattern:
yt = seq(1990,2021)
year_month_lim_oxy1 = lim_oxy1_new %>% filter ( year == yt)
year_month_lim_oxy1['day'] = 1
year_month_lim_oxy1$Date = as.Date(with(year_month_lim_oxy1,paste(year,month,day,sep='-')),"%Y-%m-%d")


#PLoT
p1 = ggplot(year_month_lim_oxy1) +
  aes(x = Date, y = DO2) +
  geom_line(size = 0.5, colour = "#112446") +
  theme_minimal()+geom_abline(intercept = 2, slope =0,linetype = "dashed",colour = "red",size = 1)+
  geom_abline(intercept = 0, slope =0,linetype = "dashed",colour = "blue",size = 1)+
  labs(title = "Time Series of dissolved oxygen",x = "DATE", y = "Disolved oxygen (ml/L)")

p2 = ggplot(year_month_lim_oxy1) +
  aes(x = Date, y = salinity) +
  geom_line(size = 0.5, colour = "#112446") +
  theme_minimal()+geom_abline(intercept = 2, slope =0,linetype = "dashed",colour = "red",size = 1)+
  labs(title = "Time Series of salinity",x = "DATE", y = "Salinity PSU")

plot = ggarrange(p1,p2,ncol = 1, nrow = 2)
plot


#INFLOW PLOT################################
plot = ggarrange(q1,q2,ncol = 2, nrow = 1)
plot





# Bothnian bay:


bothnia_oxy1 = read.delim('bothnia2.txt')
bothnia_oxy1 = bothnia_oxy1%>% arrange(Year,Month)

id_lim = 1:nrow(bothnia_oxy1)

bothnia_oxy1_new = data.frame(year = bothnia_oxy1$Year, month = bothnia_oxy1$Month, DO2 =bothnia_oxy1$Dissolved.Oxygen..ml.l.,
                          Temp = bothnia_oxy1$Temperature..degC.,
                          salinity = bothnia_oxy1$Practical.Salinity..dmnless.,
                          PO4 = bothnia_oxy1$Phosphate.Phosphorus..PO4.P...umol.l., total_PO4 = bothnia_oxy1$Total.Phosphorus..P...umol.l.,
                          si04 = bothnia_oxy1$Silicate.Silicon..SiO4.Si...umol.l.,chl = bothnia_oxy1$Chlorophyll.a..ug.l.,
                          Total_nitrogen = bothnia_oxy1$Total.Nitrogen..N...umol.l.,id_lim)




# every year monthly pattern:
yt = seq(1950,2021)
year_month_both_oxy1 = bothnia_oxy1_new %>% filter ( year == yt)
year_month_both_oxy1['day'] = 1
year_month_both_oxy1$Date = as.Date(with(year_month_both_oxy1,paste(year,month,day,sep='-')),"%Y-%m-%d")


#PLoT
ggplot(year_month_both_oxy1) +
  aes(x = Date, y = DO2) +
  geom_line(size = 0.5, colour = "#112446") +
  theme_minimal()+geom_abline(intercept = 2, slope =0,linetype = "dashed",colour = "red",size = 1)+
  geom_abline(intercept = 0, slope =0,linetype = "dashed",colour = "blue",size = 1)+
  labs(title = "Time Series of dissolved oxygen",x = "DATE", y = "Disolved oxygen (ml/L)")



# STOCKHOLM ARCHIPELAGO:

getwd()
arch_oxy1 = read.delim('arch.txt')
bothnia_oxy1 = bothnia_oxy1%>% arrange(Year,Month)

id_lim = 1:nrow(bothnia_oxy1)

arch_oxy1_new = data.frame(year = bothnia_oxy1$Year, month = bothnia_oxy1$Month, DO2 =bothnia_oxy1$Dissolved.Oxygen..ml.l.,
                              Temp = bothnia_oxy1$Temperature..degC.,
                              salinity = bothnia_oxy1$Practical.Salinity..dmnless.,
                              PO4 = bothnia_oxy1$Phosphate.Phosphorus..PO4.P...umol.l., total_PO4 = bothnia_oxy1$Total.Phosphorus..P...umol.l.,
                              si04 = bothnia_oxy1$Silicate.Silicon..SiO4.Si...umol.l.,chl = bothnia_oxy1$Chlorophyll.a..ug.l.,
                              Total_nitrogen = bothnia_oxy1$Total.Nitrogen..N...umol.l.,id_lim)


# every year monthly pattern:
yt = seq(1950,2021)
year_month_both_oxy1 = arch_oxy1_new %>% filter ( year == yt)
year_month_both_oxy1['day'] = 1
year_month_both_oxy1$Date = as.Date(with(year_month_both_oxy1,paste(year,month,day,sep='-')),"%Y-%m-%d")


#PLoT

ggplot(year_month_both_oxy1) +
  aes(x = Date, y = DO2) +
  geom_line(size = 0.5, colour = "#112446") +
  theme_minimal()+geom_abline(intercept = 2, slope =0,linetype = "dashed",colour = "red",size = 1)+
  geom_abline(intercept = 0, slope =0,linetype = "dashed",colour = "blue",size = 1)+
  labs(title = "Time Series of dissolved oxygen",x = "DATE", y = "Disolved oxygen (ml/L)")



#############

data_date_D0 = data.frame(D02 = year_month_both_oxy1$DO2, date =year_month_both_oxy1$Date )
data_date_D0 = data_date_D0%>%drop_na()
id_g = 1:nrow(data_date_D0)

data_date_D0 = data_date_D0%>%arrange(date)
data_date_D0$id = id_g

oxygot_model = gam(D02~ s(id, k = 5, bs = "cs"),
                   data =data_date_D0)

gam.check(oxygot_model)

data_gam_got = data.frame(fit = oxygot_model$fitted.values, date = data_date_D0$date)

ggplot(data_gam_got)+
  geom_line(data = data_gam_got,aes(y = fit , x = date, colour = "trend"),size = 1)+
  labs(title = "Gotland Basin",x = "DATE", y = "Disolved oxygen (ml/L)")

  