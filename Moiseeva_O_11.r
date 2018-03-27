library("tidyverse")
library("nycflights13") 
library("tidyr") 
library("stringr") 
library("dplyr") 
library("tibble") 
library("readr") 
tbl=read.csv("eddypro.csv", skip = 1, na =c("","NA","-9999","-9999.0"), comment=c("[")) 
tbl=tbl[-1,] 
tbl 
tbl=tbl[tbl$DOY > 62 & tbl$DOY < 156,]
tbl
#выбираем дни
tbl=tbl[tbl$daytime == FALSE,] 
glimpse(tbl) 
#выводим переменные
tbl = select(tbl, -(roll))
#удаляем лишнюю переменную
tbl = tbl %>% mutate_if(is.character, factor)
tbl
#преобразуем все в факоры и заменим ненужные символы
names(tbl) = str_replace_all(names(tbl), "[!]","_emph_") 
names(tbl) = names(tb1) %>% 
  str_replace_all("[!]","_emph_") %>% 
  str_replace_all("[?]","_quest_") %>% 
  str_replace_all("[*]","_star_") %>% 
  str_replace_all("[+]","_plus_") %>% 
  str_replace_all("[-]","_minus_") %>% 
  str_replace_all("[@]","_at_") %>% 
  str_replace_all("[$]","_dollar_") %>% 
  str_replace_all("[#]","_hash_") %>% 
  str_replace_all("[/]","_div_") %>% 
  str_replace_all("[%]","_perc_") %>% 
  str_replace_all("[&]","_amp_") %>% 
  str_replace_all("[\\^]","_power_") %>% 
  str_replace_all("[()]","_") 
glimpse(tbl) 
#Оставляем только численные данные 
sapply(tbl,is.numeric) 
tbl_numeric = tbl [,sapply (tbl,is.numeric) ] 
tbl_numeric
#таблица, состоящая из интересующих нас колонок 
tbl_non_numeric = tbl[,!sapply(tbl,is.numeric) ]
#таблица содержащая все остальные колонки
cor_td = cor(tbl_numeric)
cor_td
#корреляционный анализ не вышел, надо избавиться от строкгде есть NA
cor_td = cor(drop_na(tbl_numeric)) 
cor_td
cor_td = cor(drop_na(tbl_numeric)) %>% as.data.frame %>% select(h2o_flux) 
cor_td 
vars = row.names(cor_tbl)[cor_tbl$h2o_flux^2 > .1] %>% na.exclude
formula = as.formula(paste("h2o_flux~", paste(vars,collapse = "+"), sep="")) 
#создаем формулу
formula
mod=lm(formula, data = tbl)
anova(mod)
summary(mod)
formula1 = as.formula(paste("h2o_flux ~ Tau + rand_err_Tau + H + rand_err_H + LE + qc_LE + 
                            rand_err_LE + co2_flux + h2o_flux + qc_h2o_flux + rand_err_h2o_flux + 
                            co2_molar_density + co2_mole_fraction + co2_mixing_ratio + 
                            h2o_time_lag + sonic_temperature + air_temperature + air_density + 
                            air_molar_volume + es + RH + VPD + u_rot + u.+
                            + TKE + un_Tau + un_H + un_LE + un_co2_flux + un_h2o_flux + co2.1"))
formula1
mod2=lm(formula1, data = tbl)
anova(mod2)
summary(mod2)
formula2 = as.formula(paste("h2o_flux ~ Tau + LE + rand_err_H + co2_flux + rand_err_h2o_flux + 
                            co2_molar_density  + sonic_temperature + air_temperature + es + 
                            RH + VPD + u."))
mod3=lm(formula2, data = tbl)
anova(mod3)
summary(mod3)
formula3 = as.formula(paste("h2o_flux ~ Tau  +  LE + rand_err_h2o_flux + co2_molar_density 
                            + sonic_temperature + RH + u."))
mod4=lm(formula3, data = tbl)
anova(mod4)
summary(mod4)
