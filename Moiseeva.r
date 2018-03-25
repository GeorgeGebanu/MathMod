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
#избавляемся от NA
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
cor_tbl = cor(drop_na(tbl_numeric)) 
cor_tbl
cor_tbl = cor(drop_na(tbl_numeric)) %>% as.data.frame %>% select(h2o_flux) 
cor_tbl 
vars = row.names(cor_tbl)[cor_tbl$h2o_flux^2 > .1] %>% na.exclude
formula = as.formula(paste("h2o_flux~", paste(vars,collapse = "+"), sep="")) 
#создаем формулу
teaching_tbl = sample_n(tbl, floor(length(tbl$date)*.7)) 
testing_tbl = sample_n(tbl, floor(length(tbl$date)*.3)) 
formula
