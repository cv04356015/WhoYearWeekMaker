library(dplyr)
library(data.table)
setwd("D:/Users/GoogleDrive/Synchronize/[疾病管制署] 程式專案/YearWeekMaker")

wd.function = function(year){
  first.day = paste(year, "01", "01", sep = "-")
  last.day  = paste(year, "12", "31", sep = "-")
  date = as.Date(as.Date(first.day):as.Date(last.day), origin = "1970-01-01")
  wday = weekdays(date, abbreviate = T)
  dt   = data.frame(date, wday) %>% mutate(index1 = (ifelse(wday == "週日", 1, 0) %>% cumsum))
  if ((dt %>% filter(index1 == 0) %>% nrow) >= 4){
    
    if ((dt %>% filter(index1 == max(dt$index1)) %>% nrow) >= 4){
      dt   = dt %>% mutate(index1 = index1 + 1)
      dt = dt %>% mutate(yearweek = paste0(year, ifelse(index1<10, 
                                                        paste0("0", index1),
                                                        index1)))
    } else {
      dt   = dt %>% mutate(index1 = index1 + 1)
      dt = dt %>% mutate(yearweek = paste0(year, ifelse(index1<10, 
                                                        paste0("0", index1),
                                                        index1)))
      dt$yearweek[dt$index1 == max(dt$index1)] = (wd.function(year+1) %>% head(1))$yearweek        
      dt$index1  [dt$index1 == max(dt$index1)] = 1      
    }
    

    
  } else {
    
    dt = dt %>% mutate(yearweek = paste0(year, ifelse(index1<10, 
                                                      paste0("0", index1),
                                                      index1)))
    
    if ((dt %>% filter(index1 == max(dt$index1)) %>% nrow) >= 4){
      dt$yearweek[dt$index1 == 0] = paste0((year-1), (wd.function(year-1) %>% tail(1))$index1)
      dt$index1[dt$index1 == 0] = (wd.function(year-1) %>% tail(1))$index1
    } else {
      dt$yearweek[dt$index1 == 0] = paste0((year-1), (wd.function(year-1) %>% tail(1))$index1)
      dt$yearweek[dt$index1 == max(dt$index1)] = (wd.function(year+1) %>% head(1))$yearweek        
      dt$index1  [dt$index1 == max(dt$index1)] = 1      
    } 
  }
  
  dt
}

res.dt = lapply(2009:2100, wd.function) %>% do.call(rbind, .) %>% select(date, yearweek)
write.csv(res.dt, "output.csv", row.names = F)

# 比較舊檔驗證用
# cont.dt = read.csv("training.csv")
# cont.dt2 = left_join(res.dt, cont.dt, "date")
# cont.dt3 = cont.dt2 %>% mutate(comp = ifelse(yearweek.x == yearweek.y, 1, 0))
# cont.dt3[-c(1:367),] %>% filter(comp == 0)
# cont.dt3 %>% View
# res.dt  %>% str
# cont.dt %>% str
# cont.dt$date     = as.Date(cont.dt$date)
# cont.dt$yearweek = as.character(cont.dt$yearweek)