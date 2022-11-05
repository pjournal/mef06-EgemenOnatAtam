"Group Assignment: Startup Deals"
install.packages("tidyverse")
install.packages("readxl")
library(dplyr)
library(tidyverse)
library(scales)
sd21 <- read_excel("C:/Users/USER/Downloads/startup_deals_2021.xlsx")
"Kim kaç defa yatırım yapmış"
sd21 %>%group_by(Investor)%>%
  summarize(count=n()) %>%
  arrange(desc(count))

"investor yapanlar nereli"
sd21 %>%group_by(`Investor's Origin`)%>%
  summarize(count=n()) %>%
  arrange(desc(count))

"investment aşamalarının dağılımı ve grafiği"
invS <- sd21 %>%group_by(`Investment Stage`)%>%
  summarize(count=n()) %>%
  arrange(desc(count))

invS %>% ggplot()+ geom_col(mapping = aes(x=`Investment Stage`, y=count),fill = c("Black","beige","bisque3","coral2")) +labs(title="Investment per Investment Stage in Column Chart")

"investment aşamalarında toplam ne kadar para harcanmış // string gelmiş paralar onu çevirmeye çalıştım "
sd21 <- sd21 %>% mutate(`Deal Value (USD)` = as.numeric(`Deal Value (USD)`))
sd21 %>% group_by(`Investment Stage`) %>% summarize(sum = sum(`Deal Value (USD)`,na.rm=TRUE))

class(sd21$`Deal Value (USD)`)
"investment aşamalarına göre hisse alım oranı ortalama ne kadar//yine yüzdeler string"
sd21$`Stake (%)`<- str_remove(sd21$`Stake (%)`, pattern = "%")

sd21 <- sd21 %>% mutate(`Stake (%)` = as.numeric(`Stake (%)`))

class(sd21$`Stake (%)`)

sd21%>% group_by(`Investment Stage`) %>% summarize(med = median(`Stake (%)`,na.rm=TRUE))

sd21 %>% group_by(`Investment Stage`) %>% summarize(mean = mean(`Stake (%)`,na.rm=TRUE))

"Yatırım evrelerinde yüzde kaçının yatırımcısı vardı // Financial İnvestor sütünunu sanki yatırım almadan önce yatırımcısı var mı yok mu şeklinde anladım"
financial_investor <- sd21 %>% 
  group_by(`Investment Stage`) %>% 
  transmute(Percent = label_percent()(sum(`Financial Investor` == "Yes")/n()))
unique(financial_investor)

ggplot(sd21, aes(`Investment Stage`, fill = `Financial Investor`)) + 
geom_bar(position = "fill") +
labs(x="Hair Color", y=NULL) +
coord_flip()



