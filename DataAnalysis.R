library(dplyr)
library(readr)
library(rvest)

salary104 <- read_csv("C:/Users/USER/Desktop/salary104.csv")#讀檔
salary107 <- read_csv("C:/Users/USER/Desktop/salary107.csv")#讀檔
#第一題

#修改資料型態
salary104[,c(-1,-2)]<-apply(salary104[,c(-1,-2)],2,function(x){as.numeric(gsub("—|…",NA,x))})%>%as.data.frame()
salary107[,c(-1,-2)]<-apply(salary107[,c(-1,-2)],2,function(x){as.numeric(gsub("—|…",NA,x))})%>%as.data.frame()

anti104<-anti_join(salary104,salary107,by ="大職業別")#用anti_join比較不同處
anti107<-anti_join(salary107,salary104,by ="大職業別")

#統一職業名稱
salary104$大職業別<-gsub("部門","",salary104$大職業別)
salary104$大職業別<-gsub("、","_",salary104$大職業別)
salary104$大職業別<-gsub("營造業","營建工程",salary104$大職業別)
salary104$大職業別<-gsub("資訊及通訊傳播業","出版、影音製作、傳播及資通訊服務業",salary104$大職業別)
salary104$大職業別<-gsub("教育服務業","教育業",salary104$大職業別)
salary104$大職業別<-gsub("醫療保健服務業","醫療保健業",salary104$大職業別)



salary<-left_join(salary104, salary107, by ="大職業別" )#Join 104和107年度表格
salary$Calculation<-salary$`大學-薪資.y`/salary$`大學-薪資.x`#計算107年度大學畢業薪資 / 104年度大學畢業薪資

#107年度薪資較104年度薪資高的職業
salarytop10<- select(salary,大職業別,Calculation)%>%
  filter(Calculation>1)%>%
  arrange(desc(Calculation))%>%
  head(10)

#提高超過5%的的職業
salaryraise<- select(salary,大職業別,Calculation)%>%
  filter(Calculation>1.05)%>%
  arrange(desc(Calculation))
#字串處理;分析出現次數
job<-strsplit(salaryraise$大職業別,"-")%>%sapply("[",1)
table(job)

#第二題

#104年男生薪資比女生薪資多
men_higher104<-select(salary104,大職業別,`大學-女/男`)%>%
  filter(`大學-女/男`<100)%>%
  arrange(`大學-女/男`)%>%
  head(10)

#107年男生薪資比女生薪資多
men_higher107<-select(salary107,大職業別,`大學-女/男`)%>%
  filter(`大學-女/男`<100)%>%
  arrange(`大學-女/男`)%>%
  head(10)

#104年女生薪資比男生薪資多
women_higher104<-select(salary104,大職業別,`大學-女/男`)%>%
  filter(`大學-女/男`>=100)%>%
  arrange(desc(`大學-女/男`))%>%
  head(10)

#107年女生薪資比男生薪資多
women_higher107<-select(salary107,大職業別,`大學-女/男`)%>%
  filter(`大學-女/男`>=100)%>%
  arrange(desc(`大學-女/男`))%>%
  head(10)

#第三題
compare107<-select(salary107,大職業別,`大學-薪資`,`研究所-薪資`)
compare107<-mutate(compare107,rate107=salary107$`研究所-薪資`/salary107$`大學-薪資`)%>%
  arrange(desc(rate107))%>%
  head(10)

#第四題
interest<-select(salary107,大職業別,`大學-薪資`,`研究所-薪資`)%>%
  filter(salary107$大職業別%in%c("出版、影音製作、傳播及資通訊服務業-技術員及助理專業人員","出版、影音製作、傳播及資通訊服務業-事務支援人員","運輸及倉儲業-專業人員","出版、影音製作、傳播及資通訊服務業-專業人員","運輸及倉儲業-技術員及助理專業人員"))%>%
  mutate(薪資差=`研究所-薪資`-`大學-薪資`)