
# Define UI for application that draws a histogram
library(ggplot2)
d2 = read_csv("datappl.csv")
d2 <- na.omit(d2)
d2 <- data.frame(d2)
d22 <- d2[c(1,2,4)] 
d22$"年度" = as.numeric(d22$"年度")
d22$"女性獲貸人數.人." = as.numeric(d22$"女性獲貸人數.人.")
d22 <- na.omit(d22)
d22 <- data.frame(d22)
d22 <- data.frame(d22)
d22 <- data.frame(d22)
d22 <- na.omit(d22)
d2 <- na.omit(d2)
d2 <- data.frame(d2)
d222 <- d2[c(1,2,4,6)] 
d222$"合計" = as.numeric(d222$"合計")
d222$"年度" = as.numeric(d222$"年度")
d222$"女性獲貸人數.人." = as.numeric(d222$"女性獲貸人數.人.")
d222 <- na.omit(d222)
d222 <- data.frame(d222)
d222 <- na.omit(d222)
t1 = read_xlsx("table1.xlsx")
t1 <- na.omit(t1)
t1 <- data.frame(t1)
t11 <- t1[1:16,2:4]*1000
t11 <- data.frame(t11)
t11 <- na.omit(t11)
alld2t1 <- cbind(t11, d222[1:16,])
t2 = read_xlsx("table2.xlsx")
t2 <- na.omit(t2)
t2 <- data.frame(t2)
t22 <- t2[1:16,4]/t2[1:16,3]
dm = read_csv("datamoney.csv")
dm <- na.omit(dm)
dm <- data.frame(dm)
dm1 <- dm[c(2,4)] 
dm1 <- na.omit(dm1)
dm1 = as.matrix(dm1)
dm1 = as.numeric(dm1)
mycols <- runif(5,min=1,max=length(colors())) 
j1 = read_xlsx("jobdata.xlsx")
j22 <- j1[,-5:-17]
j22 <- data.frame(j22)

j33 <- j22 %>% gather("item",value,3:4) %>% 
  bind_cols(data.frame(item_id=rep(1:2,each=6)))
j1 = read_xlsx("jobdata.xlsx")
j2 <- j1[,-3:-4]
j2 <- data.frame(j2)
j3 <- j2 %>% gather("item",value,3:15) %>% 
  bind_cols(data.frame(item_id=rep(1:13,each=6)))
j22 <- j2[,-3:-11]
j222 <- j22[,-4:-6]
j222 <- data.frame(j222)
j222 <- j222 %>% gather("item",value,3) %>% 
  bind_cols(data.frame(item_id=rep(3,each=6)))
j101 = read_xlsx("job101.xlsx")
j101 <- data.frame(j101)
j101 <- j101 %>% gather("item",value,3:10) %>% 
  bind_cols(data.frame(item_id=rep(1:8,each=8)))
j103 = read_xlsx("job103.xlsx")
j103 <- data.frame(j103)
j103 <- j103 %>% gather("item",value,3:10) %>% 
  bind_cols(data.frame(item_id=rep(1:8,each=8)))
j105 = read_xlsx("job105.xlsx")
j105 <- data.frame(j105)
j105 <- j105 %>% gather("item",value,3:10) %>% 
  bind_cols(data.frame(item_id=rep(1:8,each=8)))
ld = read_xlsx("licensedata.xlsx")
ld2 <- data.frame(ld[,-4:-9])
ld2 <- ld2 %>% gather("item",value,3:4) %>% 
  bind_cols(data.frame(item_id=rep(1:2,each=6)))
ld = read_xlsx("licensedata.xlsx")
ld1 <- data.frame(ld)
ld1 <- ld1 %>% gather("item",value,4:9) %>% 
  bind_cols(data.frame(item_id=rep(1:6,each=6)))
ldedu = read_xlsx("licensedataedu.xlsx")
ldedu1 <- data.frame(ldedu[,-4:-9])
ldedu1 <- ldedu1 %>% gather("item",value,3:4) %>% 
  bind_cols(data.frame(item_id=rep(1:2,each=24)))
ldedu = read_xlsx("licensedataedu.xlsx")
ldedu11 <- data.frame(ldedu[,-10])
ldedu111 <- data.frame(ldedu11[,-3])
ldedu111 <- ldedu111 %>% gather("item",value,3:8) %>% 
  bind_cols(data.frame(item_id=rep(1:8,each=18)))
p = read_xlsx("problem.xlsx")
p <- data.frame(p[,1:11])
p <- p %>% gather("item",value,3:11) %>% bind_cols(data.frame(item_id=rep(1:9,each=24)))
ss = read_xlsx("salarysex.xlsx")
ss <- data.frame(ss)
ss <- ss %>% gather("item",value,3:4) %>% 
  bind_cols(data.frame(item_id=rep(1:2,each=6)))
se = read_xlsx("salaryedu.xlsx")
se <- data.frame(se)
se <- se %>% gather("item",value,3:4) %>% 
  bind_cols(data.frame(item_id=rep(1:2,each=24)))
te = read_xlsx("timeedu.xlsx")
te1 <- data.frame(te[,1:8])
te1 <- te1 %>% gather("item",value,3:8) %>% 
  bind_cols(data.frame(item_id=rep(1:8,each=18)))
te = read_xlsx("timeedu.xlsx")
te2 <- data.frame(te[,-3:-8])
te2 <- te2 %>% gather("item",value,3) %>% 
  bind_cols(data.frame(item_id=rep(1,each=24)))


ui <- shinyUI(navbarPage("台灣青年勞工狀況",
                         navbarMenu("失業", h1("台灣青年勞工失業資料"),tabPanel("Option2-1"),
                                                tabPanel("Option2-1")),
                          
navbarMenu("就業",
            tabPanel("Option2-1"),
            tabPanel("Option2-1"),
            tabPanel("Option2-1")),
navbarMenu("創業",
 tabPanel("創業貸款人數",h1("台灣年青創業貸款人數"),p("收集91~106年的資料，男性的人數比女性多"),plotOutput("plot1")),                                           
 tabPanel("創業貸款人數與青年人數",h1("台灣青創業貸款人數與青年人數"),plotOutput("plot3")),                                           
tabPanel("人數總比較",h1("人數總比較"),plotOutput("plot4"),plotOutput("plot41")),
tabPanel("創業貸款金額",h1("創業貸款金額"),plotOutput("plot2"),plotOutput("plot21"))),

navbarMenu("青年勞工調查",
           tabPanel("轉換工作情形",plotOutput("plot5"),plotOutput("plot51"),plotOutput("plot52")),
           tabPanel("轉換工作與教育程度",plotOutput("plot53"),plotOutput("plot54"),plotOutput("plot55")),
           tabPanel("打算考證照情形",plotOutput("plot6"),plotOutput("plot61"),plotOutput("plot62")),
           tabPanel("打算考證照與教育程度",plotOutput("plot63")),
           tabPanel("初次尋職困難",plotOutput("plot7")),
           tabPanel("薪資狀況",plotOutput("plot8")),
           tabPanel("薪資狀況與教育程度",plotOutput("plot81")),
           tabPanel("初次尋職時間與教育程度關係",plotOutput("plot9"),plotOutput("plot91")))))                                                
                    
#資料

# Define server logic required to draw a histogram

server <- function(input, output) { 
  output$plot1 <- renderPlot({ggplot(d222, aes(x = 91:106)) + 
  geom_point(aes(y = d222[,3])) + 
  geom_line(aes(y = d222[,3],  color="女性獲貸人數")) +
  geom_point(aes(y = d222[,2])) + 
  geom_line(aes(y= d222[,2], color="男性獲貸人數")) +
  geom_point(aes(y = d222[,4])) + 
  geom_line(aes(y= d222[,4], color="總獲貸人數")) + xlab("年") + ylab("人數")})
output$plot2 <- renderPlot({barplot(dm[,6],width = 1, space = NULL,beside = TRUE, col = c("#4FB0C6","#4F86C6","#C65146","#EC6A5C","#e97f02","#f8ca00","#8FBC94","#548687","#6E7783","#77AAAD","#99CCCC","#FFCC99","#CC9999","#CCCC99","#0099CC","#FF6666","#996699","#666666","#996697"), xlab = "年度", ylab = "貸款金額(千元)", legend=dm$"年度")})

output$plot3 <-renderPlot({ggplot(alld2t1, aes(x = 91:106)) + 
  geom_point(aes(y = alld2t1[,1])) + 
  geom_line(aes(y = alld2t1[,1],  color="總青年人數")) +
  geom_point(aes(y = alld2t1[,2])) + 
  geom_line(aes(y= alld2t1[,2], color="男性青年人數")) +
  geom_point(aes(y = alld2t1[,3])) + 
  geom_line(aes(y= alld2t1[,3], color="女性青年人數"))+
  geom_point(aes(y = alld2t1[,5])) + 
  geom_line(aes(y= alld2t1[,5], color="男性青年創業貸款人數"))+
  geom_point(aes(y = alld2t1[,6])) + 
  geom_line(aes(y= alld2t1[,6], color="女性青年創業貸款人數"))+
  geom_point(aes(y = alld2t1[,7])) + 
  geom_line(aes(y= alld2t1[,7], color="總青年創業貸款人數")) + xlab("年") + ylab("人數")}) 

output$plot4 <-renderPlot({ggplot(t2, aes(x = 91:106)) + 
  geom_point(aes(y =t2[,4])) + 
  geom_line(aes(y = t2[,4],  color="青年創業貸款總人數")) +
  geom_point(aes(y = t2[,2])) + 
  geom_line(aes(y= t2[,2], color="青年失業總人數")) +
  geom_point(aes(y = t2[,3])) + 
  geom_line(aes(y= t2[,3], color="青年就業總人數"))+
  geom_point(aes(y = t2[,5])) + 
  geom_line(aes(y= t2[,5], color="青年總人數")) + xlab("年") + ylab("千人")})

output$plot41 <-renderPlot({barplot(t22, col = "skyblue",xlab = "91年至106年", ylab = "創業貸款人數占就業人數比例")})

output$plot21 <-renderPlot({barplot(dm1,width = 3, space = NULL,beside = TRUE, col = c("skyblue", "pink"), xlab = "年度", ylab = "貸款金額(千元)", legend=c("男","女"))})

output$plot5 <-renderPlot({ggplot(j33,aes(x=年,value))+
  geom_bar(aes(fill=item),stat = "identity",position="dodge",width=0.8)+
  labs(title="青年有無打算轉換工作意願",y = "百分比值") + scale_x_continuous(breaks=seq(101,105,by=2))})

output$plot51 <-renderPlot({ggplot(j3,aes(x=年,value))+
  geom_bar(aes(fill=item),stat = "identity",position="dodge",width=0.8)+
  labs(title="青年打算轉換工作原因",y = "百分比值") + scale_x_continuous(breaks=seq(101,105,by=2))})

output$plot52 <- renderPlot({ggplot(j222,aes(x=年,value))+
  geom_bar(aes(fill=item),stat = "identity",position="dodge",width=0.8)+
  labs(title="青年因創業而打算轉換工作",y = "百分比值") + scale_x_continuous(breaks=seq(101,105,by=2))})

output$plot53 <- renderPlot({ggplot(j101,aes(x = 教育程度,value))+
  geom_bar(aes(fill=item),stat = "identity",position="dodge",width=0.5)+
  labs(title="101年青年打算轉換工作情形與教育程度",y = "百分比值")})

output$plot54 <- renderPlot({ggplot(j103,aes(x = 教育程度,value))+
  geom_bar(aes(fill=item),stat = "identity",position="dodge",width=0.5)+
  labs(title="103年青年打算轉換工作情形與教育程度",y = "百分比值")}) 

output$plot55 <- renderPlot({ggplot(j105,aes(x = 教育程度,value))+
  geom_bar(aes(fill=item),stat = "identity",position="dodge",width=0.5)+
  labs(title="105年青年打算轉換工作情形與教育程度",y = "百分比值") })

output$plot6 <- renderPlot({ggplot(ld2,aes(x = 年,value))+
  geom_bar(aes(fill=item),stat = "identity",position="dodge",width=0.8)+labs(title="青年有無打算考證照比例",y = "百分比值") +scale_x_continuous(breaks=seq(101,105,by=2))})

output$plot61 <- renderPlot({ggplot(ld1,aes(x = 年,value))+
  geom_bar(aes(fill=item),stat = "identity",position="dodge",width=1.5)+
  labs(title="青年打算考證照類別比例",y = "百分比值") + scale_x_continuous(breaks=seq(101,105,by=2))})

output$plot62 <- renderPlot({ggplot(ldedu1,aes(x = 教育程度,value))+
  geom_bar(aes(fill=item),stat = "identity",position="dodge",width=0.8)+ facet_grid(年~.) +labs(title="青年有無打算考證照比例",y = "百分比值")})

output$plot63 <- renderPlot({ggplot(ldedu111,aes(x = 教育程度,value))+
  geom_bar(aes(fill=item),stat = "identity",position="dodge",width=0.8)+ facet_grid(年~.) +labs(title="教育程度和青年有無打算考證照比例之關聯",y = "百分比值")})

output$plot7<- renderPlot({ggplot(p,aes(x = 教育程度,value))+
  geom_bar(aes(fill=item),stat = "identity",position="dodge",width=0.8)+ facet_grid(年~.) +labs(title=" 青年勞工初次尋職困難與教育程度",y = "百分比值")})

output$plot8<- renderPlot({ggplot(ss,aes(x = 性別,value))+
  geom_bar(aes(fill=item),stat = "identity",position="dodge",width=0.8) + facet_grid(年~.)+labs(title=" 青年勞工初次尋職與現職工作平均每月薪資比較",y = "平均每月薪資(元)")})

output$plot81<- renderPlot({ggplot(se,aes(x = 教育程度,value))+
  geom_bar(aes(fill=item),stat = "identity",position="dodge",width=0.8) + facet_grid(年~.)+labs(title=" 青年勞工初次尋職與現職工作平均每月薪資比較",y = "平均每月薪資(元)")})

output$plot9<- renderPlot({ggplot(te1,aes(x = 教育程度,value))+
  geom_bar(aes(fill=item),stat = "identity",position="dodge",width=0.8) + facet_grid(年~.)+labs(title=" 青年勞工初次尋職時間與教育關係",y = "百分比(%)")})
output$plot91<- renderPlot({ggplot(te2,aes(x = 教育程度,value))+
  geom_bar(aes(fill=item ),stat = "identity",position="dodge",width=0.8) + facet_grid(年~.)+labs(title=" 青年勞工初次尋職時間與教育關係",y = "百分比(%)")})
}

shinyApp(ui = ui, server = server)

