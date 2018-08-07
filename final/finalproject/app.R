
# Define UI for application that draws a histogram
library(ggplot2)
library(base)
library(dplyr)
library(readr)
library(shiny)
library(readxl)
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
 tabPanel("創業貸款人數",h1("台灣青年創業貸款人數"),p("收集91~106年的資料，男性的人數皆是比女性多，而整體來看，103年是高峰，有最多創業貸款的人數，但近幾年有下滑的趨勢。"),plotOutput("plot1")),                                           
 tabPanel("創業貸款人數與青年人數",h1("台灣青創業貸款人數與青年人數"),plotOutput("plot3")),                                           
tabPanel("人數總比較",h1("人數總比較"),plotOutput("plot4"),h1("創業人數占就業人數比例"),plotOutput("plot41")),
tabPanel("創業貸款金額",h1("創業貸款金額"),p("青年創業貸款的總金額也呈現下降的趨勢。"),plotOutput("plot2"),h2("性別與貸款金額"),plotOutput("plot21"))),

navbarMenu("青年勞工調查",
           tabPanel("轉換工作情形",h1("青年轉換工作意願"),p("從圖表顯示，歷年來，沒有想換工作的比例居多，而在想換工作的青年當中，原因以待遇差為多，其次為工作發展無前景。
                                            然而因為想創業而換工作的比例並未最多，但從這三年來看，因創業而想換工作的比例有逐漸增加。不過，創業貸款的人數是呈現下降的，或許能反映出有越來越多青年有想創業的意願，但實際付出行動的卻不多。"),sidebarLayout(
             sidebarPanel(
               radioButtons("Choices", label = "Choices", choices = list("有無打算轉換工作意願" = 1, "打算轉換工作原因" = 2,"因創業而想換工作" = 3))
              ),
             mainPanel(plotOutput("plotjob")))),
           tabPanel("轉換工作與教育程度",h1("101,103,105年度 青年打算轉換工作與教育程度之關聯"),sidebarLayout(
             sidebarPanel(
               radioButtons("Choices2", label = "Choices", choices = list("101年" = 4, "103年" = 5,"105年" = 6))
             ),
             mainPanel(plotOutput("plotjobedu")))),
           tabPanel("打算考證照情形",h1("青年打算考證照情形"),p("不論是從教育程度或者證照類別來看，青年打算考證照的意願是逐年降低的。"),sidebarLayout(
             sidebarPanel(
               radioButtons("Choices3", label = "Choices", choices = list("有無打算考證照" = 7, "打算考證照類別" = 8,"有無打算考證照和教育程度" = 9))
             ),
             mainPanel(plotOutput("plotlic")))),
           tabPanel("想考證照類別與教育程度",h1("青年打算考證照之類別與教育程度"),p("國中(及以下)和高中(職)的青年較多是打算考技術士證照，但隨著教育程度提高，則是想考語文證照的比例增加。"),plotOutput("plot63")),
           tabPanel("初次尋職困難與教育程度",h1("青年初次尋職所遇到的困難"),p("不論教育程度為何，青年們所遭遇到最大的困難皆是(1)不知道自己適合哪方面的工作、(2)經驗不足。
                                                       不過，可以發現的是，教育程度為高中(職)或國中的青年，尤其是僅有國中學歷的，他們遇到學歷不足的困難較其他教育程度別的青年多。"),plotOutput("plot7")),
           tabPanel("薪資狀況",h1("男性和女性青年之薪資狀況"),p("以這三年的數據來看，男性的初次尋職或現職之工作平均每月薪資皆略高於女性，但整體來看，兩者的薪資都逐漸增加。"),plotOutput("plot8")),
           tabPanel("薪資狀況與教育程度",h1("青年薪資狀況與教育程度之關係"),p("下表呈現出青年的教育程度越高，其初次尋職之工作或現職之工作的平均每月薪資也較高。"),plotOutput("plot81")),
           tabPanel("初次尋職時間與教育程度關係",h1("青年初次尋職時間與教育程度關係"),plotOutput("plot9"),plotOutput("plot91")))
))

                    
#資料

# Define server logic required to draw a histogram

server <- function(input, output) { 
  output$plot1 <- renderPlot({ggplot(d222, aes(x = 91:106)) + 
  geom_point(aes(y = d222[,3])) + 
  geom_line(aes(y = d222[,3],  color="女性獲貸人數")) +
  geom_point(aes(y = d222[,2])) + 
  geom_line(aes(y= d222[,2], color="男性獲貸人數")) +
  geom_point(aes(y = d222[,4])) + 
  geom_line(aes(y= d222[,4], color="總獲貸人數")) + xlab("年") + ylab("人數") +theme(text=element_text(family="Heiti TC Light"))})
output$plot2 <- renderPlot({barplot(dm[,6],width = 1, space = NULL,beside = TRUE, col = c("#4FB0C6","#4F86C6","#C65146","#EC6A5C","#e97f02","#f8ca00","#8FBC94","#548687","#6E7783","#77AAAD","#99CCCC","#FFCC99","#CC9999","#CCCC99","#0099CC","#FF6666","#996699","#666666","#996697"), xlab = "年度", ylab = "貸款金額(千元)", legend=dm$"年度")+theme(text=element_text(family="Heiti TC Light"))})

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
  geom_line(aes(y= alld2t1[,7], color="總青年創業貸款人數")) + xlab("年") + ylab("人數")+theme(text=element_text(family="Heiti TC Light"))}) 

output$plot4 <-renderPlot({ggplot(t2, aes(x = 91:106)) + 
  geom_point(aes(y =t2[,4])) + 
  geom_line(aes(y = t2[,4],  color="青年創業貸款總人數")) +
  geom_point(aes(y = t2[,2])) + 
  geom_line(aes(y= t2[,2], color="青年失業總人數")) +
  geom_point(aes(y = t2[,3])) + 
  geom_line(aes(y= t2[,3], color="青年就業總人數"))+
  geom_point(aes(y = t2[,5])) + 
  geom_line(aes(y= t2[,5], color="青年總人數")) + xlab("年") + ylab("千人")+theme(text=element_text(family="Heiti TC Light"))})

output$plot41 <-renderPlot({barplot(t22, col = "skyblue",xlab = "91年至106年", ylab = "創業貸款人數占就業人數比例")+theme(text=element_text(family="Heiti TC Light"))})

output$plot21 <-renderPlot({barplot(dm1,width = 3, space = NULL,beside = TRUE, col = c("skyblue", "pink"), xlab = "年度", ylab = "貸款金額(千元)", legend=c("男","女"))+theme(text=element_text(family="Heiti TC Light"))})


output$plotjob <-renderPlot({if (input$Choices == "1"){ggplot(j33,aes(x=年,value))+
  geom_bar(aes(fill=item),stat = "identity",position="dodge",width=0.8)+
  labs(title="青年有無打算轉換工作意願",y = "百分比值") + scale_x_continuous(breaks=seq(101,105,by=2))+theme(text=element_text(family="Heiti TC Light"))}
else if (input$Choices == "2"){ggplot(j3,aes(x=年,value))+
  geom_bar(aes(fill=item),stat = "identity",position="dodge",width=0.8)+
  labs(title="青年打算轉換工作原因",y = "百分比值") + scale_x_continuous(breaks=seq(101,105,by=2))+theme(text=element_text(family="Heiti TC Light"))}
else if (input$Choices == "3"){ggplot(j222,aes(x=年,value))+
  geom_bar(aes(fill=item),stat = "identity",position="dodge",width=0.8)+
  labs(title="青年因創業而打算轉換工作",y = "百分比值") + scale_x_continuous(breaks=seq(101,105,by=2))+theme(text=element_text(family="Heiti TC Light"))}})

output$plotjobedu <- renderPlot({if (input$Choices2 == "4"){ggplot(j101,aes(x = 教育程度,value))+
  geom_bar(aes(fill=item),stat = "identity",position="dodge",width=0.5)+
  labs(title="101年青年打算轉換工作情形與教育程度",y = "百分比值")+theme(text=element_text(family="Heiti TC Light"))}
else if (input$Choices2 == "5"){ggplot(j103,aes(x = 教育程度,value))+
  geom_bar(aes(fill=item),stat = "identity",position="dodge",width=0.5)+
  labs(title="103年青年打算轉換工作情形與教育程度",y = "百分比值", theme(text=element_text(family="Heiti TC Light")))}
else if(input$Choices2 == "6"){ggplot(j105,aes(x = 教育程度,value))+
  geom_bar(aes(fill=item),stat = "identity",position="dodge",width=0.5)+
  labs(title="105年青年打算轉換工作情形與教育程度",y = "百分比值")+theme(text=element_text(family="Heiti TC Light"))}})

output$plotlic <- renderPlot({if (input$Choices3 == "7"){ggplot(ld2,aes(x = 年,value))+
  geom_bar(aes(fill=item),stat = "identity",position="dodge",width=0.8)+labs(title="青年有無打算考證照比例",y = "百分比值") +scale_x_continuous(breaks=seq(101,105,by=2))+theme(text=element_text(family="Heiti TC Light"))}
else if (input$Choices3 == "8"){ggplot(ld1,aes(x = 年,value))+
  geom_bar(aes(fill=item),stat = "identity",position="dodge",width=1.5)+
  labs(title="青年打算考證照類別比例",y = "百分比值") + scale_x_continuous(breaks=seq(101,105,by=2))+theme(text=element_text(family="Heiti TC Light"))}
else if (input$Choices3 == "9"){ggplot(ldedu1,aes(x = 教育程度,value))+
  geom_bar(aes(fill=item),stat = "identity",position="dodge",width=0.8)+ facet_grid(年~.) +labs(title="青年有無打算考證照比例",y = "百分比值")+theme(text=element_text(family="Heiti TC Light"))}})

output$plot63 <- renderPlot({ggplot(ldedu111,aes(x = 教育程度,value))+
  geom_bar(aes(fill=item),stat = "identity",position="dodge",width=0.8)+ facet_grid(年~.) +labs(title="教育程度和青年所想考證照類別之關聯",y = "百分比值")+theme(text=element_text(family="Heiti TC Light"))})

output$plot7<- renderPlot({ggplot(p,aes(x = 教育程度,value))+
  geom_bar(aes(fill=item),stat = "identity",position="dodge",width=0.8)+ facet_grid(年~.) +labs(title=" 青年勞工初次尋職困難與教育程度",y = "百分比值")+theme(text=element_text(family="Heiti TC Light"))})

output$plot8<- renderPlot({ggplot(ss,aes(x = 性別,value))+
  geom_bar(aes(fill=item),stat = "identity",position="dodge",width=0.8) + facet_grid(年~.)+labs(title="青年勞工初次尋職與現職工作平均每月薪資比較", y = "平均每月薪資(元)", theme(text=element_text(family="Heiti TC Light")))})

output$plot81<- renderPlot({ggplot(se,aes(x = 教育程度,value))+
  geom_bar(aes(fill=item),stat = "identity",position="dodge",width=0.8) + facet_grid(年~.)+labs(title="青年勞工初次尋職與現職工作平均每月薪資比較", y = "平均每月薪資(元)", theme(text=element_text(family="Heiti TC Light")))})

output$plot9<- renderPlot({ggplot(te1,aes(x = 教育程度,value))+
  geom_bar(aes(fill=item),stat = "identity",position="dodge",width=0.8) + facet_grid(年~.)+labs(title=" 青年勞工初次尋職時間與教育關係",y = "百分比(%)", theme(text=element_text(family="Heiti TC Light")))})
output$plot91<- renderPlot({ggplot(te2,aes(x = 教育程度,value))+
  geom_bar(aes(fill=item),stat = "identity",position="dodge",width=0.8) + facet_grid(年~.)+labs(title=" 青年勞工初次尋職時間與教育關係",y = "百分比(%)", theme(text=element_text(family="Heiti TC Light")))})
}

shinyApp(ui = ui, server = server)

