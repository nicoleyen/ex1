
# Define UI for application that draws a histogram
library(ggplot2)
ui <- shinyUI(navbarPage("台灣青年勞工狀況",
                         navbarMenu("失業", h1("台灣青年勞工失業資料"),tabPanel("Option2-1"),
                                                tabPanel("Option2-1")),
                          
navbarMenu("就業",
            tabPanel("Option2-1"),
            tabPanel("Option2-1"),
            tabPanel("Option2-1")),
navbarMenu("創業",
 tabPanel("創業貸款人數",plotOutput("plot1")),                                           
 tabPanel("創業貸款人數與青年人數",plotOutput("plot3")),                                           
tabPanel("人數總比較",plotOutput("plot4"),plotOutput("plot41")),
tabPanel("創業貸款金額",plotOutput("plot2"),plotOutput("plot21"))),

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

