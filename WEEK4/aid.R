which(is.na[afe$AgeAtAdverseEvent])
afe <- data.frame(AdverseFoodEvents) 
complete.cases(afe)
rm.data <- afe[complete.cases(afe), ]

sum(is.na(afe))
na.omit(afe) 

fda4 <- na.omit(afe) 
fit_m2 <- data.frame(fda4[, c(7, 8, 10)], fitted = fitted(m2), resid = resid(m2),
                     infl = influence(m2)$hat )

which(is.na(afe))
na.omit(afe)
str(afe)
sum(is.na(afe))
afe2 <- na.omit(afe)
afe2
sum(is.na(afe2))

na.omit(afeg)
afeg2 <- na.omit(afeg)
sum(is.na(afeg2))

fit_m22 <- fit_m2 %>%  group_by(IndustryName) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(IndustryName = reorder(IndustryName,Count)) %>%
  head(5) %>% mutate(IndustryName = reorder(IndustryName,Count))

  ggplot(data = fit_m22, aes(x = AgeAtAdverseEvent, group = IndustryName )) +
  stat_density(geom = 'path', position = 'identity') +
  stat_density(geom = 'path', position = 'identity', aes(x = fitted)) + 
  geom_vline(xintercept = c(with(afe2, tapply(AgeAtAdverseEvent,IndustryName, mean))), linetype = 'dotted')+
  facet_grid(IndustryName ~ .) +
  scale_x_continuous(breaks = seq(100, 900, by = 100))+
  labs(x = 'Age', y = 'IndustryName')

  TransformIntoYears = function(ds)
  {if(!is.na(ds["AgeAtAdverseEvent"]))
  { x = as.numeric(ds["AgeAtAdverseEvent"])
  ds["AgeAtAdverseEvent"] = as.numeric(ds["AgeAtAdverseEvent"])
  if(ds["AgeUnit"] == "Month(s)")
  {ds["AgeAtAdverseEventInYears"] = x/12}
  else if (ds["AgeUnit"] == "Weeks(s)")
  {ds["AgeAtAdverseEventInYears"] = x*7/365}
  else if (ds["AgeUnit"] == "Day(s)")
  {ds["AgeAtAdverseEventInYears"] = x*1/365}
  else if (ds["AgeUnit"] == "Decade(s)")
  {ds["AgeAtAdverseEventInYears"] = x*10}  }
    return(ds)}
  
  AdverseFoodEvents = AdverseFoodEvents %>%
    mutate(AgeAtAdverseEventInYears = AgeAtAdverseEvent) 
  
  AdverseFoodEvents$AgeAtAdverseEventInYears = 
    as.numeric(AdverseFoodEvents$AgeAtAdverseEventInYears)
  
  AdverseFoodEvents = sapply(AdverseFoodEvents,TransformIntoYears)
  
  AdverseFoodEvents = as.data.frame(AdverseFoodEvents)

  
  
  afe <- data.frame(AdverseFoodEvents) 
  library(ggplot2)
  afeg <- filter(afe2, Gender %in% c("Male","Female")) %>%
   
       mutate(AgeAtAdverseEventInYears= as.numeric(AgeAtAdverseEventInYears))
  ggplot(data = afeg, aes(x = Gender, y = AgeAtAdverseEventInYears)) +
      geom_boxplot() + coord_flip() +
      labs( y = 'month', x = 'gender', 
                     title = 'Gender and AgeAtAdverseEvent relationship Box Plot')
  
  afe <- mutate(AgeAtAdverseEventInYears= as.numeric(AgeAtAdverseEventInYears))
  ggplot(data = afe, aes(x = Gender, y = AgeAtAdverseEventInYears)) +
    geom_boxplot() 
  
  
  dta_age <- afeg[, c('PRI_FDA.Industry.Code','AgeAtAdverseEventInYears')]
  afeg2colMeans(dta_age)
str(dta_age)

afe2 = afe2 %>% mutate(afern = ReportNo) 
afe2$afern = as.numeric(afe2$afern)
afe2 = as.data.frame(afe2)
sapply(afeg, class)
sapply(afeg, is.factor)
cor(afeg[sapply(afeg, function(x) !is.factor(x))])


as.numeric(levels(afeg$ReportNo))[afeg$ReportNo]
sapply(afeg, class)

as.numeric(afeg$ReportNo.factor)
as.numeric(as.factor(ReportNo))
sapply(afeg, class)

afeg$ReportNo <- as.numeric(as.character(afeg$ReportNo))
sapply(afeg, class)
afeg$PRI_FDA.Industry.Code <- as.numeric(as.character(afeg$PRI_FDA.Industry.Code))
sapply(afeg, class)
