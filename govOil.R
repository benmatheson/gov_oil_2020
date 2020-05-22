library(tidyverse)
library(lubridate)
library(directlabels)
library(gganimate)
library(extrafont)


setwd("projects/govOil")

rawOil <- read_csv("data/WTISPLC.csv")
rawGov <- read_csv("data/govEnd.csv")


gov <- rawGov[1:13,1:4]
gov$startDate <- mdy(gov$start)
gov$endDate <- mdy(gov$end)
gov$startMonth <- round_date(gov$startDate, unit="month")
gov$endMonth <- round_date(gov$endDate, unit="month")
gov$secondYear <- gov$startDate + 730
gov$firstYear <- gov$startDate + 365
gov$fourthYear <- gov$startDate + 1460



govFun <- function (start){
  filtered <- rawOil %>% filter(DATE >= start & DATE < (start+1460)) %>% select(WTISPLC)
# print(filtered)  
return (filtered)
}
# 
# t <- sapply(gov$startDate, testFun)
# v <- vapply(gov$startDate, testFun,  FUN.VALUE = list(1) )


r <- do.call("rbind", sapply(gov$startMonth, govFun)) #create big old dataframe
# q <- bind_rows(mapply(govFun, gov$startMonth, gov$endMonth)) #create big old dataframe


# rdf <- data.frame(r)
govOil <- data.frame(cbind(gov$name, r )) #add gov name to it


##create number of monhts
# relMonth <- seq(1:ncol(r))
relMonth <- sprintf('%0.2d', 1:ncol(r))
namVector <- paste("m0", relMonth, sep="") #paste m on it
colnames(govOil) <- c("name", namVector ) #assign colnames

###bring togeteh
govOilG  <- gather(govOil, m001:m048, key="month", value="price")
#now to match up

govFirst <- govOilG %>% filter(month=="m001")
govOilG <- left_join(govOilG, govFirst, by="name")
govOilG <- govOilG %>% select(1,2,3,5)
colnames(govOilG) <- c("name", "month", "price", "priceOrg")
govOilG$price <- as.numeric(govOilG$price)
govOilG$priceOrg <- as.numeric(govOilG$priceOrg)
govOilG <- govOilG %>%  mutate(percentChange =  (price-priceOrg)/priceOrg)

govOilG <- left_join(govOilG, gov, by="name") 



write_csv(govOilG, 'govOilG.csv')
write_csv(govOilG, 'govOilG4_year.csv')

##start here

govOilG <- read_csv('govOilG.csv')
govOilG <- read_csv('govOilG4_year.csv')
# parnell <- govOilG %>% filter (name=="Sean Parnell")
# govOilSpread <- govOilG %>% select(name, month, percentChange) %>%  spread(key=month, value=percentChange)
govOilG <- govOilG %>% mutate(mInt = as.numeric(substr(month, 3,4)))
View(govOilG)


custPal <- c('#9e0142','#d53e4f','#f46d43','#fdae61','#fee08b','#ffffbf','#e6f598','#abdda4','#66c2a5','#3288bd','#5e4fa2', "#ffffff", '#8e0152')
benPal <- colorRampPalette(c("blue", "red"))( 13)
widePal <- colorRampPalette(c("blue", "yellow", "gray", "purple", "pink", "red"))( 13)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
chrom <- c('#ffa500','#fe8c09','#f8751c','#ee5f2b','#e14a38','#d03645','#be2352','#a9105e','#93006a','#7a0076','#5f007f','#3e0087','#00008b')
chrom2 <- c('#a6cee3','#4d8dc0','#7da9a2','#95cf73','#33a02c','#d1a07f','#f3665a','#ec552f','#fdbf6f','#ff9029','#ef997a','#b294c7','#6a3d9a')
chrom1 <- c('#e41a1c','#966a84','#4e8e96','#4daf4a','#8b7488','#c25c7a','#ff7f00','#ffd623','#e3c532','#a65628','#de728c','#d98cb2','#999999')
brewRain <- c('#fca55d','#9e0142','#e2534b','#f67747','#fecb79','#a2d9a4','#c5314b','#ffe99a','#71c6a5','#cdeb9d','#4fa0b3','#5e4fa2','#4678b5')



###price###
price <- ggplot(govOilG)+
  geom_line(aes(x=month, y=price, group=name, color=name), size=.7, show.legend = F)+
  scale_colour_manual(values=brewRain)+
  theme_minimal()+
  geom_hline(yintercept=50,linetype="dotted", size=.3, color="gray" )+
  geom_hline(yintercept=100,linetype="dotted", size=.3, color="gray" )+
  geom_hline(yintercept=25,linetype="dotted", size=.3, color="gray" )+
  transition_reveal(id=name, along=mInt) +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text=element_text(family="Fira Sans", size=7))+
  geom_dl(aes(x="m010", y=price, group=name,label = name),family="Fira Sans", method = "first.bumpup") +
  ggsave("price2yrchrom.png", width=10, height=5, dpi=600, units = "in")
  
##percentage
percent <- ggplot(govOilG)+
  geom_line(aes(x=month, y=percentChange, group=name, color=name), show.legend = F)+
  scale_colour_manual(values=brewRain)+
  # transition_reveal(month, percentChange) +
  theme_minimal()+
  geom_hline(yintercept=.5,linetype="dotted", size=.3, color="gray" )+
  geom_hline(yintercept=1,linetype="dotted", size=.3, color="gray" )+
  geom_hline(yintercept=0,linetype="dotted", size=.3, color="gray" )+
  theme_void()+
  geom_label(aes(x=month, y=percentChange, label=name, group=name, color=name),size=6, face = "bold", show.legend = F)+
  # theme(text=element_text(family="Fira Sans", size=14, face = "bold"))+
  geom_text(aes(x="m022", y=.5, label="+50%"), size=13)+
  geom_text(aes(x="m022", y=1, label="+100%"),size=13)+
  geom_text(aes(x="m022", y=0, label="0%"),size=13)+
  theme(plot.margin = unit(c(.3,1,.3,.3), "cm"))+
  # geom_dl(aes(x=month, y=percentChange, group=name,label = name), method = "last.qp") +
  transition_reveal(id=name, along=mInt)

animate(percent, 100, 10, width=1000, height=1050)
anim_save("percent_bold_label_6_13.gif")

  
  # ggsave("percent2yearColor1.png", width=10, height=5, dpi=600, units = "in")


orderName <-  c("1"='William A. Egan 1', '2'='Wally Hickel 1', '3'='Keith Harvey Miller', '4'='William A. Egan 2','5'='Jay Hammond', '6'='Bill Sheffield', '7'='Steve Cowper', '8'='Wally Hickel 2', '9'='Tony Knowles', '10'='Frank Murkowski', '11'='Sarah Palin', '12'='Sean Parnell', '13'='Bill Walker')
####faceted

facet <- ggplot(govOilG, aes(x=month, y=percentChange, group=name, color=name))+
  geom_line( show.legend = F, size=1.4, linetype="solid")+
  theme(text=element_text(family="Fira Sans", size=20))+
  geom_hline(yintercept=0,linetype="dotted", size=.6, color="gray" )+
  scale_x_discrete(name="month", breaks = c("m001", "m024"),labels =c("Inaguration", "Two Years"))+
  facet_wrap(. ~ order, labeller = labeller(
    order = orderName))+
  # scale_colour_manual(values=custPal)+
  scale_colour_manual(values=brewRain)+
  # transition_reveal(month, percentChange) +
theme_void()+
  
theme(plot.margin = unit(c(.3,.3,.3,.3), "cm"))+
  theme(strip.text.x = element_text(size=17, face="bold"))+
  theme(panel.spacing.y=unit(1, "lines"))+
  # transition_reveal(id=name, along=mInt)+
  ggsave("facetPercent4yearCust.png", width=10, height=11, dpi=600, units = "in")
# animate(facet, 100, 10, width=1000, height=1050)
# anim_save("facet_bold-4.gif")

  
