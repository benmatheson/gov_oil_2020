library(tidyverse)
library(lubridate)
library(directlabels)
library(gganimate)
library(extrafont)
library(janitor)
library(stringr)
library(ggridges)
library(zoo)
library(PMwR)
library(scales)


# http://www.tax.alaska.gov/programs/oil/dailyoil/dailyoil.aspx
options(scipen=999)
# setwd("projects/govOil")
# 
# rawOil <- read_csv("data/WTISPLC.csv")
# rawOil <- read_csv("data/oil_prices - Sheet1.csv") #old
rawOil <- read_csv("data/oil_prices - Sheet2.csv") ## from Monday, 5/12
rawGov <- read_csv("data/govEnd_2020.csv")


oil <- rawOil %>% clean_names() %>% mutate( date =mdy(date) ) %>% filter (!is.na(ans)) %>% filter(date>='2002-12-02')

gov <- rawGov[1:14,1:4]
gov$start_date <- ymd(gov$start)
gov$end_date<- ymd(gov$end)
gov$start_month <- round_date(gov$start_date, unit="month")
gov$end_month<- round_date(gov$end_date, unit="month")
gov$second_year <- gov$start_date + 730
gov$first_year<- gov$start_date + 365
gov$fourth_year <- gov$start_date + 1460


govFun <- function (start, gov_name){
  filtered <- oil %>% filter(date >= start & date < (start+1460)) %>% select(date,ans, production) %>% mutate(name = gov_name)
head(filtered)
  return (filtered)
}
govFun_full <- function (start, end, gov_name){
  filtered <- oil %>% filter(date >= start & date < (end)) %>% select(date,ans, production) %>% mutate(name = gov_name)
  return (filtered)
}
#
output_full <- pmap_df (list(gov$start_date,gov$end_date, gov$name), .f= govFun_full) %>% bind_rows()
output <- map2_df (.x=gov$start_date, .f= govFun, .y = gov$name) %>% bind_rows()
output <- output_full

gov_oil <- output %>% left_join(gov, by="name") %>% select(date, ans,  name, order, start_date, production) %>% mutate(relative_date =  difftime( date,start_date, units="days"))

gov_oil_starting_price <- gov_oil %>% group_by(name) %>% summarise(starting_price=first(ans)) %>% ungroup()
gov_oil <- gov_oil %>% left_join(gov_oil_starting_price, by="name")

gov_oil <- gov_oil %>% mutate(relative_percent = (ans-starting_price)/starting_price)
gov_oil <- gov_oil %>% mutate(year = year(date))
gov_oil <- gov_oil %>% mutate(oil_value=production*ans)
gov_oil <- gov_oil %>% ungroup() %>%  mutate(oil_value_roll_30 = rollmean(x=oil_value,k=30, fill=NA))
gov_oil <- gov_oil %>% ungroup() %>%  mutate(ans_roll_60 = rollmean(x=ans,k=60, fill=NA))
gov_oil <- gov_oil %>% ungroup() %>%  mutate(ans_roll_30 = rollmean(x=ans,k=30, fill=NA))
gov_oil <- gov_oil %>% ungroup() %>%  mutate(ans_roll_10 = rollmean(x=ans,k=10, fill=NA, align="right"))
gov_oil <- gov_oil %>% ungroup() %>%  mutate(relative_percent_roll_30 = rollmean(x=relative_percent,k=30, align="right", fill=NA))
gov_oil <- gov_oil %>% ungroup() %>%  mutate(relative_percent_roll_10 = rollmean(x=relative_percent,k=10, align="right", fill=NA))
gov_oil <- gov_oil %>% ungroup() %>%  mutate(production_roll_30 = rollmean(x=production, k=30, align="center", fill=NA))
gov_oil <- gov_oil %>% ungroup() %>%  mutate(roll_ans_drop_10 = lead(ans, 10)-ans)
gov_oil <- gov_oil %>% ungroup() %>%  mutate(roll_ans_drop_10_percent = (lead(ans, 10)-ans)/ans)

gov_oil <- gov_oil %>% rowwise() %>%  mutate (last_name = str_split(name, " ")[[1]][2])


gov_oil$name <- factor(gov_oil$name, levels = c("Tony Knowles", "Frank Murkowski", "Sarah Palin",     "Sean Parnell",    "Bill Walker",    "Mike Dunleavy" ))

gov_oil_prices <- gov_oil %>% select( ans)

# gov_oil_drawdowns <- drawdowns(gov_oil_prices$ans)

gov_oil_summary_gov <- gov_oil %>% group_by(name) %>% summarize (mean_ans = mean(ans), min_ans = min(ans), max_ans = max(ans), mean_production = mean(production), min_prodcution= min(production), max_production = max(production)) %>% ungroup()


gov_oil_copy = gov_oil


unq_gov <- unique(gov_oil$name)

# custPal <- c('#9e0142','#d53e4f','#f46d43','#fdae61','#fee08b','#ffffbf','#e6f598','#abdda4','#66c2a5','#3288bd','#5e4fa2', "#ffffff", '#8e0152')
# benPal <- colorRampPalette(c("blue", "red"))( 13)
# widePal <- colorRampPalette(c("blue", "yellow", "gray", "purple", "pink", "red"))( 13)
# cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# chrom <- c('#ffa500','#fe8c09','#f8751c','#ee5f2b','#e14a38','#d03645','#be2352','#a9105e','#93006a','#7a0076','#5f007f','#3e0087','#00008b')
# chrom2 <- c('#a6cee3','#4d8dc0','#7da9a2','#95cf73','#33a02c','#d1a07f','#f3665a','#ec552f','#fdbf6f','#ff9029','#ef997a','#b294c7','#6a3d9a')
# chrom1 <- c('#e41a1c','#966a84','#4e8e96','#4daf4a','#ff7f00'  ,'#c25c7a','#ff7f00','#8b7488', 'ffd623','#e3c532','#a65628','#de728c','#d98cb2','#999999')


# brewer_set <- c( "pink", "#ff7f00",
#   "#377eb8",
#   "#e41a1c",
#   "#984ea3",
#   "#4daf4a")
# THIS iS THE PALETTE
# brewer_set <- c(
# 
#                 "#377eb8",
#                 "#4daf4a",
#                 "#984ea3",
#                   "#ff7f00",
#                   "#e41a1c"
#                   )
#TWEAKED
brewer_set <- c(
  
  "#266ca6",
  "#3c943a",
  "#693570",
  "#f77b00",
  "#e41a1c"
)


# brewer_set <- c(      "#056e12", "#421674", "#273b61", "#d6782f", "#e51d1d")


# brewRain <- c('#fca55d','#9e0142','#e2534b','#f67747','#fecb79','#a2d9a4','#c5314b','#ffe99a','#71c6a5','#cdeb9d','#4fa0b3','#5e4fa2','#4678b5')



###price###
# price <- ggplot(govOilG)+
#   geom_line(aes(x=month, y=price, group=name, color=name), size=.7, show.legend = F)+
#   scale_colour_manual(values=brewRain)+
#   theme_minimal()+
#   geom_hline(yintercept=50,linetype="dotted", size=.3, color="gray" )+
#   geom_hline(yintercept=100,linetype="dotted", size=.3, color="gray" )+
#   geom_hline(yintercept=25,linetype="dotted", size=.3, color="gray" )+
#   transition_reveal(id=name, along=mInt) +
#   
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
#   theme(text=element_text(family="Fira Sans", size=7))+
#   geom_dl(aes(x="m010", y=price, group=name,label = name),family="Fira Sans", method = "first.bumpup") +
#   ggsave("price2yrchrom.png", width=10, height=5, dpi=600, units = "in")

##percentage
percent_plot <- ggplot(gov_oil)+
  geom_line(aes(x=as.numeric(relative_date), y=relative_percent, group=name, color=name), show.legend = F)+
  # scale_colour_brewer(palette="Set1")+
  # transition_reveal(month, percentChange) +
  theme_minimal()+
  geom_hline(yintercept=.5,linetype="dotted", size=.3, color="gray" )+
  geom_hline(yintercept=1,linetype="dotted", size=.3, color="gray" )+
  geom_hline(yintercept=0,linetype="dotted", size=.3, color="gray" )+
  # theme_void()+
  geom_text(aes(x=as.numeric(relative_date), y=relative_percent_roll_30, label=last_name, group=name, color= name), hjust = -.1, size=11, fontface = "bold", show.legend = F)+
  scale_colour_manual(values=brewer_set)+
  scale_fill_manual(values=brewer_set)+
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x= element_text(face="bold"))+
  scale_x_continuous(limits =  c(-100, 2100), breaks = c(365, 730, 1096, 1460, 1825),  labels= c("Year 1", "Year 2", "Year 3", "Year 4", "Year 5"))+
  scale_y_continuous(labels=NULL)+
  ylab(" ")+
  xlab(" ")+
  
  
  # theme(text=element_text(family="Fira Sans", size=14, face = "bold"))+
  geom_text(data =  gov_oil_copy, aes(x=-50, y=.5, label="+50%"), size=13)+
  geom_text(data =  gov_oil_copy, aes(x=-50, y=1, label="+100%"),size=13)+
  geom_text(data =  gov_oil_copy, aes(x=-50, y=0, label="0%"),size=13)+
  geom_text(data =  gov_oil_copy, aes(x=-50, y=-.5, label="-50%"),size=13)+
  geom_text(data =  gov_oil_copy, aes(x=-50, y=-1, label="-100%"),size=13)+  
  # geom_text(aes(x=-365, y=-1, label="1 Year"),size=10)+
  # scale_x_continuous(breaks = c(365, 730, 1096, 1460),  labels= c("1 year", "2 year", "Year 3", "Year 4"))+
  theme(plot.margin = unit(c(.3,1,.3,.3), "cm"))+
  theme( text = element_text(size = 28),
        plot.title = element_text(family="IBM Plex Sans Medium",size=62, color="#333333"))+
  labs(title= "Relative Oil Prices for Alaska Governors", subtitle = "Alaska North Slope daily price change relative to first day in office.", caption="Data: Alaska Department of Revenue")+
  # ggsave("percent_2020.png", width=10, height=11, dpi=300, units = "in")+
  # geom_dl(aes(x=month, y=percentChange, group=name,label = name), method = "last.qp") +
  # transition_reveal(id=name, along=mInt)
  # transition_time(id=name, along=date)
  # transition_components(date) +
  transition_reveal(along = as.numeric(relative_date), id=name, keep_last = T)

  
stacked_gov <- animate(percent_plot, nframes=200, fps=16, width=1550, height=1000)
anim_save(animation = stacked_gov, "plots/percent_bold_label_200_16_roll.gif")



################FACETS##################


facet_plot <- ggplot(gov_oil)+
  geom_line(aes(x=as.numeric(relative_date), y=relative_percent, group=name, color=name), size=1, show.legend = F)+
  # scale_colour_brewer(palette="Set1")+
  # transition_reveal(month, percentChange) +
  theme_minimal()+
  geom_hline(yintercept=.5,linetype="dotted", size=1, color="gray" )+
  geom_hline(yintercept=1,linetype="dotted", size=1, color="gray" )+
  geom_hline(yintercept=0,linetype="dotted", size=1, color="gray" )+
  # geom_text(aes(x=as.numeric(relative_date), y=relative_percent, label=last_name, group=name, color= name), size=10,  hjust = -.1, fontface = "bold", show.legend = F)+
  scale_colour_manual(values=brewer_set)+
  scale_fill_manual(values=brewer_set)+
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x= element_text(face="bold"))+
  scale_x_continuous(limits =  c(-100, 2100), breaks = c(365, 730, 1096, 1460, 1825),  labels= c("Year 1", "Year 2", "Year 3", "Year 4", "Year 5"))+
  scale_y_continuous(labels=NULL)+
  ylab(" ")+
  xlab(" ")+
  
  
  # theme(text=element_text(family="Fira Sans", size=14, face = "bold"))+
  geom_text(aes(x=-20, y=.5, label="+50%"), size=6)+
  geom_text(aes(x=-20, y=1, label="+100%"),size=6)+
  geom_text(aes(x=-20, y=0, label="0%"),size=6)+
  geom_text(aes(x=-20, y=-.5, label="-50%"),size=6)+
  theme(plot.margin = unit(c(.3,.3,.3,.3), "cm"))+
  theme( text = element_text(size = 18),
         plot.title = element_text(family="IBM Plex Sans Medium",size=48, color="#333333"), plot.subtitle = element_text(margin = margin(.1,.1, 99,.1)))+
  facet_wrap(~name)+
  labs(title= "Relative Oil Prices for Alaska Governors", subtitle = "Alaska North Slope daily price change relative to first day in office.", caption = "Data: Alaska Department of Revenue")+
  ggsave("plots/facet_percent_2020.png", width=15, height=10, dpi=300, units = "in")
  



# geom_dl(aes(x=month, y=percentChange, group=name,label = name), method = "last.qp") +
  # transition_reveal(id=name, along=mInt)
  # transition_time(id=name, along=date)
  # transition_components(date) +
  # transition_reveal(along = as.numeric(relative_date), id=name)

# 
# facet_gov <- animate(facet_plot, nframes=4, fps=1, width=1550, height=1000)
# anim_save(animation = facet_gov, "facet_bold_label_4_24.gif")
# 




###roll10 or roll30 - the 30 disapers
dollar_plot <- ggplot(gov_oil)+
  geom_line(aes(x=as.numeric(relative_date), y=ans, group=name, color=name), show.legend = F)+
  # scale_colour_brewer(palette="Set1")+
  # transition_reveal(month, percentChange) +
  theme_minimal()+
  geom_hline(yintercept=100,linetype="dotted", size=1, color="gray" )+
  geom_hline(yintercept=50,linetype="dotted", size=1, color="gray" )+
  geom_hline(yintercept=150,linetype="dotted", size=1, color="gray" )+
  geom_text(aes(x=as.numeric(relative_date), y=ans_roll_10, label=last_name, group=name, color= name), size=11, hjust =-.1, fontface = "bold", show.legend = F)+
  scale_colour_manual(values=brewer_set)+
  scale_fill_manual(values=brewer_set)+
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x= element_text(face="bold"))+
  scale_x_continuous(limits =  c(-100, 2100), breaks = c(365, 730, 1096, 1460, 1825),  labels= c("Year 1", "Year 2", "Year 3", "Year 4", "Year 5"))+
  scale_y_continuous(labels=NULL)+
  ylab(" ")+
  xlab(" ")+
  
  geom_text(aes(x=-10, y=0, label="$0"), size=13)+
  geom_text(aes(x=-50, y=50, label="$50"),size=13)+
  geom_text(aes(x=-50, y=100, label="$100"),size=13)+
  geom_text(aes(x=-50, y=150, label="$150"),size=13)+
  # geom_text(aes(x=-500, y=-.5, label="-50%"),size=13)+
  theme(plot.margin = unit(c(.3,1,.3,.3), "cm"))+
  theme( text = element_text(size = 30),
         plot.title = element_text(family="IBM Plex Sans Medium",size=60, color="#333333"), plot.subtitle = element_text(margin = margin(.1,.1,3,.1)))+
  labs(title= "Oil Prices for Alaska Governors", subtitle = "Alaska North Slope Daily Price Per Barrel")+
  # ggsave("percent_2020.png", width=10, height=11, dpi=300, units = "in")+
  # geom_dl(aes(x=month, y=percentChange, group=name,label = name), method = "last.qp") +
  # transition_reveal(id=name, along=mInt)
  # transition_time(id=name, along=date)
  # transition_components(date) +
  transition_reveal(along = as.numeric(relative_date), id=name)

dollar_gov <- animate(dollar_plot, nframes=200, fps=16, width=1550, height=1000)
anim_save(animation = dollar_gov, "plots/dollar_bold_label_200_16.gif")



# , plot.subtitle = element_text(margin = margin(.1,.1,3,.1))
dollar_static <- function (){

ggplot(gov_oil)+
  geom_line(aes(x=as.numeric(relative_date), y=ans, group=name, color=name), show.legend = F)+
  # scale_colour_brewer(palette="Set1")+
  # transition_reveal(month, percentChange) +
  theme_minimal()+
  geom_hline(yintercept=100,linetype="dotted", size=1, color="gray" )+
  geom_hline(yintercept=50,linetype="dotted", size=1, color="gray" )+
  geom_hline(yintercept=150,linetype="dotted", size=1, color="gray" )+
  geom_dl(data  =  gov_oil %>% filter (name!= "Frank Murkowski"), aes(x=as.numeric(relative_date), y=ans, label=last_name, group=name, color= name), size=11, face = "bold", method=list("last.bumpup", cex=1.8, fontface="bold",  hjust =-.1, vjust =-.1), show.legend = F)+
  geom_dl(data =  gov_oil %>% filter (name== "Frank Murkowski"), aes( x=as.numeric(relative_date), y=ans, label=last_name, group=name, color= name), size=11, face = "bold", method=list("last.bumpup", cex=1.8, fontface="bold",  hjust =-.1, vjust =1), show.legend = F)+
  scale_colour_manual(values=brewer_set)+
  scale_fill_manual(values=brewer_set)+
    # xlim(-100,1800)+
  
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x= element_text(face="bold"))+
    scale_x_continuous(limits =  c(-100, 2100), breaks = c(365, 730, 1096, 1460, 1825),  labels= c("Year 1", "Year 2", "Year 3", "Year 4", "Year 5"))+
    scale_y_continuous(labels=NULL)+
    ylab(" ")+
    xlab(" ")+
  
  # theme(text=element_text(family="Fira Sans", size=14, face = "bold"))+
  geom_text(aes(x=-10, y=0, label="$0"), size=9)+
  geom_text(aes(x=-50, y=50, label="$50"),size=9)+
  geom_text(aes(x=-50, y=100, label="$100"),size=9)+
  geom_text(aes(x=-50, y=150, label="$150"),size=9)+
  # geom_text(aes(x=-500, y=-.5, label="-50%"),size=13)+
  theme(plot.margin = unit(c(.3,.3,.3,.3), "cm"))+
  theme( text = element_text(size = 20),
         plot.title = element_text(family="IBM Plex Sans Medium",size=45, color="#333333"))+
  labs(title= "Oil Prices for Alaska Governors", subtitle = "Alaska North Slope Daily Price, not adjusted for inflation or anything.",  caption="Data: Alaska Department of Revenue")+
  ggsave("plots/dollar_2020.png", width=15, height=10, dpi=300, units = "in")}

dollar_static()




# gov_oil <- gov_oil %>% mutate(relative_date_num = as.numeric(relative_date))

percent_static <- function () {
  
  percent_plot <- ggplot(gov_oil)+
    geom_line(aes(x=as.numeric(relative_date), y=relative_percent, group=name, color=name), show.legend = F)+
    # scale_colour_brewer(palette="Set1")+
    # transition_reveal(month, percentChange) +
    theme_minimal()+
    geom_hline(yintercept=.5,linetype="dotted", size=1, color="gray" )+
    geom_hline(yintercept=1,linetype="dotted", size=1, color="gray" )+
    geom_hline(yintercept=0,linetype="dotted", size=1, color="gray" )+
    # theme_void()+
    geom_dl(aes(x=as.numeric(relative_date), y=relative_percent, label=last_name, group=name, color= name), size=10, face = "bold", method=list("last.bumpup", cex=1.8, fontface="bold",  hjust =-.1), show.legend = F)+
    scale_colour_manual(values=brewer_set)+
    scale_fill_manual(values=brewer_set)+
    scale_x_continuous(limits = c(-100, 2100), breaks = c(365, 730, 1096, 1460, 1825),  labels= c("Year 1", "Year 2", "Year 3", "Year 4", "Year 5"))+
    scale_y_continuous(labels=NULL)+
    ylab(" ")+
    xlab(" ")+
    theme(axis.text.x= element_text(face="bold"))+
    
    # xlim(-100, 1800)+
    # theme(text=element_text(family="Fira Sans", size=14, face = "bold"))+
    geom_text(aes(x=-50, y=.5, label="+50%"), size=9)+
    geom_text(aes(x=-50, y=1, label="+100%"),size=9)+
    geom_text(aes(x=-50, y=0, label="0%"),size=9)+
    geom_text(aes(x=-50, y=-.5, label="-50%"),size=9)+
    geom_text(aes(x=-50, y=-1, label="-100%"),size=9)+
    theme(plot.margin = unit(c(.3,.3,.3,.3), "cm"))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), text = element_text(size = 18),
           plot.title = element_text(family="IBM Plex Sans Medium",size=45, color="#333333"))+
    labs(title= "Relative Oil Prices for Alaska Governors", subtitle = "Alaska North Slope daily price change relative to first day in office.", caption="Data: Alaska Department of Revenue")+
    ggsave("plots/percent_2020.png", width=15, height=10, dpi=300, units = "in")
    

}

percent_static()




ggplot(gov_oil)+
  geom_histogram(aes(x=ans, fill = name), bins = 60)+
  theme_minimal()

ggplot(gov_oil)+
  geom_density(aes(x=ans, fill = name), bins = 150)+
  theme_minimal()


# scale="count",
ggplot(gov_oil, aes(x=oil_value, y=name, fill = name, color=name), show.legend = F)+
  geom_violin(aes(y=ans, x=name, fill = name, color=name), scale = "count", show.legend = F)+
  # geom_density_ridges2(panel_scaling = T, stat="binline")+
  # coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  scale_color_manual(values=brewer_set)+
  scale_fill_manual(values=brewer_set)+
  # theme_minimal()+
  theme_ridges() +
  scale_y_continuous(labels=dollar_format())+
  theme( text = element_text(size = ),
         plot.title = element_text(family="IBM Plex Sans Medium",size=27, color="#333333"), plot.subtitle = element_text(margin = margin(.1,.1,3,.1)))+
  # scale_y_continuous(labels=dollar_format())+
  ylab(" ")+
  xlab(" ")+
  labs(title= "Oil Prices for Alaska Governors' Terms", subtitle = "Alaska North Slope Daily Price, not adjusted for inflation or anything.",  caption="Data: Alaska Department of Revenue")+
  ggsave("plots/violin_2020.png", width=10, height=5, dpi=300, units = "in")


# 
# ggplot(gov_oil %>% filter(year>2006 &production>400000))+
#   # geom_violin(aes(y=ans, x=name, fill = name, color=name), show.legend = F)+
# # geom_line(aes(y=oil_value, x=date, fill = name, color=name))+  # coord_flip() + # This switch X and Y axis and allows to get the horizontal version
# # geom_line(aes(y=oil_value_roll_30, x=date))+  # coord_flip() + # This switch X and Y axis and allows to get the horizontal version
# geom_line(aes(y=ans_roll_60, x=date, color=name))+  # coord_flip() + # This switch X and Y axis and allows to get the horizontal version
# # geom_line(aes(y=ans, x=date))+  # coord_flip() + # This switch X and Y axis and allows to get the horizontal version
# # geom_line(gov_oil %>% filter(year>2006 &production>400000), aes(y=oil_value_roll_30, x=date, fill = name, color=name), show.legend = F)+  # coord_flip() + # This switch X and Y axis and allows to get the horizontal version
#   scale_color_manual(values=brewer_set)+
#   scale_fill_manual(values=brewer_set)+
#   theme( text = element_text(size = 25),
#          plot.title = element_text(family="IBM Plex Sans Medium",size=60, color="#333333"), plot.subtitle = element_text(margin = margin(.1,.1,3,.1)))+
#   theme_minimal()+
#   labs(title= "Absolute Oil Prices for Alaska Governors' Terms", subtitle = "Alaska North Slope Daily Price, not adjusted for inflation or anything. Low production days of less that 400,000 barrels are not shown.",  caption="Data: Alaska Department of Revenue")+
#   ggsave("plots/line_2020.png", width=15, height=5, dpi=300, units = "in")




ggplot(gov_oil %>% filter(year>1999& production>400000))+
  # geom_violin(aes(y=ans, x=name, fill = name, color=name), show.legend = F)+
  # geom_line(aes(y=oil_value, x=date, fill = name, color=name))+  # coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  # geom_line(aes(y=oil_value_roll_30, x=date))+  # coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  # geom_line(aes(y=production, x=date, color=name))+  # coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  geom_line(aes(y=production, x=date, color=name),show.legend = F)+  # coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  # geom_line(aes(y=ans, x=date))+  # coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  # theme(text = element_text(size = 25),
  #        plot.title = element_text(family="IBM Plex Sans Medium",size=90, color="#333333"), plot.subtitle = element_text(margin = margin(.1,.1,3,.1)))+
  
    scale_color_manual(values=brewer_set)+
  scale_fill_manual(values=brewer_set)+
  theme_minimal()+
  geom_dl(data=gov_oil %>% filter(name != "Frank Murkowski"), aes(x=date, y=production, color=name, label=name),method=list("smart.grid", cex=1.3, vjust=-2, fontface="bold"))+
  geom_dl(data=gov_oil %>% filter(name == "Frank Murkowski"),aes(x=date, y=production, color=name, label=name),method=list("top.points",hjust =-.2, vjust=-1, cex=1.3, fontface="bold"))+
  ylim(400000,1.1e6)+
  labs(title= "Oil Production for Alaska Governors' Terms", subtitle = "Barrels per day of North Slope production. Small production days of less than 400K barrels are excluded.",  caption="Data: Alaska Department of Revenue")+
  theme( text = element_text(size = 20),
         plot.title = element_text(family="IBM Plex Sans Medium",size=40, color="#333333"))+
  xlab(" ")+
  ylab(" ")+
  # scale_y_continuous(labels = number_format())+
  scale_y_continuous(breaks = c (300000, 500000, 750000, 1000000),labels =c("300K", "500K", "750K", "1 Million\n Barrels"), limits = c(000000,1100000))+
  ggsave("plots/line_production_2020.png", width=15, height=10, dpi=300, units = "in")





 # ggplot(gov_oil %>% filter (production>300000))+
#   # geom_point(aes(x=ans, y=production,    theme( text = element_text(size = 22),
# plot.title = element_text(family="IBM Plex Sans Medium",size=52, color="#333333"))+
#   color=name, size=production),alpha=.3, show.legend=F) +
#   geom_jitter(aes(x=ans, y=production,  color=name, fill=name, size=production),alpha=.3, show.legend=F) +
#   # geom_segment(aes(y=ans, x=production, color=name),alpha=.3)+
#   scale_color_manual(values=brewer_set)+
# theme_minimal()+
#   # coord_flip()+
# 
#   ggsave("plots/scatter_2020.png", width=15, height=10, dpi=300, units = "in")


  # coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  




ggplot(gov_oil)+
  geom_point(aes(y=ans, x=relative_percent, color = factor(name)), bins = 10)+
  
  coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  theme_minimal()



time_plot <- ggplot(gov_oil)+
  geom_line(aes(x=date, y=ans, group=name, color=name), show.legend = F)+
  # geom_col(aes(x=date, y=roll_ans_drop_10, group=name, color=name), show.legend = F)+
  # geom_col(aes(x=date, y=roll_ans_drop_10, group=name, color=name), show.legend = F)+

  geom_hline(yintercept=100,linetype="dotted", size=.3, color="gray" )+
  geom_hline(yintercept=50,linetype="dotted", size=.3, color="gray" )+
  geom_hline(yintercept=150,linetype="dotted", size=.3, color="gray" )+
  theme_minimal()+
  ylab("Oil Price for Alaska Governors")+
  # geom_text(aes(x=as.numeric(relative_date), y=ans, label=last_name, group=name, color= name), size=11, hjust =-.1, face = "bold", show.legend = F)+
  scale_colour_manual(values=brewer_set)+
  scale_fill_manual(values=brewer_set)+
  
  # # theme(text=element_text(family="Fira Sans", size=14, face = "bold"))+
  # geom_text(aes(x=-10, y=0, label="$0"), size=13)+
  # geom_text(aes(x=-50, y=50, label="$50"),size=13)+
  # geom_text(aes(x=-50, y=100, label="$100"),size=13)+
  # geom_text(aes(x=-50, y=150, label="$150"),size=13)+
  geom_dl(aes(x=date, y=ans, color=name, label=name),method=list("top.points", cex=1.5, vjust=-1, hjust=.4,fontface="bold"))+
  theme(plot.margin = unit(c(.3,1,.3,.3), "cm"))+
  theme( text = element_text(size = 22),
         plot.title = element_text(family="IBM Plex Sans Medium",size=52, color="#333333"))+
  labs(title = "Oil Prices for Alaska Governors", subtitle ="Dollars per barrel, not adjusted for inflation or anything")+
  xlab (" ")+
  ylab( " ")+
  ggsave("plots/absolute_2020.png", width=15, height=10, dpi=300, units = "in")
 

time_plot



price_plot <- ggplot(gov_oil_summary_gov)+
  # geom_col(aes(x=reorder(name, mean_ans), y=mean_ans,fill=name), show.legend = F)+
  geom_col(aes(x=reorder(name, mean_production), y=mean_ans,fill=name), show.legend = F)+
  # geom_col(aes(x=name, y=mean_ans,fill=name), show.legend = F)+
  # geom_col(aes(x=date, y=roll_ans_drop_10, group=name, color=name), show.legend = F)+
  
  # geom_hline(yintercept=100,linetype="dotted", size=.3, color="gray" )+
  # geom_hline(yintercept=50,linetype="dotted", size=.3, color="gray" )+
  # geom_hline(yintercept=150,linetype="dotted", size=.3, color="gray" )+
  theme_minimal()+
  ylab("Alaska North Slope Crude Oil Price for Governors")+
  # geom_text(aes(x=as.numeric(relative_date), y=ans, label=last_name, group=name, color= name), size=11, hjust =-.1, face = "bold", show.legend = F)+
  scale_colour_manual(values=brewer_set)+
  scale_fill_manual(values=brewer_set)+
  coord_flip()+
  # # theme(text=element_text(family="Fira Sans", size=14, face = "bold"))+
  # geom_text(aes(x=-10, y=0, label="$0"), size=13)+
  # geom_text(aes(x=-50, y=50, label="$50"),size=13)+
  # geom_text(aes(x=-50, y=100, label="$100"),size=13)+
  # geom_text(aes(x=-50, y=150, label="$150"),size=13)+
  # geom_dl(aes(x=date, y=ans, color=name, label=name),method=list("smart.grid", cex=1.5, fontface="bold"))+
  theme(plot.margin = unit(c(1,1,1,1), "cm"))+
  theme( text = element_text(size = 20),
         plot.title = element_text(family="IBM Plex Sans Medium",size=40, color="#333333"))+
  xlab(" ")+
  ylab("Average Daily ANS Price Per Barrel")+
  scale_y_continuous(labels=dollar_format())+
  labs(title = "Average Oil Prices for Alaska Governors", subtitle ="Dollars per barrel, not adjusted for inflation or anything")+
  ggsave("plots/bar_price_2020.png", width=15, height=5, dpi=300, units = "in")



price_plot

# 
# 
# production_plot <- ggplot(gov_oil_summary_gov)+
#   geom_col(aes(x=reorder(name, mean_production), y=mean_production,fill=name), show.legend = F)+
#   # geom_col(aes(x=date, y=roll_ans_drop_10, group=name, color=name), show.legend = F)+
#   
#   # geom_hline(yintercept=100,linetype="dotted", size=.3, color="gray" )+
#   # geom_hline(yintercept=50,linetype="dotted", size=.3, color="gray" )+
#   # geom_hline(yintercept=150,linetype="dotted", size=.3, color="gray" )+
#   theme_minimal()+
#   ylab("Alaska North Slope Crude Oil Price")+
#   # geom_text(aes(x=as.numeric(relative_date), y=ans, label=last_name, group=name, color= name), size=11, hjust =-.1, face = "bold", show.legend = F)+
#   scale_colour_manual(values=brewer_set)+
#   scale_fill_manual(values=brewer_set)+
#   coord_flip()+
#   # # theme(text=element_text(family="Fira Sans", size=14, face = "bold"))+
#   # geom_text(aes(x=-10, y=0, label="$0"), size=13)+
#   # geom_text(aes(x=-50, y=50, label="$50"),size=13)+
#   # geom_text(aes(x=-50, y=100, label="$100"),size=13)+
#   # geom_text(aes(x=-50, y=150, label="$150"),size=13)+
#   # geom_dl(aes(x=date, y=ans, color=name, label=name),method=list("smart.grid", cex=1.5, fontface="bold"))+
#   theme(plot.margin = unit(c(.3,1,.3,.3), "cm"))+
#   theme( text = element_text(size = 28),
#          plot.title = element_text(family="IBM Plex Sans Medium",size=52, color="#333333"))+
#   labs(title = "Alaska Crude Oil Price", subtitle ="Dollars per barrel, not adjusted for inflation or anything")+
#   ggsave("plots/bar_production_2020.png", width=15, height=5, dpi=300, units = "in")
# 
# 
# production_plot
# all_anim <- animate (all_animated, nframes = 100, fps = 15, renderer=ffmpeg_renderer(), width=1500, height=750)
# all_anim_gif <- animate (all_animated, nframes = 100, fps = 15, width=1500, height=750)




# ggsave("percent2yearColor1.png", width=10, height=5, dpi=600, units = "in")

# 
# orderName <-  c("1"='William A. Egan 1', '2'='Wally Hickel 1', '3'='Keith Harvey Miller', '4'='William A. Egan 2','5'='Jay Hammond', '6'='Bill Sheffield', '7'='Steve Cowper', '8'='Wally Hickel 2', '9'='Tony Knowles', '10'='Frank Murkowski', '11'='Sarah Palin', '12'='Sean Parnell', '13'='Bill Walker')
# ####faceted
# 
# facet <- ggplot(govOilG, aes(x=month, y=percentChange, group=name, color=name))+
#   geom_line( show.legend = F, size=1.4, linetype="solid")+
#   theme(text=element_text(family="Fira Sans", size=20))+
#   geom_hline(yintercept=0,linetype="dotted", size=.6, color="gray" )+
#   scale_x_discrete(name="month", breaks = c("m001", "m024"),labels =c("Inaguration", "Two Years"))+
#   facet_wrap(. ~ order, labeller = labeller(
#     order = orderName))+
#   # scale_colour_manual(values=custPal)+
#   scale_colour_manual(values=brewRain)+
#   # transition_reveal(month, percentChange) +
#   theme_void()+
#   
#   theme(plot.margin = unit(c(.3,.3,.3,.3), "cm"))+
#   theme(strip.text.x = element_text(size=17, face="bold"))+
#   theme(panel.spacing.y=unit(1, "lines"))+
#   # transition_reveal(id=name, along=mInt)+
#   ggsave("facetPercent4yearCust.png", width=10, height=11, dpi=600, units = "in")
# # animate(facet, 100, 10, width=1000, height=1050)
# # anim_save("facet_bold-4.gif")
# 
# 
