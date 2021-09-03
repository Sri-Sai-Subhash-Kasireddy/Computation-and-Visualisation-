library(ggplot2)
library(tidyverse)
library(plotly)
library(treemap)
library(usmap)
library(stringr)

x=read_csv("fm.csv")
regions=read_csv("Regions.csv")
regions$X3=NULL
#Plot 1
pay_all=x[, c(11,24,25,26,27,28)]


pay_all=within(pay_all,{
  Credit[Credit=='Y']=1
  Credit[Credit=='N']=0
  SNAP[SNAP=='Y']=1
  SNAP[SNAP=='N']=0
  WIC[WIC=='Y']=1
  WIC[WIC=='N']=0
  WICcash[WICcash=='Y']=1
  WICcash[WICcash=='N']=0
  SFMNP[SFMNP=='Y']=1
  SFMNP[SFMNP=='N']=0
})

pay_all$Credit=as.numeric(pay_all$Credit)
pay_all$WIC=as.numeric(pay_all$WIC)
pay_all$WICcash=as.numeric(pay_all$WICcash)
pay_all$SNAP=as.numeric(pay_all$SNAP)
pay_all$SFMNP=as.numeric(pay_all$SFMNP)

pay_all=pay_all %>% 
  pivot_longer(cols=2:6, names_to="Payment Method", values_to="Count") %>% 
  group_by(`Payment Method`)  


ggplot(pay_all, aes(x=reorder(`Payment Method`, -Count), y=Count))+geom_bar(stat="identity")+
  xlab("Payment Methods")+ylab("Usage")+ggtitle("Most used Payment Method among all the States")

#Plot 2
pay_indi_states=x[, c(11,24,25,26,27,28)]

pay_indi_states=within(pay_indi_states,{
  Credit[Credit=='Y']=1
  Credit[Credit=='N']=0
  SNAP[SNAP=='Y']=1
  SNAP[SNAP=='N']=0
  WIC[WIC=='Y']=1
  WIC[WIC=='N']=0
  WICcash[WICcash=='Y']=1
  WICcash[WICcash=='N']=0
  SFMNP[SFMNP=='Y']=1
  SFMNP[SFMNP=='N']=0
})

pay_indi_states %>% 
  filter(Credit==1 & SNAP==1 & WIC==1 & WICcash==1 & SFMNP==1) %>% 
  group_by(State) %>%
  summarise(C=n()) %>%
  ggplot(aes(x=reorder(State,-C), y=C, fill=C))+
  geom_bar(stat="identity")+coord_flip()+
  ylab("Users")+xlab("State")+ggtitle("Usage of all the Payment Methods accross different States")+
  scale_colour_brewer()+theme(legend.position = "none")



#Plot 3
pay_indi_states %>% 
  filter(Credit==0 || SNAP==0 || WIC==0 || WICcash==0 || SFMNP==0) %>% 
  group_by(State) %>%
  summarise(C=n()) %>%
  ggplot(aes(x=reorder(State,-C), y=C, fill=C))+
  geom_bar(stat="identity")+coord_flip()+
  ylab("Users")+xlab("State")+ggtitle("State Wise Markets Not Using Any Payment Methods")+
  scale_colour_brewer()+theme(legend.position = "none")
  


#Plot 5
pm_regions=merge(regions, x, by="State")
pm_regions=within(pm_regions,{
  Credit[Credit=='Y']=1
  Credit[Credit=='N']=0
  SNAP[SNAP=='Y']=1
  SNAP[SNAP=='N']=0
  WIC[WIC=='Y']=1
  WIC[WIC=='N']=0
  WICcash[WICcash=='Y']=1
  WICcash[WICcash=='N']=0
  SFMNP[SFMNP=='Y']=1
  SFMNP[SFMNP=='N']=0
})

yo=pm_regions %>% 
  select(Region, Credit, SNAP, WIC, WICcash, SFMNP) %>% 
  pivot_longer(cols=2:6, names_to="Payment Method", values_to="Count") %>% 
  filter(Count==1) %>% 
  group_by(Region, `Payment Method`) %>% 
  summarise(Value=n()) %>% 
  arrange(desc(`Payment Method`))

pay_data=data.frame(group=c(rep("WICcash", 4), rep("WIC", 4), rep("SNAP", 4), rep("SFMNP", 4), rep("Credit", 4)), sub=yo$Region, val=yo$Value)

colors=c("#F39783","#858382", "#C1724A", "#F0AC74")
treemap(pay_data, index=c("group", "sub"),
        vSize="val",
        type="index", title="Payment Methods in Different Regions",
        fontsize.labels = c(15,12),
        fontcolor.labels = c("Black","black"),
        fontface.labels = c(4,1),
        border.col = c("black", "white"),
        border.lwds = c(3,1),
        palette = colors)
 

#Plot 6
products_regions=merge(x, regions, by="State")

products_regions=products_regions[, c(29:60)]


for(i in 1:30)
{
  products_regions[is.na(products_regions[,i]),i]=0
  products_regions[str_which(products_regions[,i],"Y"),i]=1
  products_regions[str_which(products_regions[,i],"N"),i]=0
  products_regions[str_which(products_regions[,i],"-"),i]=0
}

for (i in 1:30)
{
  products_regions[,i]=as.numeric(products_regions[,i])
}
products_regions$updateTime=NULL

products_regions %>% 
  pivot_longer(cols=1:30, names_to="Products", values_to="Quantity") %>% 
  group_by(Region,Products) %>% 
  filter(Quantity==1) %>% 
  summarise(Count=sum(Quantity)) %>% 
  ggplot(aes(x=reorder(Products, Count), y=Count, fill=Region))+geom_bar(stat="identity")+facet_wrap(~Region, ncol = 4)+coord_flip()+
  ylab("Quantity Sold")+xlab("Products")+
  ggtitle("Products Sold in Different Regions")+geom_text(aes(label=Count))+theme(legend.position = "none")

#Plot 7
media=x[,c(3:7,11)]

media %>% 
  pivot_longer(cols=1:5, names_to="Media", values_to="URL") %>% 
  drop_na() %>% 
  group_by(State, Media) %>% 
  summarise(Count=n()) %>% 
  ggplot(aes(x=reorder(State, -Count), y=Count, fill=Media))+geom_bar(stat="identity")+
  coord_flip()+scale_fill_manual(values = c("#C0392B", "#ABEBC6", "#0B5345", "#AAB7B8", "#17202A"))+
  xlab("Users")+ylab("States")+ggtitle("Social Media users across different States")

#Plot 8
media %>% 
  pivot_longer(cols=1:5, names_to="Media", values_to="URL") %>% 
  drop_na() %>% 
  group_by(State, Media) %>% 
  summarise(Count=n()) %>% 
  ggplot(aes(x=State, y=Count, fill=Media))+geom_bar(stat="identity")+
  facet_wrap(~Media, ncol=5)+coord_flip()+
  xlab("States")+ylab("Users")+ggtitle("Social Media users across different Platforms")+
  theme(legend.position = "none")
  
  
#Plot 9
s1_markets=x[,c("State", "Season1Date")]

z=s1_markets%>% 
  group_by(State) %>% 
  drop_na() %>% 
  summarise(Count=n()) 

treemap(dtf=z, index=c("State"), vSize=c("Count"))
  
  
  
