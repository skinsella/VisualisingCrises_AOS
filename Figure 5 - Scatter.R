

###################################################################
#Scatter - Fin Balance
###################################################################

item<-c("Currency and deposits","Loans","Equity", "Investment fund shares/ units","Debt securities")
Fin<-c("Luxembourg","Malta","Cyprus")

DF<- filter(nasa_10_f_bs,unit == "Percentage of gross domestic product (GDP)",
            !(geo%in%Fin),time<2009,
            sector=="Total economy",na_item%in%item,co_nco=="Consolidated")%>%
  unite(Year,"geo","time", sep = " ")%>%select(-co_nco,-unit,-sector)%>%spread(na_item,values)%>%
  gather("na_item","values",-finpos,-Year,-Loans)


Ire<-c("Ireland 2006","Ireland 2007","Ireland 2008")
Ireland<-filter(DF,Year%in%Ire)
DF<-DF%>%filter(!(Year%in%Ire))


ggplot()+geom_point(data = DF,aes(y = Loans, x = values), colour="grey40",alpha=.26)+
  geom_point(data = Ireland,aes(y = Loans, x = values, colour=finpos))+
  geom_text_repel(data = Ireland,aes(y = Loans, x = values,label=Year, colour=finpos))+
  theme_classic()+scale_color_manual(values=c("dodgerblue","red"))+
  theme(plot.title = element_text(hjust = 0.5),
        strip.text.x = element_text(size=8, face="bold"),
        strip.background = element_rect(colour="grey40")) +
  facet_wrap(na_item~.,nrow=2)+
  labs(title= "Loans vs Other Balance Sheet Items",color="Financial Position",
       shape="Financial Position",fill="Financial Position", x="Other Items as % of GDP",
       y="Loans % of GDP",caption="Source: Eurostat")


###################################################################
#Scatter - Fin Transactions
###################################################################

TR<- filter(nasa_10_f_tr,unit == "Percentage of gross domestic product (GDP)",
            !(geo%in%Fin),time<2009,
            sector=="Total economy",na_item%in%item,co_nco=="Consolidated")%>%
  unite(Year,"geo","time", sep = " ")%>%select(-co_nco,-unit,-sector)%>%spread(na_item,values)%>%
  gather("na_item","values",-finpos,-Year,-Loans)#%>%filter(!(Year%in%Ire))

Ire<-c("Ireland 2005","Ireland 2006","Ireland 2004")
Ireland<-filter(TR,Year%in%Ire)
TR<-TR%>%filter(!(Year%in%Ire))



ggplot()+geom_point(data = TR,aes(y = Loans, x = values), colour="grey40",alpha=.26)+
  geom_point(data = Ireland,aes(y = Loans, x = values, colour=finpos))+
  geom_text_repel(data = Ireland,aes(y = Loans, x = values,label=Year, colour=finpos))+
  theme_classic2()+scale_color_manual(values=c("dodgerblue","red"))+
  theme(plot.title = element_text(hjust = 0.5),
        strip.text.x = element_text(size=8, face="bold"),
        strip.background = element_rect(colour="grey40")) +
  facet_wrap(na_item~.,nrow=2)+
  labs(title= "Loans vs Other Financial Transaction Items",color="Financial Position",
       shape="Financial Position",fill="Financial Position", x="Other Items as % of GDP",
       y="Loans % of GDP",caption="Source: Eurostat")