
###################################################################
#Geofacet
###################################################################


Net<-filter(nasa_10_nf_tr,unit=="Current prices, million euro", direct=="Received", na_item%in%c("Net lending (+) /net borrowing (-)","Gross domestic product at market prices"),sector=="Households; non-profit institutions serving households")%>%
  spread(na_item,values)%>%select(-unit,-sector,-direct)
GDP<-filter(nasa_10_nf_tr,unit=="Current prices, million euro", direct=="Received", na_item=="Gross domestic product at market prices",sector=="Total economy")%>%
  spread(na_item,values)%>%select(-unit,-sector,-direct)

Net<-merge(Net,GDP, by=c("geo","time"))
Net$NetGDP<-(Net$`Net lending (+) /net borrowing (-)`/Net$`Gross domestic product at market prices`)*100
Net<-Net%>% mutate(col=if_else(geo=="Ireland","Y","N"))

Net$time<-as.numeric(Net$time)
Net<-filter(Net,time>1995)

Net$geo<-as.character(Net$geo)
Net$geo[Net$geo=="Germany (until 1990 former territory of the FRG)"]<-"Germany"


Net%>%filter(geo!="Romania")%>%ggplot()+ geom_hline(yintercept=0, linetype="dashed", color = "black", size=.5)+geom_line(aes(time, NetGDP, color=col, size=col))+geom_point(aes(time, NetGDP, color=col, size=col))+
  facet_geo(~geo, grid="eu_grid1")+
  scale_color_manual(values = c("grey70","dodgerblue"))+scale_size_manual(values=c(.75,1))+scale_x_continuous(breaks = seq(1995,2018,10))+
  theme_classic()+theme(panel.spacing = unit(1, "lines"),plot.title = element_text(hjust = 0.5, family="serif", size=14,face="bold"),strip.text.x = element_text(face = "bold",family="serif", colour = "black"),
                        axis.title.y = element_text(family="serif"),axis.title.x = element_blank(),legend.position = "none", axis.text.x = element_text(face="bold",size=9))+
  labs(y = "% of GDP",title="European Household Net Saving (+)/Borrowing(-) as % of GDP")


