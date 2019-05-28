
###################################################################
#Figure 1
###################################################################


GDP<-filter(nasq_10_nf_tr,na_item=="Gross domestic product at market prices",s_adj=="Seasonally and calendar adjusted data",
            unit=="Current prices, million euro",direct=="Paid", time<2006.25)%>%select(time,geo,values)%>%rename("GDP"=values)
HH<-filter(nasq_10_f_bs,na_item=="Total financial assets/liabilities",
           sector=="Households; non-profit institutions serving households",
           unit=="Million euro",finpos=="Liabilities", time<2006.25)%>%select(time,geo,values)%>%rename("Liabilities"=values)

DF<-merge(HH, GDP, by=c("time","geo"))
DF<-DF%>%filter(geo%in%c("Austria","Belgium","France","Germany (until 1990 former territory of the FRG)",
                         "Greece","Ireland","Italy","Netherlands","Spain","Sweden","Portugal","United Kingdom"))%>%group_by(geo)%>%arrange(time)%>%
  mutate("Household Liabilities"=(Liabilities-dplyr::lag(Liabilities,4))/GDP*100)%>%filter(time>1999.75)%>%mutate(col=if_else(geo=="Ireland",T,F))
DF$geo<-as.character(DF$geo)
DF$geo[DF$geo=="Germany (until 1990 former territory of the FRG)"]<-"Germany"


colvec <- ifelse(DF$col, "black", "grey70")


A<-DF%>%ggplot(aes(time,geo)) + geom_tile(aes(fill = `Household Liabilities`),colour = "white", na.rm = TRUE) +
  theme_minimal() +scale_x_continuous(breaks = c(2000,2001,2002,2003,2005,2004, 2006))+ 
  scale_fill_distiller(palette = "RdYlBu")+
  labs(title = "Household Liability Growth as % of GDP",x="Year") +
  theme(panel.grid.major = element_blank(), axis.text.y = element_text(size = 12,face="bold", colour = colvec),axis.text.x = element_text(angle=45), axis.title.y = element_blank(),
        panel.grid.minor = element_blank(),legend.position = "bottom",plot.title = element_text(face="bold",hjust = 0.5))

Ireland<-DF%>%filter(geo=="Ireland")
#write_csv(Ireland,path="IrelandHHLiab.csv")
B<-ggplot(Ireland,aes(x=time,y=`Household Liabilities`))+geom_line()+geom_point()+theme_base()+labs(x="Time",title="Irish Household Liability growth as % of GDP")
ggarrange(B,A,nrow=1,ncol=2,common.legend = F)
