################################################################################
#Financial Balance Sheet Time series
################################################################################



GDP<-filter(nasq_10_nf_tr,na_item=="Gross domestic product at market prices",s_adj=="Seasonally and calendar adjusted data",
            unit=="Current prices, million euro",direct=="Paid",geo=="Ireland")%>%select(time,geo,values)%>%rename("GDP"=values)
QuartBS<-nasq_10_f_bs%>% filter(geo=="Ireland", unit == "Million euro", sector == "Total economy",na_item%in%BSItems)

QuartBS<-merge(QuartBS,GDP, by="time")

QuartBS$values<-((QuartBS$values)/(QuartBS$GDP))*100
QuartBS<-QuartBS%>%group_by(na_item)%>%mutate(values=scale(values))%>%unite(pos_item,na_item,finpos,sep = " - ")
ggplot(QuartBS, aes(time, pos_item)) + geom_tile(aes(fill = values),colour = "white", na.rm = TRUE) +
  theme_bw() + theme_minimal() +scale_y_discrete()+ scale_fill_distiller(palette = "RdYlBu")+
  labs(title = "Financial Balance Sheet for Ireland",
       y = "Financial Item", x = "Year",caption="Source: Eurostat") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "none")




