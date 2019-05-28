


################################################################################################
# Slope Fin Transactions
##################################################################################################################


GDP<-filter(nasq_10_nf_tr,na_item=="Gross domestic product at market prices",s_adj=="Seasonally and calendar adjusted data",direct=="Paid",
            unit=="Current prices, million euro")%>%select(time,geo,values)%>%rename("GDP"=values)
Ireland<-filter(nasq_10_f_tr, na_item=="Net acquisition of financial assets/ Net incurrence of liabilities",unit=="Million euro",geo=="Ireland",
                sector%in%c("Other financial intermediaries (except ICPFs), financial auxiliaries, CFIs, and money lenders","Financial corporations","Non-financial corporations","General government","Rest of the world",
                            "Households; non-profit institutions serving households"))%>%spread(finpos,values)%>%mutate(Total=Assets+Liabilities)



DF2<-merge(GDP,Ireland,by=c("geo","time"))%>%group_by(sector)%>%
  mutate(Assets=(Assets)/GDP*100, Liabilities=(Liabilities)/GDP*100,
         Total=(Total)/GDP*100)%>%
  filter(time%in%c(2000,2005.75))


Ireland<-filter(nasa_10_f_tr, na_item=="Net acquisition of financial assets/ Net incurrence of liabilities",unit=="Percentage of gross domestic product (GDP)",geo=="Ireland",co_nco=="Non-consolidated",
                sector%in%c("Other financial intermediaries (except ICPFs), financial auxiliaries, CFIs, and money lenders","Financial corporations","Non-financial corporations","General government","Rest of the world",
                            "Households; non-profit institutions serving households"))%>%spread(finpos,values)%>%mutate(Total=Assets+Liabilities)%>%filter(time%in%c(2002,2005))

DF2<-Ireland[,c(3,6,9)]
DF2<-spread(DF2,time,Total)
DF2$sector<-as.character(DF2$sector)
DF2$sector[DF2$sector=="Other financial intermediaries (except ICPFs), financial auxiliaries, CFIs, and money lenders"]<-"Other financial intermediaries"
DF2$sector[DF2$sector=="Households; non-profit institutions serving households"]<-"Households"

p<-ggplot(DF2) + geom_segment(aes(x=1, xend=2, y=`2002`, yend=`2005`, colour=sector), size=.75, show.legend=F) + 
  geom_vline(xintercept=1, linetype="dashed", size=.2) + 
  geom_vline(xintercept=2, linetype="dashed", size=.2) + 
  labs(x="", y="Percentage of GDP", title="Sum of Net Acquisition of Financial Assets and Net Incurrence of Liabilities (% of GDP)") +
  scale_color_brewer(palette = "Dark2")+ # Axis labels
  xlim(.5, 2.5) + ylim(0,(1.1*(max(DF2$`2002`, DF2$`2005`))))


left_label <- paste(DF2$sector, round(DF2$`2002`),sep=", ")
right_label <- paste(DF2$sector, round(DF2$`2005`),sep=", ")

p<- p+
  geom_text_repel(label=left_label, y=DF2$`2002`, x=rep(1, NROW(DF2)), hjust=1.1, size=4.5)+
  geom_text_repel(label=right_label, y=DF2$`2005`, x=rep(2, NROW(DF2)), hjust=-0.1, size=4.5)+
  geom_text(label="2002", x=1, y=1.1*(max(DF2$`2002`, DF2$`2005`)), hjust=1.2, size=5, fontface="italic")+
  geom_text(label="2005", x=2, y=1.1*(max(DF2$`2002`, DF2$`2005`)), hjust=-0.1, size=5, fontface="italic") +theme_bw()+
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        panel.border = element_blank(),plot.title = element_text(size=12, face="bold",hjust = 0.5),
        plot.margin = unit(c(1,2,1,2), "cm"))  
p