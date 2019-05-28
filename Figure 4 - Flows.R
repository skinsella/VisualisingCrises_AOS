
###################################################################
#Transaction FLows
###################################################################


#Read Quarters and transform to long
Q1<- read.csv("Data//table_nf_tr_2003Q1.csv")
Q1<-Q1%>%dplyr::rename(Item="X")%>%gather("Sector","Value",-Item)%>%mutate(Time=2003)
Q2<- read.csv("Data//table_nf_tr_2003Q2.csv")
Q2<-Q2%>%dplyr::rename(Item="X")%>%gather("Sector","Value",-Item)%>%mutate(Time=2003.25)
Q3<- read.csv("Data//table_nf_tr_2003Q3.csv")
Q3<-Q3%>%dplyr::rename(Item="X")%>%gather("Sector","Value",-Item)%>%mutate(Time=2003.5)
Q4<- read.csv("Data//table_nf_tr_2003Q4.csv")
Q4<-Q4%>%dplyr::rename(Item="X")%>%gather("Sector","Value",-Item)%>%mutate(Time=2003.75)


#Bind together
Year03<-rbind(Q1,Q2,Q3,Q4)
Year03$Value[Year03$Value==0]<-NA
Year03$Value[Year03$Value<0]<-NA

Year03<-Year03%>%filter(!(Item%in%c("GDP","Net Lending/Borrowing in non-financial account", "net social contributions","Cash")))%>%
  group_by(Sector, Item)%>%
  summarise_at(vars(Value), funs(sum))%>%
  spread(Sector,Value)
colnames(Year03)<-c("Item","Expenditures FC","Expenditures GG","Expenditures HH","Expenditures NFC","Expenditures ROW","Income FC","Income GG","Income HH","Income NFC","Income ROW")
Year03<-Year03%>%gather(Sector,Value,-Item)%>%separate(Sector,into=c("Direct","Sector"),sep=" ")%>%na.omit(Year03)



Year03$Order<-reorder(Year03$Item,Year03$Item)
ggplot(Year03,
       aes(y = abs(Value), axis1 = Sector,axis2=Order)) +
  geom_alluvium(aes(fill = Sector), width = 1/12) +
  geom_stratum(fill="white",width = 1/7, colour="black") +
  geom_text(angle=0,stat = "stratum", label.strata = TRUE, size = 3, colour="darkred",fontface="bold") +
  scale_x_discrete(limits = c("Sector","Item"), expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Set1", labels=c("Financial Corporations", "General Government","Households", "Non-Financial Corporations","Rest Of World")) +theme_minimal()+
  ggtitle("Transaction Flow Matrix for 2003")+
  theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(),axis.text.x = element_text(size=12,face="bold"),
        legend.position = "bottom", strip.text.x =element_text(size=12, face="bold"),plot.title = element_text(face="bold",hjust = .5))+
  facet_grid(.~Direct)



Q1<- read.csv("/Data//table_nf_tr_2009Q1.csv")
Q1<-Q1%>%dplyr::rename(Item="X")%>%gather("Sector","Value",-Item)%>%mutate(Time=2009)
Q2<- read.csv("Data//table_nf_tr_2009Q2.csv")
Q2<-Q2%>%dplyr::rename(Item="X")%>%gather("Sector","Value",-Item)%>%mutate(Time=2009.25)
Q3<- read.csv("Data//table_nf_tr_2009Q3.csv")
Q3<-Q3%>%dplyr::rename(Item="X")%>%gather("Sector","Value",-Item)%>%mutate(Time=2009.5)
Q4<- read.csv("Data//table_nf_tr_2009Q4.csv")
Q4<-Q4%>%dplyr::rename(Item="X")%>%gather("Sector","Value",-Item)%>%mutate(Time=2009.75)
Year09<-rbind(Q1,Q2,Q3,Q4)



Year09<-rbind(Q1,Q2,Q3,Q4)
Year09$Value[Year09$Value==0]<-NA
Year09$Value[Year09$Value<0]<-NA

Year09<-Year09%>%filter(!(Item%in%c("GDP","Net Lending/Borrowing in non-financial account", "net social contributions","Cash")))%>%
  group_by(Sector, Item)%>%
  summarise_at(vars(Value), funs(sum))%>%spread(Sector,Value)

colnames(Year09)<-c("Item","Expenditures FC","Expenditures GG","Expenditures HH","Expenditures NFC","Expenditures ROW","Income FC","Income GG","Income HH","Income NFC","Income ROW")
Year09<-Year09%>%gather(Sector,Value,-Item)%>%separate(Sector,into=c("Direct","Sector"),sep=" ")%>%na.omit(Year03)
Year09$Order<-reorder(Year09$Item,Year09$Item)




ggplot(Year09,
       aes(y = Value, axis1 = Sector,axis2=Order)) +
  geom_alluvium(aes(fill = Sector), width = 1/12) +
  geom_stratum(fill="white",width = 1/7, colour="black") +
  geom_text(angle=0,stat = "stratum", label.strata = TRUE, size = 3, colour="darkred",fontface="bold") +
  scale_x_discrete(limits = c("Sector","Item"), expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Set1", labels=c("Financial Corporations", "General Government","Households", "Non-Financial Corporations","Rest Of World")) +theme_minimal()+
  ggtitle("Transaction Flow Matrix for 2009")+
  theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(),axis.text.x = element_text(size=12,face="bold"),
        legend.position = "bottom", strip.text.x =element_text(size=12, face="bold"),plot.title = element_text(face="bold",hjust = .5))+
  facet_grid(.~Direct)
