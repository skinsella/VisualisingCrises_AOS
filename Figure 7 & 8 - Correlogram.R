


##################################################################
#Correlogram
###################################################################

Period1<- filter(nasq_10_nf_tr, geo=="Ireland", unit == "Current prices, million euro",na_item%in%NFTRitems,
                 s_adj=="Unadjusted data (i.e. neither seasonally adjusted nor calendar adjusted data)",
                 sector == "Total economy", time%in%c(1999.00, 1999.25, 1999.50, 1999.75, 2000.00, 2000.25, 2000.50,
                                                      2000.75, 2001.00, 2001.25, 2001.50, 2001.75, 2002.00, 2002.25,
                                                      2002.50, 2002.75,2003.00, 2003.25, 2003.50, 2003.75, 2004.00, 
                                                      2004.25, 2004.50, 2004.75, 2005.00, 2005.25, 2005.50, 2005.75, 2006.00, 
                                                      2006.25,2006.50, 2006.75, 2007.00))%>%
  spread(direct,values)


#Period1<-Period1[!(Period1$Received==Period1$Paid),]

Period1$T<-Period1$Received+Period1$Paid
PeriodPre<-Period1%>%select(-unit,-geo,-sector,-s_adj,-Received,-Paid)%>%spread(na_item,T)%>%column_to_rownames("time")

PeriodPre <- PeriodPre[,colSums(is.na(PeriodPre))<nrow(PeriodPre)]
#print(d3heatmap(t(cor(na.omit(PeriodPre))),scale="none", Rowv = F, Colv = F,yaxis_font_size = "7px", key = T, main="title"))

corrplot(t(cor(na.omit(PeriodPre))), method="color", col=rev(diverge_hcl(100,c=110,l=c(40,100), power=1)),  
         type="upper", order="alphabet", tl.cex=.55,title="1999-2007", mar=c(0,0,2,0),
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=75,number.cex = .55, 
         sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE)



Period2<- filter(nasq_10_nf_tr, geo=="Ireland", unit == "Current prices, million euro",na_item%in%NFTRitems,
                 s_adj=="Unadjusted data (i.e. neither seasonally adjusted nor calendar adjusted data)", 
                 sector == "Total economy", time%in%c(2007.25,2007.5,2007.75,2008.00,2008.25, 2008.50, 2008.75, 2009.00,2009.25, 2009.50, 2009.75,
                                                      2010.00, 2010.25, 2010.50, 2010.75, 2011.00, 2011.25, 2011.50,2011.75,
                                                      2012.00,2012.25, 2012.50, 2012.75, 2013.00,2013.25, 2013.50, 2013.75,2014.00, 
                                                      2014.25, 2014.50,2014.75, 2015.00,2015.25,2015.5,2015.75,2016,2016.25,2016.5,2016.75,2017,2017.25,2017.5,2017.75))%>%
  spread(direct,values)
#Period2<-Period2[!(Period2$Received==Period2$Paid),]
Period2$T<-Period2$Received+Period2$Paid
PeriodPost<-Period2%>%select(-unit,-geo,-sector,-s_adj,-Received,-Paid)%>%spread(na_item,T)%>%column_to_rownames("time")
PeriodPost <- PeriodPost[,colSums(is.na(PeriodPost))<nrow(PeriodPost)]


corrplot(t(cor(na.omit(PeriodPost))), method="color", col=rev(diverge_hcl(100,c=110,l=c(40,100), power=1)),  
         type="upper", order="alphabet", tl.cex=.55,title="2008-2018", mar=c(0,0,2,0),
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=75,number.cex = .55, 
         sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=F)


