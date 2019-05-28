
###################################################################
#Unit Labour Costs
###################################################################


Lab<-read_csv(file="Data/UnitLaborCosts.csv")

Lab$TIME<-as.numeric(Lab$TIME)
#"NOR","JPN",
Lab<-Lab%>%filter(FREQUENCY=="A",SUBJECT=="EMP",MEASURE=="IDX2010", 
                  !(LOCATION%in%c("NOR","JPN","TUR","CHE","POL","BGR","ROU","RUS","LTU","LVA","HUN","EST","ISL","ISR","MEX","NZL","ZAF","PER")),
                  TIME>1995)
Lab$LOCATION<-countrycode(Lab$LOCATION,"iso3c","country.name")
Lab<-Lab[!is.na(Lab$LOCATION), ]
Lab$LOCATION<-as.character(Lab$LOCATION)
Lab<-Lab%>%mutate(col=if_else(LOCATION=="Ireland",T,F))


colvec <- ifelse(Lab$col, "black", "grey70")

Lab%>%#group_by(LOCATION)%>%mutate(Value=scale(Value))%>%
  ggplot(aes(x=TIME,y=LOCATION)) + geom_tile(aes(fill = Value ),colour = "white", na.rm = TRUE) +
  theme_minimal() +scale_x_continuous()+ 
  scale_fill_distiller(palette = "RdYlBu")+
  labs(title = "Unit Labour Costs By Persons Employed (2010=100)") +
  theme(panel.grid.major = element_blank(), axis.text.y = element_text(size = 12, colour = "black"),axis.text.x = element_text(angle=45), axis.title.y = element_blank(),axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),legend.position = "bottom",plot.title = element_text(face="bold",hjust = 0.5))


