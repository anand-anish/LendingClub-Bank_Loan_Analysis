#clearing working directory
rm(list=ls())

#set working directory
setwd("C:\Users\aa16400\Desktop\My Projects\Lending Club")

#install and import packages
install.packages("lubridate")
install.packages(dplyr)
install.packages(ggplot2)
install.packages(scales)
install.packages(choroplethr)
install.packages(choroplethrMaps)

library(lubridate)
library(dplyr)
library(ggplot2)
library(scales)
library(choroplethr)
library(choroplethrMaps)


#loading the files in environment
loan1<-read.csv("LoanStats3a.csv",sep=",",header=T)
loan2<-read.csv("LoanStats3b.csv",sep=",",header=T)
loan3<-read.csv("LoanStats3c.csv",sep=",",header=T)
loan4<-read.csv("LoanStats3d.csv",sep=",",header=T)

#VISUALISATIONS
# Total Loan issuance by yearly & quarterly and calculate growth rate by quarter on quarter and year on year 

loan_1<-loandata%>%group_by(issue_d,year,Quarter)%>%summarise(Tot_loan_amnt=sum(loan_amnt))

qplot(issue_d,data=loan_1,weight= Tot_loan_amnt,geom="histogram",fill=..count..)+
  theme(axis.title=element_text(face="bold", size="12", color="black"), legend.position="right") +
  theme(text = element_text(size=10))+
  labs(title="TOTAL LOAN ISSUANCE") +labs(x="YEAR",y="TOTAL LOAN ISSUED($)")+
  labs(fill="Amount(in $)")
#guides(fill= guide_legend(title="Amount(in $)"))

ggplot(loan_1, aes(factor(year),Tot_loan_amnt, fill = Quarter)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set1")+
  theme(axis.title=element_text(face="bold", size="12", color="black"), legend.position="bottom") +
  theme(text = element_text(size=10),axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title="QUARTERLY LOAN ISSUANCE") +labs(x="YEAR",y="TOTAL LOAN ISSUED($)")

# Percentage of loans based on reported loan purpose
ggplot(loandata,aes(x= factor(purpose)))+geom_bar(aes(y=(..count..)/sum(..count..)))+
  geom_text(aes(y=((..count..)/sum(..count..)),label=scales::percent((..count..)/sum(..count..))),stat="count",vjust=-0.25)+
  scale_y_continuous(labels = scales::percent)+theme_light()+
  theme(axis.title=element_text(face="bold", size="10", color="black")) +
  theme(text = element_text(size=10),axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title="Percentage of loans based on reported loan purpose",x="LOAN PURPOSE",y="PERCENTAGE OF LOANS")

#The last quarter average interest rates by different term loans and overall in our case the last quarter according to the data is 3rd Quarter of the year 2015

loan_last_quar<-filter(loandata,year=="2015",Quarter=="Q3")
loan_int_term<-loan_last_quar%>%group_by(term)%>%summarise(Av.Int.Rate=mean(int_rate))

# Loan Issuance by state - classify the states based on loan issuance by $50+ MM, $25-50 MM, $10-25 MM and $0-10 MM 
loan_state<-loandata%>%group_by(addr_state)%>%summarise(Tot_loan_amnt=sum(as.numeric(loan_amnt))) #since the sum exceeded integer range,hence we converted it using typecasting to numeric type by using as.numeric
loan_state$amnt_grp<-ifelse(loan_state$Tot_loan_amnt<=10000000,"$0-10MM",ifelse(loan_state$Tot_loan_amnt>10000000 & loan_state$Tot_loan_amnt<=25000000,"$10-25",ifelse(loan_state$Tot_loan_amnt>25000000 & loan_state$Tot_loan_amnt<=50000000,"$25-50MM","$50+MM")))

st.codes<-data.frame(
  state=as.factor(c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA",
                    "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME",
                    "MI", "MN", "MO", "MS",  "MT", "NC", "ND", "NE", "NH", "NJ", "NM",
                    "NV", "NY", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN",
                    "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")),
  full=as.factor(c("alaska","alabama","arkansas","arizona","california","colorado",
                   "connecticut","district of columbia","delaware","florida","georgia",
                   "hawaii","iowa","idaho","illinois","indiana","kansas","kentucky",
                   "louisiana","massachusetts","maryland","maine","michigan","minnesota",
                   "missouri","mississippi","montana","north carolina","north dakota",
                   "nebraska","new hampshire","new jersey","new mexico","nevada",
                   "new york","ohio","oklahoma","oregon","pennsylvania","puerto rico",
                   "rhode island","south carolina","south dakota","tennessee","texas",
                   "utah","virginia","vermont","washington","wisconsin",
                   "west virginia","wyoming"))
)

loan_state<-merge(loan_state,st.codes,by.x="addr_state",by.y="state",all.x=T)
l<-loan_state[c("full","amnt_grp")]
colnames(l)<-c("region","value")
state_choropleth(l)

#create new data frame creating the overall average interest rate(last quarter) and later merge on previous set
net_int<-mean(loan_last_quar$int_rat)
overall_quart_int<-data.frame("All",net_int)
names(overall_quart_int)[names(overall_quart_int)=="X.All."]<-"term"
names(overall_quart_int)[names(overall_quart_int)=="net_int"]<-"Av.Int.Rate"

#merging the dataframes to get the actual table
loan_int_term<-rbind(loan_int_term,overall_quart_int)

qplot(term,Av.Int.Rate,data=loan_int_term,geom = c("point","line"),group=1,col="red")+
  labs(title="Last Quarter Average Interest Rates")


#What is the loan performance details by different loan grades and overall  
test<-loandata[,c(7,9,1)]
View(test)
test_wide<-dcast(test,grade~loan_status,fun.aggregate = sum)
test_wide$`Charged Off`<-test_wide$`Charged Off`+test_wide$`Does not meet the credit policy. Status:Charged Off`
test_wide$Current<-test_wide$Current+test_wide$`Does not meet the credit policy. Status:Current`
test_wide$`Fully Paid`<-test_wide$`Fully Paid`+test_wide$`Does not meet the credit policy. Status:Fully Paid`
test_wide$`Does not meet the credit policy. Status:Fully Paid`=NULL
test_wide$`Does not meet the credit policy. Status:Current`=NULL
test_wide$`Does not meet the credit policy. Status:Charged Off`=NULL


test1<-loandata%>%group_by(grade)%>%summarise(Total_Issued=sum(as.numeric(loan_amnt)),
                                              Principal_Payments_Recieved=sum(as.numeric(total_rec_prncp)),
                                              Interest_Payments_Recieved=sum(as.numeric(total_rec_int)),
                                              Avg_Interest_Rate=mean(int_rate),Adj_Net_Annualised_Ret=mean(NAR))


#required LOAN PERFORMANCE DETAILS TABLE is
table_hist_ret<-cbind(test1[c(1,2)],test_wide[c(5,3,7,8,2)],test1[c(3,4,5,6)])

# Find the historical returns by loan grade(Historical performance by grade for all issued loans) and overall 
a<-melt(table_hist_ret,id.vars="grade",measure.vars = c("Avg_Interest_Rate","Adj_Net_Annualised_Ret"))
View(a)
ggplot(a, aes(grade,value, fill = variable)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set3")+theme_dark()+
  theme(axis.title=element_text(face="bold", size="12", color="black"), legend.position="right") +
  labs(title="HISTORICAL RETURNS BY GRADE",y="(value in %)",fill="type") 

#Find the historical average interest rates by loan terms and loan grades (also for overall)  
#BY LOAN GRADES
data_int_rate<-loandata%>%group_by(issue_d,grade)%>%summarise(Int_Rate=mean(int_rate))
qplot(issue_d,Int_Rate,data=data_int_rate,colour=grade,geom=c("point","line"))+labs(title="Historical Avg. Int Rates by Loan grades")

#BY LOAN TERMS
data_int_rate<-loandata%>%group_by(issue_d,term)%>%summarise(Int_Rate=mean(int_rate))
qplot(issue_d,Int_Rate,data=data_int_rate,colour=term,geom=c("point","line"))+labs(title="Historical Avg. Int Rates by Loan terms")

#OVERALL
data_int_rate<-loandata%>%group_by(issue_d)%>%summarise(Int_Rate=mean(int_rate))
qplot(issue_d,Int_Rate,data=data_int_rate,geom="line")+labs(title="Overall Avg. Int Rates")


#What is percentage of loans by different loan grades by each year and loan term level (also for overall) 
grade_mix<-loandata%>%group_by(year,grade)%>%summarise(amnt=sum(loan_amnt))
ggplot()+geom_bar(data=grade_mix,aes(x=factor(year),y=amnt,fill=factor(grade)),position="fill")

#make 100% stacked histograms
#library(scales)
ggplot(grade_mix,aes(x=year,y=amnt,fill=grade))+geom_bar(position="fill",stat="identity")+
  scale_y_continuous(labels=percent_format()) + labs(x="Year",y="",fill="Grade",title="GRADE MIX OVER TIME")

# Find Net Annualized returns by vintage by different loan grades and different loan terms (also for overall)
#BY LOAN GRADES
data_NAR<-loandata%>%group_by(issue_d,grade)%>%summarise(NAR=mean(NAR))
qplot(issue_d,NAR,data=data_NAR,colour=grade,geom="point")+labs(title="Net Annualised Return By Vintage(by grade)",x="Year",y="NAR(in %)")

data_NAR<-loandata%>%group_by(issue_d,term)%>%summarise(NAR=mean(NAR))
qplot(issue_d,NAR,data=data_NAR,colour=term,geom=c("point","line"))+labs(title="Net Annualised Return By Vintage(by loan terms)",x="Year",y="NAR(in %)")

================================================================================================================================================
  

