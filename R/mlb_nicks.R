#MLB Nicknames
#install.packages(c("CSS","XML","RCurl","ggplot2","ggthemes"))
library(CSS)
library(XML)
library(RCurl)
library(ggplot2)
library(ggthemes)

url<- "http://www.baseball-reference.com/friv/baseball-player-nicknames.shtml"
doc<-htmlParse(url)

names <- cssApply(doc, ".float_left", cssCharacter)
names <-names[4:length(names)]

#I looked at the table to see that Frank "Noodles" Zupo was the last major league player listed
last_majorleague<-which(str_detect(tolower(names), "zupo"))

names.majors<-names[1:(last_majorleague+1)]
names.majors.nicks<-names.majors[1:length(names.majors) %%2==0]
split.nicks.majors<- read.csv(textConnection(names.majors.nicks), head=FALSE)
nicks.majors<-with(split.nicks.majors, c(as.character(V1),as.character(V2),as.character(V3)))
nicks.majors<-nicks.majors[!(nicks.majors=="")]
nicks.majors.table<-data.frame(table(tolower(nicks.majors)))
nicks.majors.table.ten<-nicks.majors.table[nicks.majors.table$Freq>=10,]
nicks.majors.table.fifteen<-nicks.majors.table[nicks.majors.table$Freq>=15,]


ggplot(nicks.majors.table.fifteen, aes(x=reorder(Var1, Freq), weight=Freq, fill="a"))+
  geom_bar()+theme_few()+
  scale_x_discrete(name="Nickname")+
  theme(axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.title.x=element_text(size=16), axis.title.y=element_text(size=16))+
    scale_y_continuous(name=paste("Frequency among",length(names.majors.nicks),
                       "MLB players with established nicknames"),expand=c(0,10),
                       breaks=c(0,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150))+
                       scale_fill_solarized(accent="green", guide=FALSE)+coord_flip()+
  geom_text(aes(label=Var1, y=Freq), hjust=1.1, color="white", size=6, face="bold")+
  ggtitle("Most popular nicknames in Major League Baseball History\n(at least 15 players with each nickname)\nsource: baseball-reference.com")



