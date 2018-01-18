#Figure with all data

#Yield Per Recruit Graphs
library(ggplot2)


#create YPR plot
plot1<-(ggplot(data=yield_all,aes(x=mu,y=mean))+
          theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
          # geom_text(aes(10, 1, label="A"), size=10,width=0.5)+
          geom_ribbon(data=subset(yield_all,yield_all$ll=="14"),
                      color=20,fill=20,  #Dark Blue
                      aes(ymin=LCL,ymax=UCL),na.rm=FALSE,width=0.5)+
          #geom_point(data=subset(yield_all,yield_all$ll=="14"),
          #           size=3,color=1)+
          geom_ribbon(data=subset(yield_all,yield_all$ll=="16"),
                      color=5,fill=5,alpha=.4,  #light blue
                      aes(ymin=LCL,ymax=UCL),na.rm=FALSE,width=0.5)+
          #geom_point(data=subset(yield_all,yield_all$ll=="16"),
          #           size=3,color=2)+
          geom_ribbon(data=subset(yield_all,yield_all$ll=="18"),
                      color=10,fill=10,alpha=.4,  #pink
                      aes(ymin=LCL,ymax=UCL),na.rm=FALSE,width=0.5)+
          #geom_point(data=subset(yield_all,yield_all$ll=="18"),
          #           size=3,color=3)+
          ylab("Yield (kg) per 100 recruits\n")+
          xlab("\nExploitation")+
          #scale_y_continuous(breaks=c(-2.0,-1.0,0,1.0))+
          #scale_x_continuous(breaks=c(25,50,75,100))+
          coord_cartesian(ylim=c(0,80))+
          #ylim(0,45)+
          theme(axis.text.x=element_text(size=20),
                axis.text.y=element_text(size=20),
                axis.title.x=element_text(size=20),
                axis.title.y=element_text(size=20,angle=90),
                panel.border = element_blank(),
                axis.line = element_line(colour = "black"),
                #axis.ticks.x = element_blank(),
                #axis.text.x = element_blank()
                legend.position="none"
                )
)
plot1



#create Number Harvested plot
plot2<-(ggplot(data=Nharv_all,aes(x=mu,y=mean))+
          theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
          # geom_text(aes(10, 1, label="A"), size=10,width=0.5)+
          geom_ribbon(data=subset(Nharv_all,Nharv_all$ll=="14"),
                      color=20,fill=20,  #Dark Blue
                      aes(ymin=LCL,ymax=UCL),na.rm=FALSE,width=0.5)+
          #geom_point(data=subset(Nharv_all,Nharv_all$ll=="14"),
          #           size=3,color=1)+
          geom_ribbon(data=subset(Nharv_all,Nharv_all$ll=="16"),
                      color=5,fill=5,alpha=.4,  #light blue
                      aes(ymin=LCL,ymax=UCL),na.rm=FALSE,width=0.5)+
          #geom_point(data=subset(Nharv_all,Nharv_all$ll=="16"),
          #           size=3,color=2)+
          geom_ribbon(data=subset(Nharv_all,Nharv_all$ll=="18"),
                      color=10,fill=10,alpha=.4,  #pink
                      aes(ymin=LCL,ymax=UCL),na.rm=FALSE,width=0.5)+
          #geom_point(data=subset(Nharv_all,Nharv_all$ll=="18"),
          #           size=3,color=3)+
          ylab("Number harvested\n")+
          xlab("\nExploitation")+
          #scale_y_continuous(breaks=c(-2.0,-1.0,0,1.0))+
          #scale_x_continuous(breaks=c(25,50,75,100))+
          coord_cartesian(ylim=c(0,100))+
          #ylim(0,80)+
          theme(axis.text.x=element_text(size=20),
                axis.text.y=element_text(size=20),
                axis.title.x=element_text(size=20),
                axis.title.y=element_text(size=20,angle=90),
                panel.border = element_blank(),
                axis.line = element_line(colour = "black"),
                #axis.ticks.x = element_blank(),
                #axis.text.x = element_blank(),
                legend.position="none"
                )
)
plot2

#create weight Harvested plot
plot3<-(ggplot(data=wt_all,aes(x=mu,y=mean))+
          theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
          # geom_text(aes(10, 1, label="A"), size=10,width=0.5)+
          geom_ribbon(data=subset(wt_all,wt_all$ll=="14"),
                      color=20,fill=20,  #Dark Blue
                      aes(ymin=LCL,ymax=UCL),na.rm=FALSE,width=0.5)+
          #geom_point(data=subset(wt_all,wt_all$ll=="14"),
          #           size=3,color=1)+
          geom_ribbon(data=subset(wt_all,wt_all$ll=="16"),
                      color=5,fill=5,alpha=.4,  #light blue
                      aes(ymin=LCL,ymax=UCL),na.rm=FALSE,width=0.5)+
          #geom_point(data=subset(wt_all,wt_all$ll=="16"),
          #           size=3,color=2)+
          geom_ribbon(data=subset(wt_all,wt_all$ll=="18"),
                      color=10,fill=10,alpha=.4,  #pink
                      aes(ymin=LCL,ymax=UCL),na.rm=FALSE,width=0.5)+
          #geom_point(data=subset(wt_all,wt_all$ll=="18"),
          #           size=3,color=3)+
          ylab("Average weight (g) harvested\n")+
          xlab("\nExploitation")+
          #scale_y_continuous(breaks=c(-2.0,-1.0,0,1.0))+
          #scale_x_continuous(breaks=c(500,1000,1500,2000,2500))+
          coord_cartesian(ylim=c(0,3500))+
          #ylim(0,2500)+
          theme(axis.text.x=element_text(size=20),
                axis.text.y=element_text(size=20),
                axis.title.x=element_text(size=20),
                axis.title.y=element_text(size=20,angle=90),
                panel.border = element_blank(),
                axis.line = element_line(colour = "black"),
                #axis.ticks.x = element_blank(),
                #axis.text.x = element_blank(),
                legend.position="none")
)
plot3


#create average length Harvested plot
plot4<-(ggplot(data=avgl_all,aes(x=mu,y=mean))+
          theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
          # geom_text(aes(10, 1, label="A"), size=10,width=0.5)+
          geom_ribbon(data=subset(avgl_all,avgl_all$ll=="14"),
                      color=20,fill=20,  #Dark Blue
                      aes(ymin=LCL,ymax=UCL),na.rm=FALSE,width=0.5)+
          #geom_point(data=subset(avgl_all,avgl_all$ll=="14"),
          #           size=3,color=1)+
          geom_ribbon(data=subset(avgl_all,avgl_all$ll=="16"),
                      color=5,fill=5,alpha=.4,  #light blue
                      aes(ymin=LCL,ymax=UCL),na.rm=FALSE,width=0.5)+
          #geom_point(data=subset(avgl_all,avgl_all$ll=="16"),
          #           size=3,color=2)+
          geom_ribbon(data=subset(avgl_all,avgl_all$ll=="18"),
                      color=10,fill=10,alpha=.4,  #pink
                      aes(ymin=LCL,ymax=UCL),na.rm=FALSE,width=0.5)+
          #geom_point(data=subset(avgl_all,avgl_all$ll=="18"),
          #           size=3,color=3)+
          ylab("Average length (mm) harvested\n")+
          xlab("\nExploitation")+
          #scale_y_continuous(breaks=c(200,400,600))+
          #scale_x_continuous(breaks=c(25,50,75,100))+
          coord_cartesian(ylim=c(250,650))+
          #ylim(300,600)+
          theme(axis.text.x=element_text(size=20),
                axis.text.y=element_text(size=20),
                axis.title.x=element_text(size=20),
                axis.title.y=element_text(size=20,angle=90),
                panel.border = element_blank(),
                axis.line = element_line(colour = "black"),
                #axis.ticks.x = element_blank(),
                #axis.text.x = element_blank(),
                legend.position="none")
)
plot4

#create distribution of M  plot
plot5<-(ggplot(data=M_all,aes(x=mu,y=mean))+
          theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
          # geom_text(aes(10, 1, label="A"), size=10,width=0.5)+
         geom_ribbon(data=subset(M_all,M_all$ll=="14"),
                     color=1,fill=20,  #Dark Blue
                      aes(ymin=LCL,ymax=UCL),na.rm=FALSE,width=0.5)+
          #geom_point(data=subset(M_all,M_all$ll=="14"),
          #           size=3,color=1)+
          geom_ribbon(data=subset(M_all,M_all$ll=="16"),
                      color=2,fill=5,alpha=.4,  #light blue
                      aes(ymin=LCL,ymax=UCL),na.rm=FALSE,width=0.5)+
          #geom_point(data=subset(M_all,M_all$ll=="16"),
          #           size=3,color=2)+
          geom_ribbon(data=subset(M_all,M_all$ll=="18"),
                      color=3,fill=10,alpha=.4,  #pink
                      aes(ymin=LCL,ymax=UCL),na.rm=FALSE,width=0.5)+
          #geom_point(data=subset(M_all,M_all$ll=="18"),
          #           size=3,color=3)+
          ylab("Instantatneous natural mortality (M)\n")+
          xlab("\nExploitation")+
          #scale_y_continuous(breaks=c(-2.0,-1.0,0,1.0))+
          #scale_x_continuous(breaks=c(25,50,75,100))+
          #coord_cartesian(ylim=c(-2,1.5))+
          #ylim(0,8)+
          theme(axis.text.x=element_text(size=20),
                axis.text.y=element_text(size=20),
                axis.title.x=element_text(size=20),
                axis.title.y=element_text(size=20,angle=90),
                panel.border = element_blank(),
                axis.line = element_line(colour = "black"),
                #axis.ticks.x = element_blank(),
                #axis.text.x = element_blank(),
                legend.position="none")
)
plot5

#create distribution of NDie  plot
plot6<-(ggplot(data=Ndie_all,aes(x=mu,y=mean))+
          theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
          # geom_text(aes(10, 1, label="A"), size=10,width=0.5)+
          geom_ribbon(data=subset(Ndie_all,Ndie_all$ll=="14"),
                      color=1,fill=20,  #Dark Blue
                      aes(ymin=LCL,ymax=UCL),na.rm=FALSE,width=0.5)+
          geom_ribbon(data=subset(Ndie_all,Ndie_all$ll=="16"),
                      color=2,fill=5,alpha=.4,  #light blue
                      aes(ymin=LCL,ymax=UCL),na.rm=FALSE,width=0.5)+
          geom_ribbon(data=subset(Ndie_all,Ndie_all$ll=="18"),
                      color=3,fill=10,alpha=.4,  #pink
                      aes(ymin=LCL,ymax=UCL),na.rm=FALSE,width=0.5)+
          ylab("Number that die\n")+
          xlab("\nExploitation")+
          theme(axis.text.x=element_text(size=20),
                axis.text.y=element_text(size=20),
                axis.title.x=element_text(size=20),
                axis.title.y=element_text(size=20,angle=90),
                panel.border = element_blank(),
                axis.line = element_line(colour = "black"),
                legend.position="none")
)
plot6

#create distribution of NDie  plot
plot7<-(ggplot(data=cm_all,aes(x=mu,y=mean))+
          theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
          # geom_text(aes(10, 1, label="A"), size=10,width=0.5)+
          geom_ribbon(data=subset(cm_all,cm_all$ll=="14"),
                      color=1,fill=20,  #Dark Blue
                      aes(ymin=LCL,ymax=UCL),na.rm=FALSE,width=0.5)+
          geom_ribbon(data=subset(cm_all,cm_all$ll=="16"),
                      color=2,fill=5,alpha=.4,  #light blue
                      aes(ymin=LCL,ymax=UCL),na.rm=FALSE,width=0.5)+
          geom_ribbon(data=subset(cm_all,cm_all$ll=="18"),
                      color=3,fill=10,alpha=.4,  #pink
                      aes(ymin=LCL,ymax=UCL),na.rm=FALSE,width=0.5)+
          ylab("Conditional natural mortality (cm)\n")+
          xlab("\nExploitation")+
          theme(axis.text.x=element_text(size=20),
                axis.text.y=element_text(size=20),
                axis.title.x=element_text(size=20),
                axis.title.y=element_text(size=20,angle=90),
                panel.border = element_blank(),
                axis.line = element_line(colour = "black"),
                legend.position="none")
)
plot7
