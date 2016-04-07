## Animation 3.

# take df3 from ggnetwork_gganimate script - make svg plots for each time.

head(df3)
df3[df3$time==1,] #e.g. time = 1.

df3$time

df3.sp <- split(df3, df3$time)

range(df3$x)
range(df3$y)


#also in loop as well as saving as svg, would need to set factor levels of group for every df
#otherwise colors will change.

#

#to ensure color consistency
tmp<-rbind(df3.sp[[6]],data.frame(x=NA,y=NA,Group=1:max(df3$Group),na.x=NA,vertex.names=NA,xend=NA,yend=NA,na.y=NA,time=NA))

ggplot(tmp, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(color = "black") +
  geom_nodes(aes(color = factor(Group)), size = 8) + 
  geom_nodetext(aes(label = vertex.names),color="black", fontface = "bold") +
  theme_blank() +
  xlim(0,1)+ylim(0,1) +
  theme(legend.position="none") +
  guides(color=guide_legend(keyheight=0.3,default.unit="inch",override.aes = list(size=6)))






#####   make loop...


#setwd for output
setwd("C:/Users/curley1/Dropbox/Work/R/RMeetup/presentation/gists/gistimgsvg1")
#Output for each frame will be a png with HD size 800x450 :)
#png(file="output_%03d.png", width=800,height=450)



### ggplot version - compiling with ffmpeg.

#Time loop starts
for(i in 1:max(df3$time)){
  
  tmp<-rbind(df3.sp[[i]],data.frame(x=NA,y=NA,Group=1:max(df3$Group),na.x=NA,vertex.names=NA,xend=NA,yend=NA,na.y=NA,time=NA))

  pp <- ggplot(tmp, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_edges(color = "black") +
    geom_nodes(aes(color = factor(Group)), size = 8) + 
    geom_nodetext(aes(label = vertex.names),color="black", fontface = "bold") +
    theme_blank() +
    xlim(0,1)+ylim(0,1) +
    theme(legend.position="none") +
    guides(color=guide_legend(keyheight=0.3,default.unit="inch",override.aes = list(size=6)))
 
   ggsave(pp,filename=paste0("output_",i,".png", sep=""))
  
   
}

dev.off()



####  Horrible get around for ffmpeg.

#Time loop starts
for(i in 1:9){
  
  tmp<-rbind(df3.sp[[i]],data.frame(x=NA,y=NA,Group=1:max(df3$Group),na.x=NA,vertex.names=NA,xend=NA,yend=NA,na.y=NA,time=NA))
  
  pp <- ggplot(tmp, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_edges(color = "black") +
    geom_nodes(aes(color = factor(Group)), size = 8) + 
    geom_nodetext(aes(label = vertex.names),color="black", fontface = "bold") +
    theme_blank() +
    xlim(0,1)+ylim(0,1) +
    theme(legend.position="none") +
    guides(color=guide_legend(keyheight=0.3,default.unit="inch",override.aes = list(size=6)))
  
  ggsave(pp,filename=paste0("output_00",i,".png", sep=""), dpi=300)
  
  
}

dev.off()


for(i in 10:99){
  
  tmp<-rbind(df3.sp[[i]],data.frame(x=NA,y=NA,Group=1:max(df3$Group),na.x=NA,vertex.names=NA,xend=NA,yend=NA,na.y=NA,time=NA))
  
  pp <- ggplot(tmp, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_edges(color = "black") +
    geom_nodes(aes(color = factor(Group)), size = 8) + 
    geom_nodetext(aes(label = vertex.names),color="black", fontface = "bold") +
    theme_blank() +
    xlim(0,1)+ylim(0,1) +
    theme(legend.position="none") +
    guides(color=guide_legend(keyheight=0.3,default.unit="inch",override.aes = list(size=6)))
  
  ggsave(pp,filename=paste0("output_0",i,".png", sep=""), dpi=300)
  
  
}

dev.off()


for(i in 100:max(df3$time)){
  
  tmp<-rbind(df3.sp[[i]],data.frame(x=NA,y=NA,Group=1:max(df3$Group),na.x=NA,vertex.names=NA,xend=NA,yend=NA,na.y=NA,time=NA))
  
  pp <- ggplot(tmp, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_edges(color = "black") +
    geom_nodes(aes(color = factor(Group)), size = 8) + 
    geom_nodetext(aes(label = vertex.names),color="black", fontface = "bold") +
    theme_blank() +
    xlim(0,1)+ylim(0,1) +
    theme(legend.position="none") +
    guides(color=guide_legend(keyheight=0.3,default.unit="inch",override.aes = list(size=6)))
  
  ggsave(pp,filename=paste0("output_",i,".png", sep=""), dpi=300)
  
  
}

dev.off()
