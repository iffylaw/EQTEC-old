nameofplot<-c(plot1,plot2,,plot3,…………………………………….plot130)
for(i in 1:130)
{
  mypath<-file.path("images/All_sites",paste(i,".png",sep=""))
  png(file=mypath,width=9,height=7,unit="in",res=300)
  plot(x[i],y[i])
  dev.off()
}