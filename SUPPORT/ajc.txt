# Reproduction of a consumer level graphic publication found in the 
# Atlanta Journal Constitution. 
# Steve Pittard wsp@emory.edu

# Tuition Data Set - Approximate values

Wyoming <- c(3242,3342,3442,3542,3642,3752,3880,3950,4100,4404)
Georgia <- c(3411,3630,4000,4200,4500,5000,6000,7600,7700,7823)
USAver  <- c(4962,5100,5800,6000,6200,6800,7300,8000,8200,8596)
NewHamp <- c(8085,8300,9000,9800,10150,10900,12000,13750,14500,14665)

tuition <- data.frame(rbind(Wyoming,Georgia,USAver,NewHamp))
ylims <- c(0,max(tuition[,10]))

plot(1:10,tuition[1,],type="n",lwd=2,col="green",
     ylab="",ylim=ylims,yaxt="n",bty="n",xaxt="n",
     xlab="Year",main="Tuition Growth",sub="Figures adjusted for 2014 dollars")
mcols <- c("#6E8B3D","blue","red","black")
polygon(c(1,1:10,10),c(0,tuition[4,],0),col="#EEE8AA")

points(1:10,tuition[1,],type="l",lwd=3.5,col="#6E8B3D")
points(1:10,tuition[2,],type="l",lwd=3.5,col="blue")
points(1:10,tuition[3,],type="l",lwd=3.5,col="red")
points(1:10,tuition[4,],type="l",lwd=3.5,col="black")

abline(v=1:10,col="white",lwd=1.5)
segments(1,seq(2500,15000,by=2500),10,seq(2500,15000,by=2500),lty=3,col="#6E8B3D")
xlabs <- c(paste(0,1:9,sep=""),"10")
axis(1,at=1:10,labels=xlabs)
ylabs <- formatC(seq(0,15000,2500),format="d",big.mark=",")
ylabs <- paste("$",ylabs,sep="")
axis(2,at=seq(0,15000,2500),ylabs,las=1)
mtext("Avg in-state tuition for 4-year public university",side=3)

legend(04,3000,c("Wyoming (Low)"),fill=c("#6E8B3D"),bty="n",cex=0.75)
legend(04,5700,c("Georgia"),fill=c("blue"),bty="n",cex=0.75)
legend(04,7300,c("US Avg"),fill=c("red"),bty="n",cex=0.75)
legend(04,12500,c("New Hampshire (Hi)"),fill=c("black"),bty="n",cex=0.75)

text(1,c(2500,4000,5500,9000),
     paste("$",formatC(tuition[1:4,1],format="d",big.mark=","),sep=""),pos=4,cex=0.7)

segments(1,c(2500,4000,5500,9000),1.1,c(2500,4000,5500,9000))
segments(1,2500,1,tuition[1,1])
segments(1,tuition[2:4,1],1,c(4000,5500,9000))


text(9,c(3500,7000,9000,13500),
     paste("$",formatC(tuition[1:4,10],format="d",big.mark=","),sep=""),pos=4,cex=0.7)

# This could be cleaned up using some programming approaches

segments(9.8,3500,10,3500)
segments(10,3500,10,tuition[1,10])

segments(9.8,7000,10,7000)
segments(10,7000,10,tuition[2,10])

segments(10,tuition[3,10],10,9000)
segments(10,9000,9.8,9000)

segments(9.9,13500,10,13500)
segments(10,13500,10,tuition[4,10])

