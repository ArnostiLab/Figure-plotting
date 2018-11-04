require(ggplot2)

maxes <- read.csv('FlaRatesMaxes_shelf1234.csv',row.names=1,colClasses=c("character","numeric","numeric",rep("factor",6)))
tenmax <- read.csv('FlaRatesMaxes_10d_shelf1234.csv',row.names=1,colClasses=c("character","numeric","numeric",rep("factor",6)))
factors <- read.csv('FlaRatesWithFactors_shelf1234.csv',row.names=1)

fla <- read.csv("FlaRatesAdjusted_shelf1234.csv", row.names=1)
fla$timepoint <- factor(sub(pattern="stn([0-9]+)-d([0-9])-bulk-([a-z]+)-t([0-9])",replacement="t\\4",row.names(fla)))
fla$substrate <- factor(sub(pattern="stn([0-9]+)-d([0-9])-bulk-([a-z]+)-t([0-9])",replacement="\\3",row.names(fla)))
fla$site <- factor(sub(pattern="stn([0-9]+)-d([0-9])-bulk-([a-z]+)-t([0-9])",replacement="stn\\1.d\\2",row.names(fla)))
fla$depthid <- factor(sub(pattern="stn([0-9]+)-d([0-9])-bulk-([a-z]+)-t([0-9])",replacement="d\\2",row.names(fla)))
fla$stn <- factor(sub(pattern="stn([0-9]+)-d([0-9])-bulk-([a-z]+)-t([0-9])",replacement="stn\\1",row.names(fla)))
fla48 = fla[fla$timepoint=="t1",]

substrateColors <- c("#FFFFFF","#01FFFF","#017F01","#FFFE41","#0000FE","#D50002")
#adjust factor labels
maxes$stn = paste('stn',maxes$stn,sep="")
maxes$depthid <- factor(maxes$depthid,levels=c("d1","d2"),labels=c("surface","bottom"))

tenmax$stn = paste('stn',tenmax$stn,sep="")
tenmax$depthid <- factor(tenmax$depthid,levels=c("d1","d2"),labels=c("surface","bottom"))

factors$stn = paste('stn',factors$stn,sep="")
factors$depthid <- factor(factors$depthid,levels=c("d1","d2"),labels=c("surface","bottom"))

fla48$depthid <- factor(fla48$depthid,levels=c("d1","d2"),labels=c("surface","bottom"))

png('../figures/FLA_barplot.png',width=1000,height=600)
a <- ggplot(maxes,aes(x=substrate,y=mean.kcrate.nM.hr)) + geom_bar(aes(fill=substrate),color="gray30",position="dodge",stat="identity") + facet_grid(depthid~stn) + geom_errorbar(aes(ymin=mean.kcrate.nM.hr-sd.kcrate.nM.hr,ymax=mean.kcrate.nM.hr+sd.kcrate.nM.hr),color="grey30",width=0.5)
barplot <- a + theme_bw() + coord_cartesian(ylim=c(0,30)) + theme(strip.text=element_text(size=16),axis.text.x = element_text(angle = 70, hjust = 1,size=16),axis.text.y=element_text(size=16),axis.title.y=element_text(size=20,vjust=2),axis.title.x=element_text(size=20),legend.position="none",strip.background=element_blank(),strip.text.x=element_text(face="bold"),strip.text.y=element_text(face="bold")) + scale_fill_manual(values=substrateColors) + labs(y='hydrolysis rate (nM/hr)')
print(barplot)
dev.off()

png('../figures/FLA_stackbarplot_48h.png',width=600,height=800)
a <- ggplot(fla48,aes(x=stn,y=mean.kcrate.nM.hr)) + geom_bar(aes(fill=substrate),color="gray30",stat="identity") + facet_grid(depthid~.) 
barplot <- a + theme_bw() + coord_cartesian(ylim=c(0,45)) + theme(strip.text=element_text(size=16),axis.text.x = element_text(angle = 70, hjust = 1,size=16),axis.text.y=element_text(size=16),axis.title.y=element_text(size=20,vjust=2),axis.title.x=element_text(size=20),legend.position="none",strip.background=element_blank(),strip.text.x=element_text(face="bold"),strip.text.y=element_text(face="bold")) + scale_color_manual(values=c("black","#01FFFF","#017F01","#FFFE41","#0000FE","#D50002")) + scale_fill_manual(values=c("black","#01FFFF","#017F01","#FFFE41","#0000FE","#D50002")) + labs(y='hydrolysis rate (nM/hr)')
print(barplot)
dev.off()

png('../figures/FLA_barplot_48h.png',width=1000,height=600)
a <- ggplot(fla48,aes(x=substrate,y=mean.kcrate.nM.hr)) + geom_bar(aes(fill=substrate),color="gray30",position="dodge",stat="identity") + facet_grid(depthid~stn) + geom_errorbar(aes(ymin=mean.kcrate.nM.hr-sd.kcrate.nM.hr,ymax=mean.kcrate.nM.hr+sd.kcrate.nM.hr),color="grey30",width=0.5)
barplot <- a + theme_bw() + coord_cartesian(ylim=c(0,30)) + theme(strip.text=element_text(size=16),axis.text.x = element_text(angle = 70, hjust = 1,size=16),axis.text.y=element_text(size=16),axis.title.y=element_text(size=20,vjust=2),axis.title.x=element_text(size=20),legend.position="none",strip.background=element_blank(),strip.text.x=element_text(face="bold"),strip.text.y=element_text(face="bold")) + scale_fill_manual(values=substrateColors) + labs(y='hydrolysis rate (nM/hr)')
print(barplot)
dev.off()

png('../figures/FLA_maxbarplot_10d.png',width=1000,height=600)
a <- ggplot(tenmax,aes(x=substrate,y=mean.kcrate.nM.hr)) + geom_bar(aes(fill=substrate),color="gray30",position="dodge",stat="identity") + facet_grid(depthid~stn) + geom_errorbar(aes(ymin=mean.kcrate.nM.hr-sd.kcrate.nM.hr,ymax=mean.kcrate.nM.hr+sd.kcrate.nM.hr),color="grey30",width=0.5)
barplot <- a + theme_bw() + coord_cartesian(ylim=c(0,30)) + theme(strip.text=element_text(size=16),axis.text.x = element_text(angle = 70, hjust = 1,size=16),axis.text.y=element_text(size=16),axis.title.y=element_text(size=20,vjust=2),axis.title.x=element_text(size=20),legend.position="none",strip.background=element_blank(),strip.text.x=element_text(face="bold"),strip.text.y=element_text(face="bold")) + scale_fill_manual(values=substrateColors) + labs(y='hydrolysis rate (nM/hr)')
print(barplot)
dev.off()

png('../figures/FLA_maxstackbarplot_10d.png',width=600,height=800)
a <- ggplot(tenmax,aes(x=stn,y=mean.kcrate.nM.hr)) + geom_bar(aes(fill=substrate),color="gray30",stat="identity") + facet_grid(depthid~.) 
barplot <- a + theme_bw() + coord_cartesian(ylim=c(0,55)) + theme(strip.text=element_text(size=16),axis.text.x = element_text(angle = 70, hjust = 1,size=16),axis.text.y=element_text(size=16),axis.title.y=element_text(size=20,vjust=2),axis.title.x=element_text(size=20),legend.position="none",strip.background=element_blank(),strip.text.x=element_text(face="bold"),strip.text.y=element_text(face="bold")) + scale_fill_manual(values=substrateColors) + labs(y='hydrolysis rate (nM/hr)')
print(barplot)
dev.off()

levels(factors$timepoint) = c("0","2","5","10","17","30","42")
ten <- factors[factors$timepoint %in% c("0","2","5","10"),]

png('../figures/FLA_timeplot_10d_barchart.png',width=1000,height=600)
a <- ggplot(ten,aes(x=timepoint,y=mean.kcrate.nM.hr)) + geom_bar(aes(fill=substrate),color="gray30", stat="identity") + facet_grid(depthid~stn) 
timeplot <- a + theme_bw() + coord_cartesian(ylim=c(0,45)) + theme(strip.text=element_text(size=16),title=element_text(size=20),axis.text.x = element_text(angle = 70, hjust = 1,size=16),axis.text.y=element_text(size=16),axis.title.y=element_text(size=20,vjust=2),axis.title.x=element_text(size=20),legend.position="none",strip.background=element_blank(),strip.text.x=element_text(face="bold"),strip.text.y=element_text(face="bold")) + scale_color_manual(values=c("black","#01FFFF","#017F01","#FFFE41","#0000FE","#D50002")) + scale_fill_manual(values=c("black","#01FFFF","#017F01","#FFFE41","#0000FE","#D50002")) + labs(x='time elapsed (days)',y='hydrolysis rate (nM/hr)')
print(timeplot)
dev.off()

factors$timepoint <- as.numeric(as.character(factors$timepoint))

png('../figures/FLA_timeplot.png',width=1000,height=600)
a <- ggplot(factors,aes(x=timepoint,y=mean.kcrate.nM.hr,group=substrate)) + geom_point(aes(fill=substrate),color="gray30",pch=21,size=4,alpha=0.5) + geom_line(aes(color=substrate),size=1,alpha=0.6) + facet_grid(depthid~stn) + geom_errorbar(aes(ymin=mean.kcrate.nM.hr-sd.kcrate.nM.hr,ymax=mean.kcrate.nM.hr+sd.kcrate.nM.hr, color=substrate),alpha=0.3,width=0.5)
timeplot <- a + theme_bw() + coord_cartesian(ylim=c(0,30)) + theme(strip.text=element_text(size=16),title=element_text(size=20),axis.text.x = element_text(angle = 70, hjust = 1,size=16),axis.text.y=element_text(size=16),axis.title.y=element_text(size=20,vjust=2),axis.title.x=element_text(size=20),legend.position="none",strip.background=element_blank(),strip.text.x=element_text(face="bold"),strip.text.y=element_text(face="bold")) + scale_color_manual(values=c("black","#01FFFF","#017F01","#FFFE41","#0000FE","#D50002")) + scale_fill_manual(values=c("black","#01FFFF","#017F01","#FFFE41","#0000FE","#D50002")) + labs(x='time elapsed (days)',y='hydrolysis rate (nM/hr)')
print(timeplot)
dev.off()

png('../figures/FLA_timeplot_10d.png',width=1000,height=600)
a <- ggplot(factors,aes(x=timepoint,y=mean.kcrate.nM.hr,group=substrate)) + geom_point(aes(fill=substrate),color="gray30",pch=21,size=4,alpha=0.5) + geom_line(aes(color=substrate),size=1,alpha=0.6) + facet_grid(depthid~stn) + geom_errorbar(aes(ymin=mean.kcrate.nM.hr-sd.kcrate.nM.hr,ymax=mean.kcrate.nM.hr+sd.kcrate.nM.hr, color=substrate),alpha=0.3,width=0.5)
timeplot <- a + theme_bw() + coord_cartesian(ylim=c(0,30),xlim=c(0,10)) + theme(strip.text=element_text(size=16),title=element_text(size=20),axis.text.x = element_text(angle = 70, hjust = 1,size=16),axis.text.y=element_text(size=16),axis.title.y=element_text(size=20,vjust=2),axis.title.x=element_text(size=20),legend.position="none",strip.background=element_blank(),strip.text.x=element_text(face="bold"),strip.text.y=element_text(face="bold")) + scale_color_manual(values=c("black","#01FFFF","#017F01","#FFFE41","#0000FE","#D50002")) + scale_fill_manual(values=c("black","#01FFFF","#017F01","#FFFE41","#0000FE","#D50002")) + labs(x='time elapsed (days)',y='hydrolysis rate (nM/hr)')
print(timeplot)
dev.off()

ten.normalized <- data.frame()
for (site in levels(ten$site)) {
    for (timepoint in levels(ten$timepoint)) {
        subset = ten[ten$site==site&ten$timepoint==timepoint,]
        total = sum(subset$mean.kcrate.nM.hr)
        subset$normalized.mean.rate = subset$mean.kcrate.nM.hr*100/total
        ten.normalized <- rbind(ten.normalized,subset)
    }
}
ten.normalized[is.na(ten.normalized)] <- 0
levels(ten.normalized$timepoint) = c("0","2","5","10","17","30","42")
levels(ten.normalized$stn) = c("stn1","stn2","stn3","stn4")
levels(ten.normalized$depthid) = c("surface","bottom")

ten.normalized <- ten.normalized[ten.normalized$timepoint %in% c("2","5","10"),]
png('../figures/FLA_timeplot_10d_barchartnormalized.png',width=1000,height=600)
a <- ggplot(ten.normalized,aes(x=timepoint,y=normalized.mean.rate)) + geom_bar(aes(fill=substrate),color="gray30", stat="identity") + facet_grid(depthid~stn) 
timeplot <- a + theme_bw() + coord_cartesian(ylim=c(0,100)) + theme(strip.text=element_text(size=16),title=element_text(size=20),axis.text.x = element_text(angle = 70, hjust = 1,size=16),axis.text.y=element_text(size=16),axis.title.y=element_text(size=20,vjust=2),axis.title.x=element_text(size=20),legend.position="none",strip.background=element_blank(),strip.text.x=element_text(face="bold"),strip.text.y=element_text(face="bold")) + scale_color_manual(values=c("black","#01FFFF","#017F01","#FFFE41","#0000FE","#D50002")) + scale_fill_manual(values=c("black","#01FFFF","#017F01","#FFFE41","#0000FE","#D50002")) + labs(x='time elapsed (days)',y='relative contribution to rate (%)')
print(timeplot)
dev.off()
    
