library("foreign")
library(rattle)
options(scipen = 2000000)

#S1:dealing data
dataDublin=read.csv("./dublin_property.csv",header=T,stringsAsFactors = TRUE)
View(dataDublin)
str(dataDublin)
glimpse(dataDublin)

dataDublin$area[which(dataDublin$area=="null")]=NA
dataDublin$bed[which(dataDublin$bed=="null")]=NA
data=na.omit(dataDublin)

#S2:convert to num
data$price=as.numeric(gsub('[,€]', '', data$price))
data$area=as.numeric(gsub('[ac,m²]','',data$area))
data$bed=as.numeric(gsub('[,Bed]','',data$bed))

#-indxPrice=sapply(data,is.factor)
#data[indxPrice] = lapply(data[indxPrice], function(x) as.numeric(gsub("[,€]", "", x)))

#data$type=factor(data$type,levels = c("Apartment","Bungalow","Detached","Duplex","End of Terrace",
#                                      "Semi-D","Terrace","Townhouse"),
#                labels=c("1","2","3","4","5","6","7","8"))
#data$district=as.numeric(gsub('6W','66',data$district))
#write.csv(x=data,file="dublinDataForNumpy.csv")

View(data)
#write.csv(x=data,file="dublinData.csv")

str(data)
summary(data)
summary(data$type)
summary(data$bed) 

----------------------------------------------------------

#drawing table
library(DT)
datatable(data,options = list(pageLength = 50))

#circle(type) show the type distribution
install.packages("tidyverse")
install.packages("ggthemes")
library(tidyverse)
library(ggthemes)
a=table(data$type)
typeNumber=data.frame(a)
type1=typeNumber$Var1
number=typeNumber$Freq
rate=paste(round(100*number/sum(number)),"%")
pie=data.frame(type=type1,n=number,rate_per=rate)
pie$fraction = pie$n / sum(pie$n)
pie$ymax = cumsum(pie$fraction)
pie$ymin = c(0, head(pie$ymax, n = -1))

ggplot(data = pie, aes(fill = type, ymax = ymax, ymin = ymin, xmax = 4, xmin = 3)) +
  geom_rect(show.legend = F,alpha=0.8) +
  scale_fill_brewer(palette = 'Set3')+
  coord_polar(theta = "y") +
  labs(x = "", y = "", title = "",fill='Type') + 
  xlim(c(0, 5)) +
  theme_light() +
  theme(panel.grid=element_blank()) + 
  theme(axis.text=element_blank()) + 
  theme(axis.ticks=element_blank()) + 
  theme(panel.border=element_blank()) + 
  geom_text(aes(x = 4.6, y = ((ymin+ymax)/2),label = type) ,size=4)+
  geom_text(aes(x = 3.5, y = ((ymin+ymax)/2),label = rate_per) ,size=3.6)


#circle(district) show the district distribution
library(tidyverse)
library(ggthemes)
b=table(data$district)
districtNumber=data.frame(b)
district1=districtNumber$Var1
number=districtNumber$Freq
rate=paste(round(100*number/sum(number)),"%")
pie=data.frame(district=district1,n=number,rate_per=rate)
pie$fraction = pie$n / sum(pie$n)
pie$ymax = cumsum(pie$fraction)
pie$ymin = c(0, head(pie$ymax, n = -1))
library(RColorBrewer)
nb.cols=24
mycolors=colorRampPalette(brewer.pal(8,"Set2"))(nb.cols)
ggplot(data = pie, aes(fill = district, ymax = ymax, ymin = ymin, xmax = 4, xmin = 3)) +
  geom_rect(show.legend = F,alpha=0.8) +
  scale_fill_manual(values = mycolors)+
  coord_polar(theta = "y") +
  labs(x = "", y = "", title = "",fill='District') + 
  xlim(c(0, 5)) +
  theme_light() +
  theme(panel.grid=element_blank()) + 
  theme(axis.text=element_blank()) + 
  theme(axis.ticks=element_blank()) + 
  theme(panel.border=element_blank()) + 
  geom_text(aes(x = 4.6, y = ((ymin+ymax)/2),label = district) ,size=4)+
  geom_text(aes(x = 3.5, y = ((ymin+ymax)/2),label = rate_per) ,size=3.6)

help("palette")

#hist-bed, show the bed hist picture
library(ggplot2)
type_freq=data.frame(table(data$bed))
type_plot= ggplot(data = type_freq, mapping = aes(x = reorder(Var1, -Freq),y = Freq)) + geom_bar(stat = 'identity', fill = '#33FF99') + theme(axis.text.x  = element_text(angle = 45, vjust = 0.5)) + xlab('The number of bedroom') + ylab('Number')
print(type_plot)

#hist-district 
library(ggplot2)
type_district=data.frame(table(data$district))
type_plot2=ggplot(data=type_district,mapping=aes(x=reorder(Var1,-Freq),y=Freq))+geom_bar(stat='identity',fill='#00CD00')+ theme(axis.text = element_text(angle=45,vjust=0.5))+xlab('District')+ylab('Number')
print(type_plot2)

#hist(type-bed), show the type with bed hist picture
library(plotly)
library(ggplot2)
ptb=ggplot(data = data, aes(x = type, fill = bed)) +
  geom_bar(position = "dodge")
ggplotly(ptb)

#hist(district-type)
library(plotly)
library(ggplot2)
ptd=ggplot(data=data,aes(x=district, fill=type))+
  geom_bar(position="dodge")
ggplotly(ptd)

#hist(district-bed)
library(plotly)
library(ggplot2)
pdb=ggplot(data=data,aes(x=district,fill=bed))+
  geom_bar(position="dodge")
ggplotly(pdb)


#hist(type-mean.price) (type-median.price)
newData=data
install.packages("plyr")
library(plyr)
library(ggplot2)
#consider the mean price cannot behalf, so use median price
newData1=ddply(newData,.(type),function(sub){data.frame(price.mean=mean(sub$price))})
newData2=ddply(newData,.(type),function(sub){data.frame(price.median=median(sub$price))})
p3=ggplot(newData1, aes(x=type, y=price.mean)) +
  geom_bar(stat="identity", fill="lightblue", colour="white")
p4=ggplot(newData2, aes(x=type, y=price.median)) +
  geom_bar(stat="identity", fill="orange", colour="white")
ggplotly(p3)
ggplotly(p4)

#hist(type-bed-median.price)
newData=data
library(plyr)
library(ggplot2)
newData3=ddply(newData,.(type,bed),function(sub){data.frame(price.median=median(sub$price))})
p5=ggplot(data = newData3, aes(x = type, y=price.median)) +
  geom_bar(stat = "identity", aes(fill = bed), position = "dodge")
ggplotly(p5)

#hist(type-bed-mean.area)
newData=data
library(plyr)
library(ggplot2)
newData8=ddply(newData,.(type,bed),function(sub){data.frame(area.mean=mean(sub$area))})
p10=ggplot(data = newData8, aes(x = type, y=area.mean)) +
  geom_bar(stat = "identity", aes(fill = bed), position = "dodge")
ggplotly(p10)

#hist(type-area-median.price)
newData=data
library(plyr)
library(ggplot2)
newData4=ddply(newData,.(type,area),function(sub){data.frame(price.median=median(sub$price))})
p6=ggplot(data = newData4, aes(x = type, y=price.median)) +
  geom_bar(stat = "identity", aes(fill = area), position = "dodge")
ggplotly(p6)

#hist(district-type-median.price)
newData=data
library(plyr)
library(ggplot2)
newData5=ddply(newData,.(district,type),function(sub){data.frame(price.median=median(sub$price))})
p7=ggplot(data=newData5,aes(x=district,y=price.median))+
  geom_bar(stat="identity",aes(fill=type),position = "dodge")
ggplotly(p7)

#hist(district-type-mean.area)
newData9=ddply(newData,.(district,type),function(sub){data.frame(area.mean=mean(sub$area))})
p11=ggplot(data=newData9,aes(x=district,y=area.mean))+
  geom_bar(stat="identity",aes(fill=type),position = "dodge")
ggplotly(p11)

#hist(district-type-bed)
newData10=ddply(newData,.(district,type),function(sub){data.frame(bed.median=median(sub$bed))})
p12=ggplot(data=newData10,aes(x=district,y=bed.median))+
  geom_bar(stat="identity",aes(fill=type),position = "dodge")
ggplotly(p12)
#hist(district-type-bed.mean)
newData11=ddply(newData,.(district,type),function(sub){data.frame(bed.mean=mean(sub$bed))})
p13=ggplot(data=newData11,aes(x=district,y=bed.mean))+
  geom_bar(stat="identity",aes(fill=type),position = "dodge")
ggplotly(p13)

#hist(district-bed-mean.area)
newData=data
library(plyr)
library(ggplot2)
newData6=ddply(newData,.(district,bed),function(sub){data.frame(area.mean=mean(sub$area))})
p8=ggplot(data=newData6,aes(x=district,y=area.mean))+
  geom_bar(stat="identity",aes(fill=bed),position = "dodge")
ggplotly(p8)

#hist(district-bed-median.price)
newData=data
library(plyr)
library(ggplot2)
newData7=ddply(newData,.(district,bed),function(sub){data.frame(price.median=median(sub$price))})
p9=ggplot(data=newData7,aes(x=district,y=price.median))+
  geom_bar(stat="identity",aes(fill=bed),position = "dodge")
ggplotly(p9)

#map
install.packages("ggmap")
install.packages("leaflet")
library("ggmap")
library("leaflet")
if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup")
library(ggmap)

#simple map      
m = leaflet() %>%
  addTiles() %>%  
  addMarkers(lng=data$longitude, lat=data$latitude, popup="")
print(m)

#modified map
pal=colorFactor(c("green","black","red","white","DarkOrange","yellow","blue","purple"),domain=data$type)
df = data.frame(Lon=data$longitude, Lat=data$latitude)
map=leaflet(df) %>% 
  addTiles()%>%
  addCircleMarkers(color = ~pal(data$type),stroke = FALSE,fillOpacity = 1,radius=4)
print(map)

---------------------------------------------------

#normal distribution
norm.test <- function(x, breaks = 20, alpha = 0.05,plot = TRUE){
  if(plot == TRUE){
    #set graph
    opar <- par(no.readonly = TRUE)
    layout(matrix(c(1,1,2,3),2,2,byrow = TRUE),
           width = c(2,2),heights = c(2,2))
    #draw hist
    hist(x, freq = FALSE, breaks = seq(min(x),
         max(x), length = breaks), 
         main = 'X hist',
         ylab = 'Kernel density values')
    #add Kernel density values image
    lines(density(x), col = 'red', lty = 1, lwd =2)
    #add normal distribution image
    x <- x[order(x)]
    lines(x, dnorm(x, mean(x), sd(x)),
          col = 'blue', lty = 2, lwd = 2.5)
    #add exmaple
    legend('topright',
           legend = c('Kernel Density Curves','Normal distribution curve'),
           col = c('red','blue'), lty = c(1,2),
           lwd = c(2,2.5), bty = 'n')
    #draw Q-Q
    qqnorm(x, xlab = 'Real distribution', ylab = 'Normal distribution',
           main = 'x Q-Q', col = 'blue')
    qqline(x)
    #draw P-P
    P <- pnorm(x, mean(x), sd(x))
    cdf <- 0
    for(i in 1:length(x)){
      cdf[i] <- sum(x <= x[i])/length(x)}
    plot(cdf, P, xlab = 'Real distribution', ylab = 'Normal distribution',main = 'x P-P', xlim = c(0,1),ylim = c(0,1), col = 'blue')
    abline(a = 0, b = 1)
    par(opar)
  }
  #Quantitative shapiro test
  if (length(x) <= 2000) {
    shapiro <- shapiro.test(x)
    if(shapiro$p.value > alpha)
      print(paste('The quantitative results are：', 'x follows a normal distribution，',
                  'P value =',round(shapiro$p.value,5), '> 0.05'))
    else
      print(paste('The quantitative results are：', 'x not follows a normal distribution，',
                  'P value =',round(shapiro$p.value,5), '<= 0.05'))
    shapiro
  }
  else {
    ks <- ks.test(x,'pnorm')
    if(ks$p.value > alpha)
      print(paste('The quantitative results are：', 'x follows a normal distribution，',
                  'P value =',round(ks$p.value,5), '> 0.05'))
    else
      print(paste('The quantitative results are：', 'x not follows a normal distribution，',
                  'P value =',round(ks$p.value,5), '<= 0.05'))
    ks
  }
}


norm.test(data$area)
norm.test(data$price)

#plot(price-area)
library(ggplot2)
plot(data$area,data$price,main="area-price",xlab='area(m²)',col='#33FF99',pch=19)
abline(lm(data$price~data$area),col='#FF6666',lwd=2,lty=1)
lines(lowess(data$area,data$price),col='#9933FF',lwd=2,lty=2)
legend('topright',legend = c('Best-fit linear line','Best-fit smoothing curve'),col = c('#FF6666','#9933FF'), lty = c(1,2),lwd = c(2,2.5), bty = 'n')
areaData=data
areaData1=subset(areaData, areaData$area <600)
# area<600
plot(areaData1$area,areaData1$price,main="area(<600)-price",xlab='area(m²)',col='#33FF99',pch=19)
abline(lm(areaData1$price~areaData1$area),col='#FF6666',lwd=2,lty=1)
lines(lowess(areaData1$area,areaData1$price),col='#9933FF',lwd=2,lty=2)
legend('topright',legend = c('Best-fit linear line','Best-fit smoothing curve'),col = c('#FF6666','#9933FF'), lty = c(1,2),lwd = c(2,2.5), bty = 'n')

aggregate(data$area,list(data$bed),median)
aggregate(data$area,list(data$bed),mean)


------------------------------------------------------------

#model- K-means
modeldata=data
k.plot = function(data, nc, seed=1234){
                
  k = (nrow(data)-1)*sum(apply(data,2,var)) 
  for (i in 2:nc){
    
    set.seed(seed) 
    k[i] = kmeans(data, centers=i, iter.max = 100)$tot.withinss
  }
  plot(1:nc, k, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares",col = '#9933FF',
       lwd = 2, main = 'Choose best Clusters')
}

standrad =data.frame(scale(modeldata[,c('area','price')]))
Kplot =k.plot(standrad, nc = 15)

# k =6, the decrease speed gets slow

set.seed(1234)
clust = kmeans(x = standrad, centers = 6, iter.max = 100)
table(clust$cluster)

table(data$type,clust$cluster)
table(data$district,clust$cluster)

y=data.frame(data$price,data$area,data$price/data$area)
colnames(y)=c('price.€','area.m²','€/m²')
aggregate(y,list(clust$cluster),mean)
#6 clusters
#Group.1   price.€   area.m²     €/m²
#1         539679.3 116.76230 4785.652
#2         978214.8 183.18817 5548.038
#3        1537991.7 918.83333 1750.078
#4        3308448.3 399.79310 8572.173
#5        1717987.0 278.16883 6427.042
#6         325743.0  70.26882 4884.370

library(ggplot2)
plotClust = ggplot(data = y[,1:2], mapping = aes(x = data$area,y = data$price, color = factor(clust$cluster)))
plotClust = plotClust + geom_point(pch = 20, size = 3)
plotClust + scale_colour_manual(values = c("red","blue", "green", "orange",'yellow','gray'))


---------------------------------------------------------
#logistic regression
  dataDublin=read.csv("./dublin_property.csv",header=T,stringsAsFactors = TRUE)
View(dataDublin)
str(dataDublin)

dataDublin$area[which(dataDublin$area=="null")]=NA
dataDublin$bed[which(dataDublin$bed=="null")]=NA
data=na.omit(dataDublin)

data$price=as.numeric(gsub('[,€]', '', data$price))
data$area=as.numeric(gsub('[ac,m²]','',data$area))
data$bed=as.numeric(gsub('[Bed]','',data$bed))
#change district to numeric, needs to convert 6W to 66
data$district=as.numeric(gsub('[W]','6',data$district))

#Y should 0-1.So change apartment type is 1, house type is 0
data$type=factor(data$type,levels = c("Apartment","Bungalow","Detached","Duplex","End of Terrace",
                                      "Semi-D","Terrace","Townhouse"),
                 labels=c("0","1","1","0","1","1","1","1"))


View(data)
str(data)
summary(data)
table(data$type)
table(data$bed)

#creating a random sample for training and testing data
set.seed(1234)
data_rand=data[order(runif(2389)),]

data_train=data_rand[1:1911, ]
data_test=data_rand[1912:2389, ]

#build model
model_reduced=glm(type ~  area + bed +price,
                  data=data_train,family="binomial")
summary(model_reduced)

modeldata=glm(type ~ district +area + bed + latitude + longitude + price
              ,data=data_train,family = "binomial")

summary(modeldata)

anova(model_reduced,modeldata,test="Chisq")

coef(model_reduced)

exp(coef(model_reduced))

exp(confint(model_reduced))

#Y=1.005762e+00*1 + 9.974900e-01*2 + 1.082732e+01*3 + 5.242799e-01*4 + 6.687032e-01*5 + 1.000000e+00*6 

library(gmodels)
z=predict(modeldata,data_test,type="link")
p=predict(model_reduced,data_test,type="response")
#p=1/(1+exp(-z))
pred=as.numeric(p>0.5)
pred= factor(pred, levels=  c(0,1), labels = c("apartment","house"))
CrossTable(pred,data_test$type,prop.chisq = FALSE,
           prop.c=FALSE,prop.r = FALSE,
           dnn=c('predicted','actual'))

library(pROC)
pre=predict(modeldata,data)
modelroc=roc(data$type,pre)
#AUC=0.88, good model
plot(modelroc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE)

#verify
newdata=data.frame(district=8,
                   area=33,
                   bed=1,
                   latitude=53.34129, 
                   longitude=-6.28018,
                   price=195000)

newdata1=data.frame(
  area=68,
  bed=3,
  price=280000)

prediction=predict(modeldata,newdata,type="response")
prediction1=predict(model_reduced,newdata1,type="response")
print(prediction)
print(prediction1)

