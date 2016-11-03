
require("maps") #load library "maps"
##step 1: attach geographic coordinates
all_states<-map_data("state") #load data "state" which has geographic coordinates of each sate
mturkData<-read.csv("mturkdata2.csv") #load your own data that consists demographic inforamtion of your participants
states<-aggregate(rating~region,mturkData,mean) #calculate the mean ratings for each "region", in my data, "region" refers to the states where the participants current reside.
total<-merge(all_states,states,y.by="region") #merge the coordinates with my data, by "region" (i.e. states)
total$region<-factor(total$region) #make sure the region variable is categorical
total <- total[order(total$order),] #order data by the "order" variable[important!]
#you can now draw the graph
library(ggplot)
p<-ggplot()
p + 
  geom_polygon(data=total, aes(x=long, y=lat, group = group, fill=total$rating),colour="black")+ 
  scale_fill_continuous()
#But I want to add state abbreviations to the figure
#and I want darker color to correspond to "higher accentedness ratings"
#find the central location, where the abbreviations should appear
library(sp)
centroids <- setNames(do.call("rbind.data.frame", by(total, total$group, function(x) {Polygon(x[c('long', 'lat')])@labpt})), c('long', 'lat')) 
centroids$label <- total$region[match(rownames(centroids), total$group)]

#take only one label for each state (you might want to do manually select one, rather than using the following codes) 
try2<-centroids[!duplicated(centroids[,c('label')]),]

#in my data, I used the full names for each state (e.g. "california", "florida" etc.,)
# the following codes turn full names into the two-letter abbreviations 
try2$abb<-state.abb[match(try2$label,tolower(state.name))]

#using annotate(), we attach the abbreviations to the figure
#the theme functions hide labels, tick, and changes fonts.
#check ggplot2 theme function for details.

ggplot(total, aes(long, lat, group=group, fill=rating)) +
  geom_polygon(colour = "grey") +
  scale_fill_continuous(low = "cornflowerblue",high = "darkblue",guide=guide_colorbar(barwidth = 2,barheight = 10))+
  with(try2, annotate(geom="text", x = long, y=lat, label = abb, size = 6,color="white"))+
  labs(fill = "Mean Ratings")+
  labs(title = "Mean Accentedness Ratings by State")+
  theme(panel.background = element_rect(fill = "grey"),
        plot.background = element_rect(fill = "grey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  scale_y_continuous(breaks=c()) + 
  scale_x_continuous(breaks=c()) + 
  theme(panel.border =  element_blank())+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank())+
  theme(plot.title = element_text(size = 30, family = "Times",colour = "white"),
               legend.title= element_text(hjust = 0.4 ,vjust=0.3, size=20,family = "Times"),
               legend.text = element_text(hjust = 0.4 ,vjust=2, size=20,family = "Times")
             )
