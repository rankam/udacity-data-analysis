data(zipcode)
data$zip <- clean.zipcodes(data$contbr_zip)
oh <- subset(zipcode, state=='OH') # zipcode package would change my zipcode in 'data' variable to zip codes in MA for some reason. Subsetting only Ohio values from zipcode seemed to solve this issue
map.data <- merge(subset(data, contb_receipt_amt > 0), oh, by.x='contbr_zip', by.y='zip')

map.plot <- ggplot(data=subset(map.data,cand_party == 'D' | cand_party == 'R'),aes(x=longitude, y=latitude, color=cand_party, size = contb_receipt_amt)) + geom_point(position = position_jitter(w=.08,h=.08)) + scale_size_continuous(breaks = c(50,100,500,1000,2500,5000,10000),range=c(1,15)) + labs(x=NULL, y=NULL)#xlab(NULL) + ylab(NULL)  #AWESOME
map.plot <- map.plot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.ticks=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank())
map.plot

# The map makes it easy to spot the most populated cities in Ohio. The center mass is Columbus and to its left, you can see a cluster which contains Dayton at the top and Cincinatti at the bottom.
# The top right cluster is the Cleveland area and the top left is Toledo.
# The size of each point indicates the amount of the contribution. Contributions to Republicans appear to be larger than contributions to Democrats. However, there are more contributions made to Democrats than Republicans. 
