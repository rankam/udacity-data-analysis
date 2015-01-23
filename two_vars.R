data.party <- groupData(data, cand_party)
group.colors <- c('D'='blue','R'='red','G'='green','L'='yellow')
ggplot(aes(x=cand_party,y=mean_contb), data=data.party) + geom_bar(stat='identity')
ggplot(aes(x=cand_party,y=median_contb), data=data.party) + geom_bar(stat='identity')
ggplot(data,aes(data$elect_delta, color=data$cand_party)) + geom_density() + scale_fill_manual(values=group.colors)
ggplot(data,aes(log(elect_delta +1), color=cand_party)) + geom_density() + scale_fill_manual(values=group.colors)

ggplot(aes(x=log(contb_receipt_amt +1)), data=data) + geom_density(aes(color=cand_party))


ggplot(aes(x=cand_party,y=count), data=data.party) + geom_bar(stat='identity')
# DEMS HAVE MORE DONORS THAT GIVE SMALLER AMOUNTS

ggplot(aes(x=cand_party,y=median_pop), data=data.party) + geom_bar(stat='identity')
# GREEN PARTY DONORS ARE 'MORE LIKELY' TO LIVE IN CITIES W/ LARGER POPS

ggplot(aes(y=log(contb_receipt_amt),x=1), data=data) + geom_boxplot(aes(fill=cand_party))# GOOD - POSSIBLE FOR FINAL 3 PLOTS