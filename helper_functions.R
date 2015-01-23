
groupData <- function(initial_data,...){
# helper function to make frequent grouping of data with diffrent variables easier
gp_data <-group_by(na.omit(initial_data),...)
gps_data<-summarise(gp_data, 
                  mean_contb = mean(contb_receipt_amt),
                  median_contb = median(contb_receipt_amt),
                  sum_contb = sum(contb_receipt_amt),
                  mean_pop = mean(population),
                  median_pop = median(population),
                  median_elect_delta = median(elect_delta),
                  mean_elect_delta = mean(elect_delta),
                  count = n()) 
gps_data
}

add_city <- function(city){
	# adds binary feature if the contributor's zip code is within a city
	# zip codes are used because I wanted to include the surrounding areas
   sapply(data$contbr_zip, function(zip){
    if (substring(as.character(zip),1,5) %in% city){
      1
    }
    else{
      0
    }
    })
}