require(rPython)


data <- read.csv('~/projects/udacity_DataR/P00000001-OH.csv', sep=',',row.names=NULL,stringsAsFactors = FALSE)
head(data)
colnames(data)[1:18] <- colnames(data)[2:19] # The dataset had each column name shifted to the right one place with an additional column at the end filled with NAs. I renamed the columns and dropped the last column.
data <- data[1:18]

dim(data)
names(data)
str(data)
summary(data) 
describe(data)

#The data contains 19 features which makes it difficult to understand. Ideally, I would like to reduce the number of features to no more than 10. However, what I can tell is that the majority of the conbributions were made to Barack Obama (71%) and Mitt Romney (22%) with the other 12 candidates splitting the remaining 7% of contributions. I can also see that the candidate's party is not included the dataset - it will be necessary to add this information as it is vital to answering the question, "to which party did the contributor donate"?

pops <- fromJSON('~/projects/udacity_DataR/ohio_city_populations.json') # census data
data$population <- sapply(data$contbr_city,function(x) { 
  ifelse(x %in% names(pops),return(pops[as.character(x)]),return(NA))
  }) 

data$population <- as.numeric(data$population) 
data$contb_receipt_dt <- as.Date(data$contb_receipt_dt, "%d-%b-%y")
election_day <- as.Date("2012-11-06")
data$elect_delta <- sapply(data$contb_receipt_dt,function(x) {return(election_day - x)}) # days from general election day
data$contbr_zip <- substr(as.character(data$contbr_zip),1,5) # zip codes should be treated as categorical variables and I want to limit it to the main 5 digit zip code without the additional information


cand_party = list()    
for (cand in unique(data$cand_nm)){
   if(cand == 'Obama, Barack'){
    cand_party['Obama, Barack'] <- 'D'
  }     
  else if(cand == 'Stein, Jill'){
    cand_party['Stein, Jill'] <- 'G'
  } else if(cand == 'Johnson, Gary Earl'){
    cand_party['Johnson, Gary Earl'] <- 'L'
  } else{
    cand_party[as.character(cand)] <- 'R'
  }
}
data$cand_party <- sapply(as.character(data$cand_nm), function(x) {as.character(cand_party[x])})
data$contb_bins <- cut(data$contb_receipt_amt,breaks=c(0,50,100,250,500,1000,1500,2500,3500,15000))
data$pop_bins <- cut(data$population, breaks=c(0,5000,25000,50000,100000,500000,1000000))
data$elect_delta_bins <- cut(data$elect_delta,breaks=c(0,7,14,30,90,180,365,700,1500))




data <- subset(data,contb_receipt_amt >0) # I'm not concerned with negative donations - I'm not really sure what they mean 
data <- na.omit(data) # there are 100k+ rows and it's exploratory analysis so I'm going to just going to drop all NAs - if I wanted to publish my analysis or put it into "production", I would revisit the NAs to ensure they were random as opposed to systematic. Though, I guess it could be argued that understanding if NAs are random or systematic could be considered part of EDA. 
data <- subset(data, cand_party == 'D' | cand_party == 'R') # this is America, third parties get ignored


data$contbr_fnm <- sapply(data$contbr_nm, function(name) {
       f.name <- str_extract(as.character(name),", [[:alpha:]]+($|[[:space:]])")
       f.name <- str_replace(f.name,", ","")
       f.name <- str_trim(f.name)
       return(as.character(f.name))
    
  })

# data$predicted_gender <- data$gender


write.csv(data,'~/projects/udacity_DataR/data_gender_pred.csv')
# I need to predict the gender of the contributors that did not include MR., MRS., or MS. I attempted to use the gender
# package in R, but it was very slow (it never completed the task, but it had been over 8 hours when I finally stopped it).
# As such, I decided to use Python's NLTK library for the task and created a script called classify_gender.py. Running 
# classify_gender creates a new csv file (data_gender_predicted.csv) with predicted gender names and prints "You classified 0.7704 correct on the test set"
# to stdout. 

# data <- read.csv('~/projects/udacity_DataR/data_gender_predicted.csv',stringsAsFactors = FALSE)
data <- read.csv('~/projects/udacity_DataR/data_w_salary_gender.csv',stringsAsFactors = FALSE)
data$predicted_gender <- data$gender
i = 1
final_gender <- sapply(data$gender, function(gender){
  # Use predicted gender for those contributos that did not include MR., MRS., MS. in their contribution
    if (is.na(gender)){
      data$predicted_gender[i]
    }
    else{
      gender
    }
    i = i + 1
  })
data$predicted_gender <- names(final_gender)
data$gender<- sapply(as.character(data$contbr_nm), function(name) {
                  
                  if(grepl("MRS.",name)){
                    return("female")
                  }
                  else if(grepl("MR.",name)){
                      return("male")
                  }
                  else if(grepl(" MS.",name)){
                    return("female")
                  }
                  else{
                    return(as.character(NA))
                  }
                })
data$included_gender <- ifelse(is.na(data$gender), 0, 1)


# Trying to use all of the information provided in the original file, I wrote a web scraping script, get_estimated_salaries.py,
# in Python that attempted to get salary information for different occupations. Indeed.com allows you to enter an occupation and
# zip code and returns an average salary. 



rows <- paste(data$contbr_nm, data$contbr_city, data$contbr_employer, sep=" ")
data$multiple_contb <- ifelse(duplicated(rows) == TRUE, 1, 0)
# making an assumption that if the contributor's name, city, and employer show up more than once, it is the same person
# this would indicate that they made multiple contributions

cbus <- as.character(c(43002, 43004, 43016, 43017, 43026, 43035, 43054, 43065, 43081, 43082, 43085, 43119, 43123, 43137, 43147, 43201, 43202, 43203, 43204, 43205, 43206, 43207, 43210, 43211, 43212, 43213, 43214, 43215, 43217, 43219, 43220, 43221, 43222, 43223, 43224, 43227, 43228, 43229, 43230, 43231, 43235, 43240))
cleveland <- as.character(c(44101, 44103, 44104, 44105, 44106, 44107, 44111, 44112, 44113, 44114, 44115, 44117, 44119, 44120, 44121, 44125, 44127, 44134))
cincy <- as.character(c(45202, 45203, 45204, 45205, 45206, 45207, 45208, 45209, 45212, 45214, 45216, 45217, 45219, 45220, 45223, 45224, 45225, 45226, 45227, 45229, 45230, 45231, 45232, 45239, 45243))
data$cincy <- add_city(cincy)
data$cbus <- add_city(cbus)
data$cleveland <- add_city(cleveland)

data <- subset(data, estimated_salary!= 'No Data ') # drop rows without an estimated salary
data <- subset(data, cand_party == 'D' | cand_party == 'R') 
data$estimated_salary <- as.numeric(data$estimated_salary)
