####Use the delta from election day to tell temporal a story using d3

 - month by month (day by day (or week by week ))
 - should include a timeline of all important/relavent election dates
 	- primaries
 	- conventions
 	- debates
 	- dates that candidates dropped out
 	- important "political news" such as the release of Romney's 47% comment
 	- what do we know about each type of donor?
 		- possibly build a profile of each parties donor over time?
	 		- where are donations coming from?
	 		- males/females making them?
	 		- how much are they donating?

###Basic infographic of ohio
####Uses MongoDB to store all of the data (shows knowledge from previous class while also forcing me to prepare data for d3 visualiztion. mongodb interfaces with javascript while also allowing me to store unstructured data. use meteor?)
 - Each city is clickable
 	- when you click on it, zooms in and gives a basic overview of that city's donation behavior in that specific time period (possibly include arrows to scroll through each time period)
 		- number of contribuitons
 			- from day zero
 			- from the last period (the previous month if going month by month)
 		- total amount of contributions
 		- breakdown between party
 		- highest donation day in dollars
 		- highest donation day in volume<span><sup>1</sup></span>
 			- gives percent change since last time period
 		- contrast the donation amount (either dollar or volume) with polls
 			- is there a relationship between polling numbers and contribution dollars or volume
		- group by contributor employer
			- show the top 3 employers for both
				- volume of donations
				- sum of donations

<sup><sup>1</sup> Volume refers to the number of donations</sup>


###Data Analysis W/ R Project
- use statistical tests/analysis to tell a story and explain concepts
	- use elect delta to explain the log-normal distribution
	- perform mann-whitney u test on contribution amounts between R's & D's
		- explain mann-whitney u through visualizations
	- explain roc 
	- explain cross validation
	- include "this is read" for any mathematical notation
		- e.g. "read the probability of y occurring given that x has occurred is equal to some constant number,alpha, plus some number, beta, times x"


#Timeline
- What did democratic & republican contributions look like at [19 - 13] months & [13 - 7] months of the election?
	- Volume of contributions
	- Sum of contributions
	- Median Contribution
	- Single day high for total contributions and volume of contributions
	- Single day low for total contributions and volume of contributions
	- Break down of female contributions
	- Break down of male contributions
		- Did the volume of contributions increase from the previous period to this one? (Use statistical test)
		- Did the sum of contributions increase from the previous period to this one? (Use statistical test)?
		- Did the gender of contributors change (i.e more men than women ) change from the previous period to this one? (Use statistical test)?
- What did democratic & republican contributions look like for at [7-5] & [5-3] months?
	- Volume of contributions
	- Sum of contributions
	- Median Contribution
	- Single day high for total contributions and volume of contributions
	- Single day low for total contributions and volume of contributions
- What did democratic & republican contributions look like for [3-2] & [2-1] months?
	- Volume of contributions
	- Sum of contributions
	- Median Contribution
	- Single day high for total contributions and volume of contributions
	- Single day low for total contributions and volume of contributions
- What did democratic & republican contributions look like for [4-3],[3-2],[2-1],[1-0] weeks?
	- Volume of contributions
	- Sum of contributions
	- Median Contribution
	- Single day high for total contributions and volume of contributions
	- Single day low for total contributions and volume of contributions



# Functions Necessary
- group by month
	- group by 