best <- function(state, Outcome) {
## Read outcome data

	outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character",na.strings="9999")
	if(length(which(outcome$State==state))==0)
	{
		stop('invalid state')
	}else if(Outcome!="heart attack" && Outcome!="heart failure" && Outcome!="pneumonia")
	{
		stop('invalid outcome')
	}

	outcome <- subset(outcome,State==state)

## Check that state and outcome are valid
	if(Outcome == "heart attack")
	{
		outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack[is.na(as.numeric(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))] <- 9999
		outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
		outcome <- subset(outcome,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack==	min(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
		vector <- outcome$Hospital.Name
		vector <- sort(vector)
		return(vector[1])
	}else if(Outcome=="heart failure")
	{
		outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure[is.na(as.numeric(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))] <- 9999
		outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
		outcome <- subset(outcome,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure==min(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
		vector <- outcome$Hospital.Name
	 	vector <- sort(vector)
		return(vector[1])	
	}else if(Outcome=="pneumonia")
	{
		outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia[is.na(as.numeric(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))] <- 9999
		outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
		outcome <- subset(outcome,Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia==min(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
		vector <- outcome$Hospital.Name
		vector <- sort(vector)
		return(vector[1])
	}
}
