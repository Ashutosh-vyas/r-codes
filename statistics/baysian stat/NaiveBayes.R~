# AUTHOR : ASHUTOSH VYAS
# THIS CORE CONCEPT OF NAICVE BAYES IS - CONSIDERATION OF THE
# INDEPENDENCE IN THE CORRESPONDING FEATURES OF A  
# VECTOR , GIVEN SOME CONDITION

# THSI ASSIGNMENT FOCUS ON THE UNDERSTANDING OF NAIVE BAYES  
# naive bayes perform classification

# THIS IS JUST A EXPLAINATION OF NAIVE BAYES - AS THE IRIS DATASET IS RANDOM AND 
# THE VALUES THAT FEATURES TAKE ARE RANDOM THEREFORE THE RESULTS ARE NOT ACCURATE 
# BECAUSE HERE THE FEATURE VALUES ARE CONSIDERED TO BE UNIQUE .
# BUT THE CONCEPT IS SELF EXPLAINATORY



naiveBayes <- function(daataset,n,classification)
{

	# classification takes values from 1 to 3 as follows (this is taken only for iris dataset)
	#  setosa      -----> 1
	#  versicolor  -----> 2
	#  virginica   -----> 3

	# for an understanding purpose we take iris data as sample
	dataset <- as.matrix(iris)
	
	# now we take a closure look on naive bayes
	# P(A and B/C) = P(A/C)*P(B/C)
	
	# We consider the classifiers and try to fit data on it
	# according to the features, but all the features are considered
	# independent

	# We train our naive classifier based on the data set givrn to it.
	# Let us folloe the 5:1 ratio and split the given data set in 125 traing 
	# and 25 testing data set (as IRIS dataset has 150 rows)

	total_rows <- nrow(dataset)
	training_rows <- total_rows
	test_rows <- (.25) * total_rows
	train_dataset <- dataset[c(1:training_rows),]
	test_dataset <- dataset[c(test_rows:total_rows),]
	
	# We consider all the features independent , this dataset has 4 features and 5th 
	# feature is the classification, 
	
	total_features <- ncol(dataset)
	print(total_features)
	# here we know last featre is classifier, thus is not included oin the vector
	# which we make of each dataset, depending upon the total number of coloumns.
	total_cases <- 0
	favor_cases <- 0 
	
	naive_prob <- 1
	i <- 1	
	while(i <= total_features - 1)
	{
		total_cases <- 0
		favor_cases <- 0 
		j <- 1
		while(j <= nrow(train_dataset))	
		{
			if(train_dataset[j,5]==classification)
			{
				total_cases <- total_cases + 1
				if(dataset[n,i] == train_dataset[j,i])
				{
					favor_cases <- favor_cases + 1
				}
			}
			j <- j + 1
		}	
		if(total_cases != 0)
		{
			print(paste("total cases are ",total_cases))
			feature_prob <- favor_cases / total_cases
			print(feature_prob)
			naive_prob <- naive_prob * feature_prob
		}
		else
		{
			print("this classifiacation does not exist")
		}
		i <- i + 1
			
	}
	print(paste("The probablity of ",classification,"given the features is",naive_prob))		
} 



