# AUTHOR : Ashutosh Vyas
# model selection uisng Baysian inference 
# Problem statement: We are given a set of numbers [2,4,8,16,32,65], we have to find the best fit model, range is from [0,100]
# with our primary konwlwsge we estimate a list of models 
# using baysian inference theory, we will try to find the best fit model 
# THE MAJOR PURPOSE OF THIS ASSIGNMENT IS TO ILLUSTRATE THAT BAYES CAN FIND THE NEAAREST MODEL(IF NOT THE TRUE MODEL)
# GIVEN A VERY LITTLE DATASET
BayesInference <- function(dataset)
{
	
	No_models <- 4
	# MODEL 1
	# Even numbers from 0 to 100

	Model_1 <- NULL
	i <- 0
	while(i<=100)
	{
		Model_1 = c(Model_1,i)
		i <- i + 2
	}
	
	# Model_2
	# All natural numbers from 0 to 100

	Model_2 <- NULL
	i <- 0
	while(i<=100)
	{
		Model_2 = c(Model_2,i)
		i <- i + 1
	}

	# Model_3
	# All the power of 4 from 0 to 100

	Model_3 <- NULL
	i <- 0
	p <- 0
	while(p<=100)
	{
		Model_3 <- c(Model_3,p)
		p <- (4)^(i)
		i <- i + 1 
	}

	# Model_4
	# All the power of 2 from 0 to 100

	Model_4 <- NULL
	i <- 0
	p <- 0
	while(p<=100)
	{
		Model_4 <- c(Model_4,p)
		p <- (2)^(i)
		i <- i + 1 
	}
	
	# So we have these 4 models, and we will give our function a dataset, applying 
	# bayes we will find the best fit...

	# PRIOR - WE CAN SET THE PROBABLITY OF THESE MODELS (SUMMING TO 1 ) DEPENDING UPON THE 
	# KNOWLEDGE AND EXPERINCE WE HAVE

	# FOR SIMPLICITY, WE GICE THEM EQUAL PRIORS
	
	Prior_Prob_Model_1 <- 1/No_models
	Prior_Prob_Model_2 <- 1/No_models
	Prior_Prob_Model_3 <- 1/No_models
	Prior_Prob_Model_4 <- 1/No_models

	# LIKELIHOOD - WE CALCULATE THE LIKELIHOOD OF EACH MODEL I,E PROBABLITY OF
	# OF EACH DATASET GIVEN THE MODEL (for each model we calculate likelihood)

	Likelihood_model_1 <- 1
	Likelihood_model_2 <- 1
	Likelihood_model_3 <- 1
	Likelihood_model_4 <- 1

	i<- 1 
	while(i<=length(dataset))
	{
		if(is.element(dataset[i],Model_1) == TRUE)
		{
			Likelihood_model_1 <- (Likelihood_model_1)*(1/(length(Model_1)))
		}
		else
		{
			Likelihood_model_1 <- 0 
		}
		i <- i + 1
	}
	i<- 1 
 	while(i<=length(dataset))
	{
		if(is.element(dataset[i],Model_2) == TRUE)
		{
			Likelihood_model_2 <- (Likelihood_model_2)*(1/(length(Model_2)))
		}
		else
		{
			Likelihood_model_2 <- 0 
		}
		i <- i + 1
	}
	i<- 1 
 	while(i<=length(dataset))
	{
		if(is.element(dataset[i],Model_3) == TRUE)
		{
			Likelihood_model_3 <- (Likelihood_model_3)*(1/(length(Model_3)))
		}
		else
		{
			Likelihood_model_3 <- 0 
		}
		i <- i + 1
	}
	i<- 1 
 	while(i<=length(dataset))
	{
		if(is.element(dataset[i],Model_4) == TRUE)
		{
			Likelihood_model_4 <- (Likelihood_model_4)*(1/(length(Model_4)))
		}
		else
		{
			Likelihood_model_4 <- 0 
		}
		i <- i + 1
	}

	# POSTERIOR - IT IS DEFINED AS - PRIOR X LIKELIHOOD

	Posterior_model_1 <- Prior_Prob_Model_1*Likelihood_model_1
	Posterior_model_2 <- Prior_Prob_Model_2*Likelihood_model_2
	Posterior_model_3 <- Prior_Prob_Model_3*Likelihood_model_3
	Posterior_model_4 <- Prior_Prob_Model_4*Likelihood_model_4
 
	# Normalizing the posterior
	prob_Posterior_model_1 <- Posterior_model_1/(Posterior_model_1 + Posterior_model_2 + Posterior_model_3 + Posterior_model_4)
	prob_Posterior_model_2 <- Posterior_model_2/(Posterior_model_1 + Posterior_model_2 + Posterior_model_3 + Posterior_model_4)
	prob_Posterior_model_3 <- Posterior_model_3/(Posterior_model_1 + Posterior_model_2 + Posterior_model_3 + Posterior_model_4)
	prob_Posterior_model_4 <- Posterior_model_4/(Posterior_model_1 + Posterior_model_2 + Posterior_model_3 + Posterior_model_4)

	print(paste("all even ",prob_Posterior_model_1))
	print(paste("all natural ",prob_Posterior_model_2))
	print(paste("all power 4 ",prob_Posterior_model_3))
	print(paste("all power 2 ",prob_Posterior_model_4))
}
# CONCLUSION : 
# 1 - THE BAYES GIVES THE NEARBY HYPOTHESIS, IF NOT THE EXACT
# 2 - WHEN THE DATA SIZE INCREASES - IF THE REAL MODEL EXIST - BAYES CONVERGES TO MAX-LIKELIHOOD-ESTIMATION(frequentist analysis)
