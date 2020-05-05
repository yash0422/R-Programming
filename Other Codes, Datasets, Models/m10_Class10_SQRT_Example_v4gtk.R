##Source : http://gekkoquant.com/2012/05/26/neural-networks-with-r-simple-example/
## un-comment below install statement if 'neuralnet' package is not installed on your machine

#install.packages('neuralnet')
library("neuralnet")

#Going to create a neural network to perform sqare rooting
#Type ?neuralnet for more information on the neuralnet library

set.seed(1010)
#Generate 50 random numbers uniformly distributed between 0 and 100
#And store them as a dataframe
traininginput <-  as.data.frame(runif(50, min=0, max=100))
View(traininginput)
trainingoutput <- sqrt(traininginput)

#Column bind the data into one variable
trainingdata <- cbind(traininginput,trainingoutput)
colnames(trainingdata) <- c("Input","Output")
View(trainingdata)


#Train the neural network
#formula 	a symbolic description of the model to be fitted.
#data 	a data frame containing the variables specified in formula.
#hidden 	a vector of integers specifying the number of hidden neurons (vertices) in each layer.
#threshold 	is a numeric value specifying the threshold for the partial 
## derivatives of the error function as stopping criteria.
?neuralnet
set.seed(1010)
net.sqrt_model <- neuralnet( formula = Output~Input, 
                       data = trainingdata, hidden=c(10), 
                       threshold=0.01,
                       lifesign = "full",
                       lifesign.step = 10)

print(net.sqrt_model)

# trainingdata$PredictedOutPut = net.sqrt_model$response
# View(trainingdata)
#Plot the neural network
plot(net.sqrt_model)


#To test the neural network on some training data let us generate some squared numbers
testdata <- as.data.frame((1:10)^2) 
testdata
#Run them through the neural network
##compute function is used to compute the output for a given input using the neuralnet model object
?compute
net.results <- compute(net.sqrt_model, testdata) 

#Lets see what properties net.sqrt has and print the results
ls(net.results)


#Lets display a better version of the results
cleanoutput <- cbind ( testdata,  
                       sqrt(testdata),  
                       as.data.frame(net.results$net.result),
                       as.data.frame( round(net.results$net.result,digits = 0)) )
colnames(cleanoutput) <- c("Input", "Expected Output", "Neural Net Output","Rounded" )
View(cleanoutput)



  