set.seed(42)
#library(rattle)
library(rpart)


apply.pred_amt <- function(df,v_target) {
	
		data_set <- df
		attach(data_set)
		
		target <- v_target 
		nobs <- nrow(data_set)
		form <- formula(paste(target, "~ ."))
		
		#Dataset Division
		form_column <- eval(parse(text=paste("data_set",target,sep="$")))
        new_data_set <- subset(data_set, is.na(form_column))
		nobs <- nrow(data_set) # 20000 observations 
		train <- sample(nobs, 0.6*nobs) # 14000 observations
		trainset <- data_set[train, ]
		validate <- sample(setdiff(seq_len(nobs), train), 0.15*nobs) # 3000 observations
		validateset <- data_set[validate, ]
		test <- setdiff(setdiff(seq_len(nobs), train), validate) # 3000 observations
		testset <- data_set[test,]
		
		motorVars <- setdiff(colnames(data_set),list(target))#,ignore))
		dsFormula <- as.formula(paste(target,paste(motorVars,collapse=' + '),sep=" ~ "))
		model <- rpart(dsFormula, data=trainset)
		model
		model$variable.importance
		summary(model)
		cp <- printcp(model)  #Complexity Parameter
		predicted <- predict(model, newdata=testset)
		# Extract the relevant variables from the dataset.
        pred_loc <- ncol(data_set)
        sdata <- subset(data_set[test,]) 
		
		variance <-  round((predicted - sdata[,pred_loc])/sdata[,pred_loc] * 100,2)
		variance_value <- (predicted - sdata[,pred_loc])
		res <- cbind(sdata, predicted,variance_value,variance)
        # New data prediction
		paste(target, "_predicted") <- predict(model, newdata=new_data_set)
		#names(predicted_new)[1]<-paste(target, "_predicted")
		res_new <- cbind(paste(target, "_predicted"),new_data_set)
		res_new

		
  }