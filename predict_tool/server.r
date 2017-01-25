# Sibanjan Das
library(cluster)
library(fpc)
library(rpart)

shinyServer(function(input, output) {


  
  # Function that imports the data file
  dInput = reactive({
    in.file = input$file1
 
    if (is.null(in.file))
      return(NULL)

	 data <- read.csv(in.file$datapath, na.strings=c(".", "NA", "", "?"))
	  
  })
 
  # Function that render the data file and passes it to ui.R
  output$view = renderTable({
    d.input = dInput()
    if (is.null(d.input)) return(NULL)
    if (ncol(d.input>10)) d.input = d.input[] #$,1:10]
     head(dInput(), n=50)  
  })
  
    # Check boxes
  output$choose_columns <- renderUI({
    # If missing input, return to avoid error later in function
	d.input = dInput()
    if(is.null(d.input))
      return()
 
    # Get the data set with the appropriate name
    #dat <- get(input$file1)
    colnames <- names(d.input)
 
    # Create the checkboxes and select them all by default
    checkboxGroupInput("columns", "Choose columns", 
                        choices  = colnames,
                        selected = colnames)
  })
  
      # Target variable selection
  output$choose_target <- renderUI({
    # If missing input, return to avoid error later in function
	d.input = dInput()
    if(is.null(d.input))
      return()
 
    # Get the data set with the appropriate name

    colnames <- names(d.input)
 
    # Create the checkboxes and select them all by default
    selectInput("target", "Choose Target Attribute", 
                        choices  = colnames)#,
                       # selected = colnames)
  })
  #k-means cluster
  
   selectedData <- reactive({
      d.input = dInput()
      dat_cluster <- d.input
	  dat_cluster <- dat_cluster[, input$columns, drop = FALSE]
	  for (x in 1:length(dat_cluster))      # Transform the variables
      { 
         if(!is.numeric(dat_cluster[,x])) {
            column <- paste(colnames(dat_cluster)[x],"transformed",sep="_")
            temp_data <- as.numeric(dat_cluster[,x])
            temp_mat <- matrix(c(temp_data),ncol=1,byrow=TRUE)
            colnames(temp_mat) <- column
            data_clust_trans <- cbind(dat_cluster,temp_mat)
         }
      
      }
	  nums <- sapply(dat_cluster, is.numeric)  
	  dat_cluster_numeric <- dat_cluster[,nums]  #Considered only numeric values for K-Means
	  dat_cluster_numeric[is.na(dat_cluster_numeric)]=FALSE

	  dat_cluster_numeric

  })	
  clusters <- reactive({

      kmeans(selectedData(), input$clusters)
    })
	  
  output$kmeans_plot <- renderPlot({

	 clusplot(selectedData(), clusters()$cluster, color=clusters()$cluster, labels=clusters()$cluster, cex=1.0, shade=TRUE,lines=0)
   })
   
     output$clust_table <- renderTable({
         clust_table <- cbind(selectedData(), by=list(cluster=clusters()$cluster))
		 clust_table
   })
   
        output$agg_table <- renderTable({
         agg_table <-  aggregate(selectedData(), by=list(cluster=clusters()$cluster),mean)
		 agg_table
   })
 
  # Decision Tree #Function that calculates the output sent to the main panel in ui.R
  output$prediction = renderTable({
   
   d.input = dInput()
       # If missing input, return to avoid error later in function
    if(is.null(d.input))
      return()
 
    # Get the data set
    dat <- d.input

   target <- input$target
  # print("target", target)
    dat <- dat[, input$columns, drop = FALSE]
	apply.pred_amt(dat,target)

  })

  
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
		predicted_new <- predict(model, newdata=new_data_set)
		res_new <- cbind(predicted_new,new_data_set)
		names(res_new)[1]<-paste(target, "_predicted")
		res_new

		
  }

 
})

