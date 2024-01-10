library(plotly)
library(Cairo)

options(shiny.usecairo=T)


if(FALSE){
  install.packages("matlib")
}



if(FALSE){
  #To run this app
  library(shiny)
  runApp(".")
}


server <- function(input, output, session) {

  minx <- -20
  maxx <- 20
  

  ##############################################################################
  ########### Update controllers as needed #####################################
  ##############################################################################

  observe({
    #from <- min(input$show_minx,input$show_maxx) 
    #to <- max(input$show_minx,input$show_maxx) 
    #updateSliderInput(session,"int_from", min = from, max=to)
    #updateSliderInput(session,"int_to", min = from, max=to)
  })
  
    
  ##############################################################################
  ########### Distribution functions ###########################################
  ##############################################################################
  
  
  
  getSamples <- function(){
    set.seed(666)
    num_samples <- input$num_samples
    
    dat <- data.frame(
      x_a=rnorm(num_samples, input$normal_mu_a, input$normal_sigma_a),
      x_b=rnorm(num_samples, input$normal_mu_b, input$normal_sigma_b),
      class=sample(c("a","b"),num_samples,replace=TRUE, prob=c(input$occurence_a,input$occurence_b)/(input$occurence_a+input$occurence_b)),
      y=runif(num_samples, min=-0.2, max=0.2)
    )
    dat$x <- dat$x_b
    dat$x[dat$class=="a"] <- dat$x_a[dat$class=="a"]

    dat$predicted_class <- "b"
    dat$predicted_class[dat$x < input$class_a_to] <- "a"
    
    dat <- dat[order(dat$x),]
    
    print(dat)
    dat
    
    
    
  }
  
  
  
  
  # modified from https://www.digitalocean.com/community/tutorials/plot-roc-curve-r-programming
  err_metric <- function(CM){
    TN <- CM[1,1]
    TP <- CM[2,2]
    FP <- CM[1,2]
    FN <- CM[2,1]
    precision <- (TP)/(TP+FP)
    recall_score <- (FP)/(FP+TN)
    f1_score <- 2*((precision*recall_score)/(precision+recall_score))
    accuracy_model  <- (TP+TN)/(TP+TN+FP+FN)
    False_positive_rate <- (FP)/(FP+TN)
    False_negative_rate <- (FN)/(FN+TP)
    
    print(paste("Precision value of the model: ",round(precision,2)))
    print(paste("Accuracy of the model: ",round(accuracy_model,2)))
    print(paste("Recall value of the model: ",round(recall_score,2)))
    print(paste("False Positive rate of the model: ",round(False_positive_rate,2)))
    print(paste("False Negative rate of the model: ",round(False_negative_rate,2)))
    print(paste("f1 score of the model: ",round(f1_score,2)))
  }
  
  # roc x: false positive rate = 
  # roc y: true positive rate = sensitivity
  
  getRoc <- function(){
    
    s <- getSamples()#[1:5,]
    
    CM <- matrix(nrow=2,ncol=2)
    colnames(CM) <- c("a","b")
    colnames(CM) <- c("a","b")
    
    #to begin with, starting from decision boundaries at the left, all samples are categorized as b. so none as a:
    CM[1,1] <- 0
    CM[2,1] <- 0
    CM[1,2] <- sum(s$class=="a")
    CM[2,2] <- sum(s$class=="b")  # column 2 means classified as b. so [1,2] is a classified as b
    
    s$precision <- NA
    s$sensitivity <- NA
    s$false_positive_rate <- NA
    
    #Now moving from left to right
#    print(CM)        #should also add this as output!!
    for(i in 1:nrow(s)){
      if(s$class[i]=="a"){
        #now one missclassified a is done right
        CM[1,2] <- CM[1,2] - 1
        CM[1,1] <- CM[1,1] + 1
      } else {
        #now one correctly classified b is wrong
        CM[2,2] <- CM[2,2] - 1
        CM[2,1] <- CM[2,1] + 1
      }
#      print(CM)     
      
      TN <- CM[1,1]
      TP <- CM[2,2]
      FP <- CM[1,2]
      FN <- CM[2,1]
      
      # b is positive
      # a is negative
      # false positive: a classified as b  #[,]
      # false negative: b classified as a
      
#      FN <- CM[1,2]
 #     FP <- CM[2,1]
      
      s$sensitivity[i] <- (TP)/(TP+FN)  
      if(TP==0){
        s$sensitivity[i] <- 0
      }
      
      s$precision[i] <- (TP)/(TP+FP)  
      s$false_positive_rate[i] <- (FP)/(FP+TN)
      
      
      #recall_score <- (FP)/(FP+TN)
      #f1_score <- 2*((precision*recall_score)/(precision+recall_score))
      #accuracy_model  <- (TP+TN)/(TP+TN+FP+FN)
      #False_negative_rate <- (FN)/(FN+TP)
    }
#    print(s[,c("false_positive_rate", "sensitivity")])
    
    ref <- data.frame(false_positive_rate=c(0,1), sensitivity=c(0,1))
  
    
    ######## Figure out current setting
    s$class <- factor(s$class, levels=c("a","b"))
    s$predicted_class <- factor(s$predicted_class, levels=c("a","b"))
    CM <- table(s$class, s$predicted_class)
    TN <- CM[1,1]
    TP <- CM[2,2]
    FP <- CM[1,2]
    FN <- CM[2,1]
    cur_setting <- data.frame(sensitivity=NA, false_positive_rate=NA)
    cur_setting$sensitivity <- (TP)/(TP+FN)  
    if(TP==0){
      cur_setting$sensitivity <- 0
    }
    cur_setting$false_positive_rate <- (FP)/(FP+TN)

    
    ggplot(s, aes(false_positive_rate, sensitivity)) + geom_line() + 
      geom_line(data=ref, color="red") + 
      geom_point(data=cur_setting, color="red", size=5)
  }
  if(FALSE){
    getRoc()
  }
    

  
  ##############################################################################
  ########### Plot distributions ###############################################
  ##############################################################################
  
  
  output$plotPDF <- renderPlot({
    
    
    dat <- data.frame(
      x=seq(from=minx, to=maxx, length.out=5000),
      class="a"
    )
    dat$p <- dnorm(dat$x, input$normal_mu_a, input$normal_sigma_a)*input$occurence_a
    dat$in_integral <- dat$x < input$class_a_to
    
    dat_b <- data.frame(
      x=seq(from=minx, to=maxx, length.out=5000),
      class="b"
    )
    dat_b$p <- dnorm(dat_b$x, input$normal_mu_b, input$normal_sigma_b)*input$occurence_b
    dat_b$in_integral <- FALSE
    
    alldat <- rbind(dat,dat_b)    

    ggplot(alldat, aes(x,p, color=class)) + geom_line() +
#      geom_area(data = dat[dat$in_integral,], fill="red") +
      annotate("segment", x = input$class_a_to, xend = input$class_a_to, y = 0, yend = max(alldat$p), colour = "blue")+
    
      ylab("Probability density")+
      xlim(c(minx,maxx))
    
    
  })
  
  
  #output$integral_value <- renderText({
  #  p <- getDistCDF()(input$int_to) - getDistCDF()(input$int_from)
  #  paste("Integral of area, the probability:",p)
  #})
  
  
  
  ##############################################################################
  ########### Plot samples #####################################################
  ##############################################################################
  
  output$plotSamples <- renderPlot({
    dat <- getSamples()
    
    print(dat)
    
    ggplot(dat, aes(x,y,color=class)) + geom_point()+
      ylim(c(-1,1))+
      xlim(c(minx, maxx))
  })
  
  
  
  ##############################################################################
  ########### Plot ROC #########################################################
  ##############################################################################
  

  
  output$plotROC <- renderPlot({
    dat <- getSamples()
    
    
    dat$class <- factor(dat$class, levels=c("a","b"))
    dat$predicted_class <- factor(dat$predicted_class, levels=c("a","b"))
    CM <- table(dat$class, dat$predicted_class)
    
    
#    print(CM)
#    err_metric(CM)
    
    
    
    dat$class <- as.numeric(dat$class)
    dat$predicted_class <- as.numeric(dat$predicted_class)
    
    #ROC-curve using pROC library
    #library(pROC)
   # roc_score <- roc(dat$class, dat$predicted_class) #AUC score
  #  plot(roc_score)# ,main ="ROC curve -- Logistic Regression ")
    
    
    getRoc()
  })
  
}





