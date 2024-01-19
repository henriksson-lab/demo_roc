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

  #observe({
  #})
  
    
  ##############################################################################
  ########### Distribution functions ###########################################
  ##############################################################################
  
  
  
  
  getSamples <- reactive({
    set.seed(666)
    num_samples <- input$num_samples
    
    ################ Generate random points and classify them
    
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
    
    dat$class <- factor(dat$class, levels=c("a","b"))
    dat$predicted_class <- factor(dat$predicted_class, levels=c("a","b"))
    
    ################ Add classification info
    
    dat$TN <- NA
    dat$TP <- NA
    dat$FP <- NA
    dat$FN <- NA

    #to begin with, starting from decision boundaries at the left, all samples are categorized as b. so none as a:
    CM <- matrix(nrow=2,ncol=2)
    colnames(CM) <- c("a","b")
    colnames(CM) <- c("a","b")
    CM[1,1] <- 0
    CM[2,1] <- 0
    CM[1,2] <- sum(dat$class=="a")
    CM[2,2] <- sum(dat$class=="b")  # column 2 means classified as b. so [1,2] is a classified as b
    
    #Now moving from left to right
    #    print(CM)        #should also add this as output!!
    for(i in 1:nrow(dat)){
      if(dat$class[i]=="a"){
        #now one missclassified a is done right
        CM[1,2] <- CM[1,2] - 1
        CM[1,1] <- CM[1,1] + 1
      } else {
        #now one correctly classified b is wrong
        CM[2,2] <- CM[2,2] - 1
        CM[2,1] <- CM[2,1] + 1
      }
      #      print(CM)     
      
      dat$TN[i] <- CM[1,1]
      dat$TP[i] <- CM[2,2]
      dat$FP[i] <- CM[1,2]
      dat$FN[i] <- CM[2,1]

    }

    # b is positive
    # a is negative
    # false positive: a classified as b  #[,]
    # false negative: b classified as a
    
    dat$sensitivity <- dat$TP/(dat$TP+dat$FN)  
    dat$precision <- dat$TP/(dat$TP+dat$FP)  
    dat$false_positive_rate <- dat$FP/(dat$FP+dat$TN)
    dat$recall <- dat$FP/(dat$FP+dat$TN)
    #f1_score <- 2*((precision*recall_score)/(precision+recall_score))
    dat$accuracy  <- (dat$TP+dat$TN)/(dat$TP+dat$TN+dat$FP+dat$FN)    

    dat$cost <- 
      input$cost_TB*dat$TP +  #should increase with boundary
      input$cost_TA*dat$TN +  #should decrease with boundary
      input$cost_FB*dat$FP +  #should decrease with boundary
      input$cost_FA*dat$FN    #should increase with boundary

    
    dat
  })
  

  
  divNoZero <- function(a,b){
    if(a==0){
      return(0)
    } else {
      return(a/b)
    }
  }
  
  
  ######## Figure out current sensitivity etc
  getCurrentSetting <- reactive({
    dat <- getSamples()
    
    CM <- table(dat$class, dat$predicted_class)
    TN <- CM[1,1]
    TP <- CM[2,2]
    FP <- CM[1,2]
    FN <- CM[2,1]
    cur_setting <- data.frame(sensitivity=NA, false_positive_rate=NA)
    
    cur_setting$sensitivity <- divNoZero(TP,TP+FN)  
    cur_setting$false_positive_rate <- divNoZero(FP,FP+TN)
    cur_setting$precision <- divNoZero(TP,TP+FP)  
    cur_setting$recall <- divNoZero(FP,FP+TN)
    
    cur_setting
  })
  
  
  
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

    ggplot(alldat, aes(x,p, color=class)) + geom_area(data = dat[dat$in_integral,], fill="gray") +
      geom_line(data=alldat, aes(x,p, color=class)) +
      annotate("segment", x = input$class_a_to, xend = input$class_a_to, y = 0, yend = max(alldat$p), colour = "blue")+
      ylab("Probability density")+
      xlim(c(minx,maxx))
    
    #p2 <-  ggplot(dat, aes(x,cost)) + geom_point()+
    #  xlim(c(minx, maxx))
    
    
    
  })
  
  
  
  output$plotCost <- renderPlot({
    dat <- getSamples()
    
    #add samples to show continuation of cost
    first_samp <- dat[1,,drop=FALSE]
    last_samp <- dat[nrow(dat),,drop=FALSE]
    first_samp$x <- minx
    last_samp$x  <- maxx
    
    ggplot(rbind(first_samp,dat,last_samp), aes(x,cost)) + geom_line()+
      xlim(c(minx, maxx))+
      ylim(c(min(dat$cost,0),max(dat$cost,0)))

  })

  
  
  ##############################################################################
  ########### Plot samples #####################################################
  ##############################################################################
  
  output$plotSamples <- renderPlot({
    dat <- getSamples()
    
    ggplot(dat, aes(x,y,color=class)) + geom_point()+
      ylim(c(-1,1))+
      xlim(c(minx, maxx))
  })
  
  
  
  ##############################################################################
  ########### Plot ROC #########################################################
  ##############################################################################
  

  
  output$plotROC <- renderPlot({
    dat <- getSamples()
    
    
#    CM <- table(dat$class, dat$predicted_class)
#    print(CM)
#    err_metric(CM)

    #dat$class <- as.numeric(dat$class)
    #dat$predicted_class <- as.numeric(dat$predicted_class)

    ref <- data.frame(false_positive_rate=c(0,1), sensitivity=c(0,1))
    cur_setting <- getCurrentSetting()
    
    ggplot(dat, aes(false_positive_rate, sensitivity)) + geom_line() + 
      geom_line(data=ref, color="red") + 
      geom_point(data=cur_setting, color="red", size=5)
  })
  
  
  
  output$plotPR <- renderPlot({
    dat <- getSamples()
    
    ref <- data.frame(recall=c(1,0), precision=c(0,1), false_positive_rate=c(0,1), sensitivity=c(0,1))
    cur_setting <- getCurrentSetting()

    ggplot(dat, aes(recall, precision)) + geom_line() + 
      geom_line(data=ref, color="red") + 
      geom_point(data=cur_setting, color="red", size=5)
  })
  
  
  output$textAUC <- renderText({
    
    dat <- getSamples()
    
    dat <- dat[,c("false_positive_rate","sensitivity")]
    dat <- dat[order(dat$false_positive_rate, decreasing = FALSE),]

    auc <- pracma::trapz(dat$false_positive_rate, dat$sensitivity)

    HTML(paste0("<b>","AUC: ",round(auc,digits = 3),"</b>"))
  })
  
  
}





