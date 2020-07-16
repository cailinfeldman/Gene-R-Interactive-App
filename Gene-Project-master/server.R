library(tidyverse)

function(input, output) {
  output$summary <- renderText({
  
    input$goButton
    
    new_value <- 20 + input$n
    reduced_GSE120396 = GSE120396[20:new_value,]
    largevar = apply(reduced_GSE120396, 1, var)
    ind = which(largevar > quantile(largevar, 0.9))
    
    ran <- sample(1:nrow(reduced_GSE120396), 0.9 * nrow(reduced_GSE120396)) 
    
    X = as.matrix(t(reduced_GSE120396[ind,]))
    reduced_y = RNA_seq_GSE120396_rejection_status
    
    ran <- sample(1:nrow(X), 0.9 * nrow(X)) 
    
    cvK = 5  # number of CV folds
    cv_50acc5_knn = cv_50acc5_svm = cv_50acc5_rf = c()
    cv_acc_knn = cv_acc_svm = cv_acc_rf = c()
    
    n_sim = 25 ## number of repeats
    for (i in 1:n_sim){
      
    cvSets = cvTools::cvFolds(nrow(X), cvK)  # permute all the data, into 5 folds
    cv_acc_knn = cv_acc_svm = cv_acc_rf = c()
      
    for (j in 1:cvK){
      test_id = cvSets$subsets[cvSets$which == j]
      X_test = X[test_id, ]
      X_train = X[-test_id, ]
      y_test = y[test_id]
      y_train = y[-test_id]
        
      #print(X_train)
      #print(y_train)
        
      ## SVM
      svm_res <- e1071::svm(x = X_train, y = as.factor(y_train))
      fit <- predict(svm_res, X_test)
      cv_acc_svm[j] = table(fit, y_test) %>% diag %>% sum %>% `/`(length(y_test))
      }
      cv_50acc5_svm <- append(cv_50acc5_svm, mean(cv_acc_svm))
    }
    summary(cv_50acc5_svm)
  })
  
  output$plot1 <- renderPlot({
    new_value <- 20 + input$n
    reduced_GSE120396 = GSE120396[20:new_value,]
    largevar = apply(reduced_GSE120396, 1, var)
    ind = which(largevar > quantile(largevar, 0.9))
    
    ran <- sample(1:nrow(reduced_GSE120396), 0.9 * nrow(reduced_GSE120396)) 
    
    X = as.matrix(t(reduced_GSE120396[ind,]))
    reduced_y = RNA_seq_GSE120396_rejection_status
    
    ran <- sample(1:nrow(X), 0.9 * nrow(X)) 
    
    cvK = 5  # number of CV folds
    cv_50acc5_knn = cv_50acc5_svm = cv_50acc5_rf = c()
    cv_acc_knn = cv_acc_svm = cv_acc_rf = c()
    
    n_sim = 25 ## number of repeats
    for (i in 1:n_sim){
      
      cvSets = cvTools::cvFolds(nrow(X), cvK)  # permute all the data, into 5 folds
      cv_acc_knn = cv_acc_svm = cv_acc_rf = c()
      
      for (j in 1:cvK){
        test_id = cvSets$subsets[cvSets$which == j]
        X_test = X[test_id, ]
        X_train = X[-test_id, ]
        y_test = y[test_id]
        y_train = y[-test_id]
        
        #print(X_train)
        #print(y_train)
        
        ## SVM
        svm_res <- e1071::svm(x = X_train, y = as.factor(y_train))
        fit <- predict(svm_res, X_test)
        cv_acc_svm[j] = table(fit, y_test) %>% diag %>% sum %>% `/`(length(y_test))
      }
      cv_50acc5_svm <- append(cv_50acc5_svm, mean(cv_acc_svm))
    }
    boxplot(cv_50acc5_svm)
  })
  
  output$summary2 <- renderText({
    input$goButton
    new_value <- 21 + input$n
    t_GSE120396[1,][21:new_value]
    })
  }
  
