#' K-Fold Test
#' Runs a k-fold test of a given model, or a model trained based on the data
#'
#' @param x the dataset to be used for test and training
#' @param y the response variable, a vector.
#' @param k the number of fold the dataset will be broken to, with a default value
#' of 10.
#' @param model an object representing a trained model, or a function to be used for
#' training the data.
#' @param predictor a function to predict the response for the test data based on the
#' trained model. The default function is `predict`.
#' @param modelparams is a list of values to be passed into `model`, if it is a function.
#' @param summarize if TRUE, the default value, the mean and standard deviation of
#' the performance measures are returned; otherwise all performance measured are returned.
#' @param ... the list of parameters to be passed to `predictor` function.
#' @return A data.frame including the fold number and performance measures.
#' @export
kfold_test <- function(x, y, k=10, model, predictor=predict, modelparams=NULL, summarize=TRUE, ...){
  # finding train and test indexes for k folds
  TrTsIdx <- split(data.frame(i=sample(dim(x)[1])), (1:dim(x)[1]) %% k)
  results <- if(summarize){
    data.frame(fold=1:k, mcr=NA, mse=NA, time.user=NA, time.sys=NA, time.elapsed=NA)
  } else {
    data.frame(fold=rep(1:k, each=sapply(TrTsIdx, nrow)), y=NA, yhat=NA,
               time.user=NA, time.sys=NA, time.elapsed=NA)
  }
  # Result index

  r <- 1:length(TrTsIdx[[1]][[1]])
  for(i in 1:k){
    # Train if needed
    trModel <- if (class(model)[1]=='function'){
      do.call('model', args = c(x=x[-TrTsIdx[[i]][[1]],], modelparams))
    } else{
      model
    }
    # Test
    timestart = proc.time()
    #browser()
    yhat <- predictor(object=trModel, newdata = x[TrTsIdx[[i]][[1]],], ...)
    timediff = proc.time() - timestart
    # Performance Measures
    if(summarize){
      results[i, ] <- data.frame(fold=i, mcr=1-mean(y[TrTsIdx[[i]][[1]]]==yhat),
                                 mse=mean((y[TrTsIdx[[i]][[1]]]-yhat)^2),
                                 time.user=timediff['user.self'],
                                 time.sys=timediff['sys.self'],
                                 time.elapsed=timediff['elapsed'])
    } else{
      results[r, ] <- data.frame(fold=i, y=y[TrTsIdx[[i]][[1]]], yhat=yhat,
                                 time.user=timediff['user.self'],
                                 time.sys=timediff['sys.self'],
                                 time.elapsed=timediff['elapsed'],
                                 row.names = NULL)
      if(i<k)
        r <- tail(r,1)+(1:length(TrTsIdx[[i+1]][[1]]))
    }
  }
  results
}
