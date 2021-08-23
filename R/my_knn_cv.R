#' k-Nearest Neighbors Cross-Validation Function
#'
#' This function perform Cross-Validation k-Nearest Neighbors Cross-Validation.
#'
#' @param train The training input data frame.
#' @param cl True class value of \code{train}.
#' @param k_nn Integer representing the number of neighbors.
#' @param k_cv Integer representing the number of folds.
#' @keywords prediction
#'
#' @return A list of predicted \code{class} and a numeric with the
#'   cross-validation misclassification error \code{cv-err}.
#'
#' @examples
#' train <- na.omit(my_penguins) %>%
#'   dplyr::select(body_mass_g, bill_length_mm,
#'                 bill_depth_mm,flipper_length_mm)
#' cl <- na.omit(my_penguins) %>% dplyr::select(species)
#' my_knn_cv(train, cl, 5, 10)
#'
#' @export

my_knn_cv <- function(train, cl, k_nn, k_cv) {
  # split data into k_cv folds, randomly with equal probability
  split <- sample(rep(1:k_cv, length = nrow(train)))
  data <- data.frame("train" = train, "cl" = cl, "split" = split)

  mis <- c()

  #Iterate to predict
  for (i in 1:k_cv) {
    # create training and testing data
    data_train <- filter(data, split != i)
    data_test <- filter(data, split == i)

    # select train and test
    train_selected <- data_train %>% select(-split, -cl)
    test_selected <- data_test %>% select(-split, -cl)

    cl <- data_train %>% pull(cl)

    # k-nearst-neighbors
    pred_class <- knn(train = train_selected,
                      test = test_selected,
                      cl = cl,
                      k = k_nn)
    # misclassification error
    mis[i] <- mean(ifelse(data_test$cl == pred_class, 0, 1))
  }
  cv_err <- mean(mis)
  result <- list("class" = pred_class, "cv_err" = cv_err)
  return(result)
}
