#' Accept the result of scale and perform the inverse transformation
#'
#' @param data_scaled the result of scale
#' @return the result of unscale
#' @export
#' @examples
#' unscale(scale(1:5))


## unscale 接受 scale 的结果，并进行反变换
unscale <- function (data_scaled) 
{
  center <- attr(data_scaled, "scaled:center")
  scales <- attr(data_scaled, "scaled:scale")
  new_data <- scale(data_scaled, center = (-center/scales), scale = 1/scales)
  attr(new_data, "scaled:center") <- NULL
  attr(new_data, "scaled:scale") <- NULL
  return(new_data)
}

#' Accept a numeric vector as a parameter and output the index of the maximum value
#'
#' @param data Input data
#' @return the index of the maximum value
#' @export
#' @examples
#' which_max(1:100)

## 接受一个数值向量作为参数，输出最大值的下标。 若有多个值并列为最大值， 将其下标全部输出
which_max <- function(data)
{
  which(data==max(data),arr.ind=TRUE)
}

#' Convert the percentage system to a five-point system
#'
#' @param data Input data
#' @return five_greade_scores
#' @export
#' @examples
#' as_five_greade_scores(c(59,65,75,86,99))

## 将百分制成绩转换成五分制
as_five_greade_scores<-function(data)
{
cut(data, 
    breaks = c(0, 60, 70, 80, 90, 100),
    labels = c("不及格", "及格", "中", "良", "优"),
    include.lowest = TRUE)
}
