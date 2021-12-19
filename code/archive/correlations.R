library(reshape2)
library(corrplot)
library(dplyr)
library(ggcorrplot)
library(ggplot2)
library(purrr)

theft_train = read.csv('../data/clean/theft_train.csv')

theft_train_cor = theft_train %>% select(-state, -county, -fips)

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

res<-rcorr(as.matrix(theft_train_cor))

cormat <- round(cor(theft_train_cor),3)


cors <- function(df) {
  M <- Hmisc::rcorr(as.matrix(df))
  # turn all three matrices (r, n, and P into a data frame)
  Mdf <- map(M, ~data.frame(.x))
  # return the three data frames in a list
  return(Mdf)
}

cors(theft_train_cor) %>%
  map(~rownames_to_column(.x, var="measure1")) %>%
  # look at the first element of the list (r)
  first() %>%
  head()

cors(theft_train_cor) %>%
  map(~rownames_to_column(.x, var="measure1")) %>%
  # format each data set (r,P,n) long
  map(~pivot_longer(.x, -measure1, "measure2")) %>%
  # look at the first element of the list (r)
  first() %>%
  head()


ggcorrplot(cormat, method = "circle", type = "lower", tl.cex = 10)

formatted_cors(mtcars) %>%
  ggplot(aes(measure1, measure2, col=r)) + ## to get the rect filled
  geom_tile(col="black", fill="white") +
  geom_point(aes(size = abs(r)), shape=15) +
  labs(x = NULL, y = NULL, col = "Pearson's\nCorrelation", title="Correlations in Mtcars") +
  theme_classic()+
  scale_color_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(-1,1))  +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  theme(text=element_text(family="Roboto")) +
  scale_size(range=c(1,11), guide=NULL) 