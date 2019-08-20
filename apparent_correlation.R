
library(tidyverse)
library(scales)



createVecs <- function(x){
vec1 <- rnorm(n = 20, mean = 0, sd = 1)
vec2 <- rnorm(n = 20, mean = 0, sd = 1)
output <- cor(vec1, vec2)
output <- abs(output)
return(output)
}

n <- 300

output <- map_chr(seq_len(n), createVecs) %>% as.numeric()


mydata <- tibble(trials = seq(1,n,1),
                 correlation_measures = output)


ggplot(mydata) +
  geom_line(aes(trials, correlation_measures)) +
  scale_y_continuous(breaks = seq(0,1,0.1), labels = scales::percent_format(accuracy = 1)) +
  theme_minimal() +
  ggtitle("Apparent Correlation in Two Orthogonal Vectors")
