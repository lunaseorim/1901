

## Long to Wide

library(reshape2)
library(dplyr)

long <- data.frame(id = rep(paste0("ID",1:3),each = 4),
                    week = paste0("week",rep(1:4,3)),
                    value = rnorm(2))

L2W <- long %>%
  dcast(.,id~week, value.var = "value")


## Wide to Long

wide <- data.frame(id = paste0("ID",1:5),
                   access = rep(c("allow","deny"),c(2,3)),
                   point1 = rnorm(5),
                   point2 = rnorm(5),
                   point3 = rnorm(5))

W2L <- wide %>%
  melt(.,id.vars = c("id","access")) %>%
  `colnames<-`(c("id","access","point","value"))
