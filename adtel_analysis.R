

adtel <- read.csv('AdTelCase.csv')

View(adtel)

### Subset data for the Testing Period 
data <- adtel %>% filter(Period>6)

### Subset data for the Pre-Testing Period 
data_pretest <- adtel %>% filter(Period<7)

## Create new column for difference of sales for each month 
adtel$d <- adtel$VolA - adtel$VolB


### TO find the difference in means for testing period, whether there is an actual effect due to advertising 
t.test(data$VolA,data$VolB,alternative="two.sided",paired=TRUE,conf.level = 0.90)

## Pretest difference between means is zero...this shows that for first 6 months, there was no difference between groups A and B   
t.test(data_pretest$VolA,data_pretest$VolB,alternative="two.sided",paired=TRUE,conf.level = 0.90)


### Mean sales of COntrol grp
adtel %>% filter(Period>6) %>% summarise(mean(VolB))  ## 32.58 


## testing period average diff in volumes 
mean(data$d)
sd(data$d)

### To prove if the expectations of 15% increase in sales were met or not 
t.test(data$VolA,data$VolB,alternative="two.sided",paired=TRUE,conf.level = 0.90)  ## found the interval of u1 - u2 

p1 <- 1.82/32.58 *100
p2 <- 12.17/32.58 *100
p1
p2

## Therefore, there is an increase by 5.6% to 37.4% in sales due to advertising 
## Although, 15% lies in the interval, that's not what management wants. The lower end should have been 15%. Min of 15% increase
## in sales is expected. So, the strategy doesn't meet management goals. 


## This is the way to evaluate whether the experiment has been successful or not. 
