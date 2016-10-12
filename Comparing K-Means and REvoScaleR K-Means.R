setwd("C:/DataTK")

library(RODBC)
library(RevoScaleR)
library(ggplot2)
library(dplyr)

myconn <-odbcDriverConnect("driver={SQL Server};Server=SICN-KASTRUN;database=WideWorldImportersDW;trusted_connection=true")

cust.data <- sqlQuery(myconn, "SELECT 
                      fs.[Sale Key] AS SalesID
                      ,fs.[City Key] AS CityKey
                      ,c.[City] AS City
                      ,c.[State Province] AS StateProvince
                      ,c.[Sales Territory] AS SalesTerritory
                      ,fs.[Customer Key] AS CustomerKey
                      ,fs.[Stock Item Key] AS StockItem
                      ,fs.[Quantity] AS Quantity
                      ,fs.[Total Including Tax] AS Total
                      ,fs.[Profit] AS Profit
                      
                      
                      FROM [Fact].[Sale] AS  fs
                      JOIN dimension.city AS c
                      ON c.[City Key] = fs.[City Key]
                      WHERE
                      fs.[customer key] <> 0 ")

close(myconn) 


set.seed(10445)

iteration = 50

result <- data.frame(i=integer(), et=integer(), method=character(), cl_fit=integer())

for (i in 1:iteration) {
      startTimeKmeans <- proc.time()
      SalesCluster <- kmeans(cust.data[,c(1,2,6,7,8)], 9, nstart = 20, algorithm="Lloyd")
      Elapsed_time <- proc.time() - startTimeKmeans
      et <- Elapsed_time[3]
      cl_met <- 'kmeans'
      fit <- SalesCluster$betweenss/SalesCluster$totss
      print(paste('Run: ',i,' elapsed time is: ', et))
      result <- rbind(result, data.frame(i,et,cl_met,fit))
}


for (i in 1:iteration)
{

    startTimeKXmeans <- proc.time()
    rxSalesCluster <- rxKmeans(formula= ~SalesID + CityKey + CustomerKey + StockItem + Quantity, data =cust.data, numCluster=9,algorithm = "lloyd", 
                              outFile = "SalesCluster.xdf", outColName = "Cluster", overwrite = TRUE)
    Elapsed_time <- proc.time() - startTimeKXmeans
    et <- Elapsed_time[3]
    cl_met <- 'rxKmeans'
    fit <- rxSalesCluster$betweenss/rxSalesCluster$totss
    print(paste('Run: ',i,' elapsed time is: ', et))
    result <- rbind(result, data.frame(i,et,cl_met,fit))
}

help(rxKmeans)

ggplot(data=result, aes(x=i, y=et, key=cl_met)) +
  geom_line(aes(color=cl_met), stat="identity") +
  guides(fill=FALSE) +
  xlab("Iterations") + ylab("Estimated run time (s)") +
  ggtitle("Performance kmeans vs. rxKmeans")



##

str(cust.data)

##### Performance analysis of both approaches


result %>%
  group_by(cl_met) %>%
  summarize(
    average_score = mean(et)
    ,variance = var(et)
    ,st_dev = sd(et)
  )


result %>%
  filter(cl_met == "rxKmeans" & et < 1) %>%
  summarize(
    average_score = mean(et)
    ,variance = var(et)
    ,st_dev = sd(et)
    ,nof_iterations = n()
  )


result %>%
  filter(cl_met == "rxKmeans" & et >= 1) %>%
  summarize(
    average_score = mean(et)
    ,variance = var(et)
    ,st_dev = sd(et)
    ,nof_iterations = n()
  )


##### Check quality of clusters

table(SalesCluster$cluster, cust.data$SalesTerritory)
table(rxSalesCluster$cluster, cust.data$SalesTerritory)

# between_SS / total_SS
SalesCluster$betweenss/SalesCluster$totss
rxSalesCluster$betweenss/rxSalesCluster$totss


#Avg difference

mean(result[result$cl_met == 'kmeans',]$fit)*100
mean(result[result$cl_met == 'rxKmeans',]$fit)*100


#kmeans
#[1] 94.52626

#rxKmeans
#[1] 94.48219

#diff
(mean(result[result$cl_met == 'kmeans',]$fit)*100 - mean(result[result$cl_met == 'rxKmeans',]$fit)*100)
# 0.04407133



result %>% 
  group_by(cl_met) %>%
  summarize(
             average_score = mean(fit)
            ,variance = var(fit)
            ,st_dev = sd(fit)
           )



