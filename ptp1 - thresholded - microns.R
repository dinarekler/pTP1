library(readxl)

listofcont <- list()
listofdnRAR <- list()
cont1 <- data.frame(matrix(nrow = 1636)) 
dnRAR1 <- data.frame(matrix(nrow = 1636)) 

stderror <- function(x) sd(x)/sqrt(length(x))

# Iterate over the sheets, each sheet is an embryo
# For each sheet create a dataframe in the list
# For each column, remove NaN and take only the middle 1636 values
# Result: each dataframe has 15 rows (slices) and 1636 columns (points in range)

# BACKGROUND REMOVAL: the value is in the c1noBG/d1noBG. 
#                     Pay attention and modify if necessary

# CONTROL
for (j in c(1:8)) {
  c1 <- read_excel("120 control - thresholded.xlsx", sheet = j, col_types = "numeric")
  c1 <- c1[,c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40)]
  c1noBG <- c1
  cont1 <- data.frame(matrix(nrow = 1636))
  
  for (i in 1:ncol(c1noBG)) {
    x <- unlist(c1noBG[,i])
    x <- na.omit(x)
    diff <- length(x)-1636
    xs <- x[(diff/2):(diff/2+1635)]
    cont1[,i] <- xs
  }
  listofcont[[j]] <- cont1
}

# dnRAR
for (j in c(1:9)) {
  d1 <- read_excel("120 dnRAR - thresholded.xlsx", sheet = j, col_types = "numeric")
  d1 <- d1[,c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40)]
  d1noBG <- d1
  dnRAR1 <- data.frame(matrix(nrow = 1636))
  
  for (i in 1:ncol(d1noBG)) {
    x <- unlist(d1noBG[,i])
    x <- na.omit(x)
    diff <- length(x)-1636
    xs <- x[(diff/2):(diff/2+1635)]
    dnRAR1[,i] <- xs
  }
  listofdnRAR[[j]] <- dnRAR1
}

# Create a summary dataframe to sum means and stderrors of points in range, across slices
# The stderrors are not necessary at this level, I just want to see. 
# The stderrors are more relevant for the difference between embryos. 

cont.summary <- data.frame(matrix(nrow = 1636, ncol = 20))
dnRAR.summary <- data.frame(matrix(nrow = 1636, ncol = 20))

# CONTROL
k <- 1

for (j in c(1:length(listofcont))) {
  
  for (i in c(1:1636)) {
    cont.summary[i,k] <- mean(unlist(listofcont[[j]][i,]))
  }
  
  for (i in c(1:1636)) {
    cont.summary[i,k+1] <- stderror(unlist(listofcont[[j]][i,]))
  }
  
  k <- k+2
  
}

# dnRAR
k <- 1

for (j in c(1:length(listofdnRAR))) {
  
  for (i in c(1:1636)) {
    dnRAR.summary[i,k] <- mean(unlist(listofdnRAR[[j]][i,]))
  }
  
  for (i in c(1:1636)) {
    dnRAR.summary[i,k+1] <- stderror(unlist(listofdnRAR[[j]][i,]))
  }
  
  k <- k+2
  
}



# create a dataframe of the means of all embryos, and their mean and stderror

df_totalsum <- cbind(cont.summary[,c(1,3,5,7,9,11,13,15)],
                     dnRAR.summary[,c(1,3,5,7,9,11,13,15,17)])

row_mean_cont <- apply(df_totalsum[,c(1:8)], 1, mean)
row_mean_dnRAR <- apply(df_totalsum[,c(9:17)], 1, mean)
row_sem_cont <- apply(df_totalsum[,c(1:8)], 1, stderror)
row_sem_dnRAR <- apply(df_totalsum[,c(9:17)], 1, stderror)

df_totalsum <- cbind(df_totalsum, 
                     row_mean_cont,
                     row_sem_cont,
                     row_mean_dnRAR,
                     row_sem_dnRAR)

colnames(df_totalsum) <- c("cont1","cont2","cont3","cont4","cont5","cont6",
                           "cont7","cont8",
                    "dn1","dn2","dn3","dn4","dn5","dn6","dn7","dn8","dn9",
                    "cont_mean","cont_sem","dnRAR_mean","dnRAR_sem")

# Create a plot

library(ggplot2)


ggplot() +
       xlim(0,1636) + ylim(-0.1,17.5)   +

  geom_errorbar(aes(x = c(1:1636), y = df_totalsum[,"dnRAR_mean"],
                    ymin=df_totalsum[,"dnRAR_mean"]-df_totalsum[,"dnRAR_sem"], 
                    ymax=df_totalsum[,"dnRAR_mean"]+df_totalsum[,"dnRAR_sem"]), 
                color = "orange", alpha = 0.2, width=0.5) +  

  geom_errorbar(aes(x = c(1:1636), y = df_totalsum[,"cont_mean"],
                    ymin=df_totalsum[,"cont_mean"]-df_totalsum[,"cont_sem"], 
                    ymax=df_totalsum[,"cont_mean"]+df_totalsum[,"cont_sem"]), 
                color = "blue", alpha = 0.15, width=0.5) +
  
       geom_line(data = df_totalsum, 
                 aes(x = c(1:1636), y = df_totalsum[,"dnRAR_mean"])) +
  
       geom_line(data = df_totalsum, 
                 aes(x = c(1:1636), y = df_totalsum[,"cont_mean"])) +  
  
    theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_line(color = "grey")) +
    xlab("60 um           30 um          RP midline          30 um           60 um") + 
    ylab("Intensity (AU)")+ 
  scale_x_continuous(breaks = seq(0, 1636, by = 409))



# Calculate the mean of the RP 

RP_sum <- df_totalsum[c(409:1227),]

RP_cont_mean <- apply(RP_sum[,c(1:8)], 2, mean)
RP_dnRAR_mean <- apply(RP_sum[,c(9:17)], 2, mean)

RP_cont_mean_all <- mean(RP_cont_mean)
RP_dnRAR_mean_all <- mean(RP_dnRAR_mean)

RP_cont_sem_all <- stderror(RP_cont_mean)
RP_dnRAR_sem_all <- stderror(RP_dnRAR_mean)



# small unnecessary plots for training
# Make a plot

library(ggplot2)

ggplot(data = cont.summary,
       aes(x = c(1:1636), y = cont.summary[,11]))+
  geom_line() + 
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_line(color = "grey")) +
  xlab("Lat - Med - Lat") + ylab("Intensity") +
  geom_errorbar(aes(ymin=cont.summary[,11]-cont.summary[,12], 
                    ymax=cont.summary[,11]+cont.summary[,12]), 
                color = "blue", alpha = 0.1, width=0.2)



library(ggplot2)

ggplot(data = dnRAR.summary,
       aes(x = c(1:1636), y = dnRAR.summary[,17]))+
  geom_line() + 
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_line(color = "grey")) +
  xlab("Lat - Med - Lat") + ylab("Intensity") +
  geom_errorbar(aes(ymin=dnRAR.summary[,17]-dnRAR.summary[,18], 
                    ymax=dnRAR.summary[,17]+dnRAR.summary[,18]), 
                color = "red", alpha = 0.1, width=0.2)

library(ggplot2)

ggplot(data = df_totalsum,
       aes(x = c(1:1636), y = df_totalsum[,"dnRAR_mean"]))+
  geom_line() + 
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_line(color = "grey")) +
  xlab("Lat - Med - Lat") + ylab("Intensity") +
  geom_errorbar(aes(ymin=df_totalsum[,"dnRAR_mean"]-df_totalsum[,"dnRAR_sem"], 
                    ymax=df_totalsum[,"dnRAR_mean"]+df_totalsum[,"dnRAR_sem"]), 
                color = "blue", alpha = 0.1, width=0.2)
       